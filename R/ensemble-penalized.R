# Penalized-regression meta-learner for the ensemble() verb
#
# The default meta-learner: a glmnet penalized linear regression fit on the
# out-of-fold matrix (members as predictors, truth as outcome). Where the
# weighted engine combines members by a fixed weight vector, this one learns
# the combination — an intercept plus regularized coefficients — and combines
# new predictions by running them THROUGH the fitted model, not a manual sum.
#
# Tuning follows Option 1 (see ensemble-design.md): penalty/mixture are tuned
# by CV on the OOF matrix and the winner is refit on the full matrix. The
# tuning resamples come only from the OOF matrix; test_F is never in the
# tuning path, so the reported performance is honest.

## ---------------------------------------------------------------------------
## fit_ensemble_penalized()
## ---------------------------------------------------------------------------

#' Fit a Penalized-Regression Ensemble
#'
#' @description
#' Trains a glmnet penalized linear regression as the meta-learner over member
#' out-of-fold predictions. With `optimize = TRUE` (default), `penalty` and
#' `mixture` are tuned by k-fold CV on the OOF matrix; with `optimize = FALSE`,
#' fixed defaults are used (`penalty = 0.01`, `mixture = 1`).
#'
#' Combination of new predictions runs through the fitted meta-model, so the
#' `model` slot (the fitted workflow) and the `weights` slot (the glmnet
#' coefficients, for interpretation) are distinct — unlike the weighted engine,
#' where they coincide.
#'
#' @param object A `horizons_fit` object.
#' @param members Character vector of member `config_id`s.
#' @param oof The out-of-fold matrix list from [build_oof_matrix()].
#' @param rank_metric Character. Metric the object was ranked by.
#' @param optimize Logical. Tune penalty/mixture (`TRUE`) or use fixed
#'   defaults (`FALSE`). Default `TRUE`.
#'
#' @return The ensemble contract list from [build_ensemble_contract()].
#'
#' @keywords internal
#' @importFrom rlang .data
fit_ensemble_penalized <- function(object,
                                   members,
                                   oof,
                                   rank_metric,
                                   optimize = TRUE) {

  started <- Sys.time()

  member_cols <- paste0("member_", oof$members)

  ## -------------------------------------------------------------------------
  ## Step 1: Meta-training frame (the canonical OOF matrix + truth)
  ## -------------------------------------------------------------------------

  meta_frame <- tibble::as_tibble(oof$predictors[, member_cols, drop = FALSE])
  meta_frame$.truth <- oof$truth

  meta_spec <- parsnip::linear_reg(
    penalty = if (optimize) tune::tune() else 0.01,
    mixture = if (optimize) tune::tune() else 1
  ) %>%
    parsnip::set_engine("glmnet")

  meta_wflow <- workflows::workflow() %>%
    workflows::add_model(meta_spec) %>%
    workflows::add_formula(.truth ~ .)

  ## -------------------------------------------------------------------------
  ## Step 2: Tune penalty/mixture by CV on the OOF matrix (leakage-clean)
  ## -------------------------------------------------------------------------

  ## The resamples are drawn ONLY from the OOF matrix. test_F never enters the
  ## tuning path — each member's OOF prediction was already produced without
  ## seeing its own assessment fold, so tuning on these is honest.

  if (optimize) {

    folds <- rsample::vfold_cv(meta_frame, v = 5)

    grid <- tidyr::expand_grid(
      penalty = 10^seq(-6, -1, length.out = 20),
      mixture = seq(0, 1, length.out = 10)
    )

    metric_set <- yardstick::metric_set(yardstick::rmse, rrmse, yardstick::rsq,
                                        ccc, rpd, yardstick::mae)

    tune_safe <- safely_execute(
      tune::tune_grid(meta_wflow,
                      resamples = folds,
                      grid      = grid,
                      metrics   = metric_set,
                      control   = tune::control_grid(save_pred = TRUE)),
      log_error          = FALSE,
      capture_conditions = TRUE
    )

    tune_res <- handle_results(
      tune_safe,
      error_title = "Meta-learner tuning failed for the penalized ensemble."
    )

    best <- tune::select_best(tune_res, metric = rank_metric)

    final_wflow <- tune::finalize_workflow(meta_wflow, best)

  } else {

    final_wflow <- meta_wflow

  }

  ## -------------------------------------------------------------------------
  ## Step 3: Refit the meta-model on the full OOF matrix
  ## -------------------------------------------------------------------------

  fit_safe <- safely_execute(
    parsnip::fit(final_wflow, data = meta_frame),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  meta_fit <- handle_results(
    fit_safe,
    error_title = "Meta-learner refit failed for the penalized ensemble."
  )

  ## Coefficients (for interpretation/reporting) — map glmnet terms back to
  ## config_ids, drop the intercept. Members glmnet zeroed out keep coef 0.
  coefs <- tibble::as_tibble(hardhat::extract_fit_engine(meta_fit) %>%
                               stats::coef(s = best_penalty(final_wflow)) %>%
                               as.matrix(),
                             rownames = "term")
  names(coefs)[2] <- "coef"

  weights <- tibble::tibble(member = oof$members) %>%
    dplyr::left_join(
      dplyr::transmute(coefs,
                       member = sub("^member_", "", .data$term),
                       coef   = .data$coef),
      by = "member"
    )
  weights$coef[is.na(weights$coef)] <- 0

  ## -------------------------------------------------------------------------
  ## Step 4: Combined out-of-fold predictions (Phase-2 UQ by-product)
  ## -------------------------------------------------------------------------

  oof_combined <- stats::predict(meta_fit, new_data = meta_frame)$.pred

  oof_pred <- tibble::tibble(
    .row  = oof$row,
    .pred = floor_at_zero(oof_combined),
    truth = oof$truth
  )

  ## -------------------------------------------------------------------------
  ## Step 5: Members predict test_F; combine THROUGH the fitted meta-model
  ## -------------------------------------------------------------------------

  test_data   <- rsample::assessment(object$models$split)
  role_map    <- object$data$role_map
  outcome_col <- role_map$variable[role_map$role == "outcome"]

  member_pred <- dplyr::bind_rows(lapply(members, function(m) {

    pc <- predict_one_config(object, config_id = m, new_spectra = test_data,
                             interval = FALSE)

    tibble::tibble(
      config_id = m,
      sample_id = pc$sample_id,
      .pred     = pc$.pred,
      truth     = test_data[[outcome_col]]
    )

  }))

  ## Assemble the wide member-prediction matrix with the SAME column names the
  ## meta-model trained on, so the fitted workflow recognizes the features.
  test_wide <- member_pred %>%
    dplyr::select("config_id", "sample_id", ".pred") %>%
    tidyr::pivot_wider(names_from   = "config_id",
                       values_from  = ".pred",
                       names_prefix = "member_")

  ## One truth value per sample (members share it).
  truth_lookup <- dplyr::distinct(member_pred, .data$sample_id, .data$truth)

  combined <- stats::predict(
    meta_fit,
    new_data = test_wide[, member_cols, drop = FALSE]
  )$.pred

  ensemble_pred <- tibble::tibble(
    sample_id = test_wide$sample_id,
    .pred     = floor_at_zero(combined),
    truth     = truth_lookup$truth[match(test_wide$sample_id,
                                         truth_lookup$sample_id)]
  )

  ## -------------------------------------------------------------------------
  ## Step 6: Pack the contract (scoring centralized in the builder)
  ## -------------------------------------------------------------------------

  build_ensemble_contract(
    method        = "penalized",
    model         = meta_fit,
    weights       = weights,
    ensemble_pred = ensemble_pred,
    member_pred   = member_pred,
    rank_metric   = rank_metric,
    runtime_secs  = as.numeric(difftime(Sys.time(), started, units = "secs")),
    oof_pred      = oof_pred
  )

}

## ---------------------------------------------------------------------------
## best_penalty()
## ---------------------------------------------------------------------------

#' Extract the Chosen Penalty from a Finalized Workflow
#'
#' @description
#' glmnet's `coef()` needs the `s` (penalty) at which to read coefficients.
#' Pull it from the finalized workflow's model spec.
#'
#' @param wflow A finalized `workflow`.
#' @return Numeric penalty value.
#' @keywords internal
best_penalty <- function(wflow) {

  spec    <- workflows::extract_spec_parsnip(wflow)
  penalty <- spec$args$penalty

  rlang::eval_tidy(penalty)

}
