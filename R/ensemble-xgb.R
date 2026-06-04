# XGBoost meta-learner for the ensemble() verb
#
# The non-linear combiner: an xgboost model fit on the out-of-fold matrix
# (members as features, truth as outcome). Unlike the penalized engine, it can
# learn interactions between members, but over a handful of meta-features it
# can also overfit and underperform the best single member — the design spec
# (ensemble-design.md §3c) calls this out, and the contract's `improvement`
# field reports it honestly when it happens.
#
# Defaults are conservative for a small meta-feature space: shallow trees,
# modest learning rate. Structure mirrors fit_ensemble_penalized() — tune on
# the OOF matrix, refit, combine THROUGH the fitted model — swapping the
# glmnet spec for xgboost and coefficients for feature importances.

## ---------------------------------------------------------------------------
## fit_ensemble_xgb()
## ---------------------------------------------------------------------------

#' Fit an XGBoost Meta-Learner Ensemble
#'
#' @description
#' Trains an xgboost model as the meta-learner over member out-of-fold
#' predictions. With `optimize = TRUE` (default), `trees`, `tree_depth`, and
#' `learn_rate` are tuned by CV on the OOF matrix; with `optimize = FALSE`,
#' conservative defaults for a small meta-feature space are used
#' (`trees = 500`, `tree_depth = 3`, `learn_rate = 0.05`).
#'
#' The meta-model's relative weighting of members is reported as feature
#' importance (xgboost has no coefficients); the `model` slot holds the fitted
#' workflow used to combine new predictions.
#'
#' @param object A `horizons_fit` object.
#' @param members Character vector of member `config_id`s.
#' @param oof The out-of-fold matrix list from [build_oof_matrix()].
#' @param rank_metric Character. Metric the object was ranked by.
#' @param optimize Logical. Tune the xgboost hyperparameters (`TRUE`) or use
#'   conservative fixed defaults (`FALSE`). Default `TRUE`.
#'
#' @return The ensemble contract list from [build_ensemble_contract()].
#'
#' @keywords internal
#' @importFrom rlang .data
fit_ensemble_xgb <- function(object,
                             members,
                             oof,
                             rank_metric,
                             optimize = TRUE) {

  started <- Sys.time()

  member_cols <- paste0("member_", oof$members)

  ## -------------------------------------------------------------------------
  ## Step 1: Meta-training frame + spec
  ## -------------------------------------------------------------------------

  meta_frame <- tibble::as_tibble(oof$predictors[, member_cols, drop = FALSE])
  meta_frame$.truth <- oof$truth

  ## Conservative defaults for a small meta-feature space; tuned when asked.
  meta_spec <- parsnip::boost_tree(
    trees      = if (optimize) tune::tune() else 500,
    tree_depth = if (optimize) tune::tune() else 3,
    learn_rate = if (optimize) tune::tune() else 0.05
  ) %>%
    parsnip::set_engine("xgboost") %>%
    parsnip::set_mode("regression")

  meta_wflow <- workflows::workflow() %>%
    workflows::add_model(meta_spec) %>%
    workflows::add_formula(.truth ~ .)

  ## -------------------------------------------------------------------------
  ## Step 2: Tune by CV on the OOF matrix (leakage-clean — see penalized)
  ## -------------------------------------------------------------------------

  if (optimize) {

    folds <- rsample::vfold_cv(meta_frame, v = 5)

    grid <- tidyr::expand_grid(
      trees      = c(300, 500, 800),
      tree_depth = c(2, 3, 4),
      learn_rate = c(0.01, 0.05, 0.1)
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
      error_title = "Meta-learner tuning failed for the xgb ensemble."
    )

    best        <- tune::select_best(tune_res, metric = rank_metric)
    final_wflow <- tune::finalize_workflow(meta_wflow, best)

  } else {

    final_wflow <- meta_wflow

  }

  ## -------------------------------------------------------------------------
  ## Step 3: Refit on the full OOF matrix
  ## -------------------------------------------------------------------------

  fit_safe <- safely_execute(
    parsnip::fit(final_wflow, data = meta_frame),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  meta_fit <- handle_results(
    fit_safe,
    error_title = "Meta-learner refit failed for the xgb ensemble."
  )

  ## Feature importance as the reported member weighting (xgb has no coefs).
  ## Members the model never split on are absent from importance -> weight 0.
  imp <- xgboost::xgb.importance(
    model = hardhat::extract_fit_engine(meta_fit)
  )

  weights <- tibble::tibble(member = oof$members) %>%
    dplyr::left_join(
      tibble::tibble(
        member = sub("^member_", "", imp$Feature),
        coef   = imp$Gain
      ),
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

  test_wide <- member_pred %>%
    dplyr::select("config_id", "sample_id", ".pred") %>%
    tidyr::pivot_wider(names_from   = "config_id",
                       values_from  = ".pred",
                       names_prefix = "member_")

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
  ## Step 6: Pack the contract
  ## -------------------------------------------------------------------------

  build_ensemble_contract(
    method        = "xgb",
    model         = meta_fit,
    weights       = weights,
    ensemble_pred = ensemble_pred,
    member_pred   = member_pred,
    rank_metric   = rank_metric,
    runtime_secs  = as.numeric(difftime(Sys.time(), started, units = "secs")),
    oof_pred      = oof_pred
  )

}
