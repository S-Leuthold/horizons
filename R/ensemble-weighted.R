# Weighted-average meta-learner for the ensemble() verb
#
# The simplest of the three engines, and the one that proves the contract
# end to end: it derives per-member weights from the out-of-fold matrix, asks
# each member to predict the held-out Split-F test set, combines those by the
# weights, and packs the unified ensemble contract. No tuning, no meta-model —
# just inverse-RMSE (or equal) weighting. Built first precisely because it
# exercises every part of the contract with the least machinery.

## ---------------------------------------------------------------------------
## fit_ensemble_weighted()
## ---------------------------------------------------------------------------

#' Fit a Weighted-Average Ensemble
#'
#' @description
#' Combines member predictions by a fixed weighted average. With
#' `optimize = TRUE` (default) weights are proportional to inverse out-of-fold
#' RMSE, so more accurate members contribute more; with `optimize = FALSE`
#' every member is weighted equally (a plain average — the honest no-tuning
#' baseline).
#'
#' The weights come from the out-of-fold matrix, but the reported `test_F`
#' predictions come from each member predicting the held-out set fresh (via
#' [predict_one_config()], which predicts once and back-transforms once) and
#' then being combined. This keeps the evaluation honest: weights are learned
#' on OOF data, performance is measured on data no member saw.
#'
#' @param object A `horizons_fit` object.
#' @param members Character vector of member `config_id`s (from
#'   [gather_members()]).
#' @param oof The out-of-fold matrix list from [build_oof_matrix()].
#' @param rank_metric Character. Metric the object was ranked by (for the
#'   improvement comparison).
#' @param optimize Logical. Inverse-RMSE weighting (`TRUE`) or equal weights
#'   (`FALSE`). Default `TRUE`.
#'
#' @return The ensemble contract list from [build_ensemble_contract()].
#'
#' @keywords internal
fit_ensemble_weighted <- function(object,
                                  members,
                                  oof,
                                  rank_metric,
                                  optimize = TRUE) {

  started <- Sys.time()

  ## -------------------------------------------------------------------------
  ## Step 1: Derive weights from the out-of-fold matrix
  ## -------------------------------------------------------------------------

  ## Per-member OOF RMSE against truth. Column order follows oof$members, so
  ## the weight vector lines up with the member columns by position.

  member_rmse <- vapply(
    oof$members,
    function(m) {
      yardstick::rmse_vec(oof$truth, oof$predictors[[paste0("member_", m)]],
                          na_rm = TRUE)
    },
    numeric(1)
  )

  raw_weights <- if (optimize) {

    ## Inverse RMSE: better members weighted higher. Guard a zero RMSE (a
    ## member that fit the OOF data perfectly) against division by zero.
    inv <- 1 / pmax(member_rmse, .Machine$double.eps)
    inv / sum(inv)

  } else {

    rep(1 / length(members), length(members))

  }

  names(raw_weights) <- oof$members

  weights <- tibble::tibble(
    member = oof$members,
    coef   = as.numeric(raw_weights)
  )

  ## -------------------------------------------------------------------------
  ## Step 2: Combined out-of-fold predictions (the Phase-2 UQ by-product)
  ## -------------------------------------------------------------------------

  ## Weighted combination of the OOF member predictions. Carried on the
  ## contract for Phase-2 ensemble UQ (conformal calibrates on these
  ## residuals); not used for the Phase-1 point predictions.

  oof_mat        <- as.matrix(oof$predictors[, paste0("member_", oof$members)])
  oof_combined   <- as.numeric(oof_mat %*% raw_weights)

  oof_pred <- tibble::tibble(
    .row  = oof$row,
    .pred = floor_at_zero(oof_combined),
    truth = oof$truth
  )

  ## -------------------------------------------------------------------------
  ## Step 3: Members predict the held-out test set, then combine
  ## -------------------------------------------------------------------------

  ## Split F's assessment set is the held-out evaluation data fit() reserved.
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

  ## Combine per-member test_F predictions by the weights, sample-aligned.
  ensemble_pred <- member_pred %>%
    dplyr::left_join(weights, by = c("config_id" = "member")) %>%
    dplyr::group_by(.data$sample_id) %>%
    dplyr::summarise(
      .pred = sum(.data$.pred * .data$coef),
      truth = dplyr::first(.data$truth),
      .groups = "drop"
    )

  ensemble_pred$.pred <- floor_at_zero(ensemble_pred$.pred)

  ## -------------------------------------------------------------------------
  ## Step 4: Pack the contract (scoring is centralized in the builder)
  ## -------------------------------------------------------------------------

  build_ensemble_contract(
    method        = "weighted",
    model         = weights,
    weights       = weights,
    ensemble_pred = ensemble_pred,
    member_pred   = member_pred,
    rank_metric   = rank_metric,
    runtime_secs  = as.numeric(difftime(Sys.time(), started, units = "secs")),
    oof_pred      = oof_pred
  )

}
