# XGBoost meta-learner for the ensemble() verb
#
# The non-linear combiner: an xgboost model fit on the out-of-fold matrix
# (members as features, truth as outcome). Unlike the penalized engine it can
# learn interactions between members, but over a handful of meta-features it
# can also overfit and underperform the best single member — the design spec
# (ensemble-design.md §3c) calls this out, and the contract's `improvement`
# field reports it honestly when it happens.
#
# Shares the tune -> refit -> combine-through-model scaffold in
# fit_tuned_meta_learner(); this file supplies only the xgboost spec, the grid,
# and the importance-to-weights extractor. Defaults are conservative for a
# small meta-feature space: shallow trees, modest learning rate.

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
#' @param seed Integer. Seed for the CV folds (passed through to
#'   [fit_tuned_meta_learner()] so the meta-OOF is reproducible).
#'
#' @return The ensemble contract list from [build_ensemble_contract()].
#'
#' @keywords internal
fit_ensemble_xgb <- function(object,
                             members,
                             oof,
                             rank_metric,
                             optimize = TRUE,
                             seed     = DEFAULT_ENSEMBLE_SEED) {

  ## Build the spec per branch, not with an inline `if` inside boost_tree().
  ## parsnip stores model args as lazy quosures it never forces, so an
  ## `trees = if (optimize) tune() else 500` argument keeps the unevaluated
  ## quosure `^if (...) tune() else 500`; tune_args() reads `tune()` from the
  ## expression text and flags the arg tunable even when optimize = FALSE,
  ## tripping fit_resamples with "arguments have been tagged for tuning".
  ## Branching keeps each spec's args literal. (See ensemble-penalized.R.)

  spec <- if (optimize) {

    parsnip::boost_tree(
      trees      = tune::tune(),
      tree_depth = tune::tune(),
      learn_rate = tune::tune()
    ) %>%
      parsnip::set_engine("xgboost") %>%
      parsnip::set_mode("regression")

  } else {

    parsnip::boost_tree(
      trees      = 500,
      tree_depth = 3,
      learn_rate = 0.05
    ) %>%
      parsnip::set_engine("xgboost") %>%
      parsnip::set_mode("regression")

  }

  grid <- tidyr::expand_grid(
    trees      = c(300, 500, 800),
    tree_depth = c(2, 3, 4),
    learn_rate = c(0.01, 0.05, 0.1)
  )

  fit_tuned_meta_learner(
    object          = object,
    members         = members,
    oof             = oof,
    rank_metric     = rank_metric,
    optimize        = optimize,
    method          = "xgb",
    spec            = spec,
    grid            = grid,
    extract_weights = extract_weights_xgb,
    seed            = seed
  )

}

## ---------------------------------------------------------------------------
## extract_weights_xgb()
## ---------------------------------------------------------------------------

#' Extract Member Weights from a Fitted XGBoost Meta-Learner
#'
#' @description
#' Reads xgboost feature importance (Gain) as the member weighting and maps it
#' back to `config_id`s. Members the model never split on are absent from the
#' importance table and receive a weight of 0.
#'
#' @param meta_fit The fitted meta-learner workflow.
#' @param members Character vector of member `config_id`s, in matrix-column
#'   order.
#' @return Tibble with `member` and `coef`.
#' @keywords internal
extract_weights_xgb <- function(meta_fit, members) {

  imp <- xgboost::xgb.importance(
    model = hardhat::extract_fit_engine(meta_fit)
  )

  weights <- tibble::tibble(member = members) %>%
    dplyr::left_join(
      tibble::tibble(
        member = sub("^member_", "", imp$Feature),
        coef   = imp$Gain
      ),
      by = "member"
    )
  weights$coef[is.na(weights$coef)] <- 0

  weights

}
