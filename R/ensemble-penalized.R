# Penalized-regression meta-learner for the ensemble() verb
#
# The default meta-learner: a glmnet penalized linear regression fit on the
# out-of-fold matrix (members as predictors, truth as outcome). Where the
# weighted engine combines members by a fixed weight vector, this one learns
# the combination — an intercept plus regularized coefficients — and combines
# new predictions by running them THROUGH the fitted model.
#
# The shared tune -> refit -> combine-through-model scaffold lives in
# fit_tuned_meta_learner(); this file supplies only the glmnet spec, the
# penalty/mixture grid, and the coefficient-to-weights extractor.

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
#' coefficients, for interpretation) are distinct.
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

  spec <- parsnip::linear_reg(
    penalty = if (optimize) tune::tune() else 0.01,
    mixture = if (optimize) tune::tune() else 1
  ) %>%
    parsnip::set_engine("glmnet")

  grid <- tidyr::expand_grid(
    penalty = 10^seq(-6, -1, length.out = 20),
    mixture = seq(0, 1, length.out = 10)
  )

  fit_tuned_meta_learner(
    object          = object,
    members         = members,
    oof             = oof,
    rank_metric     = rank_metric,
    optimize        = optimize,
    method          = "penalized",
    spec            = spec,
    grid            = grid,
    extract_weights = extract_weights_penalized
  )

}

## ---------------------------------------------------------------------------
## extract_weights_penalized()
## ---------------------------------------------------------------------------

#' Extract Member Weights from a Fitted glmnet Meta-Learner
#'
#' @description
#' Reads the glmnet coefficients at the chosen penalty and maps them back to
#' member `config_id`s, dropping the intercept. Members glmnet zeroed out keep
#' a coefficient of 0.
#'
#' @param meta_fit The fitted meta-learner workflow.
#' @param members Character vector of member `config_id`s, in matrix-column
#'   order.
#' @return Tibble with `member` and `coef`.
#' @keywords internal
extract_weights_penalized <- function(meta_fit, members) {

  penalty <- best_penalty(workflows::extract_spec_parsnip(meta_fit))

  coefs <- tibble::as_tibble(
    as.matrix(stats::coef(hardhat::extract_fit_engine(meta_fit), s = penalty)),
    rownames = "term"
  )
  names(coefs)[2] <- "coef"

  weights <- tibble::tibble(member = members) %>%
    dplyr::left_join(
      dplyr::transmute(coefs,
                       member = sub("^member_", "", .data$term),
                       coef   = .data$coef),
      by = "member"
    )
  weights$coef[is.na(weights$coef)] <- 0

  weights

}

## ---------------------------------------------------------------------------
## best_penalty()
## ---------------------------------------------------------------------------

#' Extract the Chosen Penalty from a Finalized Model Spec
#'
#' @description
#' glmnet's `coef()` needs the `s` (penalty) at which to read coefficients.
#' Pull it from the (finalized) model spec.
#'
#' @param spec A `parsnip` model spec with a resolved `penalty`.
#' @return Numeric penalty value.
#' @keywords internal
best_penalty <- function(spec) {

  rlang::eval_tidy(spec$args$penalty)

}
