## ---------------------------------------------------------------------------
## The plsr model: parsnip's pls() on the CRAN pls package
## ---------------------------------------------------------------------------
## parsnip defines the pls() model but registers no engine for it. horizons
## registers one, "pls", at load (register_pls_engine(), called from
## .onLoad()), so the plsr model needs nothing beyond CRAN.


#' Register the "pls" engine for parsnip's pls() model
#'
#' @description
#' Fits with [pls_fit()] and predicts with [pls_predict()], for regression.
#' `num_comp` maps to the number of components. Its range is not set here:
#' the runners set it from `config$tuning$ranges` (`TUNE_RANGES` holds 1 to
#' `PLS_MAX_COMP`) and cap it by the data ([cap_by_data()]), so the table is
#' the one place it is kept. It is a submodel, as `MODEL_SPECS$plsr` names
#' it: tune fits the counts of a grid once, at the largest, and the engine
#' scores every count from that fit ([multi_predict._horizons_pls()]).
#' Registering twice in one session is a no-op.
#'
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
register_pls_engine <- function() {

  registered <- parsnip::get_from_env("pls")

  if (!is.null(registered) && any(registered$engine == "pls" & registered$mode == "regression")) {

    return(invisible(NULL))

  }

  parsnip::set_model_engine("pls", mode = "regression", eng = "pls")
  parsnip::set_dependency("pls", eng = "pls", pkg = "pls", mode = "regression")
  parsnip::set_dependency("pls", eng = "pls", pkg = "horizons", mode = "regression")

  parsnip::set_model_arg(
    model        = "pls",
    eng          = "pls",
    parsnip      = "num_comp",
    original     = "ncomp",
    func         = list(pkg = "dials", fun = "num_comp"),
    has_submodel = identical(engine_spec("pls")$submodel, "num_comp")
  )

  parsnip::set_fit(
    model = "pls",
    eng   = "pls",
    mode  = "regression",
    value = list(
      interface = "matrix",
      protect   = c("x", "y"),
      func      = c(pkg = "horizons", fun = "pls_fit"),
      defaults  = list()
    )
  )

  parsnip::set_encoding(
    model   = "pls",
    eng     = "pls",
    mode    = "regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept    = TRUE,
      remove_intercept     = TRUE,
      allow_sparse_x       = FALSE
    )
  )

  parsnip::set_pred(
    model = "pls",
    eng   = "pls",
    mode  = "regression",
    type  = "numeric",
    value = list(
      pre  = NULL,
      post = NULL,
      func = c(pkg = "horizons", fun = "pls_predict"),
      args = list(object = rlang::expr(object$fit), new_data = rlang::expr(new_data))
    )
  )

  invisible(NULL)

}


#' Fit a PLS regression for the plsr model
#'
#' @description
#' The fit function of the `"pls"` engine horizons registers for
#' [parsnip::pls()]: [pls::plsr()] by NIPALS, with the predictors centred and
#' scaled (a constant column is left unscaled, so it contributes nothing).
#' `ncomp` is capped at the number of predictors and at the rows less one, the
#' most components the data can hold. A fit asked for more uses that many and
#' warns, with class `horizons_warning_substitution`: the runners cap the tuning range from the whole training set, so a
#' fold whose recipe selects fewer predictors can still reach this cap.
#'
#' The fit also stops before a component that carries nothing more about the
#' outcome, and warns the same way. PLS with `a` components is the least-squares
#' fit over the first `a` directions of the Krylov space `X'y, (X'X)X'y, ...`,
#' and once that space holds every direction the outcome touches, the fit is
#' the least-squares fit on the predictors, and any later component would be
#' formed from rounding error. On nearly orthogonal predictors PLS also
#' reaches that fit, to within `PLS_TOL`, after a few components, and the
#' later ones would change it by less. So component `k` (from the second on) is fitted
#' only while what is left of the outcome after `k - 1` components is more
#' than `PLS_TOL` of the centred outcome's norm, its weight and score are
#' finite, and the component's covariance with that residual is more than
#' `PLS_TOL` times the norms of the scaled predictors and of the residual:
#' LSQR's two stopping tests (Paige and Saunders 1982), which are PLS's two
#' breakdowns. On PCA scores, which are uncorrelated and which scaling gives
#' one variance, the first component is already that fit, so every count
#' holds one component: plsr on them is the least-squares regression on the
#' kept components. On inputs where no component is degenerate nothing
#' changes. The first component is always fitted.
#'
#' While tuning, tune calls it at a grid stage's largest count, and the fit
#' predicts every count of the stage ([multi_predict._horizons_pls()]). Each
#' count's coefficients are what a fit at that count gives: NIPALS forms its
#' components in sequence, so a fit asked for fewer components than the stop
#' holds those, and one asked for more stops in the same place.
#'
#' @param x Numeric matrix or data frame of predictors.
#' @param y Numeric vector, the outcome.
#' @param ncomp Integer. The number of components.
#'
#' @return The `mvr` object from [pls::plsr()], of class `horizons_pls` as
#'   well (so parsnip's fit is `_horizons_pls`, which
#'   [multi_predict._horizons_pls()] serves), holding the components it
#'   used, their number in `horizons_ncomp`.
#' @keywords internal
#' @export
pls_fit <- function(x, y, ncomp = 2L) {

  x         <- as.matrix(x)
  requested <- as.integer(ncomp)
  count     <- pls_count(x, y, requested)
  fit       <- count$fit

  ### A stopped fit is refitted at the stop, so the object holds those
  ### components and no others, as a fit asked for that many does
  if (count$ncomp < fit$ncomp) fit <- pls_plsr(x, y, count$ncomp, count$sds)

  ## What ran differs from what was asked: the documented cap, or the stop
  if (count$reason != "asked") {

    warn_in_unit("substitution", pls_cap_message(count$ncomp, requested, ncol(x), nrow(x), count$reason))

  }

  fit$horizons_ncomp <- count$ncomp

  ### A class of horizons' own, which predict() still serves as an mvr
  class(fit) <- c("horizons_pls", class(fit))
  fit

}

#' The divisors a PLS fit scales its predictors by
#'
#' @description
#' Each column's standard deviation, and 1 for a column whose deviation is
#' below `PLS_TOL` or not finite: `scale = TRUE` would divide a constant
#' column by zero, which makes every prediction `NaN`. Also the Frobenius norm
#' of the centred predictors once divided, which [pls_informative()]
#' measures a component's covariance against.
#'
#' @param x Numeric matrix of predictors.
#' @return List with `sds` (the divisors, one per column) and `norm`.
#' @keywords internal
#' @noRd
pls_scale <- function(x) {

  sd  <- apply(x, 2, stats::sd)
  sds <- sd
  sds[!is.finite(sds) | sds < PLS_TOL] <- 1

  list(sds = sds, norm = sqrt((nrow(x) - 1) * sum((sd / sds)^2)))

}

#' The one pls::plsr() call of the plsr engine
#'
#' @param x Numeric matrix of predictors.
#' @param y Numeric vector, the outcome.
#' @param ncomp Integer. The components to fit.
#' @param sds Numeric. The divisors ([pls_scale()]).
#' @return The `mvr` object.
#' @keywords internal
#' @noRd
pls_plsr <- function(x, y, ncomp, sds) {

  ### A formula made here would carry this frame, x included, into the fit's
  ### terms and every saved copy of it.
  fml <- y ~ x
  environment(fml) <- baseenv()

  data   <- data.frame(y = y)
  data$x <- x

  pls::plsr(fml, data = data, ncomp = ncomp, scale = sds,
            method = "oscorespls", model = FALSE)

}

#' The components of a PLS fit that carry information about the outcome
#'
#' @description
#' The count of components before the first, from the second on, that fails
#' one of three tests, read from the fit's documented `scores`,
#' `loading.weights` and `residuals` (the outcome deflated by each component
#' in turn, centred). With `f_j` what is left of the centred outcome after `j`
#' components, component `k` is fitted only while
#'
#' 1. `f_(k-1)` is more than `PLS_TOL` of the centred outcome's norm: the
#'    outcome is not yet fitted exactly (LSQR's first test, Paige and
#'    Saunders 1982);
#' 2. `|t_k' f_(k-1)|`, the covariance of the component's score with that
#'    residual and the norm of its unnormalised weight, is more than
#'    `PLS_TOL` times `x_norm` times the residual's norm: the fit is not yet
#'    the least-squares fit (LSQR's second test);
#' 3. its weight and score are finite: pls returns a weight that is exactly
#'    zero as `NaN` (0 / 0), the exact case of 1 or 2.
#'
#' They are tested in the order 1, 3, 2, so a `NaN` never reaches a
#' comparison. Measuring the covariance against the predictors' norm rather
#' than the score's own catches a score that is itself rounding, when the
#' predictors' rank is spent. A first component that is not finite (no
#' predictor covaries with the outcome, or the outcome is constant) is left
#' as pls returns it, so the fit predicts `NaN`, as a fit of one component
#' does.
#'
#' The other PLS fits in horizons keep their own fits: CARS scores its
#' subsets with pls's own cross-validation, which returns no segment's fit to
#' test, and both it and `select_training(space = "pls")` fit spectral bands
#' centred and unscaled after dropping near-constant ones, whose Krylov space
#' does not end before their caps; the similarity space's count is the
#' user's `ncomp`.
#'
#' @param fit An `mvr` object from [pls_plsr()].
#' @param y Numeric vector, the outcome it was fitted to.
#' @param x_norm Numeric(1). The Frobenius norm of the centred, scaled
#'   predictors ([pls_scale()]).
#' @return Integer: the components that pass, at least 1, at most
#'   `fit$ncomp`.
#' @keywords internal
#' @noRd
pls_informative <- function(fit, y, x_norm) {

  scores  <- unclass(fit$scores)
  weights <- unclass(fit$loading.weights)

  ### n x 1 x ncomp, the one response's residual after each count
  residuals <- matrix(fit$residuals, nrow = length(y))

  if (!all(is.finite(weights[, 1])) || !all(is.finite(scores[, 1]))) return(fit$ncomp)

  f0_norm <- sqrt(sum((y - mean(y))^2))

  for (k in seq_len(fit$ncomp)[-1]) {

    f_prev <- residuals[, k - 1L]
    f_norm <- sqrt(sum(f_prev^2))

    ## 1. The outcome is fitted exactly
    if (f_norm <= PLS_TOL * f0_norm) return(k - 1L)

    ## 3. pls's 0 / 0: the weight was exactly zero
    if (!all(is.finite(weights[, k])) || !all(is.finite(scores[, k]))) return(k - 1L)

    ## 2. The fit is the least-squares fit on these predictors
    if (abs(sum(scores[, k] * f_prev)) <= PLS_TOL * x_norm * f_norm) return(k - 1L)

  }

  fit$ncomp

}

#' The components a PLS fit of a requested count holds, and why
#'
#' @description
#' The count capped by the data ([pls_components()]), fitted, and stopped
#' at the components that carry information ([pls_informative()]): what
#' [pls_fit()] fits, and what [pls_cap_records()] reports a fold at.
#'
#' @param x Numeric matrix of predictors.
#' @param y Numeric vector, the outcome.
#' @param requested Integer. The count asked for.
#' @return List with `ncomp` (the components held), `reason` (`"asked"`,
#'   `"cap"` or `"stop"`, why it is not `requested`), `fit` (the fit at the
#'   capped count, before any stop) and `sds` (its divisors).
#' @keywords internal
#' @noRd
pls_count <- function(x, y, requested) {

  capped  <- pls_components(requested, ncol(x), nrow(x))
  scaling <- pls_scale(x)
  fit     <- pls_plsr(x, y, capped, scaling$sds)
  held    <- if (capped > 1L) pls_informative(fit, y, scaling$norm) else capped

  reason <- if (held < capped) "stop" else if (capped < requested) "cap" else "asked"

  list(ncomp = held, reason = reason, fit = fit, sds = scaling$sds)

}


#' Predict from a plsr fit at several component counts
#'
#' @description
#' The `num_comp` submodel of the `"pls"` engine (`MODEL_SPECS$plsr`): tune
#' fits a grid stage's counts once, with [pls_fit()] at the largest, and
#' predicts every count of the stage from that fit, in one call to the fit's
#' `predict()`. A count above the components the fit holds
#' (`horizons_ncomp`) is predicted at that many, as [pls_predict()] predicts
#' a fit [pls_fit()] capped or stopped; the fit's own warning says so.
#'
#' @param object A parsnip `model_fit` of the `"pls"` engine.
#' @param new_data Predictors, with the training predictors' columns in their
#'   order.
#' @param type Not used: the engine predicts numbers.
#' @param num_comp The counts to predict at; `NULL` for the count the fit
#'   holds.
#' @param ... Not used.
#'
#' @return A tibble with a list column `.pred`, one element per row of
#'   `new_data`: a tibble of `num_comp` (the counts asked for, in their order)
#'   and `.pred`.
#' @keywords internal
#' @exportS3Method parsnip::multi_predict "_horizons_pls"
multi_predict._horizons_pls <- function(object, new_data, type = NULL, num_comp = NULL, ...) {

  fit    <- object$fit
  counts <- num_comp %||% fit$horizons_ncomp
  used   <- pmin(as.integer(counts), fit$horizons_ncomp)
  at     <- sort(unique(used))

  new_data  <- parsnip::prepare_data(object, new_data)
  newdata   <- data.frame(row = seq_len(nrow(new_data)))
  newdata$x <- as.matrix(new_data)

  ### One column per distinct count: rows by counts
  pred <- matrix(stats::predict(fit, newdata = newdata, ncomp = at), nrow = nrow(new_data))
  pred <- pred[, match(used, at), drop = FALSE]

  tibble::tibble(.pred = lapply(seq_len(nrow(pred)), function(i) {
    tibble::tibble(num_comp = counts, .pred = pred[i, ])
  }))

}


#' The components a PLS fit holds
#'
#' @param requested Integer. The count asked for.
#' @param n_predictors,n_rows Integer. The predictors and rows it is fitted on.
#' @return Integer: `requested`, capped at the predictors and the rows less
#'   one, and at least 1.
#' @keywords internal
#' @noRd
pls_components <- function(requested, n_predictors, n_rows) {

  as.integer(max(1L, min(requested, n_predictors, n_rows - 1L)))

}

#' What a PLS fit capped or stopped below the count asked for says
#'
#' @description
#' [pls_fit()]'s warning, and the words a tuned plsr row reports a fold that
#' could not hold its chosen count in ([pls_cap_records()]), so both read the
#' same line. The cap and the stop share its lead, so one pattern
#' (`PLS_CAP_PATTERN`) finds both.
#'
#' @param fitted,requested Integer. The components fitted, and asked for.
#' @param n_predictors,n_rows Integer. The predictors and rows fitted on.
#' @param reason Character(1). `"cap"`, the data's dimensions, or `"stop"`,
#'   the components after `fitted` carrying nothing ([pls_informative()]).
#' @return Character(1).
#' @keywords internal
#' @noRd
pls_cap_message <- function(fitted, requested, n_predictors, n_rows, reason = "cap") {

  if (identical(reason, "stop")) {

    first <- if (fitted == 1) "the first already fits" else cli::format_inline("the first {fitted} already fit")

    return(cli::format_inline(
      "PLS fitted {fitted} component{?s}, not the {requested} asked for: {first} the outcome as closely as these predictors can, so any further component would fit rounding error."
    ))

  }

  cli::format_inline(
    "PLS fitted {fitted} component{?s}, not the {requested} asked for: the data have {n_predictors} predictor{?s} and {n_rows} row{?s}."
  )

}

### Matches pls_cap_message()'s text, the cap's and the stop's
PLS_CAP_PATTERN <- "^PLS fitted [0-9]+ components?, not the [0-9]+ asked for: "

#' A tuned plsr row's record of the folds that could not hold its count
#'
#' @description
#' One fit serves every count of a grid stage (`num_comp` is a submodel), at
#' the stage's largest count, so [pls_fit()]'s warning in tune's notes names
#' the stage's count, not the chosen candidate's: read for the candidate, a
#' fold that capped a larger count would be reported on a row whose count it
#' held (#109). The runners leave those notes off ([drop_pls_caps()]) and
#' report the chosen count here instead, fold by fold, in [pls_fit()]'s
#' words: each fold whose predictors, or analysis rows less one, are fewer
#' than the count. `fit()` needs none: its out-of-fold fits are at the chosen
#' count, and their own warnings say it.
#'
#' @param folds The `rset` tuned over, assembled from the fold preps
#'   ([assemble_folds()]): each fold's frame holds the outcome and that
#'   fold's predictors.
#' @param best_params One-row tibble of the chosen hyperparameters.
#' @return Records shaped like [new_warning_log()]'s, as [tune_note_records()]
#'   gives them: scope `"cv"`, one per distinct text with the folds that
#'   raised it. Empty when the count fits every fold, or when `num_comp` was
#'   not tuned.
#' @keywords internal
#' @noRd
pls_cap_records <- function(folds, best_params) {

  if (!"num_comp" %in% names(best_params)) return(new_warning_log())

  requested <- as.integer(best_params$num_comp[[1]])

  ### Each capped fold's warning as it would arrive from the fit, grouped as
  ### the fold preps' warnings are
  arrivals <- Map(function(split, fold) {
    n_predictors <- ncol(split$data) - 1L
    n_rows       <- length(split$in_id)
    fitted       <- pls_components(requested, n_predictors, n_rows)
    if (fitted < requested) {
      parts <- condition_parts(pls_cap_message(fitted, requested, n_predictors, n_rows), max_chars = Inf)
      tibble::tibble(cause = parts$cause, where = parts$where, fold = fold)
    }
  }, folds$splits, resample_ids(folds))

  fold_warning_records(arrivals, nrow(folds))

}

#' Warning records without the PLS engine's cap
#'
#' @param records Records shaped like [new_warning_log()]'s, read from the
#'   tuning's notes.
#' @return `records` less those holding [pls_fit()]'s warning
#'   ([pls_cap_records()] says why).
#' @keywords internal
#' @noRd
drop_pls_caps <- function(records) {

  records[!grepl(PLS_CAP_PATTERN, records$cause), , drop = FALSE]

}


#' Predict from a plsr fit
#'
#' @description
#' The predict function of the `"pls"` engine: the prediction at the number of
#' components [pls_fit()] used.
#'
#' @param object An `mvr` object from [pls_fit()].
#' @param new_data Numeric matrix or data frame of predictors, with the
#'   training predictors' columns in their order.
#'
#' @return Numeric vector, one prediction per row.
#' @keywords internal
#' @export
pls_predict <- function(object, new_data) {

  newdata   <- data.frame(row = seq_len(nrow(new_data)))
  newdata$x <- as.matrix(new_data)

  as.numeric(stats::predict(object, newdata = newdata, ncomp = object$horizons_ncomp))

}


#' Rows in the smallest analysis set of an rset
#'
#' @param resamples An `rset`.
#' @return Integer.
#' @keywords internal
#' @noRd
min_analysis_rows <- function(resamples) {

  min(vapply(resamples$splits, function(s) length(s$in_id), integer(1)))

}
