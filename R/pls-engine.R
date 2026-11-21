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
#' While tuning, tune calls it at a grid stage's largest count, and the fit
#' predicts every count of the stage ([multi_predict._horizons_pls()]).
#'
#' @param x Numeric matrix or data frame of predictors.
#' @param y Numeric vector, the outcome.
#' @param ncomp Integer. The number of components.
#'
#' @return The `mvr` object from [pls::plsr()], of class `horizons_pls` as
#'   well (so parsnip's fit is `_horizons_pls`, which
#'   [multi_predict._horizons_pls()] serves), with the number of components
#'   used in `horizons_ncomp`.
#' @keywords internal
#' @export
pls_fit <- function(x, y, ncomp = 2L) {

  x         <- as.matrix(x)
  requested <- as.integer(ncomp)
  ncomp     <- pls_components(requested, ncol(x), nrow(x))

  ## What ran differs from what was asked, under the documented cap
  if (ncomp < requested) {

    warn_in_unit("substitution", pls_cap_message(ncomp, requested, ncol(x), nrow(x)))

  }

  ### scale = TRUE divides a constant column by a zero sd, which makes every
  ### prediction NaN.
  sds <- apply(x, 2, stats::sd)
  sds[!is.finite(sds) | sds < sqrt(.Machine$double.eps)] <- 1

  ### A formula made here would carry this frame, x included, into the fit's
  ### terms and every saved copy of it.
  fml <- y ~ x
  environment(fml) <- baseenv()

  data   <- data.frame(y = y)
  data$x <- x

  fit <- pls::plsr(fml, data = data, ncomp = ncomp, scale = sds,
                   method = "oscorespls", model = FALSE)

  fit$horizons_ncomp <- ncomp

  ### A class of horizons' own, which predict() still serves as an mvr
  class(fit) <- c("horizons_pls", class(fit))
  fit

}


#' Predict from a plsr fit at several component counts
#'
#' @description
#' The `num_comp` submodel of the `"pls"` engine (`MODEL_SPECS$plsr`): tune
#' fits a grid stage's counts once, with [pls_fit()] at the largest, and
#' predicts every count of the stage from that fit, in one call to the fit's
#' `predict()`. A count above the components the fit holds
#' (`horizons_ncomp`) is predicted at that many, as [pls_predict()] predicts
#' a fit [pls_fit()] capped; the fit's own warning says so.
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

#' What a PLS fit capped below the count asked for says
#'
#' @description
#' [pls_fit()]'s warning, and the words a tuned plsr row reports a fold that
#' could not hold its chosen count in ([pls_cap_records()]), so both read the
#' same line.
#'
#' @param fitted,requested Integer. The components fitted, and asked for.
#' @param n_predictors,n_rows Integer. The predictors and rows fitted on.
#' @return Character(1).
#' @keywords internal
#' @noRd
pls_cap_message <- function(fitted, requested, n_predictors, n_rows) {

  cli::format_inline(
    "PLS fitted {fitted} component{?s}, not the {requested} asked for: the data have {n_predictors} predictor{?s} and {n_rows} row{?s}."
  )

}

### Matches pls_cap_message()'s text
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
