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
#' `num_comp` maps to the number of components and tunes over 1 to
#' `PLS_MAX_COMP`; the runners cap that range further by the data
#' ([cap_pls_components()]). Registering twice in one session is a no-op.
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
    func         = list(pkg = "dials", fun = "num_comp", range = c(1L, PLS_MAX_COMP)),
    has_submodel = FALSE
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
#' scaled. `ncomp` is capped at the number of predictors and at the rows less
#' one, the most components the data can hold, so a fit asked for more uses
#' that many; the tuning range is capped the same way before a grid is drawn.
#'
#' @param x Numeric matrix or data frame of predictors.
#' @param y Numeric vector, the outcome.
#' @param ncomp Integer. The number of components.
#'
#' @return The `mvr` object from [pls::plsr()], with the number of components
#'   used in `horizons_ncomp`.
#' @keywords internal
#' @export
pls_fit <- function(x, y, ncomp = 2L) {

  x     <- as.matrix(x)
  ncomp <- as.integer(max(1L, min(ncomp, ncol(x), nrow(x) - 1L)))

  data   <- data.frame(y = y)
  data$x <- x

  fit <- pls::plsr(y ~ x, data = data, ncomp = ncomp, scale = TRUE,
                   method = "oscorespls", model = FALSE)

  fit$horizons_ncomp <- ncomp
  fit

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


#' Cap the plsr model's component range by the data
#'
#' @description
#' Sets `num_comp`'s upper bound to the smallest of `PLS_MAX_COMP`, the number
#' of predictors the recipe hands the model, and the smallest analysis set's
#' rows less one, so a grid or a Bayesian search proposes no component count
#' a fold cannot fit (#216). A parameter set without `num_comp` is returned
#' unchanged.
#'
#' @param param_set A `parameters` object, after [dials::finalize()].
#' @param n_predictors Integer. Predictors after the recipe.
#' @param n_rows Integer. Rows in the smallest analysis set.
#'
#' @return The parameter set.
#' @keywords internal
#' @noRd
cap_pls_components <- function(param_set, n_predictors, n_rows) {

  if (!"num_comp" %in% param_set$name) return(param_set)

  upper <- as.integer(max(1L, min(PLS_MAX_COMP, n_predictors, n_rows - 1L)))

  stats::update(param_set, num_comp = dials::num_comp(c(1L, upper)))

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
