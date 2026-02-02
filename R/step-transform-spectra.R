#' Spectral Preprocessing Recipe Step
#'
#' @description
#' Custom recipes step that applies row-wise spectral preprocessing using
#' prospectr methods (SNV, Savitzky-Golay derivatives, combinations).
#' Designed for wide-format spectral data where columns are wavenumbers.
#'
#' Supported preprocessing methods:
#' - `"raw"` — trim edge artifacts only (no transformation)
#' - `"sg"` — Savitzky-Golay smoothing (0th derivative)
#' - `"snv"` — Standard Normal Variate + edge trimming
#' - `"deriv1"` — Savitzky-Golay 1st derivative
#' - `"deriv2"` — Savitzky-Golay 2nd derivative
#' - `"snv_deriv1"` — SNV then 1st derivative
#' - `"snv_deriv2"` — SNV then 2nd derivative
#'
#' @param recipe A `recipes::recipe()` object.
#' @param ... Column selectors for spectral columns.
#' @param preprocessing Character. One of the methods listed above.
#' @param window_size Odd integer. Savitzky-Golay window size. Default 9.
#' @param role Character. Role for output columns. Default "predictor".
#' @param trained Logical. Internal recipes flag.
#' @param skip Logical. Skip during bake()? Default FALSE.
#' @param id Character. Unique step identifier.
#'
#' @return Updated recipe with spectral preprocessing step.
#' @keywords internal
#' @export
step_transform_spectra <- function(recipe,
                                   ...,
                                   preprocessing,
                                   window_size = 9,
                                   role        = "predictor",
                                   trained     = FALSE,
                                   skip        = FALSE,
                                   id          = recipes::rand_id("transform_spectra")) {

  terms <- rlang::enquos(...)

  recipes::add_step(
    recipe,
    step_transform_spectra_new(
      columns       = terms,
      preprocessing = preprocessing,
      window_size   = window_size,
      role          = role,
      trained       = trained,
      skip          = skip,
      id            = id
    )
  )

}

## ---------------------------------------------------------------------------
## Constructor
## ---------------------------------------------------------------------------

#' @export
step_transform_spectra_new <- function(columns,
                                       preprocessing,
                                       window_size,
                                       role,
                                       trained,
                                       skip,
                                       id,
                                       trained_columns = NULL) {

  out <- list(
    columns         = columns,
    preprocessing   = preprocessing,
    window_size     = window_size,
    role            = role,
    trained         = trained,
    skip            = skip,
    id              = id,
    trained_columns = trained_columns
  )

  class(out) <- c("step_transform_spectra", "step")
  out

}

## ---------------------------------------------------------------------------
## prep method
## ---------------------------------------------------------------------------

#' @export
prep.step_transform_spectra <- function(x, training, info = NULL, ...) {

  col_names   <- recipes::recipes_eval_select(x$columns, training, info)
  non_numeric <- col_names[!vapply(training[, col_names], is.numeric, logical(1))]

  if (length(non_numeric) > 0) {

    rlang::abort(paste0(
      "All spectral columns must be numeric. Non-numeric: ",
      paste(non_numeric, collapse = ", ")
    ))

  }

  ## Compute output column count after SG trimming --------------------------

  input_len    <- length(col_names)
  half_window  <- (x$window_size - 1) / 2
  out_len      <- input_len - 2 * half_window
  new_colnames <- recipes::names0(out_len, prefix = "spec")

  step_transform_spectra_new(
    columns         = col_names,
    preprocessing   = x$preprocessing,
    window_size     = x$window_size,
    role            = x$role,
    trained         = TRUE,
    skip            = x$skip,
    id              = x$id,
    trained_columns = new_colnames
  )

}

## ---------------------------------------------------------------------------
## bake method
## ---------------------------------------------------------------------------

#' @export
bake.step_transform_spectra <- function(object, new_data, ...) {

  spectral_matrix <- as.matrix(new_data[, object$columns])

  transformed_list <- lapply(seq_len(nrow(spectral_matrix)), function(i) {

    tryCatch(
      process_spectra_row(
        spectral_matrix[i, ],
        preprocessing = object$preprocessing,
        window_size   = object$window_size
      ),
      error = function(e) {
        rep(NA_real_, length(object$trained_columns))
      }
    )

  })

  ## Verify all rows produced same length -----------------------------------

  lengths_vec <- vapply(transformed_list, length, integer(1))

  if (length(unique(lengths_vec)) != 1) {

    rlang::abort("Inconsistent row lengths in transformed spectra. Check preprocessing logic.")

  }

  transformed_matrix <- do.call(rbind, transformed_list)
  metadata <- new_data[, !names(new_data) %in% object$columns, drop = FALSE]

  dplyr::bind_cols(
    metadata,
    tibble::as_tibble(transformed_matrix, .name_repair = ~ object$trained_columns)
  )

}

## ---------------------------------------------------------------------------
## print method
## ---------------------------------------------------------------------------

#' @export
print.step_transform_spectra <- function(x, width = max(20, options()$width - 30), ...) {

  cat("Spectral transformation step using", x$preprocessing, "\n")
  invisible(x)

}

## ---------------------------------------------------------------------------
## Row-level spectral processing
## ---------------------------------------------------------------------------

#' Process a Single Spectral Row
#'
#' @description
#' Applies one of the supported preprocessing methods to a single spectrum
#' (numeric vector). Uses prospectr for Savitzky-Golay and SNV operations.
#'
#' @param input_vector Numeric vector. One spectrum.
#' @param preprocessing Character. Preprocessing method.
#' @param window_size Integer. SG window size.
#'
#' @return Numeric vector (shorter than input due to SG edge trimming).
#' @keywords internal
process_spectra_row <- function(input_vector, preprocessing, window_size = 9) {

  half_window <- (window_size - 1) / 2
  start       <- 1 + half_window
  end         <- length(input_vector) - half_window

  switch(as.character(preprocessing),

    "raw" = {
      input_vector[start:end]
    },

    "sg" = {
      input_vector %>%
        matrix(nrow = 1) %>%
        prospectr::savitzkyGolay(m = 0, p = 1, w = window_size) %>%
        as.vector()
    },

    "snv" = {
      input_vector %>%
        matrix(nrow = 1) %>%
        prospectr::standardNormalVariate() %>%
        as.vector() ->
      processed
      processed[start:end]
    },

    "deriv1" = {
      input_vector %>%
        matrix(nrow = 1) %>%
        prospectr::savitzkyGolay(m = 1, p = 1, w = window_size) %>%
        as.vector()
    },

    "deriv2" = {
      input_vector %>%
        matrix(nrow = 1) %>%
        prospectr::savitzkyGolay(m = 2, p = 3, w = window_size) %>%
        as.vector()
    },

    "snv_deriv1" = {
      input_vector %>%
        matrix(nrow = 1) %>%
        prospectr::standardNormalVariate() %>%
        prospectr::savitzkyGolay(m = 1, p = 1, w = window_size) %>%
        as.vector()
    },

    "snv_deriv2" = {
      input_vector %>%
        matrix(nrow = 1) %>%
        prospectr::standardNormalVariate() %>%
        prospectr::savitzkyGolay(m = 2, p = 3, w = window_size) %>%
        as.vector()
    },

    rlang::abort(paste0("Unknown preprocessing type: ", preprocessing))

  )

}
