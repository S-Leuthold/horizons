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

#' @keywords internal
#' @noRd
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

  ## A window wider than the spectrum trims everything away. Downstream this
  ## surfaces as a `names0()` of a non-positive length or an empty predictor
  ## matrix, neither of which names the cause, so check it here with both
  ## numbers in the message.

  if (out_len < 1) {

    cli::cli_abort(c(
      "{.fn step_transform_spectra} would trim every spectral column away.",
      "x" = "{input_len} spectral column{?s} with {.code window_size = {x$window_size}} leaves {out_len} after Savitzky-Golay edge trimming.",
      "i" = "Savitzky-Golay drops {half_window} column{?s} from each end, so {.arg window_size} must be smaller than the number of spectral columns."
    ), class = "horizons_input_error")

  }

  new_colnames <- recipes::names0(out_len, prefix = "spec")

  ## The generated names must not collide with a column this step passes
  ## through (a covariate literally named `spec01`, say). bake() binds the two
  ## blocks side by side, so a collision is repaired positionally: at training
  ## a spectral band is silently displaced from everything downstream, and at
  ## predict time the bind dies inside vctrs with no mention of the column at
  ## fault. Named here, at prep, where the recipe is still fixable.

  check_transform_name_collision(new_colnames, setdiff(names(training), col_names))

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

  ## Failures are recorded, not swallowed. The fallback below is an all-NA row
  ## built to `length(object$trained_columns)`, which means the length check
  ## further down cannot detect it — so without this accounting a malformed
  ## spectrum returns NA predictions at predict time, or injects NA rows into
  ## the model matrix at train time, with no signal either way. See #52.

  failed_rows <- integer(0)
  failure_msg <- NULL

  transformed_list <- lapply(seq_len(nrow(spectral_matrix)), function(i) {

    tryCatch(
      process_spectra_row(
        spectral_matrix[i, ],
        preprocessing = object$preprocessing,
        window_size   = object$window_size
      ),
      error = function(e) {

        failed_rows <<- c(failed_rows, i)
        if (is.null(failure_msg)) failure_msg <<- conditionMessage(e)
        rep(NA_real_, length(object$trained_columns))

      }
    )

  })

  ## A non-finite input produces no error but an all-NA output, so check the
  ## results rather than trusting that a failure raised a condition.

  na_rows <- which(vapply(transformed_list,
                          function(x) all(is.na(x)),
                          logical(1)))

  bad_rows <- sort(unique(c(failed_rows, na_rows)))

  if (length(bad_rows) > 0) {

    shown <- paste(utils::head(bad_rows, 10), collapse = ", ")
    if (length(bad_rows) > 10) shown <- paste0(shown, ", ...")

    rlang::warn(paste0(
      "step_transform_spectra: ", length(bad_rows), " of ",
      nrow(spectral_matrix), " spectra produced no usable output and were ",
      "returned as NA (rows: ", shown, ").",
      if (!is.null(failure_msg)) paste0(" First error: ", failure_msg) else
        " No error was raised, so the input was likely non-finite.",
      " Downstream predictions for these rows are NA."
    ))

  }

  ## Verify all rows produced same length -----------------------------------

  lengths_vec <- vapply(transformed_list, length, integer(1))

  if (length(unique(lengths_vec)) != 1) {

    rlang::abort("Inconsistent row lengths in transformed spectra. Check preprocessing logic.")

  }

  transformed_matrix <- do.call(rbind, transformed_list)
  metadata <- new_data[, !names(new_data) %in% object$columns, drop = FALSE]

  ## Re-checked at bake as well as prep: `new_data` can carry a column the
  ## training table did not, and default name repair would resolve the clash
  ## positionally instead of reporting it. `.name_repair = "check_unique"`
  ## makes the bind itself refuse to repair silently; the check above is what
  ## names the offending column.

  check_transform_name_collision(object$trained_columns, names(metadata))

  dplyr::bind_cols(
    metadata,
    tibble::as_tibble(transformed_matrix, .name_repair = ~ object$trained_columns),
    .name_repair = "check_unique"
  )

}


## ---------------------------------------------------------------------------
## Name-collision check (shared by prep and bake)
## ---------------------------------------------------------------------------

#' Abort when a generated spectral name collides with a pass-through column
#'
#' @param generated Character vector of the step's generated column names
#'   (`spec1`, `spec01`, ... from `recipes::names0()`).
#' @param passthrough Character vector of the column names the step carries
#'   through untransformed (everything that is not a spectral column).
#'
#' @return Invisibly `TRUE`; aborts naming the colliding columns.
#' @keywords internal
#' @noRd
check_transform_name_collision <- function(generated, passthrough) {

  collisions <- intersect(generated, passthrough)

  if (length(collisions) > 0) {

    cli::cli_abort(c(
      "{.fn step_transform_spectra} would generate {length(collisions)} column name{?s} that {?is/are} already in the data.",
      "x" = "Colliding: {.val {collisions}}",
      "i" = "The step names its output {.code spec1}, {.code spec2}, ... , so a non-spectral column using that pattern (a covariate, an id, a meta column) cannot be carried through alongside it.",
      "i" = "{cli::qty(length(collisions))}Rename the offending column{?s} before building the recipe."
    ), class = "horizons_input_error")

  }

  invisible(TRUE)

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
      input_vector |>
        matrix(nrow = 1) |>
        prospectr::savitzkyGolay(m = 0, p = 1, w = window_size) |>
        as.vector()
    },

    "snv" = {
      input_vector |>
        matrix(nrow = 1) |>
        prospectr::standardNormalVariate() |>
        as.vector() ->
      processed
      processed[start:end]
    },

    "deriv1" = {
      input_vector |>
        matrix(nrow = 1) |>
        prospectr::savitzkyGolay(m = 1, p = 1, w = window_size) |>
        as.vector()
    },

    "deriv2" = {
      input_vector |>
        matrix(nrow = 1) |>
        prospectr::savitzkyGolay(m = 2, p = 3, w = window_size) |>
        as.vector()
    },

    "snv_deriv1" = {
      input_vector |>
        matrix(nrow = 1) |>
        prospectr::standardNormalVariate() |>
        prospectr::savitzkyGolay(m = 1, p = 1, w = window_size) |>
        as.vector()
    },

    "snv_deriv2" = {
      input_vector |>
        matrix(nrow = 1) |>
        prospectr::standardNormalVariate() |>
        prospectr::savitzkyGolay(m = 2, p = 3, w = window_size) |>
        as.vector()
    },

    rlang::abort(paste0("Unknown preprocessing type: ", preprocessing))

  )

}
