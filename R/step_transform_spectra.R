#' Apply Spectral Preprocessing to Recipes
#'
#' Adds a preprocessing step to a `recipes::recipe()` that transforms row-wise spectral data
#' using common methods from the `prospectr` package. Supported transformations include
#' Savitzky-Golay derivatives, Standard Normal Variate (SNV), and Multiplicative Scatter Correction (MSC).
#' Intended for use in chemometric modeling workflows with spectra in wide format (columns = wavenumbers).
#'
#' @param recipe A `recipes::recipe()` object.
#' @param ... Selector functions to choose spectral columns (e.g., `all_predictors()`).
#' @param preprocessing A character string specifying the preprocessing method. See **Details**.
#' @param window_size Odd integer specifying the window size for Savitzky-Golay filtering. Default is 9.
#' @param role Character string describing the variable role for new columns. Default is `"predictor"`.
#' @param trained Logical. Required by `recipes`; indicates if preprocessing parameters are fixed.
#' @param skip Logical. Should this step be skipped during `bake()`? Default is FALSE.
#' @param id Character string. Unique identifier for the step. Auto-generated by default.
#'
#' @return A `step_transform_spectra` object that can be added to a recipe.
#'
#' @details
#' Supported values for `preprocessing` include:
#' \itemize{
#'   \item `"raw"` — Trims edge artifacts based on `window_size`, no transformation.
#'   \item `"sg"` — Savitzky-Golay smoothing (0th derivative).
#'   \item `"snv"` — Standard Normal Variate followed by trimming.
#'   \item `"deriv1"` — Savitzky-Golay first derivative.
#'   \item `"deriv2"` — Savitzky-Golay second derivative.
#'   \item `"snv_deriv1"` — SNV followed by SG first derivative.
#'   \item `"snv_deriv2"` — SNV followed by SG second_
#'   }
#'
#' @importFrom cli cli_abort cli_alert_warning
#' @importFrom dplyr select all_of bind_cols
#' @importFrom glue glue
#' @importFrom purrr map map_lgl map_int
#' @importFrom recipes add_step step prep bake rand_id names0 recipes_eval_select
#' @importFrom rlang enquos
#' @importFrom tibble as_tibble
#' @importFrom prospectr savitzkyGolay standardNormalVariate msc
#'
#' @export



## -----------------------------------------------------------------------------
## Step 1: Spectral row-wise transformation logic
## -----------------------------------------------------------------------------

process_spectra <- function(input_vector,
                            preprocessing,
                            window_size = 9) {

  start <- 1 + ((window_size - 1) / 2)
  end   <- length(input_vector) - ((window_size - 1) / 2)

  out <-  switch(as.character(preprocessing),
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
                    as.vector() -> processed

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
                "msc_deriv1" = {
                  input_vector %>%
                    matrix(nrow = 1) %>%
                    prospectr::msc() %>%
                    prospectr::savitzkyGolay(m = 1, p = 1, w = window_size) %>%
                    as.vector()
                },
                stop(glue::glue("Unknown preprocessing type: {preprocessing}"))
  )

  return(out)
}

## -----------------------------------------------------------------------------
## Step 2: User-facing step constructor
## -----------------------------------------------------------------------------

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

## -----------------------------------------------------------------------------
## Step 3: Constructor
## -----------------------------------------------------------------------------

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
    columns          = columns,
    preprocessing    = preprocessing,
    window_size      = window_size,
    role             = role,
    trained          = trained,
    skip             = skip,
    id               = id,
    trained_columns  = trained_columns
  )
  class(out) <- c("step_transform_spectra", "step")
  return(out)
}

## -----------------------------------------------------------------------------
## Step 4: prep() method
## -----------------------------------------------------------------------------

#' @export

prep.step_transform_spectra <- function(x, training, info = NULL, ...) {

  col_names    <- recipes::recipes_eval_select(x$columns, training, info)
  non_numeric  <- col_names[!purrr::map_lgl(training[, col_names], is.numeric)]

  if (length(non_numeric) > 0) {
    cli::cli_abort("All spectral columns must be numeric. The following are not: {.val {non_numeric}}")
  }

  # Estimate number of output variables after trimming
  input_len     <- length(col_names)
  start         <- 1 + ((x$window_size - 1) / 2)
  end           <- input_len - ((x$window_size - 1) / 2)
  out_len       <- end - start + 1
  new_colnames  <- recipes::names0(out_len, prefix = "spec")

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

## -----------------------------------------------------------------------------
## Step 5: bake() method
## -----------------------------------------------------------------------------

#' @export

bake.step_transform_spectra <- function(object, new_data, ...) {

  spectral_matrix <- as.matrix(new_data[, object$columns])

  transformed_list <- purrr::map(seq_len(nrow(spectral_matrix)), function(i) {
    vec <- spectral_matrix[i, ]
    tryCatch(
      process_spectra(vec,
                      preprocessing = object$preprocessing,
                      window_size   = object$window_size),
      error = function(e) {
        cli::cli_alert_warning("Row {i} failed in process_spectra(): {e$message}")
        return(rep(NA_real_, length(object$trained_columns)))
      }
    )
  })

  lengths_vec <- purrr::map_int(transformed_list, length)
  if (length(unique(lengths_vec)) != 1) {
    cli::cli_abort("Inconsistent row lengths in transformed spectra. Check preprocessing logic.")
  }

  transformed_matrix <- do.call(rbind, transformed_list)

  metadata <- dplyr::select(new_data, -dplyr::all_of(object$columns))

  out <- dplyr::bind_cols(
    metadata,
    tibble::as_tibble(transformed_matrix, .name_repair = ~ object$trained_columns)
  )

  return(out)
}

## -----------------------------------------------------------------------------
## Step 6: print() method
## -----------------------------------------------------------------------------

#' @export

print.step_transform_spectra <- function(x,
                                         width = max(20, options()$width - 30),
                                         ...) {
  cat("Spectral transformation step using", x$preprocessing, "\n")
  invisible(x)
}
