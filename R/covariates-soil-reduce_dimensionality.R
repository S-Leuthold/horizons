#' Reduce Dimensionality of Spectral Data Using PCA
#'
#' Applies PCA to training spectra and projects both training and new data into
#' a lower-dimensional space for covariate prediction. Retains up to 80 principal
#' components using \code{FactoMineR::PCA()}.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom FactoMineR PCA
#' @importFrom stats predict
#' @importFrom tidyselect starts_with
#'
#' @param training_data A data frame containing training spectra, with numeric columns from `608` to `3992` representing wavenumbers.
#'                      Additional metadata columns are preserved.
#' @param new_data A data frame containing new spectra to project, with the same structure and wavenumber columns as `training_data`.
#'
#' @return A list with:
#'   \item{training_data}{A tibble of PCA-transformed training data with original metadata.}
#'   \item{new_data}{A tibble of PCA-transformed new data with original metadata.}
#'
#' @details
#' PCA is performed on the training set using `FactoMineR::PCA()` with scaling enabled and up to 80 components retained.
#' The trained PCA model is then used to project the `new_data` onto the same principal component space to ensure
#' consistent dimensionality reduction across datasets.
#'
#' @keywords internal

reduce_dimensions_pca <- function(training_data,
                                  new_data) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Run PCA on training spectral columns
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Fitting PCA on training data.")

  safely_execute(expr          = {FactoMineR::PCA(X          = dplyr::select(training_data, c(`608`:`3992`)),
                                                  scale.unit = TRUE,
                                                  graph      = FALSE,
                                                  ncp        = 80)},
                 default_value = NULL,
                 error_message = "Failed to perform PCA on training data") -> Training_PCA

  if(is.null(Training_PCA)) {
    return(NULL)
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Project training data into PCA space and join metadata
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Extracting PCA scores for training data.")

  safely_execute(expr          = {tibble::as_tibble(Training_PCA$ind$coord) %>%
                                   dplyr::bind_cols(dplyr::select(training_data, -c(`608`:`3992`)))},
                 default_value = NULL,
                 error_message = "Failed to extract or combine training PCA scores") -> training_scores

  if(is.null(training_scores)) {
    return(NULL)
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Project new data using the same PCA and join metadata
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Predicting PCA scores for new data.")

  safely_execute(expr         = {predict(object  = Training_PCA,
                                         newdata = dplyr::select(new_data, c(`608`:`3992`)))$coord %>%
                                                    tibble::as_tibble() %>%
                                                    dplyr::bind_cols(dplyr::select(new_data, -c(`608`:`3992`)))},
                 default_value = NULL,
                 error_message = "Failed to project new data into PCA space") -> new_scores

  if(is.null(new_scores)){
    return(NULL)
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Return as list
  ## ---------------------------------------------------------------------------

  return(list(training_data = training_scores,
              new_data      = new_scores
  ))
}

