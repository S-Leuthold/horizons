#' Reduce Spectral Dimensionality Using PCA
#'
#' Performs principal component analysis (PCA) on training MIR spectral data, and projects both
#' training and new samples into the same reduced-dimensional space. PCA is applied using
#' `FactoMineR::PCA()` with scaling enabled, retaining up to 80 components.
#'
#' @param training_data A `data.frame` or `tibble` containing training MIR spectra with numeric
#'   wavenumber columns (typically `608`–`3992`) and any number of metadata columns (e.g., `Sample_ID`, `Project`).
#' @param new_data A `data.frame` or `tibble` containing new MIR spectra to be projected into PCA space.
#'   Must have the same spectral columns as `training_data`.
#'
#' @return A named `list` with two elements:
#' \itemize{
#'   \item \strong{training_data}: A `tibble` containing the PCA-transformed training data joined to its original metadata.
#'   \item \strong{new_data}: A `tibble` containing the PCA-transformed new data joined to its original metadata.
#' }
#'
#' @details
#' PCA is fit to the spectral columns of `training_data` using `FactoMineR::PCA()` with `scale.unit = TRUE`
#' and `ncp = 80`. The resulting PCA object is used to project `new_data` via `stats::predict()` to ensure
#' dimensional consistency across datasets.
#'
#' Spectral columns should be numeric and represent evenly spaced wavenumbers between ~600–4000 cm⁻¹.
#' All non-spectral columns are preserved and returned alongside the transformed scores.
#'
#' This function is intended for dimensionality reduction prior to clustering or model calibration
#' in MIR-based soil property prediction workflows.
#'
#' @examples
#' \dontrun{
#' reduced <- reduce_dimensions_pca(training_data = ossl_data, new_data = unknown_data)
#'
#' names(reduced)
#' head(reduced$training_data)
#' head(reduced$new_data)
#' }
#'
#' @importFrom dplyr select bind_cols
#' @importFrom tibble as_tibble
#' @importFrom FactoMineR PCA
#' @importFrom stats predict
#' @importFrom cli cli_progress_step
#' @export

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
                 error_message = "Failed to perform PCA on training data") -> Training_PCA_safe

  Training_PCA <- Training_PCA_safe$result

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
                 error_message = "Failed to extract or combine training PCA scores") -> training_scores_safe

  training_scores <- training_scores_safe$result

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
                 error_message = "Failed to project new data into PCA space") -> new_scores_safe

  new_scores <- new_scores_safe$result

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

