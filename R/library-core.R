#' Library Prediction Core Orchestrator
#'
#' @description
#' High-level orchestration functions that integrate library data loading, clustering,
#' and preparation for training. Connects the library-data, library-clustering, and
#' library-train modules into complete workflows.
#'
#' @details
#' This module provides the end-to-end pipeline for library-based prediction,
#' handling the critical architectural detail: clustering uses preprocessed data
#' (SNV for stability) while training uses raw data (for config-specific preprocessing).
#'
#' @importFrom cli cli_text cli_alert_info cli_abort style_bold
#' @importFrom dplyr mutate filter
#' @keywords internal

## -----------------------------------------------------------------------------
## Main Orchestrator: Prepare Library for Training
## -----------------------------------------------------------------------------

#' Prepare Library Data for Training
#'
#' @description
#' Complete pipeline: load OSSL → preprocess for clustering → PCA → GMM clustering
#' → add cluster assignments to RAW data. Returns everything needed for training
#' cluster-specific models.
#'
#' @details
#' **Critical Architecture:**
#' - Clustering: Uses SNV-preprocessed data for stable assignments
#' - Training: Uses RAW data so build_recipe can apply config-specific preprocessing
#' - Cluster assignments: Computed on preprocessed, joined to raw
#'
#' **Memory:** Keeps raw data (~67MB) for training flexibility. Discards preprocessed
#' data after clustering (only needed once).
#'
#' @param property Character. Property from LIBRARY_PROPERTIES
#' @param k_range Integer vector. Cluster range for BIC. Default: c(5, 7, 9, 11)
#' @param variance_threshold Numeric. PCA variance to retain. Default: 0.99
#' @param remove_water_bands Logical. Experimental water band removal. Default: FALSE
#' @param min_cluster_size Integer. Minimum samples per cluster. Default: 300
#' @param max_samples Integer. For testing. Default: NULL (all)
#' @param cache_dir Character. Cache directory. Default: NULL
#' @param seed Integer. Random seed. Default: 123
#' @param verbose Logical. Print progress? Default: TRUE
#'
#' @return List with:
#' \describe{
#'   \item{library_data_raw}{Tibble with RAW spectra + property + cluster_id column}
#'   \item{gmm_model}{Trained mclust model (for assigning unknowns)}
#'   \item{pca_model}{PCA model (for projecting unknowns)}
#'   \item{n_clusters}{Integer, optimal K selected}
#'   \item{cluster_sizes}{Integer vector, samples per cluster}
#'   \item{property}{Character, property name}
#' }
#'
#' @examples
#' \dontrun{
#' library_result <- horizons:::prepare_library_for_training("clay", max_samples = 1000)
#' # Now train models on each cluster using raw data
#' }
#'
#' @keywords internal
prepare_library_for_training <- function(property,
                                         k_range            = c(5, 7, 9, 11),
                                         variance_threshold = 0.99,
                                         remove_water_bands = FALSE,
                                         min_cluster_size   = 300,
                                         max_samples        = NULL,
                                         cache_dir          = NULL,
                                         seed               = 123,
                                         verbose            = TRUE) {

  if (verbose) {
    cli::cli_text("")
    cli::cli_text("{cli::style_bold('Preparing library for training:')} {property}")
  }

  ## ---------------------------------------------------------------------------
  ## Step 1: Load and process library data
  ## ---------------------------------------------------------------------------

  get_processed_library_data(
    property           = property,
    variance_threshold = variance_threshold,
    remove_water_bands = remove_water_bands,
    max_samples        = max_samples,
    cache_dir          = cache_dir,
    verbose            = verbose
  ) -> lib_result

  if (is.null(lib_result)) {

    cli::cli_abort("Failed to prepare library data for {property}")

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Cluster library on PCA scores
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("")
    cli::cli_text("{cli::style_bold('Clustering library')}")
  }

  fit_gmm_clustering(
    pca_scores               = lib_result$pca_scores,
    k_range                  = k_range,
    covariance_regularization = TRUE,
    min_cluster_size         = min_cluster_size,
    seed                     = seed,
    verbose                  = verbose
  ) -> gmm_result

  if (is.null(gmm_result)) {

    cli::cli_abort("Failed to cluster library for {property}")

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Add cluster assignments to RAW data
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("")
    cli::cli_text("├─ {cli::style_bold('Adding cluster IDs to raw data')}...")
  }

  ## Join cluster assignments to raw library data ------------------------------

  lib_result$library_data_raw$cluster_id <- gmm_result$cluster_assignments

  if (verbose) {
    cli::cli_text("│  └─ {gmm_result$n_clusters} clusters assigned to {nrow(lib_result$library_data_raw)} samples")
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Clean up and assemble result
  ## ---------------------------------------------------------------------------

  ## (PCA scores cleanup happens below - exclude from return) -------------------

  if (verbose) {
    cli::cli_text("")
    cli::cli_text("└─ {cli::style_bold('Library ready for training')}")
    cli::cli_text("")
  }

  ## Assemble final result (exclude pca_scores - not needed after clustering) ---

  list(
    library_data_raw = lib_result$library_data_raw,  # With cluster_id column
    gmm_result       = gmm_result,                   # Full GMM result structure
    pca_model        = lib_result$pca_model,         # For projecting unknowns
    # pca_scores NOT included - only needed during clustering (~100 MB saved)
    n_samples        = nrow(lib_result$library_data_raw),
    n_clusters       = gmm_result$n_clusters,
    cluster_sizes    = gmm_result$cluster_sizes,
    property         = property
  ) -> result

  ## Free original lib_result with pca_scores ----------------------------------

  rm(lib_result, gmm_result)
  gc(verbose = FALSE)

  return(result)

}
