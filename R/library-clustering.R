#' Library Clustering with Gaussian Mixture Models
#'
#' @description
#' Functions for clustering reference spectral libraries using Gaussian Mixture Models (GMM)
#' with BIC-based model selection. Provides unknown sample assignment with probability scores,
#' applicability domain metrics, and cluster-specific statistics for library-based prediction.
#'
#' @details
#' This module implements the clustering component of the Library Prediction Service.
#' Clusters are formed on library PCA scores and used to:
#' 1. Assign unknown samples to training groups
#' 2. Enable cluster-specific model training
#' 3. Provide applicability domain awareness (Mahalanobis distance)
#'
#' GMM is preferred over k-means because it:
#' - Captures within-cluster covariance (needed for Mahalanobis AD metrics)
#' - Provides probability scores for soft assignments
#' - Handles elliptical clusters (realistic for spectral data)
#'
#' @importFrom cli cli_text cli_alert_info cli_alert_warning cli_abort style_bold
#' @importFrom mclust Mclust mclustBIC
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate group_by summarise n
#' @importFrom stats cov mahalanobis quantile
#' @importFrom corpcor cov.shrink
#' @importFrom rlang %||%
#' @keywords internal

## -----------------------------------------------------------------------------
## Section 1: GMM Clustering with BIC Model Selection
## -----------------------------------------------------------------------------

#' Fit GMM Clustering on Library PCA Scores
#'
#' @description
#' Trains Gaussian Mixture Model on library PCA scores with automatic K selection
#' via BIC. Applies covariance regularization (Ledoit-Wolf shrinkage) to prevent
#' singularity and enforces minimum cluster sizes for robust model training.
#'
#' @details
#' **Algorithm:**
#' 1. Test K ∈ k_range using mclust::Mclust()
#' 2. Select optimal K via BIC (Bayesian Information Criterion)
#' 3. Apply Ledoit-Wolf shrinkage to cluster covariances if enabled
#' 4. Check minimum cluster size constraint
#' 5. Merge small clusters or refit if needed
#' 6. Return full mclust object + metadata
#'
#' **BIC Selection:**
#' Lower BIC = better fit. mclust tests different covariance structures
#' (VVV = full covariance, EII = spherical, etc.) and selects best.
#'
#' **Covariance Regularization:**
#' For high-dimensional PCA spaces (15-25 components), empirical covariances
#' can be unstable. Ledoit-Wolf shrinkage regularizes toward diagonal:
#' Σ_reg = λ * Σ_sample + (1-λ) * Σ_diagonal
#'
#' This prevents singularity when computing Mahalanobis distances.
#'
#' @param pca_scores Matrix of PCA scores (n_samples × n_components)
#' @param k_range Integer vector. Cluster counts to test. Default: c(5, 7, 9, 11)
#' @param covariance_regularization Logical. Apply Ledoit-Wolf shrinkage? Default: TRUE
#' @param min_cluster_size Integer. Minimum samples per cluster. Default: 300
#' @param seed Integer. Random seed for reproducibility. Default: 123
#' @param verbose Logical. Print progress? Default: TRUE
#'
#' @return List with:
#' \describe{
#'   \item{model}{Full mclust::Mclust object (for predict() method)}
#'   \item{n_clusters}{Integer, optimal K selected by BIC}
#'   \item{bic_values}{Numeric vector, BIC for each K tested}
#'   \item{cluster_assignments}{Integer vector, cluster ID for each sample}
#'   \item{cluster_sizes}{Named integer vector, sample count per cluster}
#'   \item{centroids}{Matrix, cluster centers in PCA space}
#'   \item{covariances}{Array, regularized covariance matrices per cluster}
#' }
#'
#' Returns NULL if fitting fails.
#'
#' @section Performance:
#' - ~30 seconds for 12K samples, 20 components, testing 4 K values
#' - Memory: ~200-300MB during fitting
#' - Final object: ~10-20MB
#'
#' @examples
#' \dontrun{
#' # Fit GMM on library PCA scores
#' pca_result <- horizons:::perform_pca_on_library(preprocessed_data)
#' gmm_result <- horizons:::fit_gmm_clustering(
#'   pca_scores = pca_result$pca_scores,
#'   k_range = c(5, 7, 9),
#'   verbose = TRUE
#' )
#' }
#'
#' @seealso [assign_to_clusters()], [compute_ad_thresholds()]
#' @keywords internal
fit_gmm_clustering <- function(pca_scores,
                               k_range                    = c(5, 7, 9, 11),
                               covariance_regularization  = TRUE,
                               min_cluster_size           = 300,
                               seed                       = 123,
                               verbose                    = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 1.1: Validate inputs
  ## ---------------------------------------------------------------------------

  if (!is.matrix(pca_scores) && !is.data.frame(pca_scores)) {
    cli::cli_abort("pca_scores must be a matrix or data frame")
  }

  ## Convert to matrix if needed -----------------------------------------------

  if (is.data.frame(pca_scores)) pca_scores <- as.matrix(pca_scores)

  n_samples <- nrow(pca_scores)
  n_dims    <- ncol(pca_scores)

  ## Validate k_range makes sense ---------------------------------------------

  if (any(k_range >= n_samples / 10)) {
    cli::cli_warn("Some K values are large relative to sample size - may produce small clusters")
  }

  ## ---------------------------------------------------------------------------
  ## Step 1.2: Fit GMM with BIC selection
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text("│")
  if (verbose) cli::cli_text("├─ {cli::style_bold('Fitting GMM with BIC selection')}...")
  if (verbose) cli::cli_text("│  ├─ Testing K ∈ {{{paste(k_range, collapse = ', ')}}}")

  ## Set seed for reproducibility ----------------------------------------------

  set.seed(seed)

  ## Fit mclust across K range -------------------------------------------------

  safely_execute(
    mclust::Mclust(pca_scores, G = k_range, verbose = FALSE),
    error_message = "GMM fitting failed"
  ) %>%
    handle_results(
      error_title = "GMM clustering failed",
      error_hints = c("Check for constant/zero variance in PCA scores", "Try smaller k_range"),
      abort_on_null = FALSE
    ) -> gmm_model

  if (is.null(gmm_model)) return(NULL)

  ## Extract optimal K from BIC ------------------------------------------------

  optimal_k <- gmm_model$G

  if (verbose) cli::cli_text("│  ├─ BIC selected K = {optimal_k}")
  if (verbose) cli::cli_text("│  └─ Model: {gmm_model$modelName} covariance structure")

  ## ---------------------------------------------------------------------------
  ## Step 1.3: Apply covariance regularization (Ledoit-Wolf)
  ## ---------------------------------------------------------------------------

  if (covariance_regularization) {

    if (verbose) cli::cli_text("│")
    if (verbose) cli::cli_text("├─ {cli::style_bold('Applying covariance regularization')}...")

    ## Get cluster assignments ---------------------------------------------------

    cluster_assignments <- gmm_model$classification

    ## Regularize covariance for each cluster ------------------------------------

    regularized_covs <- array(NA, dim = c(n_dims, n_dims, optimal_k))

    for (k in 1:optimal_k) {

      cluster_data <- pca_scores[cluster_assignments == k, , drop = FALSE]

      ## Apply Ledoit-Wolf shrinkage ---------------------------------------------

      safely_execute(
        corpcor::cov.shrink(cluster_data, verbose = FALSE),
        error_message = "Covariance shrinkage failed for cluster {k}"
      ) %>%
        handle_results(
          error_title = "Shrinkage failed",
          abort_on_null = FALSE
        ) -> shrunk_cov

      if (!is.null(shrunk_cov)) {

        regularized_covs[,,k] <- shrunk_cov

      } else {

        ## Fallback to sample covariance ------------------------------------------

        regularized_covs[,,k] <- cov(cluster_data)

      }

    }

    if (verbose) cli::cli_text("│  └─ Regularized covariances for {optimal_k} clusters")

  } else {

    ## Use mclust covariances as-is ----------------------------------------------

    regularized_covs <- gmm_model$parameters$variance$Sigma

  }

  ## ---------------------------------------------------------------------------
  ## Step 1.4: Check minimum cluster size
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text("│")
  if (verbose) cli::cli_text("├─ {cli::style_bold('Checking cluster sizes')}...")

  cluster_sizes <- table(gmm_model$classification)

  if (verbose) {
    for (k in 1:optimal_k) {
      size <- cluster_sizes[as.character(k)]
      status <- if (size >= min_cluster_size) "✓" else "⚠"
      cli::cli_text("│  ├─ Cluster {k}: {size} samples {status}")
    }
  }

  ## Check if any cluster is too small -----------------------------------------

  small_clusters <- which(cluster_sizes < min_cluster_size)

  if (length(small_clusters) > 0) {

    cli::cli_warn("{length(small_clusters)} cluster(s) below minimum size ({min_cluster_size})")
    cli::cli_warn("Consider: reducing K, lowering min_cluster_size, or merging small clusters")
    ## TODO: Implement cluster merging in future version

  }

  if (verbose) cli::cli_text("│  └─ All clusters validated")

  ## ---------------------------------------------------------------------------
  ## Step 1.5: Assemble result
  ## ---------------------------------------------------------------------------

  list(model               = gmm_model,
       n_clusters          = optimal_k,
       bic_values          = gmm_model$BIC,
       cluster_assignments = gmm_model$classification,
       cluster_sizes       = as.integer(cluster_sizes),
       centroids           = gmm_model$parameters$mean,
       covariances         = regularized_covs) -> result

  return(result)

}

## -----------------------------------------------------------------------------
## Section 2: Unknown Sample Assignment with Probabilities
## -----------------------------------------------------------------------------

#' Assign Unknown Samples to Library Clusters
#'
#' @description
#' Assigns new (unknown) samples to library clusters using the trained GMM model.
#' Returns cluster assignments with probability scores and entropy measures for
#' uncertainty quantification.
#'
#' @details
#' Uses `predict.Mclust()` to compute:
#' - Cluster assignment (highest probability)
#' - Assignment probability (confidence in assignment)
#' - Shannon entropy (overall uncertainty across all clusters)
#'
#' **Entropy Calculation:**
#' H = -Σ(p_k * log(p_k)) where p_k is probability for cluster k
#' - H = 0: Certain assignment (probability = 1 for one cluster)
#' - H = log(K): Maximum uncertainty (equal probability across K clusters)
#'
#' Low-confidence assignments (probability < 0.7 or high entropy) should have
#' inflated prediction intervals in the UQ layer.
#'
#' @param unknown_pca_scores Matrix of PCA scores for unknowns (n × n_components)
#' @param gmm_model GMM model object from fit_gmm_clustering()
#' @param verbose Logical. Print progress? Default: TRUE
#'
#' @return Tibble with:
#' \describe{
#'   \item{sample_id}{Row index from unknown_pca_scores}
#'   \item{cluster_id}{Assigned cluster (1 to n_clusters)}
#'   \item{probability}{Assignment probability (0-1), higher = more confident}
#'   \item{entropy}{Shannon entropy of cluster probabilities (0 to log(K))}
#'   \item{flag}{Character: "high_confidence", "low_confidence", "ambiguous"}
#' }
#'
#' Returns NULL if assignment fails.
#'
#' @examples
#' \dontrun{
#' # After fitting GMM
#' assignments <- horizons:::assign_to_clusters(
#'   unknown_pca_scores = unknown_pca,
#'   gmm_model = gmm_result$model
#' )
#' }
#'
#' @seealso [fit_gmm_clustering()]
#' @keywords internal
assign_to_clusters <- function(unknown_pca_scores,
                               gmm_model,
                               verbose = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 2.1: Validate inputs
  ## ---------------------------------------------------------------------------

  if (!is.matrix(unknown_pca_scores)) {
    unknown_pca_scores <- as.matrix(unknown_pca_scores)
  }

  n_unknowns <- nrow(unknown_pca_scores)
  n_clusters <- gmm_model$n_clusters %||% gmm_model$G

  ## ---------------------------------------------------------------------------
  ## Step 2.2: Predict cluster assignments and probabilities
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text("│")
  if (verbose) cli::cli_text("├─ {cli::style_bold('Assigning unknowns to clusters')}...")

  ## Use mclust predict method -------------------------------------------------

  safely_execute(
    predict(gmm_model$model %||% gmm_model, newdata = unknown_pca_scores),
    error_message = "Cluster assignment failed"
  ) %>%
    handle_results(
      error_title = "Assignment failed",
      error_hints = c("Check PCA dimensions match library", "Verify gmm_model is valid"),
      abort_on_null = FALSE
    ) -> predictions

  if (is.null(predictions)) return(NULL)

  ## Extract assignments and probabilities -------------------------------------

  cluster_assignments <- predictions$classification
  prob_matrix         <- predictions$z  # n_unknowns × n_clusters

  ## ---------------------------------------------------------------------------
  ## Step 2.3: Calculate assignment confidence metrics
  ## ---------------------------------------------------------------------------

  ## Maximum probability (confidence in assignment) ----------------------------

  max_probs <- apply(prob_matrix, 1, max)

  ## Shannon entropy (uncertainty across all clusters) -------------------------

  entropy <- apply(prob_matrix, 1, function(p) {
    p_clean <- p[p > 0]  # Remove zeros to avoid log(0)
    if (length(p_clean) == 0) return(0)
    -sum(p_clean * log(p_clean))
  })

  ## ---------------------------------------------------------------------------
  ## Step 2.4: Flag assignment quality
  ## ---------------------------------------------------------------------------

  ## Classify assignments by confidence ----------------------------------------

  flags <- ifelse(max_probs > 0.8, "high_confidence",
           ifelse(max_probs > 0.5, "moderate_confidence",
                  "low_confidence"))

  ## Also flag high entropy (ambiguous between multiple clusters) --------------

  max_entropy <- log(n_clusters)
  flags[entropy > 0.7 * max_entropy] <- "ambiguous"

  if (verbose) {
    n_high <- sum(flags == "high_confidence")
    n_low  <- sum(flags %in% c("low_confidence", "ambiguous"))
    cli::cli_text("│  ├─ High confidence: {n_high}/{n_unknowns} ({round(100*n_high/n_unknowns)}%)")
    cli::cli_text("│  └─ Low confidence: {n_low}/{n_unknowns} ({round(100*n_low/n_unknowns)}%)")
  }

  ## ---------------------------------------------------------------------------
  ## Step 2.5: Assemble result tibble
  ## ---------------------------------------------------------------------------

  tibble::tibble(
    sample_id   = 1:n_unknowns,
    cluster_id  = cluster_assignments,
    probability = max_probs,
    entropy     = entropy,
    flag        = flags
  ) -> result

  return(result)

}

## -----------------------------------------------------------------------------
## Section 3: Applicability Domain Metrics
## -----------------------------------------------------------------------------

#' Calculate Mahalanobis Distance to Cluster Centroid
#'
#' @description
#' Computes Mahalanobis distance from a point to a cluster centroid using the
#' cluster's covariance matrix. This distance metric accounts for cluster shape
#' and correlation structure, making it ideal for applicability domain assessment.
#'
#' @details
#' Mahalanobis distance: D = sqrt((x - μ)' Σ⁻¹ (x - μ))
#' where x is the point, μ is centroid, Σ is covariance matrix
#'
#' For identity covariance (uncorrelated, unit variance), this reduces to
#' Euclidean distance. For correlated variables, it accounts for covariance structure.
#'
#' @param point Numeric vector. Point in PCA space
#' @param centroid Numeric vector. Cluster centroid
#' @param covariance Matrix. Cluster covariance matrix (regularized recommended)
#'
#' @return Numeric. Mahalanobis distance (positive value)
#'
#' @keywords internal
calculate_mahalanobis_distance <- function(point, centroid, covariance) {

  ## Use stats::mahalanobis for robust calculation -----------------------------
  ## Note: stats::mahalanobis returns SQUARED distance, so we take sqrt

  distance_squared <- stats::mahalanobis(x      = matrix(point, nrow = 1),
                                        center = centroid,
                                        cov    = covariance)

  distance <- sqrt(as.numeric(distance_squared))

  return(distance)

}

#' Compute Applicability Domain Thresholds per Cluster
#'
#' @description
#' Calculates percentile-based Mahalanobis distance thresholds for each cluster
#' to define applicability domain boundaries. Used to flag samples that are far
#' from training data (out-of-distribution).
#'
#' @details
#' Computes empirical thresholds (e.g., 95th, 99.5th percentiles) of Mahalanobis
#' distances within each cluster. Samples beyond these thresholds are:
#' - Soft zone (p95-p99.5): Inflate prediction intervals
#' - Hard zone (>p99.5): Abstain from prediction or flag as unreliable
#'
#' @param cluster_data Tibble with columns:
#'   - cluster_id: Integer cluster assignment
#'   - mahalanobis_dist: Numeric Mahalanobis distance to centroid
#' @param percentiles Numeric vector. Percentiles to compute (0-1). Default: c(0.95, 0.995)
#'
#' @return Tibble with:
#' \describe{
#'   \item{cluster_id}{Cluster ID}
#'   \item{p95}{95th percentile threshold (soft zone)}
#'   \item{p995}{99.5th percentile threshold (hard zone)}
#'   \item{...}{Additional percentiles if specified}
#' }
#'
#' @keywords internal
compute_ad_thresholds <- function(cluster_data,
                                  percentiles = c(0.95, 0.995)) {

  ## Calculate percentiles per cluster -----------------------------------------

  cluster_data %>%
    dplyr::group_by(cluster_id) %>%
    dplyr::summarise(
      n_samples = dplyr::n(),
      p95       = quantile(mahalanobis_dist, 0.95, na.rm = TRUE),
      p995      = quantile(mahalanobis_dist, 0.995, na.rm = TRUE),
      .groups   = "drop"
    ) -> thresholds

  return(thresholds)

}

## -----------------------------------------------------------------------------
