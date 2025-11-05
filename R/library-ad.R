#' Library Prediction Applicability Domain (AD) Functions
#'
#' @description
#' Functions for computing and applying applicability domain metrics in
#' library-based prediction. Uses Mahalanobis distance in model feature space
#' to assess whether new samples are within the training domain.
#'
#' @keywords internal

## -----------------------------------------------------------------------------
## AD Metadata Computation (Training Phase)
## -----------------------------------------------------------------------------

#' Compute Applicability Domain Metadata
#'
#' @description
#' Computes AD metadata from training feature matrix: centroid, covariance
#' matrix (with Ledoit-Wolf shrinkage), and distance thresholds for binning.
#'
#' @param feature_matrix Numeric matrix of preprocessed features (n × p).
#'   Must have column names. Rows are samples, columns are features (e.g., PCs).
#'
#' @return List with components:
#' \describe{
#'   \item{centroid}{Numeric vector (length p): feature means}
#'   \item{cov_matrix}{Numeric matrix (p × p): shrinkage covariance}
#'   \item{ad_thresholds}{Numeric vector (length 4): Q1, Q2, Q3, p99 thresholds}
#' }
#'
#' @details
#' ## Covariance Estimation
#'
#' Uses Ledoit-Wolf shrinkage via `corpcor::cov.shrink()` to handle high-
#' dimensional settings (p > n) and improve stability. Shrinkage target is
#' identity matrix scaled by average variance.
#'
#' ## Distance Thresholds
#'
#' Computes Mahalanobis distances for all training samples, then extracts:
#' - Q1: 25th percentile
#' - Q2: 50th percentile (median)
#' - Q3: 75th percentile
#' - p99: 99th percentile (OOD cutoff)
#'
#' Samples beyond p99 are considered out-of-domain (OOD).
#'
#' @examples
#' \dontrun{
#' # After preprocessing (e.g., PCA)
#' feature_matrix <- bake(prepped_recipe, training_data) %>%
#'   select(starts_with("PC")) %>%
#'   as.matrix()
#'
#' ad_metadata <- compute_ad_metadata(feature_matrix)
#' }
#'
#' @importFrom corpcor cov.shrink
#' @importFrom stats mahalanobis quantile
#' @importFrom cli cli_abort
#'
#' @keywords internal
compute_ad_metadata <- function(feature_matrix) {

  ## Validate inputs ---------------------------------------------------------

  if (!is.matrix(feature_matrix) || !is.numeric(feature_matrix)) {
    cli::cli_abort("feature_matrix must be a numeric matrix")
  }

  if (is.null(colnames(feature_matrix))) {
    cli::cli_abort("feature_matrix must have column names")
  }

  n_samples  <- nrow(feature_matrix)
  n_features <- ncol(feature_matrix)

  if (n_samples < 10) {
    cli::cli_abort(
      "feature_matrix must have at least 10 samples (has {n_samples})"
    )
  }

  ## Compute centroid --------------------------------------------------------

  centroid <- colMeans(feature_matrix, na.rm = FALSE)

  ## Compute shrinkage covariance --------------------------------------------

  ## Use Lediot-Wolf shrinkage (handles p > n, improves stability)
  cov_matrix <- corpcor::cov.shrink(feature_matrix, verbose = FALSE)

  ## Compute training distances ----------------------------------------------

  train_distances <- stats::mahalanobis(
    x      = feature_matrix,
    center = centroid,
    cov    = cov_matrix
  )

  ## Define AD thresholds (quartiles + p99) ----------------------------------

  ad_thresholds <- stats::quantile(
    train_distances,
    probs = c(0.25, 0.50, 0.75, 0.99),
    names = FALSE
  )

  ## Return metadata ---------------------------------------------------------

  list(
    centroid      = centroid,
    cov_matrix    = cov_matrix,
    ad_thresholds = ad_thresholds
  )

}

## -----------------------------------------------------------------------------
## AD Distance Calculation (Prediction Phase)
## -----------------------------------------------------------------------------

#' Calculate Applicability Domain Distance
#'
#' @description
#' Computes Mahalanobis distance from new samples to training centroid using
#' stored covariance matrix. Larger distances indicate samples further from
#' training domain.
#'
#' @param new_matrix Numeric matrix of preprocessed features for new samples
#'   (m × p). Must have same column names and order as training matrix.
#'
#' @param ad_metadata List from `compute_ad_metadata()` containing:
#'   - `centroid`: Training feature means
#'   - `cov_matrix`: Training covariance matrix
#'   - `ad_thresholds`: Distance thresholds for binning
#'
#' @return Numeric vector (length m) of Mahalanobis distances. Non-negative.
#'
#' @details
#' ## Mahalanobis Distance
#'
#' For a sample **x** with training centroid **μ** and covariance **Σ**:
#'
#' \deqn{D^2 = (x - \mu)^T \Sigma^{-1} (x - \mu)}
#'
#' This metric:
#' - Accounts for feature correlations (unlike Euclidean distance)
#' - Normalized by variance (scale-invariant)
#' - Follows χ² distribution under normality assumption
#'
#' ## Feature Space Matching
#'
#' New samples must be preprocessed identically to training (same SNV, PCA,
#' etc.). Column names are checked to ensure alignment.
#'
#' @examples
#' \dontrun{
#' # During prediction
#' new_features <- bake(prepped_recipe, new_data) %>%
#'   select(starts_with("PC")) %>%
#'   as.matrix()
#'
#' distances <- calculate_ad_distance(new_features, ad_metadata)
#' }
#'
#' @importFrom stats mahalanobis
#' @importFrom cli cli_abort
#'
#' @keywords internal
calculate_ad_distance <- function(new_matrix, ad_metadata) {

  ## Validate inputs ---------------------------------------------------------

  if (!is.matrix(new_matrix) || !is.numeric(new_matrix)) {
    cli::cli_abort("new_matrix must be a numeric matrix")
  }

  if (!is.list(ad_metadata) ||
      !all(c("centroid", "cov_matrix", "ad_thresholds") %in% names(ad_metadata))) {
    cli::cli_abort(
      "ad_metadata must be a list with centroid, cov_matrix, and ad_thresholds"
    )
  }

  ## Check feature alignment -------------------------------------------------

  n_features_new   <- ncol(new_matrix)
  n_features_train <- length(ad_metadata$centroid)

  if (n_features_new != n_features_train) {
    cli::cli_abort(
      "Number of features must match: new has {n_features_new}, training has {n_features_train}"
    )
  }

  new_names   <- colnames(new_matrix)
  train_names <- names(ad_metadata$centroid)

  if (!is.null(new_names) && !is.null(train_names)) {

    if (!identical(new_names, train_names)) {
      cli::cli_abort(
        "Feature column names must match training data"
      )
    }

  }

  ## Compute Mahalanobis distances -------------------------------------------

  distances <- stats::mahalanobis(
    x      = new_matrix,
    center = ad_metadata$centroid,
    cov    = ad_metadata$cov_matrix
  )

  distances

}

## -----------------------------------------------------------------------------
## AD Bin Assignment
## -----------------------------------------------------------------------------

#' Assign Applicability Domain Bins
#'
#' @description
#' Categorizes distances into quartile bins (Q1-Q4) plus out-of-domain (OOD).
#' Used for reporting and (optionally) distance-aware conformal calibration.
#'
#' @param distances Numeric vector of Mahalanobis distances from
#'   `calculate_ad_distance()`.
#'
#' @param thresholds Numeric vector (length 4) of distance cutoffs:
#'   [Q1, Q2, Q3, p99]. From `ad_metadata$ad_thresholds`.
#'
#' @return Factor (length = length(distances)) with levels:
#'   `"Q1"`, `"Q2"`, `"Q3"`, `"Q4"`, `"OOD"`.
#'
#' @details
#' ## Bin Definitions
#'
#' - **Q1** (0-25th percentile): Very safe, deep within training domain
#' - **Q2** (25-50th): Safe
#' - **Q3** (50-75th): Moderate risk
#' - **Q4** (75-99th): Higher risk, edge of training domain
#' - **OOD** (>99th): Out-of-domain, high extrapolation risk
#'
#' Predictions with OOD flag will abstain (return NA) in M4.3.
#'
#' ## Boundary Handling
#'
#' Uses `right = FALSE` in `cut()` → intervals are [a, b). Sample exactly
#' at threshold is assigned to lower bin (conservative).
#'
#' @examples
#' \dontrun{
#' distances <- calculate_ad_distance(new_features, ad_metadata)
#' bins <- assign_ad_bin(distances, ad_metadata$ad_thresholds)
#'
#' table(bins)
#' # Q1  Q2  Q3  Q4 OOD
#' # 45  50  48  35   2
#' }
#'
#' @importFrom cli cli_abort
#'
#' @keywords internal
assign_ad_bin <- function(distances, thresholds) {

  ## Validate inputs ---------------------------------------------------------

  if (!is.numeric(distances)) {
    cli::cli_abort("distances must be numeric")
  }

  if (any(distances < 0, na.rm = TRUE)) {
    cli::cli_abort("distances must be non-negative")
  }

  if (!is.numeric(thresholds) || length(thresholds) != 4) {
    cli::cli_abort("thresholds must be numeric vector of length 4")
  }

  ## Assign bins -------------------------------------------------------------

  bins <- cut(
    distances,
    breaks = c(0, thresholds, Inf),
    labels = c("Q1", "Q2", "Q3", "Q4", "OOD"),
    right  = TRUE,   # (a, b] intervals - value at threshold goes to current bin
    include.lowest = TRUE  # Include 0 in Q1
  )

  bins

}
