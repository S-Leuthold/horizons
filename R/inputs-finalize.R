#' Finalize Dataset with Outlier Detection
#'
#' @description
#' Performs comprehensive outlier detection as the final quality control step
#' before model evaluation. This function identifies problematic samples in both
#' spectral and response data using robust statistical methods, ensuring data
#' quality for downstream modeling workflows.
#'
#' @details
#' The outlier detection pipeline employs a two-pronged approach:
#'
#' **Spectral Outlier Detection:**
#' * PCA projection retaining 99% of variance (more information than Kaiser criterion)
#' * Standard covariance estimation in PC space for simplicity and stability
#' * Mahalanobis distance calculation to identify extreme spectra
#' * Chi-square threshold testing at user-specified percentile
#'
#' **Response Outlier Detection:**
#' * Interquartile Range (IQR) method with configurable multiplier
#' * Identification of values beyond Q1 - k×IQR or Q3 + k×IQR
#' * Automatic skewness detection with warnings for heavily skewed distributions
#' * Sample size warnings when n < 30
#'
#' The function provides flexible handling through flagging vs. removal options,
#' allowing users to inspect outliers before making final decisions. Diagnostic
#' warnings help users understand when outlier detection may struggle with their
#' specific data characteristics.
#'
#' @param dataset `[tibble]` Output from create_dataset() with spectra and response data
#' @param response_variable `[character]` Name of response variable to check for outliers
#' @param spectral_outlier_method `[character]` Method for spectral outlier detection.
#'   Options: `"mahalanobis"` (default), `"pca"`, or `"none"`
#' @param detect_response_outliers `[logical]` Detect outliers in response variable using IQR?
#'   Default: `TRUE`
#' @param spectral_cutoff `[numeric]` Percentile threshold for spectral outliers (0-1).
#'   Default: `0.975` (97.5th percentile)
#' @param response_cutoff `[numeric]` IQR multiplier for response outliers.
#'   Default: `1.5` (Tukey's standard)
#' @param remove_outliers `[logical]` Remove outliers (TRUE) or just flag them (FALSE)?
#'   Default: `FALSE` (flag only)
#' @param enforce_positive `[logical]` Remove samples with zero or negative response values?
#'   Useful when using log transformations. Default: `FALSE`
#' @param drop_na `[logical]` Remove samples with NA values in response variable?
#'   Required for some modeling algorithms. Default: `FALSE`
#' @param verbose `[logical]` Print progress messages and diagnostic warnings. Default: `TRUE`
#'
#' @return A `[tibble]` with the same structure as input plus quality control information:
#'   * `<original_cols>`: All original columns from input dataset
#'   * `outlier_flag`: Character. `"good"` for normal samples, `"outlier"` for flagged samples
#'
#'   If `remove_outliers = TRUE`, flagged samples are removed from the tibble.
#'   If `remove_outliers = FALSE` (default), all samples are retained with flags.
#'
#' @examples
#' \dontrun{
#' # Basic outlier detection and flagging
#' clean_data <- finalize_dataset(
#'   dataset = model_dataset,
#'   response_variable = "SOC",
#'   remove_outliers = FALSE  # Flag only
#' )
#'
#' # Strict outlier removal with custom thresholds
#' clean_data <- finalize_dataset(
#'   dataset = model_dataset,
#'   response_variable = "clay",
#'   spectral_cutoff = 0.95,    # More sensitive
#'   response_cutoff = 2.0,     # More conservative
#'   remove_outliers = TRUE     # Remove outliers
#' )
#'
#' # Skip spectral outlier detection
#' clean_data <- finalize_dataset(
#'   dataset = model_dataset,
#'   response_variable = "pH",
#'   spectral_outlier_method = "none"
#' )
#'
#' # Remove zero/negative values (important for log transformations)
#' clean_data <- finalize_dataset(
#'   dataset = model_dataset,
#'   response_variable = "SOC",
#'   enforce_positive = TRUE,  # Required for log(SOC)
#'   remove_outliers = TRUE
#' )
#'
#' # Complete data cleaning for modeling
#' clean_data <- finalize_dataset(
#'   dataset = model_dataset,
#'   response_variable = "SOC",
#'   enforce_positive = TRUE,  # Remove zeros/negatives
#'   drop_na = TRUE,           # Remove NAs
#'   remove_outliers = TRUE
#' )
#' }
#'
#' @seealso
#' [create_dataset()] for dataset preparation,
#' [evaluate_models_local()] for model evaluation,
#' [create_configs()] for configuration setup
#'
#' @family inputs
#' @keywords quality-control outlier-detection
#'
#' @importFrom cli cli_abort cli_alert_warning cli_alert_info cli_text
#' @importFrom stats quantile cov mahalanobis qchisq median sd
#'
#' @export
finalize_dataset <- function(dataset,
                            response_variable,
                            spectral_outlier_method = "mahalanobis",
                            detect_response_outliers = TRUE,
                            spectral_cutoff = 0.975,
                            response_cutoff = 1.5,
                            remove_outliers = FALSE,
                            enforce_positive = FALSE,
                            drop_na = FALSE,
                            verbose = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation
  ## ---------------------------------------------------------------------------

  ## Make sure we're working with actual data ----------------------------------

  if (!is.data.frame(dataset)) cli::cli_abort("dataset must be a data frame or tibble")

  ## Check that the response variable is actually in there ---------------------

  if (!response_variable %in% names(dataset)) cli::cli_abort("Response variable {.val {response_variable}} not found in dataset")

  ## Outlier detection math only works on numbers ------------------------------

  if (!is.numeric(dataset[[response_variable]])) cli::cli_abort("Response variable must be numeric for outlier detection")

  ## Can't detect spectral outliers without spectra ----------------------------

  spectral_pattern <- "^[0-9]+(\\.[0-9]+)?$"
  all_col_names    <- names(dataset)
  is_spectral_col  <- grepl(spectral_pattern, all_col_names)
  spectral_cols    <- all_col_names[is_spectral_col]

  if (length(spectral_cols) == 0) cli::cli_abort("No spectral columns detected in dataset")

  ## Keep cutoff parameters in sensible ranges ---------------------------------

  if (spectral_cutoff <= 0 || spectral_cutoff >= 1) cli::cli_abort("spectral_cutoff must be between 0 and 1 (you provided {spectral_cutoff})")

  if (response_cutoff <= 0) cli::cli_abort("response_cutoff must be positive (you provided {response_cutoff})")

  ## Need enough samples to make outlier detection meaningful ------------------

  if (nrow(dataset) < 10) cli::cli_abort("Need at least 10 samples for reliable outlier detection (dataset has {nrow(dataset)})")

  ## Validate the method argument ----------------------------------------------

  spectral_outlier_method <- match.arg(spectral_outlier_method,
                                       c("mahalanobis", "pca", "none"))

  ## Show the user what we're about to do --------------------------------------

  if (verbose) {

    cli::cli_text("{.strong Dataset Finalization Configuration}")
    cli::cli_text("├─ Input samples: {nrow(dataset)}")
    cli::cli_text("├─ Spectral features: {length(spectral_cols)}")
    cli::cli_text("├─ Response variable: {response_variable}")
    cli::cli_text("├─ Spectral outlier method: {spectral_outlier_method}")
    cli::cli_text("├─ Response outlier detection: {if (detect_response_outliers) 'Enabled' else 'Disabled'}")
    cli::cli_text("└─ Action: {if (remove_outliers) 'Remove outliers' else 'Flag only'}")

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Detect Spectral Outliers
  ## ---------------------------------------------------------------------------

  spectral_outliers <- integer(0)
  total_start_time  <- Sys.time()

  if (spectral_outlier_method != "none") {

    if (verbose) {

      cli::cli_text("{.strong Outlier Detection Pipeline}")
      cli::cli_text("├─ Spectral outliers")
      cli::cli_text("│  └─ Method: {spectral_outlier_method}")

    }

    ## Extract spectral matrix ------------------------------------------------

    spectral_matrix <- as.matrix(dataset[, spectral_cols])

    if (spectral_outlier_method == "mahalanobis") {

      ## Run PCA to reduce dimensionality -------------------------------------

      safely_execute(
        expr = {
          prcomp(spectral_matrix, center = TRUE, scale. = FALSE)
        },
        default_value = NULL,
        error_message = "PCA computation failed"
      ) -> pca_result_safe

      handle_results(
        safe_result   = pca_result_safe,
        error_title   = "PCA computation failed - skipping spectral outlier detection",
        error_hints   = c(
          "Check if sample size is sufficient (need at least {ncol(spectral_matrix) + 1} samples)",
          "Check for all-zero or constant spectral values",
          "Check for memory issues with large spectral matrix",
          "Check for numerical instability in covariance matrix"
        ),
        abort_on_null = FALSE
      ) -> pca_result

      if (!is.null(pca_result)) {

        ## Keep enough PCs to explain 99% of variance --------------------------

        variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
        cumulative_var     <- cumsum(variance_explained)
        n_components       <- which(cumulative_var >= 0.99)[1]

        ## But cap at n-1 for numerical stability ------------------------------

        n_components <- min(n_components, nrow(spectral_matrix) - 1, na.rm = TRUE)

        if (verbose) {

          cli::cli_text("│  └─ Using {n_components} PCs ({round(cumulative_var[n_components] * 100, 1)}% variance)")

        }

        ## Calculate Mahalanobis distance in PC space --------------------------

        pca_scores <- pca_result$x[, 1:n_components]
        center_pt  <- colMeans(pca_scores)
        cov_matrix <- cov(pca_scores)

        safely_execute(expr = {sqrt(mahalanobis(pca_scores, center = center_pt, cov = cov_matrix))},

                       default_value = NULL,
                       error_message = "Mahalanobis distance calculation failed") -> mahal_result_safe

        handle_results(
          safe_result   = mahal_result_safe,
          error_title   = "Mahalanobis calculation failed - skipping spectral outlier detection",
          error_hints   = c(
            "Check for singular covariance matrix (perfect collinearity in PC scores)",
            "Check for insufficient variance in PC scores",
            "Check for numerical precision issues"
          ),
          abort_on_null = FALSE
        ) -> mahal_dist

        if (!is.null(mahal_dist)) {

          ## Use spectral_cutoff threshold to catch extreme outliers ------------

          chi_threshold     <- sqrt(qchisq(p = spectral_cutoff, df = n_components))
          spectral_outliers <- which(mahal_dist > chi_threshold)

          if (verbose && length(spectral_outliers) > 0) {

            cli::cli_text("│  └─ ⚠ Found {length(spectral_outliers)} spectral outlier{?s}")

          } else if (verbose) {

            cli::cli_text("│  └─ ✓ No spectral outliers detected")

          }

        }

      }

    }

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Detect Response Outliers (IQR method)
  ## ---------------------------------------------------------------------------

  response_outliers <- integer(0)

  if (detect_response_outliers) {

    if (verbose) {

      ## Show header if we skipped spectral outliers ---------------------------

      if (spectral_outlier_method == "none") {

        cli::cli_text("{.strong Outlier Detection Pipeline}")

      }

      cli::cli_text("├─ Response outliers")
      cli::cli_text("│  └─ Method: IQR ({response_cutoff}× multiplier)")

    }

    response_values <- dataset[[response_variable]]

    ## Remove NAs for calculation -----------------------------------------------

    valid_idx    <- which(!is.na(response_values))
    valid_values <- response_values[valid_idx]

    ## Check for small sample size ----------------------------------------------

    if (length(valid_values) < 30 && verbose) {

      cli::cli_alert_warning("Small sample size (n = {length(valid_values)}) may reduce outlier detection reliability")

    }

    ## Calculate IQR bounds -----------------------------------------------------

    Q1  <- quantile(valid_values, 0.25)
    Q3  <- quantile(valid_values, 0.75)
    IQR <- Q3 - Q1

    lower_bound <- Q1 - response_cutoff * IQR
    upper_bound <- Q3 + response_cutoff * IQR

    ## Check for heavy skewness -------------------------------------------------

    skewness <- (mean(valid_values) - median(valid_values)) / sd(valid_values)

    if (abs(skewness) > 1 && verbose) {

      cli::cli_alert_warning("Response variable is heavily skewed (skewness = {round(skewness, 2)})")
      cli::cli_alert_info("Consider log-transformation or increasing response_cutoff for conservative detection")

    }

    ## Report bounds if verbose -------------------------------------------------

    if (verbose) {

      cli::cli_text("│  └─ Outlier bounds: [{round(lower_bound, 2)}, {round(upper_bound, 2)}]")

    }

    ## Identify outliers --------------------------------------------------------

    response_outliers <- valid_idx[valid_values < lower_bound | valid_values > upper_bound]

    ## Report results -----------------------------------------------------------

    if (verbose) {

      if (length(response_outliers) > 0) {

        cli::cli_text("│  └─ ⚠ Found {length(response_outliers)} response outlier{?s}")

      } else {

        cli::cli_text("│  └─ ✓ No response outliers detected")

      }

    }

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Combine and Flag/Remove Outliers
  ## ---------------------------------------------------------------------------

  ## Combine outlier indices from both methods ---------------------------------

  all_outliers <- unique(c(spectral_outliers, response_outliers))

  ## Add outlier flag column to dataset ----------------------------------------

  dataset$outlier_flag <- "good"

  if (length(all_outliers) > 0) {

    dataset$outlier_flag[all_outliers] <- "outlier"

  }

  ## Remove outliers if requested ----------------------------------------------

  final_samples <- nrow(dataset)

  if (remove_outliers && length(all_outliers) > 0) {

    dataset       <- dataset[-all_outliers, ]
    final_samples <- nrow(dataset)

  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Additional Data Cleaning
  ## ---------------------------------------------------------------------------

  n_nonpositive <- 0
  n_na_removed  <- 0

  if (enforce_positive || drop_na) {

    if (verbose) {

      cli::cli_text("├─ Data cleaning")

    }

    ## Remove zero or negative values if requested ---------------------------

    if (enforce_positive) {

      n_before      <- nrow(dataset)
      dataset       <- dataset[dataset[[response_variable]] > 0, ]
      n_nonpositive <- n_before - nrow(dataset)
      final_samples <- nrow(dataset)

      if (verbose) {
        cli::cli_text("│  ├─ Removed {n_nonpositive} non-positive value{?s}")
      }

    }

    ## Remove NA values if requested -----------------------------------------

    if (drop_na) {

      n_before     <- nrow(dataset)
      dataset      <- dataset[!is.na(dataset[[response_variable]]), ]
      n_na_removed <- n_before - nrow(dataset)
      final_samples <- nrow(dataset)

      if (verbose) {
        cli::cli_text("│  └─ Removed {n_na_removed} NA value{?s}")
      }

    }

  }

  ## Display final summary -----------------------------------------------------

  if (verbose) {

    total_time <- as.numeric(difftime(Sys.time(), total_start_time, units = "secs"))

    cli::cli_text("└─ {.strong Summary}")
    cli::cli_text("   ├─ Total outliers detected: {length(all_outliers)}")
    cli::cli_text("   ├─ Spectral outliers: {if (length(spectral_outliers) > 0) length(spectral_outliers) else 0}")
    cli::cli_text("   ├─ Response outliers: {if (length(response_outliers) > 0) length(response_outliers) else 0}")
    if (enforce_positive && n_nonpositive > 0) {
      cli::cli_text("   ├─ Non-positive values removed: {n_nonpositive}")
    }
    if (drop_na && n_na_removed > 0) {
      cli::cli_text("   ├─ NA values removed: {n_na_removed}")
    }
    cli::cli_text("   ├─ Final samples: {final_samples}")
    cli::cli_text("   ├─ Action: {if (remove_outliers && length(all_outliers) > 0) 'Removed' else 'Flagged only'}")
    cli::cli_text("   └─ Time: {round(total_time, 2)}s")

  }

  return(dataset)

}
