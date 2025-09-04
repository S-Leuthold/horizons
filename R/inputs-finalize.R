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
#' \strong{Spectral Outlier Detection:}
#' \itemize{
#'   \item PCA projection using Kaiser criterion for dimensionality reduction
#'   \item Robust covariance estimation (MCD) to handle existing outliers
#'   \item Mahalanobis distance calculation in PC space
#'   \item Chi-square threshold testing at specified percentile
#' }
#' 
#' \strong{Response Outlier Detection:}
#' \itemize{
#'   \item Interquartile Range (IQR) method with configurable multiplier
#'   \item Identification of values beyond Q1 - k*IQR or Q3 + k*IQR
#'   \item Suitable for both normal and moderately skewed distributions
#' }
#' 
#' The function provides flexible handling through flagging vs. removal options,
#' allowing users to inspect outliers before making final decisions.
#'
#' @param dataset Tibble. Output from create_dataset() with spectra and response data
#' @param response_variable Character. Name of response variable to check for outliers
#' @param spectral_outlier_method Character. Method for spectral outlier detection:
#'   "mahalanobis" (default), "pca", or "none"
#' @param detect_response_outliers Logical. Detect outliers in response variable using IQR?
#'   Default: TRUE
#' @param spectral_cutoff Numeric. Percentile threshold for spectral outliers (0-1).
#'   Default: 0.975
#' @param response_cutoff Numeric. IQR multiplier for response outliers.
#'   Default: 1.5
#' @param remove_outliers Logical. Remove outliers (TRUE) or just flag them (FALSE)?
#'   Default: FALSE
#' @param verbose Logical. Print progress messages. Default: TRUE
#'
#' @return A tibble with the same structure as input plus quality control information:
#'   \describe{
#'     \item{<original_cols>}{All original columns from input dataset}
#'     \item{outlier_flag}{Character. "good" for normal samples, "outlier" for flagged samples}
#'   }
#'   
#'   If \code{remove_outliers = TRUE}, flagged samples are removed from the tibble.
#'   If \code{remove_outliers = FALSE} (default), all samples are retained with flags.
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
#' }
#' 
#' @seealso 
#' \code{\link{create_dataset}} for dataset preparation,
#' \code{\link{evaluate_models_local}} for model evaluation,
#' \code{\link{create_configs}} for configuration setup
#' 
#' @family inputs
#' @keywords quality-control outlier-detection
#'
#' @export
finalize_dataset <- function(dataset,
                            response_variable,
                            spectral_outlier_method = "mahalanobis",
                            detect_response_outliers = TRUE,
                            spectral_cutoff = 0.975,
                            response_cutoff = 1.5,
                            remove_outliers = FALSE,
                            verbose = TRUE) {
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation
  ## ---------------------------------------------------------------------------
  
  if (!is.data.frame(dataset)) {
    cli::cli_abort("▶ finalize_dataset: dataset must be a data frame or tibble")
  }
  
  if (!response_variable %in% names(dataset)) {
    cli::cli_abort("▶ finalize_dataset: Response variable {.val {response_variable}} not found in dataset")
  }
  
  spectral_outlier_method <- match.arg(spectral_outlier_method, 
                                       c("mahalanobis", "pca", "none"))
  
  # Display configuration summary
  spectral_cols <- sum(suppressWarnings(!is.na(as.numeric(names(dataset)))))
  config_info <- list(
    "Input samples" = format_metric(nrow(dataset), "count"),
    "Spectral features" = format_metric(spectral_cols, "count"),
    "Response variable" = response_variable,
    "Spectral outlier detection" = if (spectral_outlier_method == "none") "Disabled" else 
                                  paste0(spectral_outlier_method, " (", format_metric(spectral_cutoff * 100, "percentage"), " threshold)"),
    "Response outlier detection" = if (detect_response_outliers) paste0("IQR method (", response_cutoff, "× multiplier)") else "Disabled",
    "Action" = if (remove_outliers) "Remove outliers" else "Flag outliers only"
  )
  
  display_config_summary("Dataset Finalization Pipeline", config_info, verbose)
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Detect Spectral Outliers
  ## ---------------------------------------------------------------------------
  
  spectral_outliers <- character()
  
  # Track processing time
  total_start_time <- Sys.time()
  
  if (spectral_outlier_method != "none") {
    
    if (verbose) {
      cli::cli_text(format_header("Outlier Detection Pipeline", style = "single", center = FALSE))
      cli::cli_text("")
      cli::cli_text(format_tree_item("Spectral Outlier Detection", level = 0, is_last = !detect_response_outliers))
      cli::cli_text(format_tree_item(paste0("Method: ", spectral_outlier_method), level = 1, is_last = FALSE))
    }
    
    # Extract spectral matrix (only numeric columns)
    spectral_cols <- names(dataset)[grepl("^[0-9]+(\\.[0-9]+)?$", names(dataset))]
    spectral_matrix <- as.matrix(dataset[, spectral_cols])
    
    if (spectral_outlier_method == "mahalanobis") {
      
      # PCA projection for dimensionality reduction
      safely_execute(
        expr = {
          prcomp(spectral_matrix, center = TRUE, scale. = FALSE)
        },
        default_value = NULL,
        error_message = "PCA computation failed"
      ) -> pca_result_safe
      
      if (is.null(pca_result_safe$result)) {
        
        if (!is.null(pca_result_safe$error)) {
          
          cli::cli_alert_danger("PCA computation failed: {.emph {pca_result_safe$error$message}}")
          cli::cli_alert_info("This may be due to:")
          cli::cli_ul(c(
            "Insufficient sample size (need at least {.val {ncol(spectral_matrix) + 1}} samples)",
            "All-zero or constant spectral values",
            "Memory issues with large spectral matrix",
            "Numerical instability in covariance matrix"
          ))
          
        }
        
        cli::cli_warn("▶ finalize_dataset: PCA failed - skipping spectral outlier detection")
        spectral_outliers <- integer(0)  # No outliers detected
        
      } else {
        
        pca_result <- pca_result_safe$result
        
        # Use Kaiser criterion: keep components with eigenvalue > mean eigenvalue
        # This is less aggressive than 95% variance and more appropriate for high-dim data
        eigenvalues <- pca_result$sdev^2
        n_components <- sum(eigenvalues > mean(eigenvalues))
        
        # Ensure we have at least 3 components for Mahalanobis distance
        n_components <- max(n_components, 3)
        
        # But not more than n-1 (for numerical stability)
        n_components <- min(n_components, nrow(spectral_matrix) - 1)
        
        if (verbose) {
          var_explained <- sum(eigenvalues[1:n_components]) / sum(eigenvalues) * 100
          cli::cli_text(format_tree_item(paste0("Using ", n_components, " PCs (Kaiser criterion, ", round(var_explained, 1), "% variance)"), 
                                       level = 1, is_last = FALSE))
        }
        
        # Use robust covariance estimation
        pca_scores <- pca_result$x[, 1:n_components]
        
        safely_execute(
          expr = {
            robustbase::covMcd(pca_scores)
          },
          default_value = NULL,
          error_message = "Robust covariance estimation failed"
        ) -> robust_cov_result
        
        if (is.null(robust_cov_result$result)) {
          
          if (!is.null(robust_cov_result$error)) {
            
            cli::cli_alert_danger("Robust covariance estimation failed: {.emph {robust_cov_result$error$message}}")
            cli::cli_alert_info("This may be due to:")
            cli::cli_ul(c(
              "Insufficient sample size for robust estimation",
              "Perfect collinearity in PC scores",
              "Numerical issues in MCD algorithm",
              "All samples are identical"
            ))
            
          }
          
          cli::cli_warn("▶ finalize_dataset: Robust covariance failed - skipping spectral outlier detection")
          spectral_outliers <- integer(0)  # No outliers detected
          
        } else {
          
          robust_cov <- robust_cov_result$result
          
          # Calculate Mahalanobis distance
          mahal_dist <- sqrt(mahalanobis(x = pca_scores,
                                         center = robust_cov$center,
                                         cov = robust_cov$cov))
          
          # Chi-square threshold
          chi_threshold <- sqrt(qchisq(p = spectral_cutoff, df = n_components))
          
          # Identify outliers
          spectral_outliers <- which(mahal_dist > chi_threshold)
          
          if (verbose && length(spectral_outliers) > 0) {
            cli::cli_text(format_tree_item(paste0("⚠ Found ", length(spectral_outliers), " spectral outliers"), 
                                         level = 1, is_last = TRUE))
          }
          
        }  # Close robust_cov if-else
        
      }  # Close PCA if-else
      
    }
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Detect Response Outliers (IQR method)
  ## ---------------------------------------------------------------------------
  
  response_outliers <- character()
  
  if (detect_response_outliers) {
    
    if (verbose) {
      if (spectral_outlier_method == "none") {
        # Start outlier detection section if no spectral outliers
        cli::cli_text(format_header("Outlier Detection Pipeline", style = "single", center = FALSE))
        cli::cli_text("")
      }
      cli::cli_text(format_tree_item("Response Outlier Detection", level = 0, is_last = TRUE))
      cli::cli_text(format_tree_item("Method: IQR with threshold", level = 1, is_last = TRUE))
    }
    
    response_values <- dataset[[response_variable]]
    
    # Remove NAs for calculation
    valid_idx <- which(!is.na(response_values))
    valid_values <- response_values[valid_idx]
    
    # IQR method
    Q1 <- quantile(valid_values, 0.25)
    Q3 <- quantile(valid_values, 0.75)
    IQR <- Q3 - Q1
    
    lower_bound <- Q1 - response_cutoff * IQR
    upper_bound <- Q3 + response_cutoff * IQR
    
    response_outliers <- valid_idx[valid_values < lower_bound | valid_values > upper_bound]
    
    if (verbose && length(response_outliers) > 0) {
      cli::cli_text(format_tree_item(paste0("⚠ Found ", length(response_outliers), " response outliers"), 
                                   level = 1, is_last = TRUE))
    }
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 4: Combine and Flag/Remove Outliers
  ## ---------------------------------------------------------------------------
  
  # Combine all outlier indices
  all_outliers <- unique(c(spectral_outliers, response_outliers))
  
  # Add outlier flag column
  dataset$outlier_flag <- "good"
  
  if (length(all_outliers) > 0) {
    dataset$outlier_flag[all_outliers] <- "outlier"
    
    if (verbose) {
      cli::cli_text(format_tree_item(paste0("✓ ", length(all_outliers), " outliers detected"), level = 1, is_last = TRUE))
    }
  }
  
  # Remove outliers if requested
  final_samples <- nrow(dataset)
  if (remove_outliers && length(all_outliers) > 0) {
    dataset <- dataset[-all_outliers, ]
    final_samples <- nrow(dataset)
  }
  
  # Display final results summary
  total_time <- as.numeric(difftime(Sys.time(), total_start_time, units = "secs"))
  
  if (verbose) {
    
    results_info <- list(
      "Final samples" = format_metric(final_samples, "count"),
      "Outliers detected" = format_metric(length(all_outliers), "count"),
      "Spectral outliers" = if (length(spectral_outliers) > 0) format_metric(length(spectral_outliers), "count") else "None",
      "Response outliers" = if (length(response_outliers) > 0) format_metric(length(response_outliers), "count") else "None",
      "Action taken" = if (remove_outliers && length(all_outliers) > 0) "Outliers removed" else "Outliers flagged only"
    )
    
    display_operation_results("Dataset finalization", results_info, total_time, "complete", verbose)
    
  }
  
  return(dataset)
  
}