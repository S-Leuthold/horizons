#' Finalize Dataset with Outlier Detection
#'
#' @description
#' Final quality control step that detects and optionally removes outliers
#' from both spectral data and response variables. Uses PCA projection and
#' Mahalanobis distance for spectral outliers.
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
#' @return A tibble with outliers flagged or removed, plus outlier_flag column
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
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Detect Spectral Outliers
  ## ---------------------------------------------------------------------------
  
  spectral_outliers <- character()
  
  if (spectral_outlier_method != "none") {
    
    if (verbose) {
      cli::cli_alert_info("Detecting spectral outliers using {.val {spectral_outlier_method}} method")
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
          cli::cli_alert_info("Using {.val {n_components}} PCs (Kaiser criterion, {.val {round(var_explained, 1)}}% variance)")
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
            cli::cli_alert_warning("Found {.val {length(spectral_outliers)}} spectral outliers")
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
      cli::cli_alert_info("Detecting response outliers using IQR method")
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
      cli::cli_alert_warning("Found {.val {length(response_outliers)}} response outliers")
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
      cli::cli_alert_info("Total outliers identified: {.val {length(all_outliers)}} out of {.val {nrow(dataset)}} samples")
    }
  }
  
  # Remove outliers if requested
  if (remove_outliers && length(all_outliers) > 0) {
    
    dataset <- dataset[-all_outliers, ]
    
    if (verbose) {
      cli::cli_alert_success("Removed {.val {length(all_outliers)}} outliers. Dataset now has {.val {nrow(dataset)}} samples")
    }
    
  } else if (!remove_outliers && length(all_outliers) > 0) {
    
    if (verbose) {
      cli::cli_alert_info("Outliers flagged but not removed. Use outlier_flag column to filter if needed")
    }
    
  }
  
  return(dataset)
  
}