#' Simple Training Set Selection for Soil Covariate Prediction
#'
#' @description
#' Simplified selection of OSSL training data that prioritizes using more data
#' over complex selection algorithms. Default uses all available OSSL samples.
#'
#' @param ossl_pca_scores Tibble with OSSL PCA scores and soil properties
#' @param max_samples Integer or NULL. Maximum samples to use (NULL = use all)
#' @param prop Numeric. Proportion for training (rest for validation)
#' @param method Character. Selection method: "all", "random", or "nearest" 
#' @param unknown_pca_scores Optional tibble with unknown PCA scores (only needed for "nearest")
#' @param verbose Logical. Print progress messages
#'
#' @return List with train_data, val_data, and selection metadata
#' @export
select_simple_training_set <- function(ossl_pca_scores,
                                      max_samples = NULL,
                                      prop = 0.85,
                                      method = "all",
                                      unknown_pca_scores = NULL,
                                      verbose = TRUE) {
  
  if (verbose) {
    cli::cli_text("")
    cli::cli_text(format_header("Training Set Selection", style = "single", center = FALSE))
  }
  
  # Calculate total samples available
  n_available <- nrow(ossl_pca_scores)
  
  # Determine number of samples to use
  if (is.null(max_samples)) {
    n_select <- n_available  # Use all available samples
  } else {
    n_select <- min(max_samples, n_available)
  }
  
  if (verbose) {
    cli::cli_text(format_tree_item(paste0("Method: ", method), level = 1, is_last = FALSE))
    cli::cli_text(format_tree_item(paste0("Selecting ", format_metric(n_select, "count"), 
                                         " from ", format_metric(n_available, "count"), 
                                         " available samples"), level = 1, is_last = FALSE))
  }
  
  # Select samples based on method
  if (method == "all") {
    # Use all available samples (or up to max_samples)
    if (n_select == n_available) {
      selected_indices <- 1:n_available
      if (verbose) {
        cli::cli_text(format_tree_item("Using ALL available OSSL samples", 
                                     level = 1, is_last = FALSE))
      }
    } else {
      # If max_samples is set, randomly sample
      set.seed(0307)
      selected_indices <- sample(1:n_available, n_select)
      if (verbose) {
        cli::cli_text(format_tree_item(paste0("Randomly sampling ", n_select, " from ", n_available), 
                                     level = 1, is_last = FALSE))
      }
    }
    
  } else if (method == "random") {
    # Simple random selection
    set.seed(0307)
    selected_indices <- sample(1:n_available, n_select)
    
  } else if (method == "nearest") {
    # Distance-based selection (requires unknown_pca_scores)
    if (is.null(unknown_pca_scores)) {
      cli::cli_abort("Method 'nearest' requires unknown_pca_scores")
    }
    
    # Extract PCA dimensions
    pca_cols <- grep("^Dim\\.", names(unknown_pca_scores), value = TRUE)
    unknown_matrix <- as.matrix(unknown_pca_scores[, pca_cols])
    ossl_matrix <- as.matrix(ossl_pca_scores[, pca_cols])
    
    # Calculate mean distance from each OSSL sample to unknown centroid
    unknown_center <- colMeans(unknown_matrix)
    distances <- apply(ossl_matrix, 1, function(ossl_point) {
      sqrt(sum((ossl_point - unknown_center)^2))
    })
    
    # Select the nearest samples
    selected_indices <- order(distances)[1:n_select]
    
    if (verbose) {
      dist_range <- range(distances[selected_indices])
      cli::cli_text(format_tree_item(paste0("Distance range: ", 
                                           round(dist_range[1], 2), " - ", 
                                           round(dist_range[2], 2)), 
                                           level = 1, is_last = FALSE))
    }
    
  } else {
    cli::cli_abort("Unknown selection method: {method}. Use 'all', 'random', or 'nearest'")
  }
  
  # Split into train/validation
  n_train <- round(prop * n_select)
  n_val <- n_select - n_train
  
  # Shuffle and split
  set.seed(0307)
  shuffled <- sample(selected_indices)
  train_indices <- shuffled[1:n_train]
  val_indices <- shuffled[(n_train + 1):n_select]
  
  # Extract data
  train_data <- ossl_pca_scores[train_indices, ]
  val_data <- ossl_pca_scores[val_indices, ]
  
  if (verbose) {
    cli::cli_text(format_tree_item(paste0("Training: ", format_metric(nrow(train_data), "count"), 
                                         " samples"), level = 1, is_last = FALSE))
    cli::cli_text(format_tree_item(paste0("Validation: ", format_metric(nrow(val_data), "count"), 
                                         " samples"), level = 1, is_last = TRUE))
  }
  
  return(list(
    train_data = train_data,
    val_data = val_data,
    selected_indices = selected_indices,
    selection_method = method,
    selection_quality = list(
      n_selected = n_select,
      n_available = n_available,
      prop_selected = n_select / n_available
    )
  ))
}