#' Sample Model Configurations with Optional Factorial Design Support
#' 
#' @description
#' Intelligently samples model configurations from the full configuration space.
#' Ensures baseline coverage (no covariates) and supports factorial designs for
#' covariate interaction analysis. Uses stratified sampling to maintain balance
#' across model types, preprocessing methods, and feature selection approaches.
#'
#' @param configs Data frame of all possible model configurations
#' @param n_per_group Integer. Number of configurations to sample per pipeline block (default: 5)
#' @param ensure_baseline Logical. Include at least one no-covariate config per block (default: TRUE)
#' @param factorial_pairs List of character vectors. Covariate pairs for factorial coverage (default: NULL)
#' @param verbose Logical. Print detailed sampling diagnostics (default: FALSE)
#'
#' @return A data frame of sampled configurations maintaining the structure of the input
#'
#' @details
#' The function operates in four phases:
#' 1. Baseline reservation: Ensures each pipeline block has a no-covariate configuration
#' 2. Factorial reservation: Creates 2x2 factorial designs for specified covariate pairs
#' 3. Budget calculation: Determines remaining slots per pipeline block
#' 4. Diverse sampling: Fills remaining slots with stratified random sampling
#'
#' A pipeline block is defined as a unique combination of:
#' model × transformation × preprocessing × feature_selection
#'
#' @examples
#' \dontrun{
#' # Basic usage with defaults
#' sampled <- sample_configs(all_configs, n_per_group = 5)
#' 
#' # With factorial design for covariate interactions
#' sampled <- sample_configs(all_configs, 
#'                          n_per_group = 10,
#'                          factorial_pairs = list(c("Clay", "pH"), 
#'                                               c("AI", "MAP")))
#' }
#'
#' @export
sample_configs <- function(configs,
                          n_per_group = 5,
                          ensure_baseline = TRUE,
                          factorial_pairs = NULL,
                          verbose = FALSE) {
  
  ## ---------------------------------------------------------------------------
  ## Step 0: Validate inputs and prepare data
  ## ---------------------------------------------------------------------------
  
  # Basic input validation
  if (!is.data.frame(configs)) {
    stop("configs must be a data frame")
  }
  
  if (nrow(configs) == 0) {
    if (verbose) cli::cli_alert_warning("No configurations provided")
    return(tibble::tibble())
  }
  
  # Add helper columns for sampling logic
  configs %>%
    dplyr::mutate(
      # Count covariates in each configuration
      n_covariates = purrr::map_int(covariates, function(x) {
        if (is.null(x) || length(x) == 0) return(0)
        
        # Convert to character string
        cov_str <- if (is.list(x)) {
          paste(unlist(x), collapse = "+")
        } else {
          paste(as.character(x), collapse = "+")
        }
        
        # Check for empty cases (now cov_str is always a single string)
        if (length(cov_str) == 0 || cov_str == "" || cov_str == "NA" || cov_str == "NoCovs") return(0)
        
        # Count by splitting
        length(trimws(unlist(strsplit(cov_str, "[+,;]"))))
      }),
      
      # Create unique identifier for each pipeline block
      block_id = paste(model, transformation, preprocessing, feature_selection, sep = "_")
    ) -> configs_prep
  
  if (verbose) {
    cli::cli_h3("Configuration Sampling")
    cli::cli_alert_info("Total configurations: {nrow(configs_prep)}")
    cli::cli_alert_info("Pipeline blocks: {dplyr::n_distinct(configs_prep$block_id)}")
    cli::cli_alert_info("Target per block: {n_per_group}")
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Reserve baseline configurations (no covariates)
  ## ---------------------------------------------------------------------------
  
  reserved_configs <- tibble::tibble()
  
  if (ensure_baseline) {
    configs_prep %>%
      dplyr::filter(n_covariates == 0) %>%
      dplyr::group_by(block_id) %>%
      dplyr::slice_sample(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(sampling_reason = "baseline") -> baseline_configs
    
    reserved_configs <- dplyr::bind_rows(reserved_configs, baseline_configs)
    
    if (verbose) {
      cli::cli_alert_success("Reserved {nrow(baseline_configs)} baseline configurations")
    }
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Reserve factorial configurations for covariate pairs
  ## ---------------------------------------------------------------------------
  
  if (!is.null(factorial_pairs) && length(factorial_pairs) > 0) {
    
    factorial_configs <- tibble::tibble()
    
    for (pair in factorial_pairs) {
      # Validate pair
      if (length(pair) != 2) {
        if (verbose) cli::cli_alert_warning("Skipping invalid pair (need exactly 2 covariates)")
        next
      }
      
      cov_a <- pair[1]
      cov_b <- pair[2]
      
      # Find configurations for each factorial cell
      configs_prep %>%
        dplyr::anti_join(reserved_configs, by = names(configs_prep)) %>%
        dplyr::mutate(
          # Parse covariates for this configuration
          parsed_covs = purrr::map(covariates, function(x) {
            if (is.null(x) || length(x) == 0) return(character(0))
            cov_str <- if (is.list(x)) {
              paste(unlist(x), collapse = "+")
            } else {
              paste(as.character(x), collapse = "+")
            }
            if (length(cov_str) == 0 || cov_str == "" || cov_str == "NA" || cov_str == "NoCovs") return(character(0))
            trimws(unlist(strsplit(cov_str, "[+,;]")))
          }),
          
          # Check presence of each covariate
          has_a = purrr::map_lgl(parsed_covs, ~cov_a %in% .x),
          has_b = purrr::map_lgl(parsed_covs, ~cov_b %in% .x),
          
          # Assign to factorial cell
          factorial_cell = dplyr::case_when(
            has_a & has_b ~ "both",
            has_a & !has_b ~ "a_only", 
            !has_a & has_b ~ "b_only",
            TRUE ~ "neither"
          )
        ) %>%
        dplyr::filter(factorial_cell != "neither") %>%
        dplyr::group_by(block_id, factorial_cell) %>%
        dplyr::slice_sample(n = 1) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(sampling_reason = paste0("factorial_", cov_a, "_x_", cov_b)) %>%
        dplyr::select(-parsed_covs, -has_a, -has_b, -factorial_cell) -> pair_configs
      
      factorial_configs <- dplyr::bind_rows(factorial_configs, pair_configs)
      
      if (verbose) {
        cli::cli_alert_info("Reserved {nrow(pair_configs)} configs for {cov_a} × {cov_b} factorial")
      }
    }
    
    # Add to reserved configs
    reserved_configs <- dplyr::bind_rows(reserved_configs, factorial_configs)
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Calculate remaining budget and sample additional configs
  ## ---------------------------------------------------------------------------
  
  # Count how many configs we've reserved per block
  reserved_configs %>%
    dplyr::count(block_id, name = "n_reserved") -> reserved_counts
  
  # Calculate remaining budget per block
  configs_prep %>%
    dplyr::distinct(block_id) %>%
    dplyr::left_join(reserved_counts, by = "block_id") %>%
    dplyr::mutate(
      n_reserved = tidyr::replace_na(n_reserved, 0),
      n_remaining = pmax(0, n_per_group - n_reserved)
    ) -> block_budgets
  
  # Get configurations not yet sampled
  configs_prep %>%
    dplyr::anti_join(reserved_configs, by = setdiff(names(configs_prep), "sampling_reason")) -> available_configs
  
  # Sample remaining slots per block
  additional_configs <- tibble::tibble()
  
  for (block in unique(block_budgets$block_id)) {
    # Get budget for this block
    budget <- block_budgets %>%
      dplyr::filter(block_id == block) %>%
      dplyr::pull(n_remaining)
    
    if (budget > 0) {
      # Sample from available configs for this block
      block_configs <- available_configs %>%
        dplyr::filter(block_id == block)
      
      # Calculate how many to sample (minimum of budget or available configs)
      n_to_sample <- min(budget, nrow(block_configs))
      
      if (n_to_sample > 0) {
        block_configs %>%
          dplyr::slice_sample(n = n_to_sample) %>%
          dplyr::mutate(sampling_reason = "diverse") -> block_sample
        
        additional_configs <- dplyr::bind_rows(additional_configs, block_sample)
      }
    }
  }
  
  if (verbose && nrow(additional_configs) > 0) {
    cli::cli_alert_success("Sampled {nrow(additional_configs)} additional configurations")
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 4: Combine and finalize sampled configurations
  ## ---------------------------------------------------------------------------
  
  # Combine all sampled configurations
  dplyr::bind_rows(reserved_configs, additional_configs) %>%
    dplyr::select(-n_covariates, -block_id, -sampling_reason) %>%
    dplyr::distinct() %>%
    dplyr::slice_sample(prop = 1) -> final_configs  # Shuffle to avoid ordering bias
  
  ## ---------------------------------------------------------------------------
  ## Step 5: Report sampling summary if verbose
  ## ---------------------------------------------------------------------------
  
  if (verbose) {
    cli::cli_h3("Sampling Summary")
    cli::cli_alert_success("Total configurations sampled: {nrow(final_configs)}")
    
    # Model distribution
    final_configs %>%
      dplyr::count(model) -> model_dist
    
    cli::cli_text("Models: {paste(model_dist$model, '(', model_dist$n, ')', collapse = ', ')}")
    
    # Covariate distribution
    final_configs %>%
      dplyr::mutate(
        n_cov = purrr::map_int(covariates, function(x) {
          if (is.null(x) || length(x) == 0) return(0)
          cov_str <- if (is.list(x)) {
            paste(unlist(x), collapse = "+")
          } else {
            paste(as.character(x), collapse = "+")
          }
          if (length(cov_str) == 0 || cov_str == "" || cov_str == "NA" || cov_str == "NoCovs") return(0)
          length(trimws(unlist(strsplit(cov_str, "[+,;]"))))
        })
      ) %>%
      dplyr::count(n_cov) %>%
      dplyr::arrange(n_cov) -> cov_dist
    
    cli::cli_text("Covariate distribution: {paste(cov_dist$n_cov, 'covs:', cov_dist$n, collapse = ', ')}")
  }
  
  return(final_configs)
}