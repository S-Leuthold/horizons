#' Evaluate Model Configuration with Fit-Level Parallelization
#'
#' @description
#' Evaluates a single model configuration with parallel tuning operations optimized
#' for HPC environments. This function handles the actual model fitting with
#' parallelization at the tuning level (grid search and Bayesian optimization)
#' rather than at the model level.
#'
#' Key features:
#' - Parallel grid search across hyperparameter combinations
#' - Parallel Bayesian optimization across CV folds
#' - Automatic worker allocation based on task requirements
#' - Memory-efficient processing with minimal overhead
#' - Comprehensive error handling and recovery
#'
#' @param config_row A single-row tibble containing model configuration. Must include:
#'   `model`, `transformation`, `preprocessing`, `feature_selection`, `covariates`, `include_covariates`
#' @param input_data A tibble with preprocessed spectral data including `Sample_ID`,
#'   wavenumber columns, and the target response variable
#' @param covariate_data Optional tibble of predicted covariates matched by `Sample_ID`
#' @param variable Character. Name of the response variable to predict
#' @param row_index Integer. Row index in the configuration grid for labeling
#' @param output_dir Character. Directory path for saving outputs and logs
#' @param n_workers Integer. Number of parallel workers to use (default = NULL, auto-detect)
#' @param grid_size Integer. Number of grid search candidates (default = 10)
#' @param bayesian_iter Integer. Number of Bayesian optimization iterations (default = 15)
#' @param cv_folds Integer. Number of cross-validation folds (default = 5)
#' @param pruning Logical. Enable early pruning of poor models (default = TRUE)
#' @param save_output Logical. Save full model output to disk (default = FALSE)
#' @param verbose Logical. Print detailed progress messages (default = TRUE)
#'
#' @return A list containing:
#' \itemize{
#'   \item \strong{status_summary}: One-row tibble with metrics and status
#'   \item \strong{saved_path}: Path to saved model file (if save_output = TRUE)
#'   \item \strong{timing_info}: Detailed timing for each phase
#'   \item \strong{memory_info}: Memory usage statistics
#' }
#'
#' @details
#' ## Parallelization Strategy
#'
#' The function implements intelligent parallelization:
#'
#' \strong{Grid Search Phase}:
#' - Parallelizes across hyperparameter combinations
#' - Each worker evaluates one combination across all CV folds
#' - Uses work-stealing for automatic load balancing
#'
#' \strong{Bayesian Optimization Phase}:
#' - Parallelizes across CV folds within each iteration
#' - Sequential iterations (each builds on previous results)
#' - Typically uses fewer workers (limited by CV folds)
#'
#' \strong{Final Fitting}:
#' - Sequential fitting of best model on full training set
#' - Evaluation on holdout test set
#'
#' ## Memory Management
#'
#' - Lightweight garbage collection between phases
#' - Efficient data passing to workers
#' - Automatic cleanup of temporary objects
#'
#' @examples
#' \dontrun{
#' result <- evaluate_model_fit_parallel(
#'   config_row     = config[1, ],
#'   input_data     = spectral_data,
#'   covariate_data = predicted_covs,
#'   variable       = "MAOM_C_g_kg",
#'   row_index      = 1,
#'   output_dir     = "outputs",
#'   n_workers      = 50
#' )
#' }
#'
#' @seealso
#' \code{\link{run_hpc_evaluation}}, \code{\link{manage_worker_pool}},
#' \code{\link{evaluate_model_config}}
#'
#' @importFrom cli cli_h2 cli_alert_success cli_alert_warning cli_alert_danger cli_alert_info
#' @importFrom dplyr tibble mutate filter arrange slice rename select pull
#' @importFrom fs path
#' @importFrom qs qsave
#' @importFrom glue glue
#' @importFrom future plan
#' @importFrom furrr future_map furrr_options
#' @importFrom tune tune_grid tune_bayes control_grid control_bayes select_best finalize_workflow
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom rsample initial_split training testing vfold_cv
#' @importFrom yardstick metric_set rrmse rsq
#' @importFrom parsnip fit
#' @importFrom recipes prep bake
#' @importFrom hardhat extract_parameter_set_dials
#' @importFrom dials finalize
#' @importFrom rlang sym
#' @export

evaluate_model_fit_parallel <- function(config_row,
                                       input_data,
                                       covariate_data = NULL,
                                       variable,
                                       row_index,
                                       output_dir,
                                       n_workers      = NULL,
                                       grid_size      = 10,
                                       bayesian_iter  = 15,
                                       cv_folds       = 5,
                                       pruning        = TRUE,
                                       save_output    = FALSE,
                                       verbose        = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Initialize Timing and Memory Tracking
  ## ---------------------------------------------------------------------------

  start_time     <- Sys.time()
  start_memory   <- as.numeric(pryr::mem_used()) / 1073741824  # GB
  timing_info    <- list()
  memory_info    <- list()

  ## Create workflow ID --------------------------------------------------------

  wflow_id <- clean_workflow_id(
    model             = config_row$model,
    transformation    = config_row$transformation,
    preprocessing     = config_row$preprocessing,
    feature_selection = config_row$feature_selection,
    covariates        = config_row$covariates
  )

  if (verbose) {
    cli::cli_h2("Evaluating: {wflow_id}")
  }

  ## ---------------------------------------------------------------------------
  ## Step 1: Data Preparation and Splitting
  ## ---------------------------------------------------------------------------

  prep_start <- Sys.time()

  ## Validate response variable ------------------------------------------------

  if (!variable %in% colnames(input_data)) {
    
    error_msg <- glue::glue("Variable '{variable}' not found in input_data")
    
    if (verbose) {
      cli::cli_alert_danger(error_msg)
    }
    
    return(list(
      status_summary = tibble::tibble(
        row          = row_index,
        wflow_id     = wflow_id,
        rsq          = NA_real_,
        rmse         = NA_real_,
        rrmse        = NA_real_,
        saved_path   = NA_character_,
        error_message = error_msg,
        status       = "error"
      ),
      saved_path     = NA_character_,
      timing_info    = list(total_seconds = 0),
      memory_info    = list(peak_gb = start_memory)
    ))
  }

  ## Rename response for consistency -------------------------------------------
  
  ## Safe rename without rlang dependency --------------------------------------
  
  names(input_data)[names(input_data) == variable] <- "Response"

  ## Create train/test split ---------------------------------------------------

  set.seed(row_index * 123)  # Reproducible split based on row index
  
  split <- rsample::initial_split(input_data, prop = 0.8, strata = Response)
  train <- rsample::training(split)
  test  <- rsample::testing(split)
  
  ## Create CV folds -----------------------------------------------------------
  
  folds <- rsample::vfold_cv(train, v = cv_folds, strata = Response)

  timing_info$prep_seconds <- as.numeric(difftime(Sys.time(), prep_start, units = "secs"))

  ## ---------------------------------------------------------------------------
  ## Step 2: Build Recipe and Workflow
  ## ---------------------------------------------------------------------------

  recipe_start <- Sys.time()

  if (verbose) {
    cli::cli_alert_info("Building recipe and workflow")
  }

  ## Build recipe --------------------------------------------------------------

  safely_execute(
    expr = {
      build_recipe(
        input_data               = train,
        response_transformation  = config_row$transformation,
        spectral_transformation  = config_row$preprocessing,
        feature_selection_method = config_row$feature_selection,
        covariate_selection      = config_row$covariates[[1]],
        covariate_data           = covariate_data
      )
    },
    default_value = NULL,
    error_message = glue::glue("Recipe building failed for {wflow_id}")
  ) -> recipe_result

  if (is.null(recipe_result$result)) {
    
    error_msg <- conditionMessage(recipe_result$error)
    
    if (verbose) {
      cli::cli_alert_danger("Recipe building failed: {error_msg}")
    }
    
    return(list(
      status_summary = tibble::tibble(
        row          = row_index,
        wflow_id     = wflow_id,
        rsq          = NA_real_,
        rmse         = NA_real_,
        rrmse        = NA_real_,
        saved_path   = NA_character_,
        error_message = error_msg,
        status       = "error"
      ),
      saved_path     = NA_character_,
      timing_info    = timing_info,
      memory_info    = list(peak_gb = start_memory)
    ))
  }

  recipe <- recipe_result$result

  ## Define model specification ------------------------------------------------

  safely_execute(
    expr = {
      define_model_specifications(model_type = config_row$model)
    },
    default_value = NULL,
    error_message = glue::glue("Model specification failed for {wflow_id}")
  ) -> model_spec_result

  if (is.null(model_spec_result$result)) {
    
    error_msg <- conditionMessage(model_spec_result$error)
    
    if (verbose) {
      cli::cli_alert_danger("Model specification failed: {error_msg}")
    }
    
    return(list(
      status_summary = tibble::tibble(
        row          = row_index,
        wflow_id     = wflow_id,
        rsq          = NA_real_,
        rmse         = NA_real_,
        rrmse        = NA_real_,
        saved_path   = NA_character_,
        error_message = error_msg,
        status       = "error"
      ),
      saved_path     = NA_character_,
      timing_info    = timing_info,
      memory_info    = list(peak_gb = start_memory)
    ))
  }

  model_spec <- model_spec_result$result

  ## Create workflow -----------------------------------------------------------

  workflow <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(model_spec)

  ## Handle mtry parameter finalization ----------------------------------------

  param_set <- hardhat::extract_parameter_set_dials(workflow)

  if ("mtry" %in% param_set$name) {
    
    eval_data <- recipe %>%
      recipes::prep() %>%
      recipes::bake(new_data = NULL) %>%
      dplyr::select(-Project, -Sample_ID, -Response)
    
    param_set <- param_set %>%
      dials::finalize(eval_data)
    
    rm(eval_data)
  }

  timing_info$recipe_seconds <- as.numeric(difftime(Sys.time(), recipe_start, units = "secs"))

  ## ---------------------------------------------------------------------------
  ## Step 3: Grid Search with Parallel Tuning
  ## ---------------------------------------------------------------------------

  grid_start <- Sys.time()

  if (verbose) {
    cli::cli_alert_info("Starting grid search ({grid_size} candidates × {cv_folds} folds)")
  }

  ## Calculate optimal workers for grid search ---------------------------------

  n_grid_tasks <- grid_size  # Each hyperparameter combo is a task

  ## Auto-detect workers if not specified --------------------------------------

  if (is.null(n_workers)) {
    n_workers <- min(50, parallel::detectCores() - 2)
  }

  ## Set up worker pool for grid search ----------------------------------------

  worker_config_grid <- manage_worker_pool(
    n_workers_requested = n_workers,
    task_type           = "grid_search",
    n_tasks             = n_grid_tasks,
    cv_folds            = cv_folds,
    verbose             = FALSE
  )

  if (verbose) {
    cli::cli_alert_info("Grid search using {worker_config_grid$n_workers} workers")
  }

  ## Run parallel grid search --------------------------------------------------

  suppressWarnings({
    suppressMessages({
      safely_execute(
        expr = {
          tune::tune_grid(
            object     = workflow,
            resamples  = folds,
            param_info = param_set,
            grid       = grid_size,
            metrics    = yardstick::metric_set(rrmse, rsq),
            control    = tune::control_grid(
              save_pred     = FALSE,
              save_workflow = TRUE,
              verbose       = FALSE,
              allow_par     = TRUE,  # Enable parallelization
              parallel_over = "everything",
              pkgs          = c("horizons", "tidymodels")
            )
          )
        },
        default_value = NULL,
        error_message = glue::glue("Grid search failed for {wflow_id}")
      ) -> grid_result
    })
  })

  ## Clean up worker pool ------------------------------------------------------

  worker_config_grid$cleanup_fn()

  timing_info$grid_seconds <- as.numeric(difftime(Sys.time(), grid_start, units = "secs"))

  if (is.null(grid_result$result)) {
    
    error_msg <- conditionMessage(grid_result$error)
    
    if (verbose) {
      cli::cli_alert_danger("Grid search failed: {error_msg}")
    }
    
    return(list(
      status_summary = tibble::tibble(
        row          = row_index,
        wflow_id     = wflow_id,
        rsq          = NA_real_,
        rmse         = NA_real_,
        rrmse        = NA_real_,
        saved_path   = NA_character_,
        error_message = error_msg,
        status       = "error"
      ),
      saved_path     = NA_character_,
      timing_info    = timing_info,
      memory_info    = list(peak_gb = as.numeric(pryr::mem_used()) / 1073741824)
    ))
  }

  grid_res <- grid_result$result

  ## ---------------------------------------------------------------------------
  ## Step 4: Optional Early Pruning
  ## ---------------------------------------------------------------------------

  if (pruning) {
    
    min_rrmse <- min(
      tune::collect_metrics(grid_res) %>%
        dplyr::filter(.metric == "rrmse") %>%
        dplyr::pull(mean),
      na.rm = TRUE
    )
    
    if (is.infinite(min_rrmse) || min_rrmse > 50) {
      
      if (verbose) {
        cli::cli_alert_warning("Pruning model: Poor grid search performance (RRMSE = {round(min_rrmse, 1)}%)")
      }
      
      return(list(
        status_summary = tibble::tibble(
          row          = row_index,
          wflow_id     = wflow_id,
          rsq          = NA_real_,
          rmse         = NA_real_,
          rrmse        = min_rrmse,
          saved_path   = NA_character_,
          error_message = glue::glue("Pruned: RRMSE {round(min_rrmse, 1)}%"),
          status       = "pruned"
        ),
        saved_path     = NA_character_,
        timing_info    = timing_info,
        memory_info    = list(peak_gb = as.numeric(pryr::mem_used()) / 1073741824)
      ))
    }
  }

  ## Memory cleanup after grid search -----------------------------------------

  gc(verbose = FALSE, full = FALSE)
  memory_info$post_grid_gb <- as.numeric(pryr::mem_used()) / 1073741824

  ## ---------------------------------------------------------------------------
  ## Step 5: Bayesian Optimization with Parallel CV
  ## ---------------------------------------------------------------------------

  bayes_start <- Sys.time()

  if (verbose) {
    cli::cli_alert_info("Starting Bayesian optimization ({bayesian_iter} iterations)")
  }

  ## Set up worker pool for Bayesian optimization -----------------------------

  worker_config_bayes <- manage_worker_pool(
    n_workers_requested = n_workers,
    task_type           = "bayesian",
    n_tasks             = cv_folds,  # Can only parallelize across CV folds
    cv_folds            = cv_folds,
    verbose             = FALSE
  )

  if (verbose) {
    cli::cli_alert_info("Bayesian optimization using {worker_config_bayes$n_workers} workers per iteration")
  }

  ## Run Bayesian optimization -------------------------------------------------

  suppressWarnings({
    suppressMessages({
      safely_execute(
        expr = {
          tune::tune_bayes(
            object     = workflow,
            initial    = grid_res,  # Start from grid search results
            resamples  = folds,
            param_info = param_set,
            iter       = bayesian_iter,
            metrics    = yardstick::metric_set(rrmse, rsq),
            control    = tune::control_bayes(
              save_pred     = FALSE,
              save_workflow = TRUE,
              verbose       = FALSE,
              seed          = 307,
              no_improve    = 10L,
              allow_par     = TRUE,  # Enable parallelization
              parallel_over = "resamples",  # Parallelize across CV folds
              pkgs          = c("horizons", "tidymodels")
            )
          )
        },
        default_value = NULL,
        error_message = glue::glue("Bayesian optimization failed for {wflow_id}")
      ) -> bayes_result
    })
  })

  ## Clean up worker pool ------------------------------------------------------

  worker_config_bayes$cleanup_fn()

  timing_info$bayes_seconds <- as.numeric(difftime(Sys.time(), bayes_start, units = "secs"))

  if (is.null(bayes_result$result)) {
    
    error_msg <- conditionMessage(bayes_result$error)
    
    if (verbose) {
      cli::cli_alert_danger("Bayesian optimization failed: {error_msg}")
    }
    
    return(list(
      status_summary = tibble::tibble(
        row          = row_index,
        wflow_id     = wflow_id,
        rsq          = NA_real_,
        rmse         = NA_real_,
        rrmse        = NA_real_,
        saved_path   = NA_character_,
        error_message = error_msg,
        status       = "error"
      ),
      saved_path     = NA_character_,
      timing_info    = timing_info,
      memory_info    = list(peak_gb = as.numeric(pryr::mem_used()) / 1073741824)
    ))
  }

  bayes_res <- bayes_result$result

  ## Memory cleanup after Bayesian optimization -------------------------------

  gc(verbose = FALSE, full = FALSE)
  memory_info$post_bayes_gb <- as.numeric(pryr::mem_used()) / 1073741824

  ## ---------------------------------------------------------------------------
  ## Step 6: Finalize and Evaluate Best Model
  ## ---------------------------------------------------------------------------

  eval_start <- Sys.time()

  if (verbose) {
    cli::cli_alert_info("Finalizing and evaluating best model")
  }

  ## Select best parameters ----------------------------------------------------

  best_params <- tune::select_best(bayes_res, metric = "rrmse")

  ## Finalize workflow ---------------------------------------------------------

  final_wf <- tune::finalize_workflow(workflow, best_params)

  ## Fit on full training set --------------------------------------------------

  fitted_wf <- parsnip::fit(final_wf, data = train)

  ## Predict on test set -------------------------------------------------------

  test_pred <- predict(fitted_wf, new_data = test) %>%
    dplyr::bind_cols(test %>% dplyr::select(Response))

  ## Calculate metrics ---------------------------------------------------------

  metrics <- yardstick::metrics(test_pred, truth = Response, estimate = .pred)

  rmse_val <- metrics$.estimate[metrics$.metric == "rmse"]
  rsq_val  <- metrics$.estimate[metrics$.metric == "rsq"]
  mae_val  <- metrics$.estimate[metrics$.metric == "mae"]

  ## Calculate RRMSE -----------------------------------------------------------

  mean_response <- mean(test$Response, na.rm = TRUE)
  rrmse_val     <- (rmse_val / mean_response) * 100

  ## Calculate RPD -------------------------------------------------------------

  sd_response <- sd(test$Response, na.rm = TRUE)
  rpd_val     <- sd_response / rmse_val

  timing_info$eval_seconds <- as.numeric(difftime(Sys.time(), eval_start, units = "secs"))

  ## ---------------------------------------------------------------------------
  ## Step 7: Save Output (if requested)
  ## ---------------------------------------------------------------------------

  saved_path <- NA_character_

  if (save_output) {
    
    file_name  <- glue::glue("model_{row_index}_{wflow_id}.qs")
    saved_path <- fs::path(output_dir, file_name)
    
    model_output <- list(
      workflow     = workflow,
      tuning_results = list(grid = grid_res, bayes = bayes_res),
      best_params  = best_params,
      final_workflow = final_wf,
      fitted_workflow = fitted_wf,
      metrics      = list(
        rmse  = rmse_val,
        rrmse = rrmse_val,
        rsq   = rsq_val,
        mae   = mae_val,
        rpd   = rpd_val
      )
    )
    
    qs::qsave(model_output, saved_path)
    
    if (verbose) {
      cli::cli_alert_success("Model saved to: {saved_path}")
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 8: Compile Results
  ## ---------------------------------------------------------------------------

  ## Calculate total time and memory -------------------------------------------

  total_time    <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  peak_memory   <- as.numeric(pryr::mem_used()) / 1073741824
  memory_delta  <- peak_memory - start_memory

  timing_info$total_seconds <- total_time
  memory_info$peak_gb       <- peak_memory
  memory_info$delta_gb      <- memory_delta

  ## Create status summary -----------------------------------------------------

  status_summary <- tibble::tibble(
    row          = row_index,
    wflow_id     = wflow_id,
    rsq          = rsq_val,
    rmse         = rmse_val,
    rrmse        = rrmse_val,
    rpd          = rpd_val,
    saved_path   = saved_path,
    error_message = NA_character_,
    status       = "success"
  )

  if (verbose) {
    cli::cli_alert_success(
      "Evaluation complete: RRMSE = {round(rrmse_val, 2)}%, R² = {round(rsq_val, 3)}, Time = {round(total_time, 1)}s"
    )
  }

  ## ---------------------------------------------------------------------------
  ## Step 9: Return Results
  ## ---------------------------------------------------------------------------

  return(list(
    status_summary = status_summary,
    saved_path     = saved_path,
    timing_info    = timing_info,
    memory_info    = memory_info
  ))
}