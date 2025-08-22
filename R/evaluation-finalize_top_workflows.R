#' Finalize Top Workflows from Parallel Evaluation Results
#'
#' @description
#' Takes the top-performing models from parallel evaluation results and refits them
#' with expanded hyperparameter tuning for ensemble stacking. Uses best parameters
#' from parallel results as warm starts for Bayesian optimization, then saves
#' workflows in the format expected by `build_ensemble_stack()`.
#'
#' @param parallel_results A tibble returned by `evaluate_models_parallel()` containing
#'   model performance metrics and best parameters
#' @param input_data A tibble containing spectral features and response variable
#' @param covariate_data Optional tibble containing external covariates
#' @param variable Character. Name of the response variable to model
#' @param n_best Integer. Number of top models to finalize (default: 10)
#' @param output_dir Character. Directory to save finalized workflows (default: tempdir())
#' @param grid_size_final Integer. Grid search size for refitting (default: 20)
#' @param bayesian_iter_final Integer. Bayesian optimization iterations (default: 25)
#' @param cv_folds_final Integer. Cross-validation folds for final tuning (default: 10)
#' @param verbose Logical. Print progress information (default: TRUE)
#' @param parallel Logical. Enable parallel processing for model tuning. Defaults to `FALSE` (safe for nested contexts).
#' @param n_workers Integer. Number of parallel workers for tuning. If `NULL`, uses `min(10, detectCores()-1)` for safety.
#' @param allow_nested Logical. Allow parallel processing even when already in parallel context. Defaults to `FALSE` (recommended).
#'
#' @return A tibble containing finalized model information with columns:
#' \itemize{
#'   \item Configuration columns from input
#'   \item \strong{saved_path}: Path to saved .qs file containing workflow
#'   \item \strong{final_metrics}: Performance metrics from expanded tuning
#'   \item \strong{status}: "success" or "failed"
#' }
#'
#' @details
#' This function bridges the gap between memory-efficient parallel evaluation and
#' full workflow saving for ensemble building. It:
#' - Selects top N models by RRMSE from parallel results
#' - Creates initial grids around the best parameters from parallel runs
#' - Performs expanded Bayesian tuning starting from these good parameter regions
#' - Saves complete workflows in .qs format for `build_ensemble_stack()`
#'
#' @export

finalize_top_workflows <- function(parallel_results,
                                   input_data,
                                   covariate_data = NULL,
                                   variable,
                                   n_best = 10,
                                   output_dir = NULL,
                                   grid_size_final = 20,
                                   bayesian_iter_final = 25,
                                   cv_folds_final = 10,
                                   seed = 123,
                                   verbose = TRUE,
                                   parallel = FALSE,
                                   n_workers = NULL,
                                   allow_nested = FALSE) {

  start_time <- Sys.time()
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Validation and Setup
  ## ---------------------------------------------------------------------------
  
  # Basic validation
  if (!is.data.frame(parallel_results) || nrow(parallel_results) == 0) {
    cli::cli_abort("parallel_results must be a non-empty data frame")
  }
  
  if (!variable %in% colnames(input_data)) {
    cli::cli_abort("Variable '{variable}' not found in input_data")
  }
  
  # Setup output directory
  if (is.null(output_dir)) {
    output_dir <- file.path(tempdir(), "finalized_workflows")
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  if (verbose) {
    cli::cli_h1("Finalizing Top Workflows for Ensemble Stacking")
    cli::cli_alert_info("Input models: {nrow(parallel_results)}")
    cli::cli_alert_info("Target models: {n_best}")
    cli::cli_alert_info("Variable: {variable}")
    cli::cli_alert_info("Output directory: {output_dir}")
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Select Top N Models by RRMSE
  ## ---------------------------------------------------------------------------
  
  top_models <- parallel_results %>%
    dplyr::filter(status == "success", !is.na(rrmse)) %>%
    dplyr::arrange(rrmse) %>%
    dplyr::slice_head(n = n_best)
  
  if (nrow(top_models) == 0) {
    cli::cli_abort("No successful models found in parallel_results")
  }
  
  if (nrow(top_models) < n_best) {
    cli::cli_alert_warning("Only {nrow(top_models)} successful models available (requested {n_best})")
  }
  
  if (verbose) {
    cli::cli_h2("Selected top models")
    cli::cli_alert_success("Top {nrow(top_models)} models selected by RRMSE")
    cli::cli_alert_info("Best RRMSE: {round(min(top_models$rrmse), 3)}")
    cli::cli_alert_info("Worst selected RRMSE: {round(max(top_models$rrmse), 3)}")
    
    # Show model distribution
    model_dist <- top_models %>%
      dplyr::count(model, name = "n_selected") %>%
      dplyr::arrange(dplyr::desc(n_selected))
    
    cli::cli_alert_info("Model distribution:")
    for (i in 1:nrow(model_dist)) {
      cli::cli_alert_info("  • {model_dist$model[i]}: {model_dist$n_selected[i]} models")
    }
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Setup Parallel Processing with Safety Controls
  ## ---------------------------------------------------------------------------

  # Determine safe worker count
  if (is.null(n_workers)) {
    max_cores <- parallel::detectCores(logical = TRUE)
    n_workers <- pmax(1, pmin(max_cores - 1, 10))  # Cap at 10 for safety
  }

  # Check for nested parallelization
  current_plan_class <- class(future::plan())[1]
  if (!allow_nested && !identical(current_plan_class, "sequential")) {
    if(verbose) cli::cli_alert_warning("Nested parallelization detected. Setting parallel=FALSE for safety")
    parallel <- FALSE
  }

  # Determine if we can use parallel for tuning
  use_parallel_tuning <- parallel && n_workers > 1
  
  if (verbose && use_parallel_tuning) {
    cli::cli_alert_info("Parallel tuning enabled with {n_workers} workers")
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Data Preparation
  ## ---------------------------------------------------------------------------
  
  if (verbose) {
    cli::cli_h2("Preparing data for final tuning")
  }
  
  # Rename response variable for consistency with horizons patterns
  input_data_clean <- input_data %>%
    dplyr::rename(Response = !!rlang::sym(variable))
  
  # Create train/test split with consistent seed for reproducibility
  set.seed(seed)
  split <- rsample::initial_split(input_data_clean, prop = 0.8, strata = Response)
  train_data <- rsample::training(split)
  test_data <- rsample::testing(split)
  
  # Create CV folds for final tuning with consistent seed
  set.seed(seed + 1)  # Different seed for CV folds
  cv_folds <- rsample::vfold_cv(train_data, v = cv_folds_final, strata = Response)
  
  if (verbose) {
    cli::cli_alert_success("Data split: {nrow(train_data)} training, {nrow(test_data)} testing")
    cli::cli_alert_success("Created {cv_folds_final}-fold cross-validation resamples")
  }
  
  # Memory cleanup
  rm(input_data_clean)
  gc(verbose = FALSE, full = TRUE)
  
  ## Memory management helper function ----------------------------------------
  
  defragment_memory <- function() {
    for(i in 1:3) {
      gc(verbose = FALSE, full = TRUE)
      Sys.sleep(0.1)
    }
    gc(verbose = FALSE, full = TRUE)
  }
  
  ## Workflow validation helper -----------------------------------------------
  
  validate_workflow_for_stacking <- function(workflow, tuning_results, wflow_id) {
    
    # Check workflow has required components
    if (!inherits(workflow, "workflow")) {
      return(list(valid = FALSE, reason = "Not a valid workflow object"))
    }
    
    # Check has recipe and model
    if (is.null(workflows::extract_recipe(workflow, estimated = FALSE))) {
      return(list(valid = FALSE, reason = "Workflow missing recipe"))
    }
    
    if (is.null(workflows::extract_spec_parsnip(workflow))) {
      return(list(valid = FALSE, reason = "Workflow missing model specification"))
    }
    
    # Check tuning results exist and have required structure
    if (is.null(tuning_results)) {
      return(list(valid = FALSE, reason = "Missing tuning results"))
    }
    
    if (!inherits(tuning_results, "tune_results")) {
      return(list(valid = FALSE, reason = "Invalid tuning results object"))
    }
    
    # Check has performance metrics
    metrics <- tune::collect_metrics(tuning_results)
    if (nrow(metrics) == 0) {
      return(list(valid = FALSE, reason = "No metrics in tuning results"))
    }
    
    return(list(valid = TRUE, reason = "Workflow validation passed"))
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 4: Sequential Workflow Rebuilding and Refitting
  ## ---------------------------------------------------------------------------
  
  if (verbose) {
    cli::cli_h2("Rebuilding and refitting workflows sequentially")
    cli::cli_alert_info("Processing {nrow(top_models)} models with expanded tuning")
  }
  
  # Initialize storage for results
  finalized_results <- vector("list", nrow(top_models))
  
  # Sequential loop through each top model
  for (i in seq_len(nrow(top_models))) {
    
    model_start_time <- Sys.time()
    current_model <- top_models[i, ]
    
    if (verbose) {
      cli::cli_h3("Model {i}/{nrow(top_models)}: {current_model$model} - {current_model$preprocessing}")
      cli::cli_alert_info("Original RRMSE: {round(current_model$rrmse, 3)}")
    }
    
    # Create workflow ID for consistency
    wflow_id <- clean_workflow_id(
      model             = current_model$model,
      transformation    = current_model$transformation,
      preprocessing     = current_model$preprocessing,
      feature_selection = current_model$feature_selection,
      covariates        = current_model$covariates[[1]]
    )
    
    ## Step 4.1: Rebuild Recipe -----------------------------------------------
    
    recipe_result <- safely_execute(
      expr = {
        build_recipe(
          input_data               = train_data,
          response_transformation  = current_model$transformation,
          spectral_transformation  = current_model$preprocessing,
          feature_selection_method = current_model$feature_selection,
          covariate_selection      = current_model$covariates[[1]],
          covariate_data           = covariate_data
        )
      },
      default_value = NULL,
      error_message = glue::glue("Recipe building failed for {wflow_id}")
    )
    
    if (is.null(recipe_result$result)) {
      finalized_results[[i]] <- list(
        wflow_id = wflow_id,
        status = "failed",
        error_stage = "recipe_building",
        error_message = conditionMessage(recipe_result$error)
      )
      next
    }
    
    recipe <- recipe_result$result
    
    ## Step 4.2: Create Model Specification -----------------------------------
    
    model_spec_result <- safely_execute(
      expr = {
        define_model_specifications(model_type = current_model$model)
      },
      default_value = NULL,
      error_message = glue::glue("Model specification failed for {wflow_id}")
    )
    
    if (is.null(model_spec_result$result)) {
      finalized_results[[i]] <- list(
        wflow_id = wflow_id,
        status = "failed", 
        error_stage = "model_specification",
        error_message = conditionMessage(model_spec_result$error)
      )
      next
    }
    
    model_spec <- model_spec_result$result
    
    ## Step 4.3: Create Workflow ----------------------------------------------
    
    workflow_result <- safely_execute(
      expr = {
        workflows::workflow() %>%
          workflows::add_recipe(recipe) %>%
          workflows::add_model(model_spec)
      },
      default_value = NULL,
      error_message = glue::glue("Workflow creation failed for {wflow_id}")
    )
    
    if (is.null(workflow_result$result)) {
      finalized_results[[i]] <- list(
        wflow_id = wflow_id,
        status = "failed",
        error_stage = "workflow_creation", 
        error_message = conditionMessage(workflow_result$error)
      )
      next
    }
    
    workflow <- workflow_result$result
    
    ## Handle mtry finalization if needed (random forest) --------------------
    
    param_set <- hardhat::extract_parameter_set_dials(workflow)
    
    if ("mtry" %in% param_set$name) {
      eval_data <- recipe %>%
        recipes::prep() %>%
        recipes::bake(new_data = NULL) %>%
        dplyr::select(-Project, -Sample_ID, -Response)
      
      param_set <- hardhat::extract_parameter_set_dials(workflow) %>%
        dials::finalize(eval_data)
      
      rm(eval_data)
    }
    
    if (verbose) {
      cli::cli_alert_success("Workflow rebuilt successfully")
    }
    
    ## Step 4.4: Create Expanded Grid Around Best Parameters ------------------
    
    if (verbose) {
      cli::cli_alert_info("Creating expanded grid around best parameters")
    }
    
    # Extract best parameters as a tibble
    best_params_row <- current_model$best_params[[1]]
    
    # Create expanded grid around best parameters
    expanded_grid_result <- safely_execute(
      expr = {
        create_expanded_parameter_grid(
          best_params = best_params_row,
          param_set = param_set,
          grid_size = grid_size_final
        )
      },
      default_value = NULL,
      error_message = glue::glue("Parameter grid expansion failed for {wflow_id}")
    )
    
    if (is.null(expanded_grid_result$result)) {
      finalized_results[[i]] <- list(
        wflow_id = wflow_id,
        status = "failed",
        error_stage = "parameter_expansion",
        error_message = conditionMessage(expanded_grid_result$error)
      )
      next
    }
    
    expanded_grid <- expanded_grid_result$result
    
    if (verbose) {
      cli::cli_alert_success("Created expanded grid with {nrow(expanded_grid)} parameter combinations")
    }
    
    ## Step 4.5: Initial Grid Search on Expanded Grid ------------------------
    
    if (verbose) {
      cli::cli_alert_info("Running grid search on expanded parameters")
    }
    
    grid_result <- safely_execute(
      expr = {
        suppressWarnings({
          suppressMessages({
            tune::tune_grid(
              object = workflow,
              resamples = cv_folds,
              grid = expanded_grid,
              metrics = yardstick::metric_set(rrmse, rsq),
              control = tune::control_grid(
                save_pred = FALSE,
                save_workflow = FALSE,
                verbose = FALSE,
                allow_par = use_parallel_tuning
              )
            )
          })
        })
      },
      default_value = NULL,
      error_message = glue::glue("Expanded grid search failed for {wflow_id}")
    )
    
    if (is.null(grid_result$result)) {
      finalized_results[[i]] <- list(
        wflow_id = wflow_id,
        status = "failed",
        error_stage = "expanded_grid_search",
        error_message = conditionMessage(grid_result$error)
      )
      next
    }
    
    grid_res <- grid_result$result
    
    ## Step 4.6: Bayesian Optimization Starting from Grid Results ------------
    
    if (verbose) {
      cli::cli_alert_info("Running Bayesian optimization ({bayesian_iter_final} iterations)")
    }
    
    bayes_result <- safely_execute(
      expr = {
        suppressWarnings({
          suppressMessages({
            tune::tune_bayes(
              object = workflow,
              initial = grid_res,  # Use grid results as starting point
              resamples = cv_folds,
              param_info = param_set,
              iter = bayesian_iter_final,
              metrics = yardstick::metric_set(rrmse, rsq),
              control = tune::control_bayes(
                save_pred = FALSE,
                save_workflow = FALSE,
                verbose = FALSE,
                seed = 307,
                no_improve = 10L,
                allow_par = use_parallel_tuning
              )
            )
          })
        })
      },
      default_value = NULL,
      error_message = glue::glue("Bayesian optimization failed for {wflow_id}")
    )
    
    if (is.null(bayes_result$result)) {
      finalized_results[[i]] <- list(
        wflow_id = wflow_id,
        status = "failed",
        error_stage = "bayesian_optimization",
        error_message = conditionMessage(bayes_result$error)
      )
      next
    }
    
    bayes_res <- bayes_result$result
    
    # Get final best parameters and metrics
    final_best_params <- tune::select_best(bayes_res, metric = "rrmse")
    final_metrics <- tune::show_best(bayes_res, metric = "rrmse", n = 1)
    
    model_duration <- difftime(Sys.time(), model_start_time, units = "mins")
    
    if (verbose) {
      cli::cli_alert_success("Model {i} completed in {round(model_duration, 1)} minutes")
      cli::cli_alert_success("Final RRMSE: {round(final_metrics$mean[1], 3)} (was {round(current_model$rrmse, 3)})")
    }
    
    # Store successful result
    finalized_results[[i]] <- list(
      wflow_id = wflow_id,
      workflow = workflow,
      tuning_results = bayes_res,  # Full tuning results for stacking
      best_params_final = final_best_params,
      final_metrics = final_metrics,
      original_rrmse = current_model$rrmse,
      improvement = current_model$rrmse - final_metrics$mean[1],
      duration_mins = as.numeric(model_duration),
      status = "success"
    )
    
    # Memory cleanup between models
    defragment_memory()
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 5: Save Results in .qs Format for Ensemble Stacking
  ## ---------------------------------------------------------------------------
  
  if (verbose) {
    cli::cli_h2("Saving finalized workflows for ensemble stacking")
  }
  
  # Filter successful results
  successful_results <- finalized_results[sapply(finalized_results, function(x) x$status == "success")]
  
  if (length(successful_results) == 0) {
    cli::cli_abort("No models were successfully finalized")
  }
  
  if (verbose) {
    cli::cli_alert_success("Successfully finalized {length(successful_results)}/{nrow(top_models)} models")
  }
  
  # Initialize summary tibble for return
  summary_results <- vector("list", length(successful_results))
  
  # Save each successful model and create summary
  for (i in seq_along(successful_results)) {
    
    result <- successful_results[[i]]
    
    # Validate workflow before saving
    validation <- validate_workflow_for_stacking(result$workflow, result$tuning_results, result$wflow_id)
    
    if (!validation$valid) {
      if (verbose) {
        cli::cli_alert_warning("Workflow validation failed for {result$wflow_id}: {validation$reason}")
      }
      next  # Skip this workflow
    }
    
    # Create the tuned_models tibble in the format expected by build_ensemble_stack()
    tuned_models_tibble <- tibble::tibble(
      wflow_id = result$wflow_id,
      workflow = list(result$workflow),
      result = list(result$tuning_results)  # Bayesian tuning results for stacking
    )
    
    # Create the full model result object in the same format as current system
    model_result <- list(
      tuned_models = tuned_models_tibble,
      status = "success",
      wflow_id = result$wflow_id,
      final_metrics = result$final_metrics,
      best_params = result$best_params_final,
      improvement = result$improvement,
      duration_mins = result$duration_mins
    )
    
    # Generate filename following horizons convention
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- glue::glue("model_output_finalized_{variable}_{i}_{timestamp}.qs")
    filepath <- file.path(output_dir, filename)
    
    # Save the .qs file
    qs::qsave(model_result, filepath)
    
    if (verbose) {
      cli::cli_alert_success("Saved: {result$wflow_id}")
      cli::cli_alert_info("  • File: {basename(filepath)}")
      cli::cli_alert_info("  • Final RRMSE: {round(result$final_metrics$mean[1], 3)}")
      cli::cli_alert_info("  • Improvement: {round(result$improvement, 3)}")
    }
    
    # Add to summary results
    summary_results[[i]] <- tibble::tibble(
      row = i,
      wflow_id = result$wflow_id,
      model = top_models$model[i],
      transformation = top_models$transformation[i], 
      preprocessing = top_models$preprocessing[i],
      feature_selection = top_models$feature_selection[i],
      covariates = top_models$covariates[i],
      original_rrmse = result$original_rrmse,
      final_rrmse = result$final_metrics$mean[1],
      improvement = result$improvement,
      final_rsq = result$final_metrics[result$final_metrics$.metric == "rsq", ".estimate"][[1]],
      duration_mins = result$duration_mins,
      saved_path = filepath,
      status = "success"
    )
  }
  
  # Combine summary results
  final_summary <- dplyr::bind_rows(summary_results)
  
  # Save summary table
  summary_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S") 
  summary_filename <- glue::glue("finalized_summary_{variable}_{summary_timestamp}.qs")
  summary_filepath <- file.path(output_dir, summary_filename)
  
  qs::qsave(final_summary, summary_filepath)
  
  ## ---------------------------------------------------------------------------
  ## Step 6: Final Summary and Return
  ## ---------------------------------------------------------------------------
  
  total_duration <- difftime(Sys.time(), start_time, units = "mins")
  
  if (verbose) {
    cli::cli_h1("Workflow Finalization Complete")
    cli::cli_alert_success("Successfully finalized {nrow(final_summary)} models")
    cli::cli_alert_success("Total runtime: {round(total_duration, 1)} minutes")
    cli::cli_alert_success("Average per model: {round(total_duration / nrow(final_summary), 1)} minutes")
    
    # Show improvement summary
    if (nrow(final_summary) > 0) {
      avg_improvement <- mean(final_summary$improvement, na.rm = TRUE)
      best_improvement <- max(final_summary$improvement, na.rm = TRUE)
      cli::cli_alert_info("RRMSE improvements:")
      cli::cli_alert_info("  • Average: {round(avg_improvement, 3)}")
      cli::cli_alert_info("  • Best: {round(best_improvement, 3)}")
    }
    
    cli::cli_alert_info("Output directory: {output_dir}")
    cli::cli_alert_info("Summary file: {basename(summary_filepath)}")
    cli::cli_alert_success("Ready for ensemble stacking with build_ensemble_stack()")
  }
  
  return(final_summary)
}