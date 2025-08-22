#' Evaluate a Single Model Configuration for Parallel Processing
#'
#' @description
#' Evaluates a single spectral model configuration with full tuning (grid search + Bayesian optimization)
#' but returns minimal information for memory efficiency in parallel processing. Designed for use
#' within parallel evaluation pipelines where many models run simultaneously.
#'
#' @param input_data A tibble containing spectral features with columns: Sample_ID, Wavenumber,
#'   Absorbance, and the target response variable
#' @param covariate_data Optional tibble containing external covariates, matched by Sample_ID
#' @param variable Character. Name of the response variable to model
#' @param model Character. Model type (e.g., "random_forest", "cubist", "plsr")
#' @param transformation Character. Outcome transformation (e.g., "No Transformation", "Square Root Transformation")
#' @param preprocessing Character. Spectral preprocessing method (e.g., "raw", "snv", "deriv1")
#' @param feature_selection Character. Feature selection method (e.g., "none", "pca", "correlation")
#' @param covariates Character vector of covariate names to include
#' @param include_covariates Logical. Whether to include covariates in the model
#' @param pruning Logical. Whether to skip poor-performing models after grid tuning (default = TRUE)
#' @param grid_size Integer. Number of initial grid search candidates (default = 10)
#' @param bayesian_iter Integer. Number of Bayesian tuning iterations (default = 15)
#' @param cv_folds Integer. Number of cross-validation folds (default = 5)
#' @param seed Integer. Random seed for reproducible train/test splits and CV folds (default = NULL)
#' @param parallel Logical. Enable parallel processing for model tuning. Defaults to `FALSE` (safe for nested contexts).
#' @param n_workers Integer. Number of parallel workers for tuning. If `NULL`, uses `min(10, detectCores()-1)` for safety.
#' @param allow_nested Logical. Allow parallel processing even when already in parallel context. Defaults to `FALSE` (recommended).
#'
#' @return A list containing minimal information for memory efficiency:
#' \itemize{
#'   \item \strong{model_id}: Unique identifier for this configuration
#'   \item \strong{metrics}: Tibble with rmse, rrmse, rsq, rpd
#'   \item \strong{best_params}: Best hyperparameters from tuning
#'   \item \strong{runtime_seconds}: Total execution time
#'   \item \strong{status}: "success", "failed", or "pruned"
#'   \item \strong{error_info}: Detailed error information if failed
#'   \item \strong{tuning_info}: Convergence and iteration details
#' }
#'
#' @export

evaluate_single_model_parallel <- function(input_data,
                                          covariate_data     = NULL,
                                          variable,
                                          model,
                                          transformation,
                                          preprocessing,
                                          feature_selection,
                                          covariates         = NULL,
                                          include_covariates = FALSE,
                                          grid_size          = 10,
                                          bayesian_iter      = 15,
                                          cv_folds           = 5,
                                          seed               = NULL,
                                          parallel           = FALSE,
                                          n_workers          = NULL,
                                          allow_nested       = FALSE) {


  start_time   <- Sys.time()
  start_memory <- as.numeric(pryr::mem_used()) / 1048576  # Convert to MB

  ## ---------------------------------------------------------------------------
  ## Step 0: Memory management function and error classification
  ## ---------------------------------------------------------------------------

  # OPTIMIZED: Minimal memory management - avoid excessive garbage collection
  defragment_memory <- function(aggressive = FALSE) {
    # Single gc() call only when needed - R's automatic GC is usually sufficient
    gc(verbose = FALSE)
  }
  
  ## Simple error classification based on stage -------------------------------
  
  classify_error_type <- function(stage) {
    switch(stage,
      "validation"          = "data",
      "recipe_building"     = "preprocessing", 
      "model_specification" = "model_setup",
      "workflow_creation"   = "model_setup",
      "grid_search"         = "tuning",
      "bayesian_tuning"     = "tuning", 
      "final_evaluation"    = "evaluation",
      "parallel_wrapper"    = "system",
      "unknown"
    )
  }

  ## ---------------------------------------------------------------------------
  ## Step 1: Setup and data splitting
  ## ---------------------------------------------------------------------------

  ## Light defensive check -----------------------------------------------------

  if(!variable %in% colnames(input_data)) {

    return(list(model_id        = paste(model, transformation, preprocessing, feature_selection, sep = "_"),
                metrics         = NULL,
                best_params     = NULL,
                runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
                status          = "failed",
                error_info      = list(stage      = "validation",
                                       message    = paste("Variable", variable, "not found in input_data"),
                                       error_type = classify_error_type("validation")),
                tuning_info     = NULL))
    }

  ## Create nice workflow ids --------------------------------------------------

  clean_workflow_id(model             = model,
                    transformation    = transformation,
                    preprocessing     = preprocessing,
                    feature_selection = feature_selection,
                    covariates        = covariates) -> wflow_id

  ## Rename response variable for consistency ----------------------------------

  input_data <- dplyr::rename(input_data, Response = !!rlang::sym(variable))

  ## Create train/test split ---------------------------------------------------
  
  # Set seed for reproducible train/test split
  if (!is.null(seed)) {
    set.seed(seed)
  }
  split <- rsample::initial_split(input_data, prop = 0.8, strata = Response)
  train <- rsample::training(split)
  test  <- rsample::testing(split)
  
  # Set different seed for CV folds to ensure variation
  if (!is.null(seed)) {
    set.seed(seed + 1)
  }
  folds <- rsample::vfold_cv(train, v = cv_folds)

  ## Clean up input data from memory -------------------------------------------

  rm(input_data)
  defragment_memory()

  ## ---------------------------------------------------------------------------
  ## Step 2: Build Recipe
  ## ---------------------------------------------------------------------------

  recipe_start <- Sys.time()

  ## Build the recipe for this configuration -----------------------------------

  safely_execute(
    expr = {build_recipe(input_data               = train,
                         response_transformation  = transformation,
                         spectral_transformation  = preprocessing,
                         feature_selection_method = feature_selection,
                         covariate_selection      = covariates,
                         covariate_data           = covariate_data)},
    default_value = NULL,
    error_message = glue::glue("Recipe building failed for {wflow_id}")
    ) -> recipe_result

  if (is.null(recipe_result$result)) {

    return(list(model_id        = wflow_id,
                metrics         = NULL,
                best_params     = NULL,
                runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
                status          = "failed",
                error_info      = list(stage        = "recipe_building",
                                       message      = conditionMessage(recipe_result$error),
                                       error_object = recipe_result$error,
                                       error_type   = classify_error_type("recipe_building")),
                tuning_info     = NULL))
    }

  recipe      <- recipe_result$result
  recipe_time <- as.numeric(difftime(Sys.time(), recipe_start, units = "secs"))

  ## ---------------------------------------------------------------------------
  ## Step 3: Create Model Specification and Workflow
  ## ---------------------------------------------------------------------------

  ## Create model specs --------------------------------------------------------

  safely_execute(expr          = {define_model_specifications(model_type = model)},
                 default_value = NULL,
                 error_message = glue::glue("Model specification failed for {wflow_id}")) -> model_spec_result

  if (is.null(model_spec_result$result)) {

    return(list(model_id        = wflow_id,
                metrics         = NULL,
                best_params     = NULL,
                runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
                status          = "failed",
                error_info      = list(stage        = "model_specification",
                                       message      = conditionMessage(model_spec_result$error),
                                       error_object = model_spec_result$error,
                                       error_type   = classify_error_type("model_specification")),
                tuning_info     = list(recipe_time = recipe_time)))
  }

  model_spec <- model_spec_result$result

  ## Create workflow -----------------------------------------------------------

  safely_execute(expr          = {workflows::workflow() %>%
                                    workflows::add_recipe(recipe) %>%
                                    workflows::add_model(model_spec)},
                 default_value = NULL,
                 error_message = glue::glue("Workflow creation failed for {wflow_id}")) -> workflow_result

  if (is.null(workflow_result$result)) {

    return(list(model_id        = wflow_id,
                metrics         = NULL,
                best_params     = NULL,
                runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
                status          = "failed",
                error_info      = list(stage        = "workflow_creation",
                                       message      = conditionMessage(workflow_result$error),
                                       error_object = workflow_result$error,
                                       error_type   = classify_error_type("workflow_creation")),
                tuning_info     = list(recipe_time = recipe_time)))
    }

  workflow <- workflow_result$result

  ## Handle mtry finalization if needed (random forest) -----------------------

  param_set <- hardhat::extract_parameter_set_dials(workflow)

  if("mtry" %in% param_set$name) {

    recipe %>%
      recipes::prep() %>%
      recipes::bake(new_data = NULL) %>%
      dplyr::select(-Project, -Sample_ID, -Response) -> eval_data

    hardhat::extract_parameter_set_dials(workflow) %>%
      dials::finalize(eval_data) -> param_set

    rm(eval_data)
    defragment_memory()
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Initial Grid Search
  ## ---------------------------------------------------------------------------

  grid_start <- Sys.time()

  ## CRITICAL: allow_par must be FALSE since we're already in parallel worker --

  safely_execute(
    expr = {
      suppressWarnings({
        suppressMessages({
          tune::tune_grid(
            object     = workflow,
            resamples  = folds,
            param_info = param_set,
            grid       = grid_size,
            metrics    = yardstick::metric_set(rrmse, rsq),
            control    = tune::control_grid(
              save_pred     = FALSE,      # Don't save predictions (memory)
              save_workflow = FALSE,      # Don't save workflow (memory)
              verbose       = FALSE,
              allow_par     = FALSE,      # CRITICAL: No nested parallelization
              parallel_over = NULL        # CRITICAL: No parallel here
            )
          )
        })
      })
    },
    default_value = NULL,
    error_message = glue::glue("Grid tuning failed for {wflow_id}")
  ) -> grid_result

  grid_time <- as.numeric(difftime(Sys.time(), grid_start, units = "secs"))

  if (is.null(grid_result$result)) {

    ## Try to capture more error information ------------------------------------

    error_details <- list(
      stage      = "grid_search",
      message    = conditionMessage(grid_result$error),
      error_class = class(grid_result$error),
      error_type = classify_error_type("grid_search"),
      grid_size  = grid_size,
      param_info = names(param_set)
    )

    ## Try to get tune notes if available ---------------------------------------

    if (exists(".Last.tune.result")) {
      tryCatch({
        error_details$tune_notes <- capture.output(tune::show_notes(.Last.tune.result))
      }, error = function(e) NULL)
    }

    return(list(model_id        = wflow_id,
                metrics         = NULL,
                best_params     = NULL,
                runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
                status          = "failed",
                error_info      = error_details,
                tuning_info     = list(recipe_time = recipe_time,
                                       grid_time   = grid_time)))
  }

  grid_res <- grid_result$result

  ## Clean up memory - OPTIMIZED: Removed unnecessary intermediate GC call

  ## ---------------------------------------------------------------------------
  ## Step 5: Bayesian Optimization
  ## ---------------------------------------------------------------------------

  bayes_start <- Sys.time()

  safely_execute(
    expr = {
      suppressWarnings({
        suppressMessages({
          tune::tune_bayes(
            object     = workflow,
            initial    = grid_res,  # Use grid search results as starting point
            resamples  = folds,
            param_info = param_set,
            iter       = bayesian_iter,
            metrics    = yardstick::metric_set(rrmse, rsq),
            control    = tune::control_bayes(
              save_pred     = FALSE,      # Don't save predictions (memory)
              save_workflow = FALSE,      # Don't save workflow (memory)
              verbose       = FALSE,
              seed          = 307,
              no_improve    = 10L,        # Stop after 10 iterations with no improvement
              allow_par     = FALSE,      # CRITICAL: No nested parallelization
              parallel_over = NULL        # CRITICAL: No parallel here
            )
          )
        })
      })
    },
    default_value = NULL,
    error_message = glue::glue("Bayesian tuning failed for {wflow_id}")
  ) -> bayes_result

  bayes_time <- as.numeric(difftime(Sys.time(), bayes_start, units = "secs"))

  if (is.null(bayes_result$result)) {

    ## Capture detailed error information ---------------------------------------

    error_details <- list(
      stage         = "bayesian_tuning",
      message       = conditionMessage(bayes_result$error),
      error_class   = class(bayes_result$error),
      error_type    = classify_error_type("bayesian_tuning"),
      bayesian_iter = bayesian_iter,
      param_info    = names(param_set)
    )

    ## Try to get tune notes ----------------------------------------------------

    if (exists(".Last.tune.result")) {
      tryCatch({
        error_details$tune_notes <- capture.output(tune::show_notes(.Last.tune.result))
      }, error = function(e) NULL)
    }

    return(list(model_id        = wflow_id,
                metrics         = NULL,
                best_params     = NULL,
                runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
                status          = "failed",
                error_info      = error_details,
                tuning_info     = list(recipe_time  = recipe_time,
                                       grid_time    = grid_time,
                                       bayes_time   = bayes_time,
                                       grid_success = TRUE)))  # Grid worked, Bayes failed
  }

  bayes_res <- bayes_result$result

  ## Extract convergence information -------------------------------------------

  n_iterations_used <- nrow(tune::collect_metrics(bayes_res))

  ## Clean up memory - OPTIMIZED: Removed unnecessary intermediate GC call

  ## ---------------------------------------------------------------------------
  ## Step 6: Select Best Model and Evaluate on Test Set
  ## ---------------------------------------------------------------------------

  eval_start <- Sys.time()

  ## Select best model based on RRMSE -----------------------------------------

  best_params <- tune::select_best(bayes_res, metric = "rrmse")

  ## Finalize workflow with best parameters -----------------------------------

  final_wf <- tune::finalize_workflow(workflow, best_params)

  ## Fit on full training set and evaluate on test set ------------------------

  safely_execute(
    expr = {
      ## Fit the finalized workflow on training data -------------------------

      fitted_wf <- parsnip::fit(final_wf, data = train)

      ## Predict on test set --------------------------------------------------

      test_pred <- predict(fitted_wf, new_data = test) %>%
        dplyr::bind_cols(test %>% dplyr::select(Response))

      ## Calculate metrics ----------------------------------------------------

      metrics <- yardstick::metrics(test_pred, truth = Response, estimate = .pred)

      ## Add RRMSE and RPD ----------------------------------------------------

      rmse_val <- metrics$`.estimate`[metrics$`.metric` == "rmse"]
      rsq_val  <- metrics$`.estimate`[metrics$`.metric` == "rsq"]
      mae_val  <- metrics$`.estimate`[metrics$`.metric` == "mae"]

      ## Calculate RRMSE ------------------------------------------------------

      mean_val  <- mean(test$Response, na.rm = TRUE)
      rrmse_val <- (rmse_val / mean_val) * 100

      ## Calculate RPD (Ratio of Performance to Deviation) -------------------

      sd_val  <- sd(test$Response, na.rm = TRUE)
      rpd_val <- sd_val / rmse_val

      ## Return metrics tibble ------------------------------------------------

      tibble::tibble(rmse  = rmse_val,
                     rrmse = rrmse_val,
                     rsq   = rsq_val,
                     mae   = mae_val,
                     rpd   = rpd_val,
                     n_test = nrow(test))
    },
    default_value = NULL,
    error_message = glue::glue("Final evaluation failed for {wflow_id}")
  ) -> final_fit_result

  eval_time <- as.numeric(difftime(Sys.time(), eval_start, units = "secs"))

  if (is.null(final_fit_result$result)) {
    
    # Log the specific error that caused final evaluation to fail
    error_msg <- if(!is.null(final_fit_result$error)) {
      conditionMessage(final_fit_result$error)
    } else {
      "Unknown error in final evaluation"
    }
    
    # Enhanced error reporting for debugging
    cat(sprintf("[DEBUG] Final evaluation failed for %s: %s\n", wflow_id, error_msg))

    return(list(model_id        = wflow_id,
                metrics         = NULL,
                best_params     = best_params,  # We at least have the best params
                runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
                status          = "failed",
                error_info      = list(stage       = "final_evaluation",
                                       message     = error_msg,
                                       error_class = if(!is.null(final_fit_result$error)) class(final_fit_result$error) else "unknown",
                                       error_type  = classify_error_type("final_evaluation")),
                tuning_info     = list(recipe_time    = recipe_time,
                                       grid_time      = grid_time,
                                       bayes_time     = bayes_time,
                                       eval_time      = eval_time,
                                       n_iterations   = n_iterations_used,
                                       grid_success   = TRUE,
                                       bayes_success  = TRUE)))
  }

  metrics <- final_fit_result$result

  ## Final memory cleanup ------------------------------------------------------

  rm(train, test, folds, recipe, model_spec, workflow, grid_res, bayes_res, final_wf)
  defragment_memory()

  ## ---------------------------------------------------------------------------
  ## Step 7: Return Minimal Results
  ## ---------------------------------------------------------------------------

  total_time  <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  end_memory  <- as.numeric(pryr::mem_used()) / 1048576  # Convert to MB
  memory_used <- end_memory - start_memory

  return(list(model_id        = wflow_id,
              metrics         = metrics,
              best_params     = best_params,
              runtime_seconds = total_time,
              status          = "success",
              error_info      = NULL,
              tuning_info     = list(recipe_time              = recipe_time,
                                     grid_time                = grid_time,
                                     bayes_time               = bayes_time,
                                     eval_time                = eval_time,
                                     n_iterations             = n_iterations_used,
                                     grid_size                = grid_size,
                                     bayesian_iter_requested  = bayesian_iter,
                                     cv_folds                 = cv_folds,
                                     memory_used_mb           = memory_used,
                                     start_memory_mb          = start_memory,
                                     end_memory_mb            = end_memory)))
}
