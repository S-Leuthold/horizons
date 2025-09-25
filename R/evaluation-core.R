#' Evaluate a Single Model Configuration
#'
#' @description
#' Core function for evaluating a single model configuration.
#' Implements a complete modeling pipeline: recipe building → model specification →
#' hyperparameter tuning (grid + Bayesian) → final evaluation on test set.
#' Designed for sequential model evaluation with optional parallel cross-validation.
#'
#' @param config_row Single-row data frame or tibble containing model configuration with columns:
#'   \itemize{
#'     \item \code{model}: Character. Model type (e.g., "random_forest", "cubist", "xgboost")
#'     \item \code{transformation}: Character. Response transformation (e.g., "Log Transformation")
#'     \item \code{preprocessing}: Character. Spectral preprocessing (e.g., "snv", "raw")
#'     \item \code{feature_selection}: Character. Feature selection method (e.g., "pca", "none")
#'     \item \code{covariates}: List column or NULL. Covariate names to include
#'   }
#' @param input_data Data frame containing full dataset with spectral features and response.
#'   Used for recipe preparation and preprocessing steps.
#' @param data_split An \code{rsplit} object from rsample package defining train/test split.
#'   Ensures consistent data partitioning across all model configurations.
#' @param config_id Character or numeric. Unique identifier for this configuration,
#'   used in progress messages and error reporting.
#' @param covariate_data Optional data frame containing covariate predictions.
#'   Must have same row ordering as \code{input_data}. NULL if no covariates.
#' @param variable Character. Name of response variable column in \code{input_data}.
#' @param output_dir Character path to directory for saving outputs (checkpoints, logs).
#'   NULL skips file output.
#' @param grid_size Integer. Number of hyperparameter combinations for initial grid search.
#'   Uses Latin hypercube sampling for better parameter space coverage. Default: 10.
#' @param bayesian_iter Integer. Number of Bayesian optimization iterations after grid search.
#'   Set to 0 to skip Bayesian optimization. Default: 15.
#' @param cv_folds Integer. Number of cross-validation folds for hyperparameter tuning.
#'   Minimum 2, recommended 5-10. Uses stratified sampling on response. Default: 10.
#' @param parallel_cv Logical. Enable parallel processing for cross-validation folds.
#'   Uses future package for cross-platform parallel processing. Default: TRUE.
#' @param n_cv_cores Integer or NULL. Number of cores for parallel CV.
#'   NULL auto-detects (total cores - 2). Ignored if parallel_cv = FALSE.
#' @param prune_models Logical. Skip Bayesian optimization for models that don't beat
#'   baseline (mean predictor) by required margin. Saves computation. Default: FALSE.
#' @param prune_threshold Numeric between 0 and 1. Multiplier for baseline RRMSE.
#'   Model must achieve RRMSE < baseline_RRMSE * threshold to proceed to Bayesian.
#'   Example: 0.9 = must beat baseline by 10%. Default: 0.9.
#' @param seed Integer. Random seed for reproducibility of CV splits and tuning. Default: 123.
#'
#' @return A single-row tibble containing:
#'   \describe{
#'     \item{config_id}{Configuration identifier}
#'     \item{workflow_id}{Unique workflow identifier string}
#'     \item{model}{Model type used}
#'     \item{transformation}{Response transformation applied}
#'     \item{preprocessing}{Spectral preprocessing method}
#'     \item{feature_selection}{Feature selection method}
#'     \item{covariates}{Concatenated covariate names or NA}
#'     \item{rsq}{R-squared on test set}
#'     \item{rmse}{Root mean squared error on test set}
#'     \item{rrmse}{Relative RMSE as percentage}
#'     \item{rpd}{Ratio of performance to deviation}
#'     \item{ccc}{Lin's concordance correlation coefficient}
#'     \item{mae}{Mean absolute error on test set}
#'     \item{grid_seconds}{Time for grid search in seconds}
#'     \item{bayes_seconds}{Time for Bayesian optimization in seconds}
#'     \item{total_seconds}{Total execution time in seconds}
#'     \item{status}{"success", "failed", or "pruned"}
#'     \item{error_message}{Error description if failed, NA otherwise}
#'     \item{fitted_workflow}{List column with fitted workflow object for stacking}
#'   }
#'
#' @details
#' The function executes the following pipeline:
#' \enumerate{
#'   \item Input validation and configuration extraction
#'   \item Recipe building with specified preprocessing and feature selection
#'   \item Model specification from configuration
#'   \item Workflow creation (recipe + model)
#'   \item Cross-validation split generation (stratified on response)
#'   \item Optional parallel backend setup (OS-aware)
#'   \item Grid search with Latin hypercube sampling
#'   \item Aggressive memory cleanup after grid search
#'   \item Optional pruning based on baseline comparison
#'   \item Bayesian optimization (if not pruned) with early stopping
#'   \item Final model fit on full training set
#'   \item Test set evaluation with comprehensive metrics
#' }
#'
#' Baseline comparison for pruning uses RMSE of mean predictor. Models must beat
#' baseline * threshold to proceed to Bayesian optimization.
#'
#' All errors are caught and returned as failed results with descriptive messages,
#' ensuring batch evaluation continues even if individual models fail.
#'
#' @seealso
#' \code{\link{build_recipe}} for recipe construction details
#' \code{\link{define_model_specifications}} for supported model types
#' \code{\link{rrmse}}, \code{\link{rpd}}, \code{\link{ccc}} for custom metrics
#'
#' @examples
#' \dontrun{
#' # Create configuration
#' config <- tibble::tibble(
#'   model = "random_forest",
#'   transformation = "Log Transformation",
#'   preprocessing = "snv",
#'   feature_selection = "pca",
#'   covariates = list(c("clay", "sand"))
#' )
#'
#' # Create train/test split
#' split <- rsample::initial_split(spectral_data, prop = 0.8)
#'
#' # Evaluate single model
#' result <- evaluate_single_model_local(
#'   config_row = config[1, ],
#'   input_data = spectral_data,
#'   data_split = split,
#'   config_id = "RF_001",
#'   variable = "SOC",
#'   parallel_cv = TRUE,
#'   prune_models = TRUE
#' )
#' }
#'
#' @importFrom yardstick mae rmse rsq
#' @keywords internal

evaluate_configuration <- function(config_row,
                                   input_data,
                                   data_split,
                                   config_id,
                                   covariate_data  = NULL,
                                   variable,
                                   output_dir      = NULL,
                                   grid_size       = 10,
                                   bayesian_iter   = 15,
                                   cv_folds        = 10,
                                   allow_par       = TRUE,
                                   n_cv_cores      = NULL,
                                   prune_models    = FALSE,
                                   prune_threshold = 0.9,
                                   seed            = 307,
                                   verbose         = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Setup
  ## ---------------------------------------------------------------------------

  if(verbose){

  cli::cli_text("")
  cli::cli_text("{.strong Model Progress:}")

  }

  start_time <- Sys.time()

  ## ========== DEBUG: File-based logging setup ==========
  debug_log_file <- if (!is.null(output_dir)) {
    file.path(output_dir, "debug_worker_output.log")
  } else {
    NULL
  }

  debug_msg <- function(msg, ...) {
    if (!is.null(debug_log_file)) {
      formatted <- sprintf(msg, ...)
      cat(sprintf("[%s] [CORE-%s] %s\n",
                  format(Sys.time(), "%H:%M:%S"),
                  config_id, formatted),
          file = debug_log_file, append = TRUE)
    }
  }

  debug_msg("evaluate_configuration started | n_cv_cores: %s | allow_par: %s",
           n_cv_cores, allow_par)
  ## ========== END DEBUG ==========

  set.seed(seed)

    ## ---------------------------------------------------------------------------
    ## Step 0.1: Input Validation
    ## ---------------------------------------------------------------------------

    ## Make sure these are all structured failures to allow for error propogation

    ## Check that input data is a dataframe --------------------------------------

    if (!is.data.frame(config_row) || nrow(config_row) != 1) {

      return(create_failed_result(config_id     = config_id,
                                  config_clean  = list(model             = "unknown",
                                                       transformation    = "unknown",
                                                       preprocessing     = "unknown",
                                                       feature_selection = "unknown"),
                                  error_message = glue::glue("config_row must be a single-row data frame"),
                                  workflow_id   = config_id,
                                  error_detail  = NULL,
                               #  error_level   = "core",
                                  error_stage   = "Input Validation (Step 0.1)",
                                  error_trace   = NULL,
                                  warnings      = NULL,
                                  messages      = NULL))

    }

    ## Check that we actually have a split data object ---------------------------

    if (!inherits(data_split, "rsplit")) {

      return(create_failed_result(config_id     = config_id,
                                  config_clean  = list(model             = "unknown",
                                                       transformation    = "unknown",
                                                       preprocessing     = "unknown",
                                                       feature_selection = "unknown"),
                                  error_message = glue::glue("data_split must be an rsplit object from rsample"),
                                  workflow_id   = config_id,
                                  error_detail  = NULL,
                               #  error_level   = "core",
                                  error_stage   = "Input Validation (Step 0.1)",
                                  error_trace   = NULL,
                                  warnings      = NULL,
                                  messages      = NULL))

    }

    ## Check that the varaible is the right format and exists --------------------

    if (!is.character(variable) || length(variable) != 1) {

      return(create_failed_result(config_id     = config_id,
                                  config_clean  = list(model             = "unknown",
                                                       transformation    = "unknown",
                                                       preprocessing     = "unknown",
                                                       feature_selection = "unknown"),
                                  error_message = glue::glue("variable must be a single character string"),
                                  workflow_id   = config_id,
                                  error_detail  = NULL,
                               #  error_level   = "core",
                                  error_stage   = "Input Validation (Step 0.1)",
                                  error_trace   = NULL,
                                  warnings      = NULL,
                                  messages      = NULL))

    }

    if (!variable %in% names(input_data)) {

      return(create_failed_result(config_id     = config_id,
                                  config_clean  = list(model             = "unknown",
                                                       transformation    = "unknown",
                                                       preprocessing     = "unknown",
                                                       feature_selection = "unknown"),
                                  error_message = glue::glue("variable '{variable}' not found in input_data"),
                                  workflow_id   = config_id,
                                  error_detail  = NULL,
                               #  error_level   = "core",
                                  error_stage   = "Input Validation (Step 0.1)",
                                  error_trace   = NULL,
                                  warnings      = NULL,
                                  messages      = NULL))

    }

    ## Check that the cv_folds make sense ----------------------------------------

    if (!is.numeric(cv_folds) || cv_folds < 2) {

      return(create_failed_result(config_id     = config_id,
                                  config_clean  = list(model             = "unknown",
                                                       transformation    = "unknown",
                                                       preprocessing     = "unknown",
                                                       feature_selection = "unknown"),
                                  error_message = glue::glue("cv_folds must be at least 2, got {cv_folds}"),
                                  workflow_id   = config_id,
                                  error_detail  = NULL,
                               #  error_level   = "core",
                                  error_stage   = "Input Validation (Step 0.1)",
                                  error_trace   = NULL,
                                  warnings      = NULL,
                                  messages      = NULL))

    }

    ## Check that we have the right size for the grid ----------------------------

    if (!is.numeric(grid_size) || grid_size <= 0) {

      return(create_failed_result(config_id     = config_id,
                                  config_clean  = list(model             = "unknown",
                                                       transformation    = "unknown",
                                                       preprocessing     = "unknown",
                                                       feature_selection = "unknown"),
                                  error_message = glue::glue("grid_size must be positive, got {grid_size}"),
                                  workflow_id   = config_id,
                                  error_detail  = NULL,
                               #  error_level   = "core",
                                  error_stage   = "Input Validation (Step 0.1)",
                                  error_trace   = NULL,
                                  warnings      = NULL,
                                  messages      = NULL))

    }

    ## Check that bayesian_iter is valid -----------------------------------------

    if (!is.numeric(bayesian_iter) || bayesian_iter < 0) {

      return(create_failed_result(config_id     = config_id,
                                  config_clean  = list(model             = "unknown",
                                                       transformation    = "unknown",
                                                       preprocessing     = "unknown",
                                                       feature_selection = "unknown"),
                                  error_message = glue::glue("bayesian_iter must be non-negative, got {bayesian_iter}"),
                                  workflow_id   = config_id,
                                  error_detail  = NULL,
                               #  error_level   = "core",
                                  error_stage   = "Input Validation (Step 0.1)",
                                  error_trace   = NULL,
                                  warnings      = NULL,
                                  messages      = NULL))

    }

    ## Check that prune_threshold is valid ---------------------------------------

    if (!is.numeric(prune_threshold) || prune_threshold <= 0 || prune_threshold > 1) {

      return(create_failed_result(config_id     = config_id,
                                  config_clean  = list(model             = "unknown",
                                                       transformation    = "unknown",
                                                       preprocessing     = "unknown",
                                                       feature_selection = "unknown"),
                                  error_message = glue::glue("prune_threshold must be between 0 and 1, got {prune_threshold}"),
                                  workflow_id   = config_id,
                                  error_detail  = NULL,
                               #  error_level   = "core",
                                  error_stage   = "Input Validation (Step 0.1)",
                                  error_trace   = NULL,
                                  warnings      = NULL,
                                  messages      = NULL))

    }

    ## Check that n cores is valid -----------------------------------------------

    if (!is.null(n_cv_cores) && (!is.numeric(n_cv_cores) || n_cv_cores <= 0)) {

      return(create_failed_result(config_id     = config_id,
                                  config_clean  = list(model             = "unknown",
                                                       transformation    = "unknown",
                                                       preprocessing     = "unknown",
                                                       feature_selection = "unknown"),
                                  error_message = glue::glue("n_cv_cores must be positive if specified, got {n_cv_cores}"),
                                  workflow_id   = config_id,
                                  error_detail  = NULL,
                               #  error_level   = "core",
                                  error_stage   = "Input Validation (Step 0.1)",
                                  error_trace   = NULL,
                                  warnings      = NULL,
                                  messages      = NULL))

    }

  if(verbose) cli::cli_text("├─ Inputs validated.")


  ## ---------------------------------------------------------------------------
  ## Step 1: Extract Configurations
  ## ---------------------------------------------------------------------------

  ## Pull the configs from normal columns --------------------------------------

  list(model             = as.character(config_row$model),
       transformation    = as.character(config_row$transformation),
       preprocessing     = as.character(config_row$preprocessing),
       feature_selection = as.character(config_row$feature_selection)) -> config_clean

  ## Pull the covariates from the list column ----------------------------------

  tryCatch({

    cov_val <- config_row$covariates[[1]]

    if (length(cov_val) > 0) as.character(cov_val) else NULL

    }, error = function(e) NULL) -> config_clean$covariates

  ## Check against accepted constants ------------------------------------------

  ##TODO: Figure out how to deal with user extendability here

  if (!config_clean$model %in% VALID_MODELS) {

    return(create_failed_result(config_id     = config_id,
                                config_clean  = config_clean,
                                error_message = glue::glue("Invalid model type '{config_clean$model}'")))

  }

  if (!config_clean$transformation %in% VALID_TRANSFORMATIONS) {

    return(create_failed_result(config_id     = config_id,
                                config_clean  = config_clean,
                                error_message = glue::glue("Invalid transformation '{config_clean$transformation}'")))

  }

  if (!config_clean$preprocessing %in% VALID_PREPROCESSING) {

    return(create_failed_result(config_id     = config_id,
                                config_clean  = config_clean,
                                error_message = glue::glue("Invalid preprocessing '{config_clean$preprocessing}'")))

  }

  if (!config_clean$feature_selection %in% VALID_FEATURE_SELECTION) {

    return(create_failed_result(config_id     = config_id,
                                config_clean  = config_clean,
                                error_message = glue::glue("Invalid feature selection '{config_clean$feature_selection}'")))

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Create Workflow ID
  ## ---------------------------------------------------------------------------

  clean_workflow_id(model             = config_clean$model,
                    transformation    = config_clean$transformation,
                    preprocessing     = config_clean$preprocessing,
                    feature_selection = config_clean$feature_selection,
                    covariates        = config_clean$covariates) -> workflow_id

  ## ---------------------------------------------------------------------------
  ## Step 3: Build Recipe
  ## ---------------------------------------------------------------------------

  safely_execute(expr = {build_recipe(input_data               = input_data,
                                      spectral_transformation  = config_clean$preprocessing,
                                      response_transformation  = config_clean$transformation,
                                      feature_selection_method = config_clean$feature_selection,
                                      covariate_selection      = config_clean$covariates,
                                      covariate_data           = covariate_data)},
                 default_value      = NULL,
                 error_message      = "Recipe building failed",
                 capture_conditions = TRUE) -> recipe_result

  ## ---------------------------------------------------------------------------

  handle_results(safe_result   = recipe_result,
                 error_title   = "Recipe building failed for {workflow_id}",
                 error_hints   = c("Check that input_data contains required columns",
                                   "Verify transformation and preprocessing methods are valid",
                                   "Ensure covariate_data matches input_data rows if provided"),
                 abort_on_null = FALSE,
                 silent        = FALSE) -> recipe

  ## ---------------------------------------------------------------------------

  if (is.null(recipe)) {

    actual_error <- recipe_result$error$message %||% "Unknown error"

    return(create_failed_result(config_id     = config_id,
                                config_clean  = config_clean,
                                error_message = glue::glue("Recipe building failed: {actual_error}"),
                                workflow_id   = workflow_id,
                                error_detail  = recipe_result$error,
                                error_stage   = "recipe_building",
                                error_trace   = recipe_result$trace,
                                warnings      = recipe_result$warnings %||% NULL,
                                messages      = recipe_result$messages %||% NULL))
  }

  if(verbose) cli::cli_text("├─ Recipe built.")

  ## ---------------------------------------------------------------------------
  ## Step 4: Extract Training Data
  ## ---------------------------------------------------------------------------

  safely_execute(expr = {list(train_data = rsample::training(data_split),
                              test_data  = rsample::testing(data_split))},
                 default_value      = NULL,
                 error_message      = "Data split extraction failed",
                 capture_conditions = TRUE) -> split_result

  ## ---------------------------------------------------------------------------

  handle_results(safe_result   = split_result,
                 error_title   = "Failed to extract training/testing data from data_split",
                 error_hints   = c("Check that data_split is a valid rsplit object",
                                   "Verify rsample package is properly loaded",
                                   "Ensure data_split was created successfully"),
                 abort_on_null = FALSE,
                 silent        = FALSE) -> split_data

  ## ---------------------------------------------------------------------------

  if (is.null(split_data)) {

    actual_error <- split_result$error$message %||% "Unknown error"

    return(create_failed_result(config_id     = config_id,
                                config_clean  = config_clean,
                                error_message = glue::glue("Data split extraction failed: {actual_error}"),
                                workflow_id   = workflow_id,
                                error_detail  = split_result$error,
                                error_stage   = "data_extraction",
                                error_trace   = split_result$trace,
                                warnings      = split_result$warnings %||% NULL,
                                messages      = split_result$messages %||% NULL))
  }

  train_data <- split_data$train_data
  test_data  <- split_data$test_data

  ## ---------------------------------------------------------------------------
  ## Step 5: Define Model Specification
  ## ---------------------------------------------------------------------------

  safely_execute(expr = {define_model_specifications(config_clean$model)},
                 default_value      = NULL,
                 error_message      = "Model spec creation failed",
                 capture_conditions = TRUE) -> model_spec_result

  ## ---------------------------------------------------------------------------

  handle_results(safe_result   = model_spec_result,
                 error_title   = "Failed to define model specification for {config_clean$model}",
                 error_hints   = c("Check that model type '{config_clean$model}' is valid",
                                   "Verify model is supported in current tidymodels version",
                                   "Ensure required model packages are installed"),
                 abort_on_null = FALSE,
                 silent        = FALSE) -> model_spec

  ## ---------------------------------------------------------------------------

  if (is.null(model_spec)) {

    actual_error <- model_spec_result$error$message %||% "Unknown error"

    return(create_failed_result(config_id     = config_id,
                                config_clean  = config_clean,
                                error_message = glue::glue("Model specification failed: {actual_error}"),
                                workflow_id   = workflow_id,
                                error_detail  = model_spec_result$error,
                                error_stage   = "model_specification",
                                error_trace   = model_spec_result$trace,
                                warnings      = model_spec_result$warnings %||% NULL,
                                messages      = model_spec_result$messages %||% NULL))
  }

  if(verbose) cli::cli_text("├─ Model specified.")

  ## ---------------------------------------------------------------------------
  ## Step 6: Create Workflow
  ## ---------------------------------------------------------------------------

  safely_execute(expr = {workflows::workflow() %>%
                          workflows::add_recipe(recipe) %>%
                          workflows::add_model(model_spec)},
                          default_value      = NULL,
                          error_message      = "Workflow creation failed",
                          capture_conditions = TRUE) -> wflow_result

  handle_results(safe_result   = wflow_result,
                 error_title   = "Failed to create workflow for {workflow_id}",
                 error_hints   = c("Check recipe and model specification compatibility",
                                   "Verify tidymodels package versions are compatible",
                                   "Ensure recipe preprocessing matches model requirements"),
                 abort_on_null = FALSE,
                 silent        = FALSE) -> wflow

  if (is.null(wflow)) {

    actual_error <- wflow_result$error$message %||% "Unknown error"

    return(create_failed_result(config_id     = config_id,
                                config_clean  = config_clean,
                                error_message = glue::glue("Workflow creation failed: {actual_error}"),
                                workflow_id   = workflow_id,
                                error_detail  = wflow_result$error,
                                error_stage   = "workflow_creation",
                                error_trace   = wflow_result$trace,
                                warnings      = wflow_result$warnings %||% NULL,
                                messages      = wflow_result$messages %||% NULL))
  }

  if(verbose) cli::cli_text("├─ Workflow created.")

  ## ---------------------------------------------------------------------------
  ## Step 7: Create CV Splits from Training Data
  ## ---------------------------------------------------------------------------

  safely_execute(expr = {rsample::vfold_cv(data   = train_data,
                                           v      = cv_folds,
                                           strata = !!variable)},
                 default_value      = NULL,
                 error_message      = "CV split creation failed",
                 capture_conditions = TRUE) -> cv_splits_result

  ## ---------------------------------------------------------------------------

  handle_results(safe_result   = cv_splits_result,
                 error_title   = "Failed to create {cv_folds}-fold CV splits",
                 error_hints   = c("Check that training data has sufficient samples",
                                   "Verify variable has enough levels for stratification",
                                   "Ensure variable column exists in training data"),
                 abort_on_null = FALSE,
                 silent        = FALSE) -> cv_splits

  ## ---------------------------------------------------------------------------

  if (is.null(cv_splits)) {

    actual_error <- cv_splits_result$error$message %||% "Unknown error"

    return(create_failed_result(config_id     = config_id,
                                config_clean  = config_clean,
                                error_message = glue::glue("CV split creation failed: {actual_error}"),
                                workflow_id   = workflow_id,
                                error_detail  = cv_splits_result$error,
                                error_stage   = "cv_creation",
                                error_trace   = cv_splits_result$trace,
                                warnings      = cv_splits_result$warnings %||% NULL,
                                messages      = cv_splits_result$messages %||% NULL))
  }


  ## ---------------------------------------------------------------------------
  ## Step 8: Initial Grid Search
  ## ---------------------------------------------------------------------------

    ## -------------------------------------------------------------------------
    ## Step 8.1: Finalize parameter sets as needed (just rf really)
    ## -------------------------------------------------------------------------

    ## Check if mtry is part of the parameter set --------------------------------

    param_set <- workflows::extract_parameter_set_dials(wflow)

    ## Bake the data to get the final dataset, then finalize the param set ------

    if ("mtry" %in% param_set$name) {

      recipe %>%
        recipes::prep() %>%
        recipes::bake(new_data = NULL) %>%
        dplyr::select(-Response) -> eval_data

      param_set <- dials::finalize(param_set, eval_data)

      # Explicit cleanup to prevent memory accumulation in long HPC runs
      rm(eval_data)
      invisible(gc(verbose = FALSE))

    }

    ## -------------------------------------------------------------------------
    ## TEST: New parallel backend option for the resamples
    ## -------------------------------------------------------------------------

    ## Set multicore options for better performance
    options(
      mc.cores = n_cv_cores,
      mc.preschedule = FALSE  # Dynamic scheduling better for uneven CV folds
    )

    RNGkind("L'Ecuyer-CMRG")
    set.seed(seed)

    doMC::registerDoMC(cores = n_cv_cores)

    stopifnot(foreach::getDoParName() == "doMC")
    stopifnot(foreach::getDoParWorkers() == n_cv_cores)

    ## ========== DEBUG: Parallel backend verification ==========
    debug_msg("Parallel backend configured | Backend: %s | Workers: %d | mc.cores: %s | mc.preschedule: %s",
             foreach::getDoParName(),
             foreach::getDoParWorkers(),
             getOption('mc.cores'),
             getOption('mc.preschedule'))
    ## ========== END DEBUG ==========

    ## -------------------------------------------------------------------------
    ## Step 8.2: Run grid search and save results
    ## -------------------------------------------------------------------------

    grid_start_time <- Sys.time()

    if(verbose) cli::cli_text("├─ Starting grid search.")

    ## ========== DEBUG: Grid search parallelization ==========
    debug_msg("Starting grid search | allow_par: %s | CV folds: %d | grid_size: %d",
             allow_par, cv_folds, grid_size)
    debug_msg("Current plan: %s | R processes before grid: %s",
             class(future::plan())[1],
             system('ps aux | grep "[R]" | wc -l', intern = TRUE))
    ## ========== END DEBUG ==========

    ## Run the search, using rrmse as primary metric -----------------------------
    ## This could be a debatable choice-- RPD might be better? -------------------

    safely_execute(expr = {suppressMessages({tune::tune_grid(object     = wflow,
                                                             resamples  = cv_splits,
                                                             grid       = grid_size,
                                                             metrics    = yardstick::metric_set(rrmse,
                                                                                                yardstick::rmse,
                                                                                                yardstick::rsq,
                                                                                                yardstick::mae,
                                                                                                rpd,
                                                                                                ccc),
                                                             param_info = param_set,
                                                             control    = tune::control_grid(save_pred     = FALSE,
                                                                                             save_workflow = FALSE,
                                                                                             verbose       = FALSE,
                                                                                             extract       = NULL,
                                                                                             allow_par     = allow_par,
                                                                                             parallel_over = "everything"))})},
                   default_value      = NULL,
                   error_message      = "Grid search failed",
                   capture_conditions = TRUE) -> grid_tune_result

    ## ---------------------------------------------------------------------------

    handle_results(safe_result   = grid_tune_result,
                   error_title   = "Grid search failed for {workflow_id}",
                   error_hints   = c("Check that all model parameters are valid",
                                     "Verify parallel processing setup if using parallel_cv",
                                     "Ensure CV folds have sufficient samples for model fitting",
                                     "Try reducing grid_size if memory issues occur"),
                   abort_on_null = FALSE,
                   silent        = FALSE) -> grid_results

    ## ---------------------------------------------------------------------------

    if (is.null(grid_results)) {

      actual_error <- grid_tune_result$error$message %||% "Unknown error"

      return(create_failed_result(config_id     = config_id,
                                  config_clean  = config_clean,
                                  error_message = glue::glue("Grid tuning failed: {actual_error}"),
                                  workflow_id   = workflow_id,
                                  error_detail  = grid_tune_result$error,
                                  error_stage   = "grid_tuning",
                                  error_trace   = grid_tune_result$trace,
                                  warnings      = grid_tune_result$warnings %||% NULL,
                                  messages      = grid_tune_result$messages %||% NULL))
    }

    if(length(grid_tune_result$warnings) > 0 && stringr::str_detect(string  = grid_tune_result$warnings[[1]],
                           pattern = "All models failed.")) {

      capture.output(tune::show_notes(.Last.tune.result)) %>%
        as_tibble() %>%
        filter(value != "───────────────────────",
               value != "unique notes:") -> error_note

      actual_error <- paste("All models failed: ", paste(unique(error_note$value), collapse = "; "))

      return(create_failed_result(config_id     = config_id,
                                  config_clean  = config_clean,
                                  error_message = glue::glue("All models failed!"),
                                  workflow_id   = workflow_id,
                                  error_detail  = grid_tune_result$actual_error,
                                  error_stage   = "grid_tuning",
                                  error_trace   = grid_tune_result$trace,
                                  warnings      = grid_tune_result$warnings %||% NULL,
                                  messages      = grid_tune_result$messages %||% NULL))

    }



    ## ---------------------------------------------------------------------------

    grid_seconds <- as.numeric(difftime(Sys.time(), grid_start_time, units = "secs"))

    ##TODO: Use the util-reporting.R functionality here.

    if (!is.null(grid_results) && verbose) cli::cli_text("│  └─ Grid search complete: {round(grid_seconds, 1)}s")

    ## ========== DEBUG: Grid search complete ==========
    debug_msg("Grid search complete | Duration: %.1f seconds | R processes after grid: %s",
             grid_seconds,
             system('ps aux | grep "[R]" | wc -l', intern = TRUE))
    ## ========== END DEBUG ==========

    # Aggressive memory cleanup after grid search --------------------------------

    invisible(gc(verbose = FALSE, full = TRUE))

  ## ---------------------------------------------------------------------------
  ## Step 9: Optional pruning based on baseline comparison
  ## ---------------------------------------------------------------------------

  skip_bayesian <- FALSE

  if (prune_models) {

    baseline_mean <- mean(train_data[[variable]], na.rm = TRUE)

    ## Gently fail if response mean is essentially zero ------------------------

    if (abs(baseline_mean) < .Machine$double.eps) {

      cli::cli_alert_warning("Response mean too close to zero, skipping pruning")

    } else {

      ## Calculate baseline RRMSE ----------------------------------------------

      baseline_rrmse  <- (sd(train_data[[variable]], na.rm = TRUE) / abs(baseline_mean)) * 100
      threshold_rrmse <- baseline_rrmse * prune_threshold

      ## Get best performance from grid results --------------------------------

      safely_execute(expr = {tune::collect_metrics(grid_results) %>%
                               dplyr::filter(.metric == "rrmse") %>%
                               dplyr::summarise(min_rrmse = min(mean, na.rm = TRUE)) %>%
                               dplyr::pull(min_rrmse)},
                     default_value      = NULL,
                     error_message      = "Grid metrics extraction failed",
                     capture_conditions = TRUE) -> best_perf_result

      ## -----------------------------------------------------------------------

      handle_results(safe_result   = best_perf_result,
                     error_title   = "Failed to extract best RRMSE from grid results",
                     error_hints   = c("Check that grid_results contains valid metrics",
                                       "Verify 'rrmse' metric was computed during tuning",
                                       "Ensure grid search produced at least one result"),
                     abort_on_null = FALSE,
                     silent        = FALSE) -> best_perf

      ## -----------------------------------------------------------------------

      if (is.null(best_perf)) {

        ## Fallback to skip pruning if metrics extraction fails ----------------

        best_perf <- Inf
        cli::cli_alert_warning("Could not extract grid metrics, skipping pruning")

      }

      # Skip Bayesian if grid performance is terrible --------------------------

      if (is.infinite(best_perf) || best_perf > threshold_rrmse) {

        ##TODO: Use the util-reporting.R functionality here too.

        skip_bayesian <- TRUE
        cli::cli_text("├─ Pruning: RRMSE {round(best_perf, 1)}% > {round(threshold_rrmse, 1)}% threshold (skipping Bayesian)")

      }
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 10: Optimize parameters with Bayesian search
  ## ---------------------------------------------------------------------------

  bayes_seconds <- 0
  bayes_error_info <- NULL

  if (!skip_bayesian && bayesian_iter > 0) {

    bayes_start_time <- Sys.time()

    if (verbose) cli::cli_text("├─ Starting Bayesian optimization")

    ## ========== DEBUG: Bayesian optimization parallelization ==========
    debug_msg("Starting Bayesian optimization | allow_par: %s | Iterations: %d",
             allow_par, bayesian_iter)
    debug_msg("R processes before Bayes: %s",
             system('ps aux | grep "[R]" | wc -l', intern = TRUE))
    ## ========== END DEBUG ==========

    ## Run the search across a Gaussian process model -----------------------------

    safely_execute(expr = {suppressMessages({
                                             suppressWarnings({
                                               tune::tune_bayes(object     = wflow,
                                                              resamples  = cv_splits,
                                                              initial    = grid_results,
                                                              iter       = bayesian_iter,
                                                              param_info = param_set,
                                                              metrics    = yardstick::metric_set(rrmse,
                                                                                                 yardstick::rmse,
                                                                                                 yardstick::rsq,
                                                                                                 yardstick::mae,
                                                                                                 rpd,
                                                                                                 ccc),
                                                              control   = tune::control_bayes(save_pred     = FALSE,
                                                                                              save_workflow = FALSE,
                                                                                              verbose       = FALSE,
                                                                                              no_improve    = BAYES_NO_IMPROVE_LIMIT,
                                                                                              allow_par     = allow_par,
                                                                                              parallel_over = "everything"))
                                             })
                                           })},
                 default_value      = NULL,
                 error_message      = "Bayesian optimization failed",
                 capture_conditions = TRUE) -> bayes_tune_result

    ## -------------------------------------------------------------------------

    handle_results(safe_result   = bayes_tune_result,
                   error_title   = "Bayesian optimization failed for {workflow_id}",
                   error_hints   = c("Grid search results will be used instead",
                                     "This may indicate parameter space issues or convergence problems",
                                     "Consider reducing bayesian_iter if problems persist"),
                   abort_on_null = FALSE,
                   silent        = FALSE) -> final_tune_results

    ## -------------------------------------------------------------------------

    if (is.null(final_tune_results)) {

      cli::cli_text("│  └─ {cli::col_red('!! Using grid search results (Bayesian optimization failed)')}")

      final_tune_results <- grid_results

    }

    ## -------------------------------------------------------------------------

    if(length(bayes_tune_result$warnings) > 0 && stringr::str_detect(string  = bayes_tune_result$warnings[[1]],
                                                                     pattern = "All models failed.")) {

      capture.output(tune::show_notes(.Last.tune.result)) %>%
        as_tibble() %>%
        filter(value != "───────────────────────",
               value != "unique notes:") -> error_note

      actual_error <- paste(unique(error_note$value), collapse = "; ")

      cli::cli_text("│  └─ {cli::col_red('!! All models failed Bayesian optimization.')}")
      cli::cli_text("│     └─ {.val {actual_error}}")
      cli::cli_text("│     └─ Using grid results as a fallback.")

      final_tune_results <- grid_results

    }

    ## -------------------------------------------------------------------------

    bayes_seconds <- as.numeric(difftime(Sys.time(), bayes_start_time, units = "secs"))

    if (!is.null(final_tune_results) && !identical(final_tune_results, grid_results)) {

        cli::cli_text("│  └─ Bayesian optimization complete: {round(bayes_seconds, 1)}s")

        ## ========== DEBUG: Bayesian complete ==========
        debug_msg("Bayesian optimization complete | Duration: %.1f seconds | R processes after Bayes: %s",
                 bayes_seconds,
                 system('ps aux | grep "[R]" | wc -l', intern = TRUE))
        ## ========== END DEBUG ==========

      } else {

        cli::cli_text("│  └─ Bayesian optimization failed: {round(bayes_seconds, 1)}s")
    }

    } else {

    # Use grid results if Bayesian was skipped ---------------------------------

      final_tune_results <- grid_results

  }

  ## ---------------------------------------------------------------------------
  ## Step 11: Final model fit
  ## ---------------------------------------------------------------------------

    ## -------------------------------------------------------------------------
    ## Step 11.1: Select best hyper parameters
    ## -------------------------------------------------------------------------

    ## Pull out the final parameter set ----------------------------------------

    safely_execute(expr = {tune::select_best(final_tune_results, metric = "rrmse")},
                   default_value      = NULL,
                   error_message      = "Parameter selection failed",
                   capture_conditions = TRUE) -> best_params_result

    ## -----------------------------------------------------------------------

    handle_results(safe_result   = best_params_result,
                   error_title   = "Failed to select best parameters",
                   error_hints   = c("Check that final_tune_results contains valid metrics",
                                     "Verify 'rrmse' metric was computed during tuning",
                                     "Ensure at least one parameter combination succeeded"),
                   abort_on_null = FALSE,
                   silent        = FALSE) -> best_params

    ## -----------------------------------------------------------------------

    if (is.null(best_params)) {

      actual_error <- best_params_result$error$message %||% "Unknown error"

      return(create_failed_result(config_id     = config_id,
                                  config_clean  = config_clean,
                                  error_message = glue::glue("Parameter selection failed: {actual_error}"),
                                  workflow_id   = workflow_id,
                                  error_detail  = best_params_result$error,
                                  error_stage   = "parameter_selection",
                                  error_trace   = best_params_result$trace,
                                  warnings      = best_params_result$warnings %||% NULL,
                                  messages      = best_params_result$messages %||% NULL))

    }

    if(verbose) cli::cli_text("├─ Final parameters selected.")

    ## -------------------------------------------------------------------------
    ## Step 11.2: Finalize the workflow
    ## -------------------------------------------------------------------------

    ## Fit the new parameters to the workflow ----------------------------------

    safely_execute(expr = {tune::finalize_workflow(wflow, best_params)},
                   default_value      = NULL,
                   error_message      = "Workflow finalization failed",
                   capture_conditions = TRUE) -> final_wflow_result

    ## -----------------------------------------------------------------------

    handle_results(safe_result   = final_wflow_result,
                   error_title   = "Failed to finalize workflow with best parameters",
                   error_hints   = c("Check parameter compatibility with workflow",
                                     "Verify best_params contains all required parameters",
                                     "Ensure workflow recipe and model are properly configured"),
                   abort_on_null = FALSE,
                   silent        = FALSE) -> final_wflow

    ## -----------------------------------------------------------------------

    if (is.null(final_wflow)) {

      actual_error <- final_wflow_result$error$message %||% "Unknown error"

      return(create_failed_result(config_id     = config_id,
                                  config_clean  = config_clean,
                                  error_message = glue::glue("Workflow finalization failed: {actual_error}"),
                                  workflow_id   = workflow_id,
                                  error_detail  = final_wflow_result$error,
                                  error_stage   = "workflow_finalization",
                                  error_trace   = final_wflow_result$trace,
                                  warnings      = final_wflow_result$warnings %||% NULL,
                                  messages      = final_wflow_result$messages %||% NULL))
    }

  if(verbose) cli::cli_text("├─ Finalized workflow created.")

  ## ---------------------------------------------------------------------------
  ## Step 12: Evaluate fitted model on the test set
  ## ---------------------------------------------------------------------------

  safely_execute(expr = {tune::last_fit(final_wflow,
                                        split   = data_split,
                                        metrics = yardstick::metric_set(rrmse,
                                                                        yardstick::rmse,
                                                                        yardstick::rsq,
                                                                        yardstick::mae,
                                                                        rpd,
                                                                        ccc))},
                 default_value      = NULL,
                 error_message      = "Final model fitting failed",
                 capture_conditions = TRUE) -> final_fit_result

  ## ---------------------------------------------------------------------------

  handle_results(safe_result   = final_fit_result,
                 error_title   = "Final model fitting failed for {workflow_id}",
                 error_hints   = c("Check that training data has sufficient samples",
                                   "Verify model can handle the preprocessed features",
                                   "Ensure test set is compatible with training data format",
                                   "Try reducing model complexity if memory issues occur"),
                 abort_on_null = FALSE,
                 silent        = FALSE) -> final_fit

  ## ---------------------------------------------------------------------------

  if (is.null(final_fit)) {

    actual_error <- final_fit_result$error$message %||% "Unknown error"

    return(create_failed_result(config_id     = config_id,
                                config_clean  = config_clean,
                                error_message = glue::glue("Final model fitting failed: {actual_error}"),
                                workflow_id   = workflow_id,
                                error_detail  = final_fit_result$error,
                                error_stage   = "final_fitting",
                                error_trace   = final_fit_result$trace,
                                warnings      = final_fit_result$warnings %||% NULL,
                                messages      = final_fit_result$messages %||% NULL))
  }


  if(verbose) cli::cli_text("├─ Finalized model tested on validation data.")

  ## ---------------------------------------------------------------------------
  ## Step 13: Extract metrics for the finalized test
  ## ---------------------------------------------------------------------------

    ## -------------------------------------------------------------------------
    ## Step 13.1: Simple extraction - no error handling
    ## -------------------------------------------------------------------------

    test_predictions <- tune::collect_predictions(final_fit)

    ## -------------------------------------------------------------------------
    ## Step 13.2: Back-transform if needed
    ## -------------------------------------------------------------------------

    if (config_clean$transformation != "none") {

      safely_execute(expr = {back_transform_predictions(predictions    = test_predictions$.pred,
                                                        transformation = config_clean$transformation,
                                                        warn           = FALSE)},
                     default_value      = NULL,
                     error_message      = "Back-transformation failed",
                     capture_conditions = TRUE) -> back_transform_result

      ## -----------------------------------------------------------------------

      handle_results(safe_result   = back_transform_result,
                     error_title   = "Back-transformation failed for {config_clean$transformation}",
                     error_hints   = c("Check for negative values with log transformation",
                                       "Verify transformation method is valid",
                                       "Original transformed predictions will be used"),
                     abort_on_null = FALSE,
                     silent        = FALSE) -> back_transformed

      ## -----------------------------------------------------------------------

      if (!is.null(back_transformed)) {

        test_predictions$.pred <- back_transformed

        } else {

          cli::cli_alert_warning("Using transformed predictions (back-transformation failed)")

        }
      }

      ## -----------------------------------------------------------------------
      ## Step 13.3: Calculate metrics
      ## -----------------------------------------------------------------------

      safely_execute(expr = {compute_original_scale_metrics(truth     = test_predictions[[variable]],
                                                            estimate = test_predictions$.pred,
                                                            metrics  = yardstick::metric_set(rrmse,
                                                                                             yardstick::rmse,
                                                                                             yardstick::rsq,
                                                                                             yardstick::mae,
                                                                                             rpd,
                                                                                             ccc)) %>%
                              tidyr::pivot_wider(names_from  = .metric,
                                                 values_from = .estimate)},
                     default_value      = NULL,
                     error_message      = "Metric calculation failed",
                     capture_conditions = TRUE) -> test_metrics_result

      ## -----------------------------------------------------------------------

       handle_results(safe_result   = test_metrics_result,
                      error_title   = "Metric calculation failed for {workflow_id}",
                      error_hints   = c("Check for all-constant predictions (causes division by zero)",
                                        "Verify truth and estimate vectors have same length",
                                        "Ensure predictions contain valid numeric values"),
                      abort_on_null = FALSE,
                      silent        = FALSE) -> test_metrics

      ## -----------------------------------------------------------------------

      if (is.null(test_metrics)) {

        cli::cli_alert_warning("Metric calculation failed, returning NA.")

        test_metrics <- tibble::tibble(rmse  = NA_real_,
                                       rsq   = NA_real_,
                                       mae   = NA_real_,
                                       rrmse = NA_real_,
                                       rpd   = NA_real_,
                                       ccc   = NA_real_)
      }


    if(verbose) cli::cli_text("└─ Configuration evaluation complete.")

    ## -------------------------------------------------------------------------
    ## Step 14: Compile and return results
    ## -------------------------------------------------------------------------

    tibble::tibble(config_id         = config_id,
                   workflow_id       = workflow_id,
                   model             = config_clean$model,
                   transformation    = config_clean$transformation,
                   preprocessing     = config_clean$preprocessing,
                   feature_selection = config_clean$feature_selection,
                   covariates        = paste(config_clean$covariates %||% "", collapse = "-"),
                   best_params       = list(best_params),
                   rsq               = test_metrics$rsq %||% NA_real_,
                   rmse              = test_metrics$rmse %||% NA_real_,
                   rrmse             = test_metrics$rrmse %||% NA_real_,
                   rpd               = test_metrics$rpd %||% NA_real_,
                   ccc               = test_metrics$ccc %||% NA_real_,
                   mae               = test_metrics$mae %||% NA_real_,
                   grid_seconds      = grid_seconds,
                   bayes_seconds     = bayes_seconds,
                   total_seconds     = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
                   status            = if (skip_bayesian) "pruned" else "success",
                   error_message     = NA_character_,
                   error_stage       = NA_character_,
                   error_class       = NA_character_,
                   has_trace         = FALSE,
                   n_warnings        = 0L,  # TODO: Aggregate warnings from all stages
                   warning_summary   = NA_character_) -> result

  ## ========== DEBUG: Configuration complete ==========
  debug_msg("Configuration %s complete | Status: %s | Total time: %.1f seconds | Metrics: RMSE=%.3f, R2=%.3f, RPD=%.2f",
           config_id,
           if (skip_bayesian) "pruned" else "success",
           as.numeric(difftime(Sys.time(), start_time, units = "secs")),
           test_metrics$rmse %||% NA_real_,
           test_metrics$rsq %||% NA_real_,
           test_metrics$rpd %||% NA_real_)
  ## ========== END DEBUG ==========

  return(result)
}
