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
                                   parallel_cv     = TRUE,
                                   n_cv_cores      = NULL,
                                   prune_models    = FALSE,
                                   prune_threshold = 0.9,
                                   seed            = 307) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Setup
  ## ---------------------------------------------------------------------------

  start_time <- Sys.time()

  ## ---------------------------------------------------------------------------
  ## Step 2: Input Validation
  ## ---------------------------------------------------------------------------

  if (!is.data.frame(config_row) || nrow(config_row) != 1) {

    cli::cli_abort("config_row must be a single-row data frame")

  }

  if (!inherits(data_split, "rsplit")) {

    cli::cli_abort("data_split must be an rsplit object from rsample")

  }

  if (!is.character(variable) || length(variable) != 1) {

    cli::cli_abort("variable must be a single character string")

  }

  if (!variable %in% names(input_data)) {

    cli::cli_abort("variable '{variable}' not found in input_data")

  }

  if (!is.numeric(cv_folds) || cv_folds < 2) {

    cli::cli_abort("cv_folds must be at least 2, got {cv_folds}")

  }

  if (!is.numeric(grid_size) || grid_size <= 0) {

    cli::cli_abort("grid_size must be positive, got {grid_size}")

  }

  if (!is.numeric(bayesian_iter) || bayesian_iter < 0) {

    cli::cli_abort("bayesian_iter must be non-negative, got {bayesian_iter}")

  }

  if (!is.numeric(prune_threshold) || prune_threshold <= 0 || prune_threshold > 1) {

    cli::cli_abort("prune_threshold must be between 0 and 1, got {prune_threshold}")

  }

  if (!is.null(n_cv_cores) && (!is.numeric(n_cv_cores) || n_cv_cores <= 0)) {

    cli::cli_abort("n_cv_cores must be positive if specified, got {n_cv_cores}")

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Extract Configuration
  ## ---------------------------------------------------------------------------

  ## -------------------------------------------------------------------------
  ## Step 3.1: Extract and Clean Configuration
  ## -------------------------------------------------------------------------

  list(model             = as.character(config_row$model),
       transformation    = as.character(config_row$transformation),
       preprocessing     = as.character(config_row$preprocessing),
       feature_selection = as.character(config_row$feature_selection)) -> config_clean

  ## -------------------------------------------------------------------------
  ## Step 3.2: Handle Covariates from List Column (with Closure Safety)
  ## -------------------------------------------------------------------------

  if ("covariates" %in% names(config_row)) {

    cov_value <- config_row$covariates[[1]]

    # CRITICAL FIX: Check for closure/function before processing
    if (is.function(cov_value) || "closure" %in% class(cov_value)) {

      # Log the error with detailed information for debugging
      cli::cli_abort(paste0("▶ evaluate_configuration: Closure detected in covariates for config_id {config_id}.",
                           " This indicates a parallel processing variable scoping issue.",
                           " Covariate class: {paste(class(cov_value), collapse = ', ')}.",
                           " This error occurs when list columns contain unevaluated expressions",
                           " that become closures in parallel worker environments.",
                           " Fix: Apply parallel-safe covariate processing before model evaluation."))

    } else if (is.null(cov_value) || length(cov_value) == 0) {

      config_clean$covariates <- NULL

    } else {

      # Additional safety: Force character coercion
      config_clean$covariates <- tryCatch({
        as.character(cov_value)
      }, error = function(e) {
        cli::cli_abort("▶ evaluate_configuration: Cannot convert covariates to character for config_id {config_id}: {e$message}")
      })

    }

  } else {

    config_clean$covariates <- NULL

  }

  ## -------------------------------------------------------------------------
  ## Step 3.3: Validate Against Package Constants
  ## -------------------------------------------------------------------------

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

  ## ---------------------------------------------------------------------------
  ## Step 4: Create Workflow ID
  ## ---------------------------------------------------------------------------

  clean_workflow_id(model             = config_clean$model,
                    transformation    = config_clean$transformation,
                    preprocessing     = config_clean$preprocessing,
                    feature_selection = config_clean$feature_selection,
                    covariates        = config_clean$covariates) -> workflow_id

  ## ---------------------------------------------------------------------------
  ## Step 5: Build Recipe
  ## ---------------------------------------------------------------------------

  safely_execute(
    expr = {
      build_recipe(input_data               = input_data,
                   spectral_transformation  = config_clean$preprocessing,
                   response_transformation  = config_clean$transformation,
                   feature_selection_method = config_clean$feature_selection,
                   covariate_selection      = config_clean$covariates,
                   covariate_data           = covariate_data)
    },
    default_value = NULL,
    error_message = glue::glue("Failed to build recipe for {workflow_id}")) -> recipe_result

  ## ---------------------------------------------------------------------------

  if (is.null(recipe_result$result)) {

    actual_error <- recipe_result$error$message %||% "Unknown error"

    return(create_failed_result(config_id     = config_id,
                                config_clean  = config_clean,
                                error_message = glue::glue("Recipe building failed: {actual_error}"),
                                workflow_id   = workflow_id,
                                error_detail  = recipe_result$error,
                                error_stage   = "recipe_building",
                                error_trace   = recipe_result$trace,
                                warnings      = recipe_result$warnings,
                                messages      = recipe_result$messages))

  }

  recipe <- recipe_result$result

  ## ---------------------------------------------------------------------------
  ## Step 6: Extract Training Data
  ## ---------------------------------------------------------------------------

  train_data <- rsample::training(data_split)
  test_data  <- rsample::testing(data_split)

  ## ---------------------------------------------------------------------------
  ## Step 7: Define Model Specification
  ## ---------------------------------------------------------------------------

  safely_execute(
    expr = {
      define_model_specifications(config_clean$model)
    },
    default_value = NULL,
    error_message = glue::glue("Failed to define model spec for {config_clean$model}")) -> model_spec_result

  ## ---------------------------------------------------------------------------

  if (is.null(model_spec_result$result)) {

    actual_error <- model_spec_result$error$message %||% "Unknown error"

    return(
      create_failed_result(
        config_id     = config_id,
        config_clean  = config_clean,
        error_message = glue::glue("Model specification failed: {actual_error}"),
        workflow_id   = workflow_id,
        error_detail  = model_spec_result$error,
        error_stage   = "model_specification",
        error_trace   = model_spec_result$trace,
        warnings      = model_spec_result$warnings,
        messages      = model_spec_result$messages
      )
    )

  }

  model_spec <- model_spec_result$result

  ## ---------------------------------------------------------------------------
  ## Step 8: Create Workflow
  ## ---------------------------------------------------------------------------

  safely_execute(
    expr = {
      workflows::workflow() %>%
        workflows::add_recipe(recipe) %>%
        workflows::add_model(model_spec)
    },
    default_value = NULL,
    error_message = glue::glue("Failed to create workflow for {workflow_id}")) -> wflow_result

  if (is.null(wflow_result$result)) {

    actual_error <- wflow_result$error$message %||% "Unknown error"

    return(create_failed_result(config_id     = config_id,
                                config_clean  = config_clean,
                                error_message = glue::glue("Workflow creation failed: {actual_error}"),
                                workflow_id   = workflow_id,
                                error_detail  = wflow_result$error,
                                error_stage   = "workflow_creation",
                                error_trace   = wflow_result$trace,
                                warnings      = wflow_result$warnings,
                                messages      = wflow_result$messages))

    }

  wflow <- wflow_result$result

  ## ---------------------------------------------------------------------------
  ## Step 9: Create CV Splits from Training Data
  ## ---------------------------------------------------------------------------

  # Standard k-fold CV with stratification on response variable
  # Future enhancement: Support leave-one-site-out or spatial CV strategies

  safely_execute(
    expr = {
      tryCatch(
        {
          rsample::vfold_cv(
            data   = train_data,
            v      = cv_folds,
            strata = !!variable
          )
        },
        error = function(e) {
          cli::cli_alert_warning("Stratification failed, using random CV splits: {conditionMessage(e)}")
          rsample::vfold_cv(
            data = train_data,
            v    = cv_folds
          )
        }
      )
    },
    default_value = NULL,
    error_message = glue::glue("Failed to create {cv_folds}-fold CV splits")) -> cv_splits_result

  if (is.null(cv_splits_result$result)) {

    actual_error <- cv_splits_result$error$message %||% "Unknown error"

    return(create_failed_result(config_id     = config_id,
                                config_clean  = config_clean,
                                error_message = glue::glue("CV split creation failed: {actual_error}"),
                                workflow_id   = workflow_id))

  }

  cv_splits <- cv_splits_result$result

  ## ---------------------------------------------------------------------------
  ## Step 10: Setup Parallel Backend (if requested)
  ## ---------------------------------------------------------------------------

  if (parallel_cv && !is.null(n_cv_cores) && n_cv_cores > 1) {

    # Use context-aware backend selection
    old_plan <- setup_parallel_backend(
      n_workers = n_cv_cores,
      force_backend = NULL,  # Auto-detect optimal backend
      memory_limit_gb = 2,   # Limit for spectral data
      enable_work_stealing = TRUE,  # Dynamic load balancing
      verbose = FALSE
    )

    # Ensure restoration on exit
    on.exit({
      restore_parallel_settings(old_plan, verbose = FALSE)
      optimize_parallel_memory(force_gc = TRUE, verbose = FALSE)
    }, add = TRUE)

    # Initialize reproducible RNG for parallel CV
    if (!is.null(seed)) {
      setup_parallel_rng(seed, n_cv_cores, verbose = FALSE)
    }

    # Get actual backend used for display
    backend_display <- get_backend_display()

    cli::cli_text("{.strong Processing Steps:}")
    cli::cli_text("├─ Cross-validation: {cv_folds}-fold parallel ({n_cv_cores} {backend_display} workers)")

  } else {

    # Sequential processing
    cli::cli_text("{.strong Processing Steps:}")
    cli::cli_text("├─ Cross-validation: {cv_folds}-fold sequential")

  }

  ## ---------------------------------------------------------------------------
  ## Step 11: Grid Search
  ## ---------------------------------------------------------------------------

  ## -------------------------------------------------------------------------
  ## Step 11.1: Generate Latin Hypercube Grid
  ## -------------------------------------------------------------------------

  # Generate Latin hypercube grid for better parameter space coverage
  grid_start_time <- Sys.time()
  cli::cli_text("├─ Grid search: Generating Latin hypercube design...")

  safely_execute(
    expr = {
      # Extract parameter set from workflow
      param_set <- workflows::extract_parameter_set_dials(wflow)

      # Finalize any unknown parameters (especially mtry for Random Forest and num_comp for PLSR)
      if (any(param_set$name %in% c("mtry", "num_comp"))) {
        # Prepare data for finalization (following main branch approach)
        recipe %>%
          recipes::prep() %>%
          recipes::bake(new_data = NULL) %>%
          dplyr::select(-Response) -> eval_data

        # Finalize parameter set with processed data
        param_set <- param_set %>%
          dials::finalize(eval_data)
      }

      # Generate Latin hypercube design
      dials::grid_latin_hypercube(
        param_set,
        size = grid_size
      )
    },
    default_value = NULL,
    error_message = glue::glue("Failed to generate parameter grid")
  ) ->
  grid_result

  if (is.null(grid_result$result)) {

    # Fall back to auto-grid if Latin hypercube fails
    cli::cli_alert_warning("Latin hypercube failed, using auto-grid")
    grid <- grid_size  # Let tune auto-generate

  } else {

    grid <- grid_result$result

  }

  ## -------------------------------------------------------------------------
  ## Step 11.2: Run Grid Search
  ## -------------------------------------------------------------------------

  # Run grid search with RRMSE as primary metric

  grid_start_time <- Sys.time()

  safely_execute(
    expr = {
      tune::tune_grid(
        object    = wflow,
        resamples = cv_splits,
        grid      = grid,
        metrics   = yardstick::metric_set(rrmse, yardstick::rmse, yardstick::rsq, yardstick::mae, rpd, ccc),
        control   = tune::control_grid(
          save_pred     = FALSE,      # Save memory
          save_workflow = FALSE,      # Save memory
          verbose       = FALSE,
          extract       = NULL,
          allow_par     = parallel_cv,  # Match our parallel flag
          parallel_over = "resamples"    # Parallelize over CV folds
        )
      )
    },
    default_value = NULL,
    error_message = glue::glue("Grid search failed for {workflow_id}")
  ) ->
  grid_tune_result

  if (is.null(grid_tune_result$result)) {

    actual_error <- grid_tune_result$error$message %||% "Unknown error"

    return(
      create_failed_result(
        config_id     = config_id,
        config_clean  = config_clean,
        error_message = glue::glue("Grid tuning failed: {actual_error}"),
        workflow_id   = workflow_id,
        error_detail  = grid_tune_result$error,
        error_stage   = "grid_tuning",
        error_trace   = grid_tune_result$trace,
        warnings      = grid_tune_result$warnings,
        messages      = grid_tune_result$messages
      )
    )

  }

  grid_results <- grid_tune_result$result
  grid_seconds <- as.numeric(difftime(Sys.time(), grid_start_time, units = "secs"))

  if (!is.null(grid_results)) {
    cli::cli_text("│  └─ Grid search complete: {round(grid_seconds, 1)}s")
  }

  # Aggressive memory cleanup after grid search
  invisible(gc(verbose = FALSE, full = TRUE))

  ## ---------------------------------------------------------------------------
  ## Step 12: Prune Grid Results (Optional)
  ## ---------------------------------------------------------------------------

  skip_bayesian <- FALSE

  if (prune_models) {

    baseline_rmse <- sd(train_data[[variable]], na.rm = TRUE)
    baseline_mean <- mean(train_data[[variable]], na.rm = TRUE)


    if (abs(baseline_mean) < .Machine$double.eps) {

      cli::cli_alert_warning("Response mean too close to zero, skipping pruning")
      skip_bayesian <- FALSE

    } else {

      baseline_rrmse <- (baseline_rmse / abs(baseline_mean)) * 100


      tune::collect_metrics(grid_results) %>%
        dplyr::filter(.metric == "rrmse") %>%
        dplyr::summarise(min_rrmse = min(mean, na.rm = TRUE)) %>%
        dplyr::pull(min_rrmse) ->
      best_rrmse


      threshold_rrmse <- baseline_rrmse * prune_threshold

      if (is.infinite(best_rrmse) || best_rrmse > threshold_rrmse) {

        skip_bayesian <- TRUE
        cli::cli_text("├─ Pruning check: RRMSE {round(best_rrmse, 1)}% > {round(threshold_rrmse, 1)}% threshold")
        cli::cli_text("│  └─ {.emph Skipping Bayesian optimization}")

      }

    }

  }

  ## ---------------------------------------------------------------------------
  ## Step 13: Bayesian Optimization
  ## ---------------------------------------------------------------------------

  bayes_seconds <- 0

  if (!skip_bayesian && bayesian_iter > 0) {

    bayes_start_time <- Sys.time()
    cli::cli_text("├─ Bayesian optimization: Running {bayesian_iter} iterations...")

    safely_execute(
      expr = {
        # Get finalized parameter set for Bayesian optimization
        param_set <- workflows::extract_parameter_set_dials(wflow)

        # Finalize any unknown parameters using the same logic as grid generation
        if (any(param_set$name %in% c("mtry", "num_comp"))) {
          # Prepare data for finalization (following main branch approach)
          recipe %>%
            recipes::prep() %>%
            recipes::bake(new_data = NULL) %>%
            dplyr::select(-Response) -> eval_data

          # Finalize parameter set with processed data
          param_set <- param_set %>%
            dials::finalize(eval_data)
        }

        tune::tune_bayes(
          object    = wflow,
          resamples = cv_splits,
          initial   = grid_results,  # Start from grid search results
          iter      = bayesian_iter,
          param_info = param_set,    # Provide finalized parameter info
          metrics   = yardstick::metric_set(rrmse, yardstick::rmse, yardstick::rsq, yardstick::mae, rpd, ccc),
          control   = tune::control_bayes(
            save_pred     = FALSE,
            save_workflow = FALSE,
            verbose       = FALSE,
            no_improve    = BAYES_NO_IMPROVE_LIMIT,  # Stop after N iterations without improvement
            allow_par     = parallel_cv,
            parallel_over = "resamples"
          )
        )
      },
      default_value = NULL,
      error_message = glue::glue("Bayesian optimization failed for {workflow_id}")
    ) ->
    bayes_tune_result

    if (is.null(bayes_tune_result$result)) {

      # If Bayesian fails, keep grid results but log the warning
      cli::cli_alert_warning("Bayesian optimization failed, using grid results")
      if (bayes_tune_result$n_warnings > 0) {
        cli::cli_alert_info("Warnings during Bayesian optimization: {bayes_tune_result$warnings[[1]]}")
      }
      final_tune_results <- grid_results
      # Store Bayesian failure info for potential logging later
      bayes_error_info <- list(
        error = bayes_tune_result$error,
        warnings = bayes_tune_result$warnings
      )

    } else {

      final_tune_results <- bayes_tune_result$result
      bayes_error_info <- NULL

    }

    bayes_seconds <- as.numeric(difftime(Sys.time(), bayes_start_time, units = "secs"))
    cli::cli_text("│  └─ Bayesian optimization complete: {round(bayes_seconds, 1)}s")

  } else {

    # Use grid results if Bayesian was skipped
    final_tune_results <- grid_results

  }

  ## ---------------------------------------------------------------------------
  ## Step 14: Final Model Fit
  ## ---------------------------------------------------------------------------

  ## -------------------------------------------------------------------------
  ## Step 14.1: Select Best Hyperparameters
  ## -------------------------------------------------------------------------

  safely_execute(
    expr = {
      tune::select_best(
        final_tune_results,
        metric = "rrmse"
      )
    },
    default_value = NULL,
    error_message = glue::glue("Failed to select best parameters")
  ) ->
  best_params_result

  if (is.null(best_params_result$result)) {

    actual_error <- best_params_result$error$message %||% "Unknown error"

    return(
      create_failed_result(
        config_id     = config_id,
        config_clean  = config_clean,
        error_message = glue::glue("Parameter selection failed: {actual_error}"),
        workflow_id   = workflow_id
      )
    )

  }

  best_params <- best_params_result$result

  ## -------------------------------------------------------------------------
  ## Step 14.2: Finalize Workflow with Best Parameters
  ## -------------------------------------------------------------------------

  safely_execute(
    expr = {
      tune::finalize_workflow(wflow, best_params)
    },
    default_value = NULL,
    error_message = glue::glue("Failed to finalize workflow")
  ) ->
  final_wflow_result

  if (is.null(final_wflow_result$result)) {

    actual_error <- final_wflow_result$error$message %||% "Unknown error"

    return(
      create_failed_result(
        config_id     = config_id,
        config_clean  = config_clean,
        error_message = glue::glue("Workflow finalization failed: {actual_error}"),
        workflow_id   = workflow_id
      )
    )

  }

  final_wflow <- final_wflow_result$result

  ## ---------------------------------------------------------------------------
  ## Step 15: Test Set Evaluation
  ## ---------------------------------------------------------------------------

  # Fit on full training set and evaluate on test set

  safely_execute(
    expr = {
      tune::last_fit(
        final_wflow,
        split   = data_split,
        metrics = yardstick::metric_set(rrmse, yardstick::rmse, yardstick::rsq, yardstick::mae, rpd, ccc)
      )
    },
    default_value = NULL,
    error_message = glue::glue("Failed to fit final model")
  ) ->
  final_fit_result

  if (is.null(final_fit_result$result)) {

    actual_error <- final_fit_result$error$message %||% "Unknown error"

    return(
      create_failed_result(
        config_id     = config_id,
        config_clean  = config_clean,
        error_message = glue::glue("Final model fitting failed: {actual_error}"),
        workflow_id   = workflow_id,
        error_detail  = final_fit_result$error,
        error_stage   = "final_fitting",
        error_trace   = final_fit_result$trace,
        warnings      = final_fit_result$warnings,
        messages      = final_fit_result$messages
      )
    )

  }

  final_fit <- final_fit_result$result

  ## ---------------------------------------------------------------------------
  ## Step 16: Extract Metrics and Return Results
  ## ---------------------------------------------------------------------------

  # For transformed responses, we need to back-transform predictions and recalculate metrics
  # Extract predictions first

  tune::collect_predictions(final_fit) ->
  test_predictions

  # Back-transform if needed
  if (config_clean$transformation != "none") {

    test_predictions$.pred <- back_transform_predictions(
      test_predictions$.pred,
      config_clean$transformation,
      warn = FALSE
    )

  }

  # Calculate metrics on original scale
  compute_original_scale_metrics(
    truth = test_predictions[[variable]],
    estimate = test_predictions$.pred,
    metrics = yardstick::metric_set(rrmse, yardstick::rmse, yardstick::rsq, yardstick::mae, rpd, ccc)
  ) %>%
    tidyr::pivot_wider(names_from = .metric, values_from = .estimate) ->
  test_metrics

  # Create result row

  tibble::tibble(config_id         = config_id,
                 workflow_id       = workflow_id,
                 model             = config_clean$model,
                 transformation    = config_clean$transformation,
                 preprocessing     = config_clean$preprocessing,
                 feature_selection = config_clean$feature_selection,
                 covariates        = if (!is.null(config_clean$covariates)) {
                                       paste(config_clean$covariates, collapse = "-")
                                     } else {
                                       NA_character_
                                     },
                 # Best parameters from tuning (stored as list column)
                 best_params       = list(best_params),
                 # Metrics (now accessing from wide format)
                 rsq               = test_metrics$rsq %||% NA_real_,
                 rmse              = test_metrics$rmse %||% NA_real_,
                 rrmse             = test_metrics$rrmse %||% NA_real_,
                 rpd               = test_metrics$rpd %||% NA_real_,
                 ccc               = test_metrics$ccc %||% NA_real_,
                 mae               = test_metrics$mae %||% NA_real_,
                 # Timing
                 grid_seconds      = grid_seconds,
                 bayes_seconds     = bayes_seconds,
                 total_seconds     = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
                 # Status
                 status            = if (skip_bayesian) "pruned" else "success",
                 error_message     = NA_character_,
                 # Error tracking fields (NA for successful models)
                 error_stage       = NA_character_,
                 error_class       = NA_character_,
                 has_trace         = FALSE,
                 # Warning/message tracking (currently placeholders)
                 n_warnings        = 0L,  # TODO: Aggregate warnings from all stages
                 warning_summary   = NA_character_
                 # Note: fitted_workflow removed to prevent memory leak
                 # Use fit_final_models() function separately for model stacking
  )
}
