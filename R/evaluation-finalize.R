#' Finalize Top Models with Bayesian Optimization
#'
#' @description
#' Refines the top-performing models from evaluation results using Bayesian optimization
#' with warm-start from initial grid search results. Generates cross-validation predictions
#' needed for ensemble stacking.
#'
#' The function:
#' 1. Selects top N models based on the specified metric
#' 2. Creates parameter search spaces around best parameters
#' 3. Performs Bayesian optimization to refine hyperparameters
#' 4. Generates CV predictions for ensemble stacking
#' 5. Saves finalized workflows and results
#'
#' @param evaluation_results A tibble from `evaluate_models_local()` or `evaluate_models_hpc()`
#'   containing columns: `status`, `workflow_id`, `best_params`, and the specified metric
#' @param input_data Data frame with predictor features and response variable
#' @param covariate_data Optional data frame with additional covariate predictors. Default: `NULL`
#' @param variable Character string. Name of the response variable column in `input_data`
#' @param n_best Integer. Number of top models to finalize. Default: `10`
#' @param metric Character string. Metric for selecting top models:
#'   - `"rrmse"`: Relative RMSE (default)
#'   - `"rmse"`: Root mean squared error
#'   - `"ccc"`: Concordance correlation coefficient
#'   - `"rsq"`: R-squared
#'   - `"rpd"`: Ratio of performance to deviation
#'   - `"mae"`: Mean absolute error
#' @param output_dir Character string or `NULL`. Directory to save finalized workflows.
#'   If `NULL`, uses a temporary directory. Default: `NULL`
#' @param train_prop Numeric between 0 and 1. Proportion of data for training. Default: `0.8`
#' @param bayesian_iter Integer. Number of Bayesian optimization iterations. Default: `15`
#' @param cv_folds Integer. Number of cross-validation folds for generating predictions. Default: `10`
#' @param seed Integer. Random seed for reproducibility. Default: `0307`
#' @param allow_par Logical. Enable parallel processing for cross-validation. Default: `FALSE`
#' @param n_cores Integer or `NULL`. Number of cores for parallel processing.
#'   If `NULL` and `allow_par = TRUE`, uses available cores minus 1. Default: `NULL`
#' @param verbose Logical. Print detailed progress information. Default: `TRUE`
#'
#' @return
#' A tibble containing finalized model information with columns:
#' - `wflow_id`: Model identifier
#' - `workflow`: Finalized workflow object
#' - `cv_predictions`: Cross-validation predictions for stacking
#' - `metrics`: Performance metrics from CV
#' - `best_params`: Final optimized hyperparameters
#' - `optimization_time`: Time taken for optimization (minutes)
#' - `path`: Path to saved workflow file
#' - Original columns from input (model type, preprocessing, etc.)
#'
#' @examples
#' \dontrun{
#' # Finalize top 5 models based on RMSE
#' finalized <- finalize_top_workflows(
#'   evaluation_results = model_results,
#'   input_data = spectral_data,
#'   variable = "SOC",
#'   n_best = 5,
#'   metric = "rmse",
#'   bayesian_iter = 20,
#'   verbose = TRUE
#' )
#'
#' # Access finalized workflows
#' finalized$workflow[[1]]  # First workflow
#' finalized$cv_predictions[[1]]  # CV predictions for stacking
#' }
#'
#' @export

finalize_top_workflows <- function(evaluation_results,
                                   input_data,
                                   covariate_data = NULL,
                                   variable,
                                   n_best         = 10,
                                   metric         = "rrmse",
                                   output_dir     = NULL,
                                   train_prop     = 0.8,
                                   bayesian_iter  = 15,
                                   cv_folds       = 10,
                                   seed           = 0307,
                                   allow_par      = FALSE,
                                   n_workers      = NULL,
                                   verbose        = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Preflight checks
  ## ---------------------------------------------------------------------------

    ## -------------------------------------------------------------------------
    ## Step 0.1: Input Validation
    ## -------------------------------------------------------------------------

    ## Check out the evaluation results ----------------------------------------

    if (!is.data.frame(evaluation_results)) cli::cli_abort("{.arg evaluation_results} must be a data frame")

    if (nrow(evaluation_results) == 0) cli::cli_abort("{.arg evaluation_results} is empty")

    if (!"workflow_id" %in% names(evaluation_results)) cli::cli_abort("Need {.var workflow_id} column to identify models")

    if (!"best_params" %in% names(evaluation_results)) cli::cli_abort("Need {.var best_params} column for optimization")

    if (!"status" %in% names(evaluation_results)) cli::cli_abort("Need {.var status} column from evaluation functions")

    if (!metric %in% names(evaluation_results)) {

      intersect(c("rrmse",
                  "ccc",
                  "rsq",
                  "rpd",
                  "rmse",
                  "mae"),
                names(evaluation_results)) -> available_metrics

      if (length(available_metrics) > 0) {

        cli::cli_abort(c("Metric {.val {metric}} not found in evaluation results",
                         "i" = "Available metrics: {.val {available_metrics}}"))

      } else {

        cli::cli_abort("Selected metric {.val {metric}} not found and no alternative metrics available")

      }
    }


    ## Check input_data --------------------------------------------------------

    if (!is.data.frame(input_data)) cli::cli_abort("{.arg input_data} must be a data frame")

    if (!variable %in% names(input_data)) cli::cli_abort("Variable {.var {variable}} not found in {.arg input_data}")

    ## Check numeric parameters ------------------------------------------------

    if (n_best < 1) cli::cli_abort("{.arg n_best} must be at least {.val 1}")

    if (bayesian_iter < 0) cli::cli_abort("{.arg bayesian_iter} must be non-negative")

    if (cv_folds < 2) cli::cli_abort("{.arg cv_folds} must be at least {.val 2}")

    if (train_prop <= 0 || train_prop >= 1) cli::cli_abort("{.arg train_prop} must be between {.val 0} and {.val 1}")

    ## -------------------------------------------------------------------------
    ## Step 0.2: Setup the environment
    ## -------------------------------------------------------------------------

    #$ Setup output directory --------------------------------------------------

    if (is.null(output_dir)) output_dir <- file.path(tempdir(), paste0("finalized_", format(Sys.time(), "%Y%m%d_%H%M%S")))

    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    ## Setup parallel backend for CV if requested ------------------------------

    if (allow_par) {

      ## Determine cores -------------------------------------------------------

      n_cores <- min(n_workers, parallel::detectCores() - 1)

      ## Set up the plan -------------------------------------------------------

      future::plan(future::multisession, workers = n_cores)

      # Ensure restoration on exit ---------------------------------------------

      on.exit({

        future::plan(future::sequential)
        gc(verbose = FALSE, full = TRUE)

        }, add = TRUE)

    }

    ## Start timing --------------------------------------------------------------

    start_time <- Sys.time()

    ## -------------------------------------------------------------------------
    ## Step 0.3: Verbose output
    ## -------------------------------------------------------------------------

    if (verbose) {

      cli::cli_text("{.strong Finalizing top {n_best} models from {nrow(evaluation_results)} candidates}")
      cli::cli_text("├─ Selection metric: {.field {metric}}")
      cli::cli_text("├─ Optimization: {.val {bayesian_iter}} Bayesian iterations")
      cli::cli_text("├─ Cross-validation: {.val {cv_folds}} folds")
      cli::cli_text("├─ Train/test split: {.val {round(train_prop * 100, 0)}}% / {.val {round((1-train_prop) * 100, 0)}}%")
      cli::cli_text("├─ Parallel processing: {.field {ifelse(allow_par, paste0('enabled (',n_cores, ' workers)'), 'disabled')}}")
      cli::cli_text("└─ Output directory: {.path {output_dir}}")
      cli::cli_text("")

    }

  ## ---------------------------------------------------------------------------
  ## Step 1: Select (America's Next) Top Models
  ## ---------------------------------------------------------------------------

  ## Filter for successful models with valid metrics ---------------------------

  evaluation_results %>%
    dplyr::filter(status == "success",
                  !is.na(!!sym(metric)),
                  !is.null(best_params),
                  lengths(best_params) > 0) -> successful_models

  if (nrow(successful_models) == 0) cli::cli_abort("No successful models with valid {.field {metric}} found")

  ## Determine sort direction based on metric ----------------------------------

  lower_is_better <- metric %in% c("rrmse", "rmse", "mae")

  ## Select top N models -------------------------------------------------------

  if (lower_is_better) {

    successful_models %>%
      dplyr::arrange(!!sym(metric)) %>%
      dplyr::slice_head(n = n_best) -> top_models

    } else {

    successful_models %>%
      dplyr::arrange(dplyr::desc(!!sym(metric))) %>%
      dplyr::slice_head(n = n_best) -> top_models

    }

  ## Warn if fewer models available than requested -----------------------------

  if (nrow(top_models) < n_best) cli::cli_alert_warning("Only {nrow(top_models)} models available (requested {n_best})")

  ## Verbose output ------------------------------------------------------------

  if (verbose) {

    cli::cli_text("")
    cli::cli_text("{.strong Selected {nrow(top_models)} models by {.field {metric}}}")

    ## Show metric range -------------------------------------------------------

    if (lower_is_better) {

      min(top_models[[metric]]) -> best_metric
      max(top_models[[metric]]) -> worst_metric

    } else {

      max(top_models[[metric]]) -> best_metric
      min(top_models[[metric]]) -> worst_metric

    }

    cli::cli_text("├─ Best {.field {metric}}: {.val {round(best_metric, 3)}}")
    cli::cli_text("├─ Cutoff {.field {metric}}: {.val {round(worst_metric, 3)}}")
    cli::cli_text("└─ Top models:")

    ## Show selected models (nested under the tree) ---------------------------

    for (i in seq_len(min(5, nrow(top_models)))) {

      top_models$workflow_id[i]         -> model_name
      round(top_models[[metric]][i], 3) -> model_metric

      ## Determine if this is the last visible model --------------------------

      (i == min(5, nrow(top_models))) && (nrow(top_models) <= 5) -> is_last

      if (is_last) {

        cli::cli_text("   └─ {.val {model_name}} ({.field {metric}}: {.val {model_metric}})")

      } else {

        cli::cli_text("   ├─ {.val {model_name}} ({.field {metric}}: {.val {model_metric}})")

      }
    }

    if (nrow(top_models) > 5) {

      cli::cli_text("   └─ ... and {.val {nrow(top_models) - 5}} more")

    }

    cli::cli_text("")

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Data Preparation
  ## ---------------------------------------------------------------------------

  ## Rename response variable for consistency -------------------------------

  input_data %>%
    dplyr::rename(Response = !!rlang::sym(variable)) -> input_data_clean

  ## Create train/test split -------------------------------------------------

  set.seed(seed)

  rsample::initial_split(input_data_clean,
                        prop = train_prop,
                        strata = Response) -> data_split

  train_data <- rsample::training(data_split)
  test_data  <- rsample::testing(data_split)

  ## Create CV folds ---------------------------------------------------------

  set.seed(seed + 1)

  rsample::vfold_cv(train_data,
                    v = cv_folds,
                    strata = Response) -> cv_resamples

  ## Verbose output ----------------------------------------------------------

  if (verbose) {

    response_range <- range(train_data$Response, na.rm = TRUE)
    response_sd    <- sd(train_data$Response, na.rm = TRUE)

    cli::cli_text("")
    cli::cli_text("{.strong Data preparation complete}")
    cli::cli_text("├─ Training samples: {.val {nrow(train_data)}}")
    cli::cli_text("├─ Test samples: {.val {nrow(test_data)}}")
    cli::cli_text("├─ CV folds: {.val {cv_folds}}")
    cli::cli_text("├─ Response range: [{.val {round(response_range[1], 2)}}, {.val {round(response_range[2], 2)}}]")
    cli::cli_text("└─ Response SD: {.val {round(response_sd, 2)}}")
    cli::cli_text("")

  }

  ## Clean up ----------------------------------------------------------------

  rm(input_data_clean)
  gc(verbose = FALSE, full = TRUE)

  ## ---------------------------------------------------------------------------
  ## Step 3: Process the models one by one and finalize.
  ## ---------------------------------------------------------------------------

  ## Initialize results storage -----------------------------------------------

  finalized_results <- vector("list", nrow(top_models))

  ## Process each model -------------------------------------------------------

  for (i in seq_len(nrow(top_models))) {

    model_start_time <- Sys.time()
    current_model    <- top_models[i, ]

    if (verbose) {

      cli::cli_text("")
      cli::cli_text("{.strong Processing model {i}/{nrow(top_models)}: {.val {current_model$workflow_id}}}")
      cli::cli_text("├─ Building recipe and workflow...")

    }

    ## -------------------------------------------------------------------------
    ## Step 3.1: Build workflow recipe
    ## -------------------------------------------------------------------------

    # Parse covariate string: evaluation results store as "ph-MAP-AI"
    # but build_recipe expects c("ph", "MAP", "AI")
    if (!is.null(current_model$covariates) &&
        current_model$covariates != "" &&
        !current_model$covariates %in% c("NA", "No Covariates")) {
      covariate_list <- strsplit(current_model$covariates, "-")[[1]]
    } else {
      covariate_list <- NULL
    }

    build_recipe(input_data               = train_data,
                 response_transformation  = current_model$transformation,
                 spectral_transformation  = current_model$preprocessing,
                 feature_selection_method = current_model$feature_selection,
                 covariate_selection      = covariate_list,
                 covariate_data           = covariate_data) -> recipe

    ## -------------------------------------------------------------------------
    ## Step 3.2: Build out the model specification
    ## -------------------------------------------------------------------------

    define_model_specifications(current_model$model) -> model_spec

    ## -------------------------------------------------------------------------
    ## Step 3.3: Create worfklow
    ## -------------------------------------------------------------------------

    workflows::workflow() %>%
      workflows::add_recipe(recipe) %>%
      workflows::add_model(model_spec) -> workflow

    if (verbose) cli::cli_text("│  └─ Workflow ready.")

    ## -------------------------------------------------------------------------
    ## Step 3.4: Extract and prepare parameters
    ## -------------------------------------------------------------------------

    ## Validate best_params structure ------------------------------------------

    if (is.null(current_model$best_params[[1]]) || !is.data.frame(current_model$best_params[[1]]) || nrow(current_model$best_params[[1]]) == 0) {

      cli::cli_alert_danger("Invalid parameter structure for model {i}, skipping")

      list(workflow_id = current_model$workflow_id,
           status      = "failed",
           reason      = "invalid_parameters") -> finalized_results[[i]]

      next

    }

    ## Extract parameter set and finalize if needed (just mtry tbh) ------------

    param_set <- hardhat::extract_parameter_set_dials(workflow)

    if ("mtry" %in% param_set$name) {

      recipe %>%
        recipes::prep() %>%
        recipes::bake(new_data = NULL) %>%
        dplyr::select(-Response) -> finalize_data

      param_set %>%
        dials::finalize(finalize_data) -> param_set

      rm(finalize_data)
      gc(verbose = FALSE, full = TRUE)

    }

    ## -------------------------------------------------------------------------
    ## Step 3.5: Bayesian optimization with warm start
    ## -------------------------------------------------------------------------

    ## Extract best parameters from evaluation --------------------------------

    current_model$best_params[[1]] -> best_params

    ## Create grid around best parameters for warm-start ----------------------

    if (!is.null(best_params) && nrow(best_params) > 0) {

      param_set$name -> param_names
      best_params[, param_names, drop = FALSE] -> best_params

      ## Build exploration grid around current best ---------------------------

      purrr::map(param_names, function(param_name) {

        best_value <- best_params[[param_name]]
        param_row  <- param_set[param_set$name == param_name, ]
        param_obj  <- param_row$object[[1]]

        if (!inherits(param_obj, "quant_param")) return(best_value)

        # Get the valid range --------------------------------------------------

        lower <- param_obj$range$lower
        upper <- param_obj$range$upper

        if (param_obj$type == "integer") {

          seq(best_value - 2, best_value + 2, by = 1) %>%
            purrr::keep(~ .x >= lower & .x <= upper) %>%
            unique()

        } else {

          c(0.8, 0.9, 1.0, 1.1, 1.2) %>%
            purrr::map_dbl(~ best_value * .x) %>%
            purrr::keep(~ .x >= lower & .x <= upper)

        }

      }) %>% purrr::set_names(param_names) -> grid_values

      expand.grid(grid_values, stringsAsFactors = FALSE) -> initial_grid

      # PLSR protection: num_comp must be >= 2 to avoid dimension issues --------

      if (current_model$model == "plsr" && "num_comp" %in% names(initial_grid)) {

        initial_grid %>%
          dplyr::filter(num_comp >= 2) -> initial_grid

        # If filtering removes all rows, create a safe default grid
        if (nrow(initial_grid) == 0) {
          initial_grid <- data.frame(num_comp = c(2, 3, 4, 5))
        }

      }

      # Limit to 25 points max -------------------------------------------------

      if (nrow(initial_grid) > 25) {

        dplyr::slice_sample(initial_grid, n = 25) -> initial_grid

      }

    } else {

      initial_grid <- NULL

    }

    ## Run optimization with warm-start ---------------------------------------

    if (verbose) cli::cli_text("├─ Running optimization...")

    if (!is.null(initial_grid) && nrow(initial_grid) > 0) {

      if (verbose) cli::cli_text("│  ├─ Warm-start grid: {.val {nrow(initial_grid)}} points")

      tune::tune_grid(object    = workflow,
                      resamples = cv_resamples,
                      grid      = initial_grid,
                      metrics   = yardstick::metric_set(rrmse, yardstick::rsq, yardstick::rmse, ccc, rpd, yardstick::mae),
                      control   = tune::control_grid(save_pred     = FALSE,
                                                     save_workflow = FALSE,
                                                     verbose       = FALSE,
                                                     allow_par     = allow_par)) -> initial_results

    } else {

      initial_results <- 5

    }

    ## -------------------------------------------------------------------------
    ## Step 3.6: Bayesian optimization with a warm start.
    ## -------------------------------------------------------------------------

    if (verbose) cli::cli_text("│  ├─ Bayesian optimization: {.val {bayesian_iter}} iterations")

    tune::tune_bayes(object     = workflow,
                     resamples  = cv_resamples,
                     initial    = initial_results,
                     iter       = bayesian_iter,
                     metrics    = yardstick::metric_set(rrmse, yardstick::rsq, yardstick::rmse, ccc, rpd, yardstick::mae),
                     param_info = param_set,
                     control    = tune::control_bayes(save_pred     = FALSE,
                                                      save_workflow = FALSE,
                                                      verbose       = FALSE,
                                                      no_improve    = 5L,
                                                      allow_par     = allow_par,
                                                      seed          = seed)) -> bayes_results

    tune::select_best(bayes_results, metric = metric) -> final_best_params

    if (verbose) cli::cli_text("│  └─ Bayesian optimization complete.")

    ## -------------------------------------------------------------------------
    ## Step 3.7: Finalize workflow with best parameters
    ## -------------------------------------------------------------------------

    workflow %>%
      tune::finalize_workflow(final_best_params) -> finalized_workflow

    ## -------------------------------------------------------------------------
    ## Step 3.8: Generate CV predictions for stacking
    ## -------------------------------------------------------------------------

    if (verbose) cli::cli_text("├─ Generating CV predictions for stacking...")

    tune::fit_resamples(finalized_workflow,
                        resamples = cv_resamples,
                        metrics   = yardstick::metric_set(rrmse, yardstick::rsq, yardstick::rmse, ccc, rpd, yardstick::mae),
                        control   = tune::control_resamples(save_pred     = TRUE,
                                                            save_workflow = TRUE,
                                                            allow_par     = allow_par)) -> cv_fit

    ## Back-transform CV predictions if needed --------------------------------

    if (current_model$transformation != "none" && current_model$transformation != "No Transformation") {

      back_transform_cv_predictions(cv_fit, current_model$transformation) -> cv_fit

    }

    ## Store results for stacking ----------------------------------------------

    list(workflow_id  = current_model$workflow_id,
         workflow     = finalized_workflow,
         cv_results   = cv_fit,
         final_params = final_best_params,
         status       = "success") -> finalized_results[[i]]

    if (verbose) {

      model_time <- round(as.numeric(difftime(Sys.time(), model_start_time, units = "mins")), 1)

      cli::cli_text("│  └─ CV predictions complete")
      cli::cli_text("└─ Complete in {.val {model_time}} minutes")
      cli::cli_text("")

    }

    ## Clean up memory after each model ---------------------------------------

    rm(recipe, model_spec, workflow, param_set, finalized_workflow, cv_fit, bayes_results, final_best_params)

    if (exists("initial_grid")) rm(initial_grid)
    if (exists("initial_results")) rm(initial_results)

    gc(verbose = FALSE, full = TRUE)

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Save and return results
  ## ---------------------------------------------------------------------------

  ## Filter successful results ------------------------------------------------

  purrr::keep(finalized_results, ~ .x$status == "success") -> successful_results

  if (length(successful_results) == 0) cli::cli_abort("No models successfully finalized")

  ## Create summary tibble for return -----------------------------------------

  tibble::tibble(wflow_id       = purrr::map_chr(successful_results, "workflow_id"),
                 workflow       = purrr::map(successful_results, "workflow"),
                 cv_predictions = purrr::map(successful_results, "cv_results"),
                 final_params   = purrr::map(successful_results, "final_params")) %>%
    dplyr::mutate(metrics = purrr::map(cv_predictions, ~ tune::collect_metrics(.x))) -> results_tibble

  ## Save to disk for persistence ---------------------------------------------

  file.path(output_dir, "finalized") -> finalized_dir

  if (!fs::dir_exists(finalized_dir)) fs::dir_create(finalized_dir)

  file.path(finalized_dir,
            paste0("finalized_models_",
                   format(Sys.time(), "%Y%m%d_%H%M%S"),
                   ".qs")) -> save_path

  qs::qsave(results_tibble, save_path)

  ## Calculate final metrics --------------------------------------------------

  results_tibble %>%
    tidyr::unnest(metrics) %>%
    dplyr::filter(.metric == metric) %>%
    dplyr::select(wflow_id, mean, std_err) -> final_metrics

  ## Final summary ------------------------------------------------------------

  difftime(Sys.time(), start_time, units = "mins") -> total_time

  if (verbose) {

    cli::cli_text("{.strong Finalization complete}")
    cli::cli_text("├─ Models finalized: {.val {nrow(results_tibble)}}")
    cli::cli_text("├─ Total time: {.val {round(total_time, 1)}} minutes")
    cli::cli_text("├─ Average per model: {.val {round(total_time/nrow(results_tibble), 1)}} minutes")
    cli::cli_text("└─ Saved to: {.path {save_path}}")

    ## Show final performance --------------------------------------------------

    if (lower_is_better) {
      which.min(final_metrics$mean) -> best_idx
      which.max(final_metrics$mean) -> worst_idx
    } else {
      which.max(final_metrics$mean) -> best_idx
      which.min(final_metrics$mean) -> worst_idx
    }

    cli::cli_text("{.strong Final {.field {metric}} performance:}")
    cli::cli_text("├─ Best: {.val {final_metrics$wflow_id[best_idx]}} = {.val {round(final_metrics$mean[best_idx], 3)}} ± {.val {round(final_metrics$std_err[best_idx], 3)}}")
    cli::cli_text("└─ Worst: {.val {final_metrics$wflow_id[worst_idx]}} = {.val {round(final_metrics$mean[worst_idx], 3)}} ± {.val {round(final_metrics$std_err[worst_idx], 3)}}")
    cli::cli_text("")

  }

  ## Final cleanup ------------------------------------------------------------

  gc(verbose = FALSE, full = TRUE)

  return(results_tibble)
}
