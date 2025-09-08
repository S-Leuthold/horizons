#' Fit a Cubist Model Using Similarity-Selected Training Data
#'
#' Builds, tunes, and evaluates a Cubist model to predict a single soil covariate
#' using similarity-selected OSSL training data. The function applies a max entropy 
#' grid search followed by Bayesian optimization to tune hyperparameters, then fits 
#' the final model and returns both performance metrics and workflow objects.
#'
#' @param train_data A `tibble` containing PCA-transformed predictors (`Dim.1`, `Dim.2`, etc.)
#'   and the target covariate column. Should be pre-selected similar samples.
#' @param val_data A `tibble` with validation data (same structure as train_data)
#'   for computing unbiased performance metrics.
#' @param covariate A character string. Name of the target covariate (e.g., `"clay"`, `"ph"`).
#' @param verbose Logical. If `TRUE`, prints progress messages. Defaults to `FALSE`.
#' @param parallel Logical. Enable parallel processing for tuning. Defaults to `FALSE`.
#' @param n_workers Integer. Number of parallel workers. If `NULL`, uses safe defaults.
#' @param allow_nested Logical. Allow parallel processing in nested context. Defaults to `FALSE`.
#' @param bayesian_iter Integer. Number of Bayesian optimization iterations (default: 10).
#'
#' @return A named `list` with the following components:
#' \itemize{
#'   \item \strong{fitted_workflow}: A fitted Cubist workflow trained on train_data.
#'   \item \strong{best_params}: A `tibble` with the best hyperparameter configuration.
#'   \item \strong{validation_metrics}: A `tibble` of metrics computed on val_data.
#'   \item \strong{train_metrics}: A `tibble` of metrics computed on train_data.
#' }
#'
#' @details
#' The modeling pipeline follows three main stages:
#' \enumerate{
#'   \item Stratified data split into training/testing sets.
#'   \item Max entropy grid search for tuning `committees`, `neighbors`, and `max_rules` using `tune::tune_grid()`.
#'   \item Bayesian optimization using `tune::tune_bayes()` for refinement.
#' }
#' The model is finalized with `tune::finalize_workflow()` and fitted to the training set.
#' Performance metrics are computed on the hold-out set using `tune::last_fit()` and `soilspec::eval()`.
#'
#' Parallel tuning is enabled with `future::plan(multisession)` and automatically reset afterward.
#' All error handling is wrapped with `safely_execute()` for fault-tolerant orchestration.
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(
#'   Dim.1 = rnorm(100),
#'   Dim.2 = rnorm(100),
#'   Dim.3 = rnorm(100),
#'   Sand  = runif(100, 50, 80)
#' )
#'
#' result <- fit_cubist_model(input_data = df, covariate = "Sand", verbose = TRUE)
#' result$Evaluation
#' }
#'
#' @seealso
#' \code{\link{predict_soil_covariates}}, \code{\link{perform_pca_on_ossl}}
#'
#' @importFrom dplyr select rename mutate bind_rows starts_with
#' @importFrom purrr map
#' @importFrom tidyr drop_na
#' @importFrom tibble tibble as_tibble
#' @importFrom stats quantile predict
#' @importFrom rsample initial_split training testing vfold_cv
#' @importFrom recipes recipe
#' @importFrom dials grid_space_filling neighbors
#' @importFrom rules committees max_rules
#' @importFrom parsnip cubist_rules set_engine set_mode fit
#' @importFrom workflows workflow add_model add_recipe
#' @importFrom tune tune_grid tune_bayes control_grid control_bayes select_best finalize_workflow last_fit collect_predictions
#' @importFrom future plan multisession sequential
#' @importFrom glue glue
#' @importFrom cli cli_progress_step cli_alert_danger cli_alert_warning
#' @export


fit_cubist_model <- function(train_data,
                             val_data,
                             covariate,
                             verbose = FALSE,
                             parallel = FALSE,
                             n_workers = NULL,
                             allow_nested = FALSE,
                             bayesian_iter = 10) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Data validation and preparation
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_progress_step("Preparing training data for covariate {.val {covariate}}")
  }

  # Validate inputs
  if (!covariate %in% names(train_data)) {
    cli::cli_abort("Covariate '{covariate}' not found in train_data")
  }
  
  if (!covariate %in% names(val_data)) {
    cli::cli_abort("Covariate '{covariate}' not found in val_data")
  }

  # Prepare training data with response variable
  train_data %>%
    dplyr::mutate(Response = .data[[covariate]]) %>%
    dplyr::select(Response, dplyr::starts_with("Dim.")) %>%
    tidyr::drop_na() -> Train_Data

  # Prepare validation data with response variable  
  val_data %>%
    dplyr::mutate(Response = .data[[covariate]]) %>%
    dplyr::select(Response, dplyr::starts_with("Dim.")) %>%
    tidyr::drop_na() -> Val_Data

  if (nrow(Train_Data) < 20) {
    cli::cli_abort("Insufficient training data for {covariate}: {nrow(Train_Data)} samples (minimum 20)")
  }
  
  if (nrow(Val_Data) < 10) {
    cli::cli_abort("Insufficient validation data for {covariate}: {nrow(Val_Data)} samples (minimum 10)")
  }

  ## ---------------------------------------------------------------------------
  ## Step 1: Cross-validation setup
  ## ---------------------------------------------------------------------------

  set.seed(0307)
  
  # Use fewer CV folds for faster grid search (5 instead of 10)
  n_cv_folds <- ifelse(nrow(Train_Data) > 500, 5, 3)

  safely_execute(
    expr = {
      rsample::vfold_cv(Train_Data, v = n_cv_folds, strata = "Response")
    },
    default_value = NULL,
    error_message = "Failed to create CV folds for {covariate}"
  ) -> CV_Folds_safe

  CV_Folds <- CV_Folds_safe$result

  if (is.null(CV_Folds)) {
    return(NULL)
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Define and Tune Cubist Model
  ## ---------------------------------------------------------------------------

    ## ---------------------------------------------------------------------------
    ## Stage 1: Create tidymodels recipe (minimal - PCA already normalized)
    ## ---------------------------------------------------------------------------
    
    # Simple recipe - PCA scores are already normalized
    # Create minimal recipe to avoid large object embedding
    formula_obj <- stats::as.formula("Response ~ .")
    model_recipe <- recipes::recipe(formula_obj, data = Train_Data[1, ])
    
    ## ---------------------------------------------------------------------------
    ## Stage 2: Define cubist model specifications
    ## ---------------------------------------------------------------------------

    model_spec <- parsnip::cubist_rules(
      committees = tune::tune(),
      neighbors  = tune::tune(),
      max_rules  = tune::tune()
    ) %>%
      parsnip::set_engine("Cubist") %>%
      parsnip::set_mode("regression")

    # Create workflow with recipe
    wf <- workflows::workflow() %>%
      workflows::add_model(model_spec) %>%
      workflows::add_recipe(model_recipe)

    ## ---------------------------------------------------------------------------
    ## Stage 2: Initial Grid Search for Hyperparameter Tuning
    ## ---------------------------------------------------------------------------

    ## Step 2.1: Configure parallel processing with safety controls -----------

    # Determine safe worker count
    if (is.null(n_workers)) {
      max_cores <- parallel::detectCores(logical = TRUE)
      n_workers <- pmax(1, pmin(max_cores - 1, 10))  # Cap at 10 for safety
    }

    # Check for nested parallelization using number of workers (simple and reliable)
    current_workers <- future::nbrOfWorkers()
    if (!allow_nested && current_workers > 1) {
      if(verbose) cli::cli_alert_warning("Nested parallelization detected ({current_workers} workers active). Setting parallel=FALSE for safety")
      parallel <- FALSE
    }

    # Set parallel plan with context-aware backend
    if (parallel && n_workers > 1) {
      # Use context-aware parallel setup
      old_plan <- safely_execute(
        expr = setup_parallel_backend(
          n_workers = n_workers,
          force_backend = NULL,  # Auto-detect
          memory_limit_gb = 2,   # 2GB for spectroscopy
          enable_work_stealing = FALSE,  # Not needed for grid tuning
          verbose = FALSE
        ),
        default_value = future::plan(),  # Fallback to current plan
        error_message = glue::glue("Failed to set parallel plan for {covariate} tuning")
      )$result
      
      # Ensure cleanup on exit
      old_mc_cores <- getOption("mc.cores", 1L)
      on.exit({
        restore_parallel_settings(old_plan, verbose = FALSE)
        options(mc.cores = old_mc_cores)
        optimize_parallel_memory(force_gc = TRUE, verbose = FALSE)
      }, add = TRUE)
      
      # Temporarily override mc.cores to match n_workers
      options(mc.cores = n_workers)
      
      if(verbose) {
        backend_display <- get_backend_display()
        cli::cli_text(format_tree_item(paste0("ℹ Using ", backend_display, " processing for ", covariate, " (", n_workers, " workers)"), level = 1, is_last = FALSE))
      }
    } else {
      if(verbose) cli::cli_text(format_tree_item(paste0("ℹ Using sequential processing for ", covariate, " (parallel=", parallel, ", n_workers=", n_workers, ")"), level = 1, is_last = FALSE))
    }

    # Reduce grid size for faster search (6 points instead of 10)
    grid_size <- ifelse(bayesian_iter > 0, 6, 10)  # Smaller grid if using Bayesian
    
    dials::grid_space_filling(rules::committees(range = c(5L, 15L)),  # Moderate range
                              dials::neighbors(range = c(0L, 5L)),   # Allow 0 for no smoothing
                              dials::max_rules(range = c(25L, 75L)), # Reasonable range
                              size = grid_size,
                              type = "max_entropy") -> grid

    if(verbose) cli::cli_progress_step("Running grid search for {.val {covariate}}")

    safely_execute(expr          = {tune::tune_grid(object    = wf,
                                   resamples = CV_Folds,
                                   grid      = grid,
                                   control   = tune::control_grid(allow_par = TRUE, 
                                                                   save_pred = FALSE,  # Don't save predictions to save memory
                                                                   extract = NULL))},
                   default_value = NULL,
                   error_message = glue::glue("Grid tuning faliled for {covariate}")) -> grid_res_safe

    grid_res <- grid_res_safe$result

    if(is.null(grid_res)){
      safely_execute(expr = {future::plan(sequential)})
      return(NULL)
    }

    ## ---------------------------------------------------------------------------
    ## Stage 3: Optional Bayesian Optimization for Hyperparameter Tuning
    ## ---------------------------------------------------------------------------

    if (bayesian_iter > 0) {
      if(verbose) {
        opt_text <- paste0("Bayesian optimization: ", bayesian_iter, " iterations")
        cli::cli_text(format_tree_item(opt_text, level = 1, is_last = FALSE))
      }

      safely_execute(expr        = {tune::tune_bayes(object    = wf,
                                                     resamples = CV_Folds,
                                                     initial   = grid_res,
                                                     iter      = bayesian_iter,
                                                     control   = tune::control_bayes(allow_par = TRUE,
                                                                                      save_pred = FALSE,
                                                                                      extract = NULL))},
                     default_value = NULL,
                     error_message = glue::glue("Bayesian tuning failed for {covariate}")) -> bayes_res_safe

      bayes_res <- bayes_res_safe$result

      if(is.null(bayes_res)){
        safely_execute(expr = {future::plan(sequential)})
        return(NULL)
      }
      
      # Use Bayesian results
      final_tune_result <- bayes_res
    } else {
      if(verbose) {
        cli::cli_text(format_tree_item("Skipping Bayesian optimization (using grid search only)", level = 1, is_last = FALSE))
      }
      # Use grid search results only
      final_tune_result <- grid_res
    }

    ## ---------------------------------------------------------------------------
    ## Stage 4: Finalizing workflow
    ## ---------------------------------------------------------------------------

    cli::cli_progress_step("Finalizing workflow for {.val {covariate}}")

    best_params <- tune::select_best(final_tune_result,
                                     metric = "rmse")

    final_wf    <- tune::finalize_workflow(wf,
                                           best_params)

  ## ---------------------------------------------------------------------------
  ## Step 3: Final Model Fitting
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_progress_step("Fitting final model for {.val {covariate}}")
  }

  safely_execute(
    expr = {
      parsnip::fit(final_wf, Train_Data)
    },
    default_value = NULL,
    error_message = "Failed to fit final workflow for {covariate}"
  ) -> fitted_model_safe

  fitted_model <- fitted_model_safe$result

  if (is.null(fitted_model)) {
    return(NULL)
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Validation Metrics
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_progress_step("Computing validation metrics for {.val {covariate}}")
  }

  safely_execute(
    expr = {
      # Predict on validation set
      val_predictions <- stats::predict(fitted_model, Val_Data)
      
      # Calculate validation metrics
      val_metrics <- tibble::tibble(
        observed = Val_Data$Response,
        predicted = val_predictions$.pred
      ) %>%
        tidyr::drop_na() %>%
        {
          # Use horizons custom metrics if available, otherwise basic metrics
          if (requireNamespace("soilspec", quietly = TRUE)) {
            soilspec::eval(pred = .$predicted, obs = .$observed, obj = "quant")
          } else {
            # Fallback to basic metrics
            rmse_val <- sqrt(mean((.$observed - .$predicted)^2))
            r2_val <- cor(.$observed, .$predicted)^2
            tibble::tibble(
              rmse = rmse_val,
              r2 = r2_val
            )
          }
        }
    },
    default_value = NULL,
    error_message = "Failed to compute validation metrics for {covariate}"
  ) -> val_metrics_safe

  ## ---------------------------------------------------------------------------
  ## Step 5: Training Set Metrics (for comparison)
  ## ---------------------------------------------------------------------------

  safely_execute(
    expr = {
      # Predict on training set
      train_predictions <- stats::predict(fitted_model, Train_Data)
      
      # Calculate training metrics
      train_metrics <- tibble::tibble(
        observed = Train_Data$Response,
        predicted = train_predictions$.pred
      ) %>%
        tidyr::drop_na() %>%
        {
          if (requireNamespace("soilspec", quietly = TRUE)) {
            soilspec::eval(pred = .$predicted, obs = .$observed, obj = "quant")
          } else {
            # Fallback to basic metrics
            rmse_val <- sqrt(mean((.$observed - .$predicted)^2))
            r2_val <- cor(.$observed, .$predicted)^2
            tibble::tibble(
              rmse = rmse_val,
              r2 = r2_val
            )
          }
        }
    },
    default_value = NULL,
    error_message = "Failed to compute training metrics for {covariate}"
  ) -> train_metrics_safe

  val_metrics <- val_metrics_safe$result
  train_metrics <- train_metrics_safe$result

  if (is.null(val_metrics) || is.null(train_metrics)) {
    return(NULL)
  }

  if (verbose) {
    best_params_text <- paste0("Best params: ", 
      "committees=", round(best_params$committees),
      ", neighbors=", round(best_params$neighbors))
    cli::cli_text(format_tree_item(best_params_text, level = 1, is_last = TRUE))
  }

  return(list(
    fitted_workflow = fitted_model,
    best_params = best_params,
    validation_metrics = val_metrics,
    train_metrics = train_metrics,
    # Add diagnostic data
    train_data = Train_Data,
    val_data = Val_Data
  ))

}
