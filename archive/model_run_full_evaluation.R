#' Run Full Ensemble Model Evaluation on Spectral Data
#'
#' Constructs and evaluates an ensemble of models to predict a soil property using
#' MIR spectra and optional covariates. The function performs model grid construction,
#' recipe creation, hyperparameter tuning (grid + Bayesian), model finalization,
#' and evaluation on a hold-out test set. Designed to support high-throughput model screening
#' and optimization in soil spectroscopy contexts.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom cli cli_alert_danger cli_progress_step cli_progress_done
#' @importFrom tune select_best control_grid finalize_workflow
#' @importFrom yardstick metric_set
#' @importFrom workflowsets workflow_map
#'
#' @param input_data A data frame containing columns `Sample_ID`, `Wavenumber`, `Absorbance`, and the response variable to predict.
#' @param models Character vector of model types to include (e.g., `"Cubist"`, `"PLSR"`, `"Random Forest"`).
#' @param transformations Character vector of response transformation labels. Options include:
#'   `"No Transformation"`, `"Log Transformation"`, `"Square Root Transformation"`, `"Box-Cox Transformation"`.
#' @param preprocessing Character vector of spectral preprocessing methods. Options include:
#'   `"No Preprocessing"`, `"Savitzky Golay - 0 Deriv"`, `"Savitzky Golay - 1 Deriv"`, and SNV variants.
#' @param variable Character string specifying the name of the response variable to model (must be present in `input_data`).
#' @param include_covariates Logical. If `TRUE`, includes additional covariates during modeling.
#' @param covariate_data Optional data frame of covariates (must include `Sample_ID` column). Required if `include_covariates = TRUE`.
#' @param expand_covariate_grid Logical. If `TRUE`, generates all possible covariate subsets for inclusion in the model grid.
#' @param grid_size Integer. Number of parameter combinations to evaluate per model in the initial grid search (default = 10).
#' @param bayesian_iter Integer. Number of Bayesian tuning iterations per model (default = 15).
#' @param cv_folds Integer. Number of cross-validation folds used in resampling (default = 5).
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{evaluation_results}{A tibble with hold-out performance metrics for each finalized model (`rsq`, `rmse`, `rrmse`).}
#'   \item{tuned_models}{A tibble of tuned workflows, including `final_wf`, `fitted_wf`, and back-transformed tuning results.}
#'   \item{training_data}{The internal training split used during model fitting.}
#'   \item{evaluation_data}{The held-out test set used to evaluate final models.}
#'}
#'
#' @details
#' This function integrates several modular components from the `tidymodels` and `workflowsets` ecosystems.
#' It supports flexible ensemble definitions and prioritizes interpretability of evaluation metrics.
#' The full modeling lifecycle is run automatically, including recipe generation, tuning,
#' and model fitting across a user-defined grid of modeling strategies.
#'
#' @section Model Types:
#' Supported models include:
#' \itemize{
#'   \item Partial Least Squares Regression (PLSR)
#'   \item Cubist
#'   \item Random Forest
#'   \item Support Vector Machine (SVM)
#'   \item Bagged Neural Network (BNN)
#' }
#'
#' @seealso
#' [build_model_grid()], [build_recipe()], [run_bayesian_tuning()], [evaluate_final_models()],
#' [workflowsets::workflow_set()], [tune::tune_grid()], [yardstick::metric_set()]
#'
#' @examples
#' \dontrun{
#' results <- full_model_evaluation(
#'   input_data            = my_spectra_data,
#'   models                = c("Cubist", "PLSR"),
#'   transformations       = c("No Transformation", "Log Transformation"),
#'   preprocessing         = c("Savitzky Golay - 0 Deriv", "SNV + SG1"),
#'   variable              = "SOC",
#'   include_covariates    = TRUE,
#'   covariate_data        = soil_covariates,
#'   expand_covariate_grid = FALSE,
#'   grid_size             = 10,
#'   bayesian_iter         = 15,
#'   cv_folds              = 5
#' )
#' }
#'
#' @export

full_model_evaluation <- function(input_data,
                                  models,
                                  transformations,
                                  preprocessing,
                                  variable,
                                  include_covariates    = FALSE,
                                  covariate_data        = NULL,
                                  expand_covariate_grid = FALSE,
                                  grid_size             = 10,
                                  bayesian_iter         = 15,
                                  cv_folds              = 5) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Data Preprocessing
  ## ---------------------------------------------------------------------------

  missing_vars <- setdiff(variable, colnames(input_data))

  if (length(missing_vars) > 0) {

    cli::cli_abort("Missing response vars: ", paste(missing_vars, collapse = ", "))

    }

  set.seed(0307)

  input_data      <- dplyr::rename(input_data, Response = !!rlang::sym(variable))
  data_split      <- rsample::initial_split(input_data, prop = 0.8, strata = Response)
  training_data   <- rsample::training(data_split)
  evaluation_data <- rsample::testing(data_split)
  folds           <- rsample::vfold_cv(training_data, v = cv_folds)

  ## ---------------------------------------------------------------------------
  ## Step 2: Build model grid
  ## ---------------------------------------------------------------------------

  tryCatch({
    model_grid <- build_model_grid(models                = models,
                                   transformations       = transformations,
                                   preprocessing         = preprocessing,
                                   variable              = variable,
                                   include_covariates    = include_covariates,
                                   covariate_data        = covariate_data,
                                   expand_covariate_grid = expand_covariate_grid)
    }, error = function(e) {

      cli::cli_alert_danger("Grid construction failed: {e$message}")
      stop("Terminating.")

    })

  cli::cli_alert_success("Model grid built: {nrow(model_grid)} rows")

  ## ---------------------------------------------------------------------------
  ## Step 3: Set up Workflow Sets
  ## ---------------------------------------------------------------------------

  ##

  tryCatch({
    wf_grid <- tibble::tibble(
      model_spec = purrr::map(model_grid$Model, define_model_specifications),
      recipe     = purrr::pmap(list(model_grid$Transformation,
                                    model_grid$Preprocessing,
                                    model_grid$Covariates),
                               ~ build_recipe(input_data              = training_data,
                                              response_transformation = ..1,
                                              spectral_transformation = ..2,
                                              covariate_selection     = ..3,
                                              covariate_data          = covariate_data)))
    }, error = function(e) {

      cli::cli_alert_danger("Workflow grid error: {e$message}")
      stop("Terminating.")

    })

  ##

  tryCatch({
    wf_set <- workflowsets::workflow_set(preproc = wf_grid$recipe,
                                         models  = wf_grid$model_spec,
                                         cross   = FALSE)
    }, error = function(e) {

      cli::cli_alert_danger("workflow_set failed: {e$message}")
      stop("Terminating.")

    })

  ##

  tryCatch({

    wf_set <- dplyr::mutate(wf_set,
                            wflow_id = clean_workflow_id(model          = model_grid$Model,
                                                         transformation = model_grid$Transformation,
                                                         preprocessing  = model_grid$Preprocessing,
                                                         covariates     = model_grid$Covariates
                                                         ))
    }, error = function(e) {

      cli::cli_alert_danger("Failed to assign wflow_id: {e$message}")
      stop("Terminating.")

    })

  ## ---------------------------------------------------------------------------
  ## Step 4: Initial Grid Tuning (Stable, mem hog)
  ## ---------------------------------------------------------------------------
  #
  #
  #   control_grid <- tune::control_grid(save_pred     = TRUE,
  #                                      allow_par     = FALSE,
  #                                      save_workflow = FALSE,
  #                                      verbose       = FALSE,
  #                                      parallel_over = "everything")
  #
  #   cli::cli_alert_info("Starting initial grid search: {.val {grid_size * nrow(wf_set) * cv_folds}} models to be evaluated.")
  #
  #   tryCatch({
  #     workflowsets::workflow_map(object    = wf_set,
  #                                fn        = "tune_grid",
  #                                resamples = folds,
  #                                metrics   = yardstick::metric_set(rrmse, rsq),
  #                                grid      = grid_size,
  #                                control   = control_grid) -> initial_tune_results
  #
  #     }, error = function(e) {
  #
  #     cli::cli_alert_danger("Initial tuning failed: {e$message}")
  #     stop("Terminating.")
  #
  #     })
  #
  #   cli::cli_alert_success("Initial tuning complete.")
  #

  ## ---------------------------------------------------------------------------
  ## Step 4: Initial Grid Tuning (Memory Efficient)
  ## ---------------------------------------------------------------------------

  control_grid <- tune::control_grid(
    save_pred     = FALSE,
    save_workflow = FALSE,
    verbose       = FALSE,
    allow_par     = FALSE,
    parallel_over = "resamples"
  )

  cli::cli_alert_info("Starting initial grid search on {.val {nrow(wf_set)}} workflows ({.val {grid_size}} x {.val {cv_folds}} resamples each)")

  wf_ids  <- wf_set$wflow_id
  wf_objs <- purrr::map(wf_set$info, ~ .x$workflow[[1]])

  initial_tune_results <- purrr::pmap_dfr(
    .l = list(wflow = wf_objs, id = wf_ids),
    .f = function(wflow, id) {
      cli::cli_progress_step("Tuning model {.val {id}} ({.val {which(wf_ids == id)}} of {.val {length(wf_ids)}})")

      tryCatch({
        tuned <- tune::tune_grid(
          object    = wflow,
          resamples = folds,
          grid      = grid_size,
          metrics   = yardstick::metric_set(rrmse, rsq),
          control   = control_grid
        )

        tibble::tibble(
          wflow_id = id,
          info     = list(tibble::tibble(workflow = list(wflow))),
          option   = list(attr(tuned, "option") %||% list()),
          result   = list(tuned)
        )
      }, error = function(e) {
        cli::cli_alert_warning("Tuning failed for {.val {id}}: {conditionMessage(e)}")
        tibble::tibble(
          wflow_id = id,
          info     = list(tibble::tibble(workflow = list(wflow))),
          option   = list(list()),
          result   = list(NULL)
        )
      })
    }
  )

  # Assign class and rank column for workflow_set compatibility
  class(initial_tune_results) <- c("workflow_set", class(initial_tune_results))
  initial_tune_results$rank <- NA_integer_

  ## ---------------------------------------------------------------------------
  ## Step 5: Filter + Bayesian Tuning
  ## ---------------------------------------------------------------------------

  halfway_tuned  <- filter_workflows(wf_set_tuned = initial_tune_results)
  wf_set_updated <- halfway_tuned$passed_workflows

  cli::cli_alert_info("{nrow(wf_set_updated)} workflows passed filter.")

  cli::cli_alert_info("Starting initial grid search: {.val {grid_size * nrow(wf_set_updated) * cv_folds}} models to be evaluated.")

  tryCatch({
    run_bayesian_tuning(tuned_wf_set = wf_set_updated,
                        folds        = folds,
                        iterations   = bayesian_iter,
                        parallel     = FALSE) -> tuned_models

  }, error = function(e) {

    cli::cli_alert_danger("Bayesian tuning failed: {e$message}")
    stop("Terminating.")

  })

  cli::cli_alert_success("Bayesian tuning complete.")

  ## ---------------------------------------------------------------------------
  ## Step 6: Finalize Workflows
  ## ---------------------------------------------------------------------------

  cli::cli_alert_info("Finalizing tuned models.")

  print(tuned_models)

  tuned_models %>%
    dplyr::mutate(best_model = purrr::map(result, tune::select_best, metric = "rrmse"),
                  workflow   = purrr::map(info, ~ .x$workflow[[1]]),
                  final_wf   = purrr::map2(workflow, best_model, tune::finalize_workflow),
                  fitted_wf  = purrr::map(final_wf, ~ parsnip::fit(.x, data = training_data))) -> tuned_models

  cli::cli_alert_success("Finalization complete.")

  ## ---------------------------------------------------------------------------
  ## Step 7: Holdout Evaluation
  ## ---------------------------------------------------------------------------

  cli::cli_alert_info("Evaluating models on holdout data.")

  evaluation_results <- evaluate_final_models(
    finalized_wf_sets = tuned_models,
    holdout_data      = evaluation_data
  )

  cli::cli_progress_step("Model evaluation complete. Minimum RRMSE = {round(min(evaluation_results$rrmse), 3)}%, Maximum RRMSE = {round(max(evaluation_results$rrmse), 3)}%")
  cli::cli_progress_done()

  return(list(
    evaluation_results = evaluation_results,
    tuned_models       = tuned_models,
    training_data      = training_data,
    evaluation_data    = evaluation_data
  ))
}



