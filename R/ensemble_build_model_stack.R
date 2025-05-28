#' Build, Blend, and Evaluate a Stacked Ensemble from Top Workflows
#'
#' This function performs end-to-end ensemble modeling using a set of pre-defined workflows.
#' It splits the input dataset into training and testing subsets, performs stratified cross-validation
#' with Bayesian hyperparameter tuning for each workflow, refits each finalized model on resamples,
#' adds them to a stack, blends the stack using penalized regression, and evaluates predictions
#' on the holdout test set.
#'
#' The function assumes that all workflows in \code{top_workflows} are complete and contain
#' both preprocessing and model specification steps. The response variable is renamed internally
#' to \code{Response} for compatibility.
#'
#' @param top_workflows A tibble containing the candidate workflows. Must include columns:
#'   \code{wflow_id} (character) and \code{workflow} (list-column of \code{workflow()} objects).
#' @param input_data A \code{data.frame} or \code{tibble} containing the full dataset, including predictors and the outcome variable.
#' @param variable Character. The name of the outcome variable (unquoted) to use for stratified resampling and evaluation.
#' @param test_prop Numeric. Proportion of the data to allocate to the holdout test set (default = \code{0.2}).
#' @param cv_folds Integer. Number of cross-validation folds used for resampling during tuning (default = \code{5}).
#'
#' @return A named list with the following elements:
#' \itemize{
#'   \item \code{model_stack}: A fitted \code{stacks} ensemble model.
#'   \item \code{predictions}: A tibble with observed and predicted values on the test set.
#'   \item \code{evaluation_metrics}: A tibble of holdout performance metrics (RMSE, R², MAE, rRMSE).
#'   \item \code{tuned_models}: A tibble of models successfully tuned and refit on CV resamples.
#' }
#'
#' @importFrom dplyr mutate filter rename select bind_cols
#' @importFrom purrr map2 map_lgl
#' @importFrom rsample initial_split training testing vfold_cv
#' @importFrom stacks stacks add_candidates blend_predictions fit_members collect_parameters
#' @importFrom tune tune_bayes control_bayes select_best finalize_workflow fit_resamples control_resamples
#' @importFrom cli cli_abort cli_warn cli_alert_success cli_progress_step cli_inform
#' @importFrom yardstick rmse rsq mae metric_set metrics
#' @importFrom stats predict
#'
#' @examples
#' \dontrun{
#' model_results <- build_ensemble_stack(
#'   top_workflows = filtered_workflows,
#'   input_data    = my_data,
#'   variable      = "SOC_pct"
#' )
#'
#' model_results$evaluation_metrics
#' model_results$predictions
#' }
#'
#' @usage
#' build_ensemble_stack(
#'   top_workflows,
#'   input_data,
#'   variable,
#'   test_prop = 0.2,
#'   cv_folds = 5
#' )
#'
#' @seealso
#'   \code{\link[stacks]{stacks}}, \code{\link[stacks]{add_candidates}},
#'   \code{\link[stacks]{blend_predictions}}, \code{\link[stacks]{fit_members}},
#'   \code{\link[tune]{tune_bayes}}, \code{\link[tune]{fit_resamples}},
#'   \code{\link[yardstick]{rmse}}, \code{\link[rsample]{initial_split}}
#'
#' @export

build_ensemble_stack <- function(top_workflows,
                                 input_data,
                                 variable,
                                 test_prop = 0.2,
                                 cv_folds  = 5){

  ## ---------------------------------------------------------------------------
  ## Step 0: Validate updates
  ## ---------------------------------------------------------------------------

  if (!"wflow_id" %in% colnames(top_workflows) || !"workflow" %in% colnames(top_workflows)) {

     cli::cli_abort("Input `top_workflows` must contain columns {.val wflow_id} and {.val workflow}.")

  }

  if (!variable %in% names(input_data)) {

     cli::cli_abort("Variable {.val {variable}} not found in training data.")

    }

  ## ---------------------------------------------------------------------------
  ## Step 1: Set up tuning infrastructe.
  ## ---------------------------------------------------------------------------

    ## -------------------------------------------------------------------------
    ## Stage 1: Clean up and split the data
    ## -------------------------------------------------------------------------

    cli::cli_progress_step("Creating {.val {cv_folds}}-fold CV resamples stratified by {.val {variable}}")

    input_data %>%
      rename(Response = !!sym(variable)) -> input_data

    split_data   <- rsample::initial_split(data   = input_data,
                                           prop   = 1 - test_prop,
                                           strata = "Response")

    training_data <- rsample::training(split_data)
    testing_data  <- rsample::testing(split_data)

    resamples     <- rsample::vfold_cv(data   = training_data,
                                       v      = cv_folds,
                                       strata = "Response")

    ## -------------------------------------------------------------------------
    ## Stage 2: Create the rule sets for tuning.
    ## -------------------------------------------------------------------------

    stack_controls <- tune::control_bayes(verbose       = FALSE,
                                          save_pred     = TRUE,
                                          save_workflow = TRUE,
                                          no_improve    = 10L,
                                          seed          = 0307,
                                          uncertain     = 5)

    refit_controls <- tune::control_resamples(save_workflow = TRUE,
                                              save_pred     = TRUE)

  ## ---------------------------------------------------------------------------
  ## Step 2: Tune the models.
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Tuning top workflows with {.fun tune_bayes}")

  top_workflows %>%
    dplyr::mutate(results = purrr::map2(.x = workflow,
                                        .y = wflow_id,
                                        .f =  ~ tryCatch({

                                          tuned <- tune::tune_bayes(object    = .x,
                                                                    resamples = resamples,
                                                                    iter      = 20,
                                                                    control   = stack_controls,
                                                                    metrics   = yardstick::metric_set(yardstick::rmse, yardstick::rsq, rrmse))

                                          best_params <- tune::select_best(tuned, metric = "rmse")

                                          finalized_wf <- tune::finalize_workflow(.x, best_params)

                                          tune::fit_resamples(object    = finalized_wf,
                                                              resamples = resamples,
                                                              control   = refit_controls)

                                          }, error = function(e) {

                                            cli::cli_warn("Model failed to tune: {.emph {e$message}}")
                                            NULL

                                        })
    )) %>%
    dplyr::filter(!purrr::map_lgl(results, is.null)) -> tuned_models

    ## -------------------------------------------------------------------------

    if (nrow(tuned_models) == 0) {

      cli::cli_abort("All models failed to tune. Cannot build stack.")

    }

  ## ---------------------------------------------------------------------------
  ## Step 3: Stack and blend the models.
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Building model stack from {nrow(tuned_models)} candidates")

  model_stack <- stacks::stacks()

  for(i in seq_len(nrow(tuned_models))){

    suppressMessages({

      model_stack <- stacks::add_candidates(data_stack = model_stack,
                                            candidates = tuned_models$results[[i]],
                                            name       = tuned_models$wflow_id[[i]])
    })
  }

  cli::cli_progress_step("Blending and fitting stacked ensemble")

    model_stack %>%
      stacks::blend_predictions(metric       = yardstick::metric_set(rrmse),
                                mixture      = 0.5,
                                penalty      = 0.0001,
                                non_negative = FALSE) %>%
      stacks::fit_members() -> model_stack

  cli::cli_alert_success("Stacked ensemble built successfully with {.val {nrow(tuned_models)}} models.")

  ## ---------------------------------------------------------------------------
  ## Step 4: Predict on hold out data
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Predicting on hold out data")

  predict(object   = model_stack,
          new_data = testing_data) %>%
    dplyr::rename(Predicted = .pred) %>%
    dplyr::mutate(Observed = testing_data$Response) %>%
    dplyr::select(Observed, Predicted, everything()) -> predictions

  yardstick::metric_set(yardstick::rmse, yardstick::rsq, yardstick::mae, rrmse)

  yardstick::metrics(data     = predictions,
                     truth    = Observed,
                     estimate = Predicted) %>%
    dplyr::mutate(model = "Stacked Ensemble") -> metrics


  ## ---------------------------------------------------------------------------
  ## Step 5: Return output
  ## ---------------------------------------------------------------------------

  return(list(model_stack        = model_stack,
              predictions        = predictions,
              evaluation_metrics = metrics,
              tuned_models       = tuned_models))

}
