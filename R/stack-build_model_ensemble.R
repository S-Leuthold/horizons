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

build_ensemble_stack <- function(results_dir,
                                 input_data,
                                 variable,
                                 filter_metric = "rsq",
                                 n_best        = 10,
                                 test_prop     = 0.2,
                                 cv_folds      = 10,
                                 verbose       = TRUE){

  ## ---------------------------------------------------------------------------
  ## Step 0: Validate updates
  ## ---------------------------------------------------------------------------

  if (!variable %in% names(input_data)) {

     cli::cli_abort("Variable {.val {variable}} not found in training data.")

    }

  ## ---------------------------------------------------------------------------
  ## Step 1: Read model evaluation results
  ## ---------------------------------------------------------------------------

  safely_execute(expr = {fs::dir_ls(path   = results_dir,
                                    regexp = "refit_summary.*\\.qs$") %>%
                          .[1] -> batch_summary_path

                         qs::qread(batch_summary_path)},
                 default_value = NULL,
                 error_message = "Error loading batch results file -- check that batch_summary_...qs exists.") -> batch_summary_safe

  batch_summary <- batch_summary_safe$result

  if(is.null(batch_summary)){

    cli::cli_abort("Batch summary returned NULL-- check that file exists in results directory.")

  }

  if(verbose) cli::cli_alert_success("Successfully located and read batch summary results.")

  ## ---------------------------------------------------------------------------
  ## Step 2: Determine and read in top models
  ## ---------------------------------------------------------------------------

    ## ---------------------------------------------------------------------------
    ## Stage 1: Define a directionality for those chosen filter metric
    ## ---------------------------------------------------------------------------

    c(rmse   = "asc",
      mae    = "asc",
      rrmse  = "asc",
      rsq    = "desc",
      r2     = "desc",
      ccc    = "desc",
      rpiq   = "desc") -> metric_dir

    direction <- metric_dir[[filter_metric]]

    if (is.null(direction)) {
      cli::cli_alert_danger("Unknown filter metric: {.val {filter_metric}}. Please use a supported metric.")
      return(NULL)
    }

    ## ---------------------------------------------------------------------------
    ## Stage 2: Sort the model results based on the metric
    ## ---------------------------------------------------------------------------

    if (direction == "asc") {

      batch_summary %>%
        dplyr::arrange(.data[[filter_metric]]) %>%
        dplyr::slice_head(n = n_best)

      } else {

      batch_summary %>%
        dplyr::arrange(dplyr::desc(.data[[filter_metric]])) %>%
        dplyr::slice_head(n = n_best)

      } -> top_models

    if(verbose) cli::cli_alert_success("Top {.val {n_best}} models filtered by {.val {filter_metric}}.")

    ## ---------------------------------------------------------------------------
    ## Stage 3: Safely read the models into a list object.
    ## ---------------------------------------------------------------------------

    safely_execute(expr = {top_models %>%
                            dplyr::pull(saved_path) %>%
                            purrr::map(qs::qread) %>%
                            dplyr::bind_rows() %>%
                            purrr::pluck(., "tuned_models")},
                   default_value = NULL,
                   error_message = "Error loading the top models from disk") -> top_models_safe

    top_models <- top_models_safe$result

    if(is.null(top_models)){
      cli::cli_alert_danger("Error reading in model results from best performing models. Returning NULL.")
      return(NULL)
    }

    if(verbose){

      cli::cli_alert_success("Top model information successfully read from disk.")

      cli::cli_h1("Candidate Models for Ensemble")

      top_models %>%
        dplyr::mutate(Model        = dplyr::case_when(str_detect(wflow_id, "random_forest") ~ "Random Forest",
                                                      str_detect(wflow_id, "cubist")        ~ "Cubist",
                                                      str_detect(wflow_id, "xgboost")       ~ "XGBoost",
                                                      str_detect(wflow_id, "lightgbm")      ~ "LightGBM",
                                                      str_detect(wflow_id, "elastic_net")   ~ "Elastic Net",
                                                      str_detect(wflow_id, "svm_rbf")       ~ "SVM (RBF)",
                                                      str_detect(wflow_id, "mars")          ~ "MARS",
                                                      str_detect(wflow_id, "plsr")          ~ "PLSR",
                                                      str_detect(wflow_id, "mlp_nn")        ~ "MLP Neural Net",
                                                      TRUE ~ wflow_id),
                      Transformation = case_when(str_detect(wflow_id, "NoTrans") ~ "None",
                                                 str_detect(wflow_id, "Log")     ~ "Log",
                                                 str_detect(wflow_id, "Sqrt")    ~ "Square Root",
                                                 TRUE ~ NA_character_),
                      Preprocessing = case_when(str_detect(wflow_id, "snv_deriv2") ~ "SNV + Derivative 2",
                                                str_detect(wflow_id, "snv_deriv1") ~ "SNV + Derivative 1",
                                                str_detect(wflow_id, "msc_deriv1") ~ "MSC + Derivative 1",
                                                str_detect(wflow_id, "deriv2")     ~ "Derivative 2",
                                                str_detect(wflow_id, "deriv1")     ~ "Derivative 1",
                                                str_detect(wflow_id, "snv")        ~ "SNV",
                                                str_detect(wflow_id, "msc")        ~ "MSC",
                                                str_detect(wflow_id, "sg")         ~ "Savitzky-Golay",
                                                str_detect(wflow_id, "raw")        ~ "Raw",
                                                TRUE ~ NA_character_),
                      check      = stringr::str_count(wflow_id, "_"),
                      Covariates = case_when(check == 3 ~ stringr::str_split_i(wflow_id, "_", i = 4),
                                             check == 4 ~ stringr::str_split_i(wflow_id, "_", i = 5),
                                             check == 5 ~ stringr::str_split_i(wflow_id, "_", i = 6),
                                             check == 6 ~ stringr::str_split_i(wflow_id, "_", i = 7),
                                             check == 7 ~ stringr::str_split_i(wflow_id, "_", i = 8))) %>%
        dplyr::select(Model, Transformation, Preprocessing, Covariates) %>%
        dplyr::distinct() %>%
        print(., n = Inf)

        cli::cli_h1("")
      }

  ## ---------------------------------------------------------------------------
  ## Step 3: Set up tuning infrastructure
  ## ---------------------------------------------------------------------------

    ## -------------------------------------------------------------------------
    ## Stage 1: Clean up and split the data
    ## -------------------------------------------------------------------------

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

    if (verbose) cli::cli_alert_success("{.val {cv_folds}}-fold CV resamples stratified by {.val {variable}} successfully created.")

    ## -------------------------------------------------------------------------
    ## Stage 2: Finalize the workflows, and refit.
    ## -------------------------------------------------------------------------

    metrics_set <- yardstick::metric_set(rmse, rsq, rrmse)

    top_models %>%
      dplyr::mutate(best_params = purrr::map(result,~ tune::select_best(.x, metric = "rrmse")),
                    finalized_wf = purrr::map2(workflow, best_params, tune::finalize_workflow)) -> top_models

    if(verbose) cli::cli_alert_success("Best model parameters successfully extracted from top workflows.")

    if(verbose) pb <- cli::cli_progress_bar("Refitting models:", total = nrow(top_models))

    purrr::pmap(list(wf = top_models$finalized_wf,
                     id = top_models$wflow_id),
                function(wf, id) {

                  if (verbose) cli::cli_progress_update(id = pb, status = id)

                  suppressMessages({
                    safely_execute(expr = {tune::fit_resamples(object    = wf,
                                                               resamples = resamples,
                                                               control   = tune::control_resamples(save_pred     = TRUE,
                                                                                                   save_workflow = TRUE))},
                                   default_value = NULL,
                                   error_message = glue::glue("Refit failed for {id}"))
                    })
                  }
                ) -> stack_fit_safe

    if(verbose) cli::cli_progress_done(id = pb)

    if(verbose) cli::cli_alert_success("All models successfully refit on new crossvalidation folds.")


    purrr::pmap(list(wf = top_models$fitted_wf,
                     id = top_models$wflow_id),
                function(wf, id) {
                  safely_execute(expr = {predict(wf,
                                                 new_data = testing_data) %>%
                                  dplyr::rename(Predicted = .pred) %>%
                                  dplyr::mutate(Observed = testing_data$Response,
                                                Model    = id)  -> preds

                    if(stringr::str_detect(id, "Sqrt")) {
                      preds %>%
                        mutate(Predicted = Predicted^2) -> preds
                    }

                    if(stringr::str_detect(id, "Log")) {
                      preds %>%
                        mutate(Predicted = exp(Predicted)) -> preds
                    }

                    preds},
                                 default_value = NULL,
                                 error_message = glue::glue("Prediction failed for {id}"))
                  }
                ) -> candidate_predictions_safe

    candidate_predictions <- NULL

    for(i in 1:nrow(top_models)){

    candidate_predictions <- bind_rows(candidate_predictions,
                                       candidate_predictions_safe[[i]]$result)
    }

    candidate_predictions %>%
      dplyr::group_by(Model) %>%
      yardstick::metrics(truth = Observed, estimate = Predicted) %>%
      dplyr::filter(.metric %in% c("rmse", "rsq", "mae")) %>%
      tidyr::pivot_wider(names_from = .metric, values_from = .estimate) -> candidate_metrics

  ## ---------------------------------------------------------------------------
  ## Step 4: Stack the models.
  ## ---------------------------------------------------------------------------

  model_stack <- stacks::stacks()

  for(i in seq_len(nrow(top_models))){

    suppressMessages({

      model_stack <- stacks::add_candidates(data_stack = model_stack,
                                            candidates = stack_fit_safe[[i]]$result,
                                            name       = top_models$wflow_id[[i]])
    })
  }

  if(verbose) cli::cli_alert_success("Model stack built from {nrow(top_models)} candidates")

  ## ---------------------------------------------------------------------------
  ## Step 5: Tune the parameters for the blending and finalize
  ## ---------------------------------------------------------------------------

  tune_blend(model_stack = model_stack,
             test_data   = testing_data) -> best_params

  model_stack %>%
    stacks::blend_predictions(metric       = yardstick::metric_set(rrmse),
                              mixture      = best_params$mixture,
                              penalty      = best_params$penalty,
                              non_negative = best_params$non_negative) %>%
    stacks::fit_members() -> model_stack


  if(verbose) cli::cli_alert_success("Model stack blended and finalized.")

  ## ---------------------------------------------------------------------------
  ## Step 5: Predict on hold out data
  ## ---------------------------------------------------------------------------

  predict(object   = model_stack,
          new_data = testing_data) %>%
    dplyr::rename(Predicted = .pred) %>%
    dplyr::mutate(Observed = testing_data$Response) %>%
    dplyr::select(Observed, Predicted, everything()) -> predictions

  yardstick::metrics(data     = predictions,
                     truth    = Observed,
                     estimate = Predicted) %>%
    dplyr::mutate(model = "Stacked Ensemble") -> metrics

  metric_vals <- metrics %>%
    dplyr::filter(.metric %in% c("rmse", "rsq", "mae")) %>%
    dplyr::mutate(.estimate = round(.estimate, 3)) %>%
    tidyr::pivot_wider(names_from = .metric, values_from = .estimate)

  rmse_val  <- metric_vals$rmse
  rsq_val   <- metric_vals$rsq
  mae_val <- metric_vals$mae

  if(verbose) cli::cli_alert_success("Holdout data evaluated-- RMSE: {.val {rmse_val}}, R²: {.val {rsq_val}}, MAE: {.val {mae_val}}")

  ## ---------------------------------------------------------------------------
  ## Step 6: Return output
  ## ---------------------------------------------------------------------------

  return(list(model_stack           = model_stack,
              predictions           = predictions,
              evaluation_metrics    = metrics,
              tuned_models          = top_models,
              candidate_predictions = candidate_predictions,
              candidate_metrics     = candidate_metrics))

}
