#' @title Build Model Stack
#' @description Builds a stacked ensemble from top workflows by refitting on training data,
#' blending predictions, and optionally evaluating on holdout data.
#'
#' @param top_workflows A `workflow_set` tibble with tuned models (e.g., from `filter_workflows()`).
#' @param training_data A data frame for retraining the top models.
#' @param holdout_data Optional. A test/holdout set for evaluating final stack performance.
#' @param blend_args Optional. A list of arguments to pass to `blend_predictions()`.
#' @param verbose Logical. Print progress updates (default = TRUE).
#'
#' @return A list with:
#'   - `stack`: Fitted stacks object
#'   - `blend_weights`: Tibble of blend weights per member model
#'   - `model_summary`: Optional tibble of model IDs and initial metrics
#'   - `holdout_eval`: Optional predictions and metrics on the holdout set
#'
#' @export
 
build_model_stack <- function(top_workflows,
                              training_data,
                              holdout_data = NULL,
                              blend_args   = list(),
                              verbose      = TRUE) {
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Refit Models on Training Data
  ## ---------------------------------------------------------------------------
  
  if (verbose) cli::cli_h2("▸ Refitting top models on training data")
  
  top_workflows %>%
    dplyr::mutate(best_params = purrr::map(result, tune::select_best, metric = "rrmse"),
                  workflow    = purrr::map(info, ~ .x$workflow[[1]]),
                  final_wf    = purrr::map2(workflow, best_params, tune::finalize_workflow),
                  fitted_wf   = purrr::map(final_wf, ~ parsnip::fit(.x, data = training_data))) -> finalize_wfs
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Initialize Stacks and Add Candidates
  ## ---------------------------------------------------------------------------
  
  if (verbose) cli::cli_h2("▸ Adding candidates to stack")
  
  ensemble_stack <- stacks::stacks()
  
  for (i in seq_len(nrow(finalized_wfs))) {
    model_name <- finalized_wfs$wflow_id[i]
    tuned_res  <- finalized_wfs$result[[i]]
    
    stacks::add_candidates(ensemble_stack, 
                           tuned_res, 
                           name = model_name) -> ensemble_stack
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Blend Predictions
  ## ---------------------------------------------------------------------------
  
  if (verbose) cli::cli_h2("▸ Blending predictions")
  
  ensemble_stack <- do.call(stacks::blend_predictions,
                            c(list(data_stack = ensemble_stack), 
                              blend_args))
  
  blend_weights <- stacks::collect_parameters(ensemble_stack)
  
  ## ---------------------------------------------------------------------------
  ## Step 4: Fit Ensemble Members
  ## ---------------------------------------------------------------------------
  
  if (verbose) cli::cli_h2("▸ Fitting ensemble members")
  
  ensemble_stack <- stacks::fit_members(ensemble_stack)
  
  ## ---------------------------------------------------------------------------
  ## Step 5: Optional Holdout Evaluation
  ## ---------------------------------------------------------------------------
  
  holdout_eval <- NULL
  
  if (!is.null(holdout_data)) {
    if (verbose) cli::cli_h2("▸ Predicting on holdout set")
    
    predict(object  = ensemble_stack, 
            newdata = holdout_data) %>%
      dplyr::bind_cols(holdout_data) -> preds
    
    yardstick::metric_set(yardstick::rmse, 
                          yardstick::rsq)(data = preds,
                                          truth = Response,  # assumes renamed upstream
                                          estimate = .pred) -> metrics
    
    holdout_eval <- list(predictions = preds, metrics = metrics)
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 6: Return Full Stack
  ## ---------------------------------------------------------------------------
  
  finalized_wfs %>%
    dplyr::select(wflow_id, result) %>%
    dplyr::mutate(metrics = purrr::map(result, tune::collect_metrics)) %>%
    tidyr::unnest(metrics) %>%
    dplyr::filter(.metric %in% c("rrmse", "rsq")) -> model_summary
  
  return(list(stack         = ensemble_stack,
              blend_weights = blend_weights,
              model_summary = model_summary,
              holdout_eval  = holdout_eval))
}
