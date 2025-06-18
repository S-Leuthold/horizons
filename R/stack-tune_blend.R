#' Tune Blending Parameters for Ensemble Stack
#'
#' This helper function performs a grid search over blending hyperparameters
#' for an ensemble stack built using `stacks::blend_predictions()`. It explores
#' combinations of penalty strength, mixture (ridge-lasso balance), and
#' non-negativity constraints to identify the configuration that maximizes
#' predictive performance on a provided test set.
#'
#' @param model_stack A fitted `stacks` object created from `add_candidates()`,
#'        `blend_predictions()`, and `fit_members()`.
#' @param test_data A `data.frame` or `tibble` containing the holdout data used
#'        to evaluate blending performance. Must include the same predictors
#'        used in the stack.
#'
#' @return A tibble with the best performing blend configuration based on
#'         R², including the corresponding `penalty`, `mixture`, and
#'         `non_negative` values.
#'
#' @examples
#' \dontrun{
#'   best_blend <- tune_blend(model_stack = my_stack, test_data = holdout)
#' }
#'
#' internal


tune_blend <- function(model_stack,
                       test_data,
                       penalty_grid  = 10^(seq(-5, -0, length.out = 25)),
                       mixture_grid  = seq(0, 1, by = 0.1),
                       negative_grid = c(TRUE, FALSE),
                       verbose       = TRUE){

  on.exit(future::plan(sequential), add = TRUE)

  tuning_grid <- expand.grid(penalty         = penalty_grid,
                             mixture         = mixture_grid,
                             non_negative    = negative_grid)

  future::plan(multisession, workers = parallel::detectCores() - 2)

  cli::cli_alert_success("Initialized parallel workflow with {.val {parallel::detectCores() - 2}} workers.")

  safely_execute <- horizons:::safely_execute

  progressr::handlers(global = TRUE)
  progressr::handlers("cli")

  progressr::with_progress({

    p <- progressr::progressor(steps = nrow(tuning_grid))

    furrr::future_pmap(.l = tuning_grid,
                       .f = function(penalty,
                                     mixture,
                                     non_negative){

                p(message = "Testing a range of blending parameters.")

                safely_execute(expr = {stacks::blend_predictions(data_stack   = model_stack,
                                                                 penalty      = penalty,
                                                                 mixture      = mixture,
                                                                 non_negative = non_negative,
                                                                 metric       = yardstick::metric_set(rsq)) %>%
                                        stacks::fit_members()},
                               default_value = NULL,
                               error_message = glue::glue("Blending failed for λ={penalty}, α={mixture}, neg={non_negative}")) -> blended_safe

                blended <- blended_safe$result

                if(is.null(blended)) return(tibble(penalty, mixture, rsq = NA_real_))

                predict(obj      = blended,
                        new_data = test_data) %>%
                  dplyr::mutate(Observed = test_data$Response) -> blended_preds

                yardstick::metrics(data     = blended_preds,
                                   truth    = Observed,
                                   estimate = .pred) -> metrics

                tibble::tibble(penalty      = penalty,
                               mixture      = mixture,
                               non_negative = non_negative,
                               rsq          = metrics %>% dplyr::filter(.metric == "rsq") %>% dplyr::pull(.estimate))},
              .options  = furrr::furrr_options(seed = TRUE)) -> results
    })

  future::plan(sequential)

  results %>%
    dplyr::bind_rows() %>%
    dplyr::filter(!is.na(rsq)) %>%
    dplyr::arrange(desc(rsq)) %>%
    dplyr::slice(1) -> tuned_params

  if (verbose) cli::cli_alert_success("Best blend (R2 = {round(tuned_params$rsq,2)}): λ={tuned_params$penalty}, α={tuned_params$mixture}, neg={tuned_params$non_negative}")

  return(tuned_params)

  }
