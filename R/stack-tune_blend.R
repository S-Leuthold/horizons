#' Tune Ensemble Blending Hyperparameters via Grid Search
#'
#' Performs a parallelized grid search over blending hyperparameters for a
#' stacked ensemble model built with the `stacks` package. Explores combinations
#' of penalty strength (`λ`), mixture (`α`), and non-negativity constraints
#' to identify the configuration that maximizes holdout set performance (R²).
#'
#' @param model_stack A fitted `stacks::stacks()` object. Must have already
#'   undergone `add_candidates()` and `blend_predictions()` steps.
#' @param test_data A data frame containing the holdout observations with
#'   predictors and a `Response` column (target variable). Used to evaluate
#'   final predictions.
#' @param penalty_grid Numeric vector of penalty values (λ) to test. Defaults to
#'   25 log-spaced values from 1e-5 to 1.
#' @param mixture_grid Numeric vector of mixture values (α) from 0 (ridge) to 1 (lasso).
#'   Defaults to increments of 0.1.
#' @param negative_grid Logical vector indicating whether to constrain coefficients
#'   to be non-negative. Defaults to `c(TRUE, FALSE)`.
#' @param verbose Logical; if TRUE, prints best configuration to console. Default is TRUE.
#'
#' @return A tibble with the best blend configuration based on R². Columns include:
#'   \itemize{
#'     \item{\code{penalty}}{Optimal penalty value (λ)}
#'     \item{\code{mixture}}{Optimal mixture value (α)}
#'     \item{\code{non_negative}}{Whether coefficients were constrained to be ≥ 0}
#'     \item{\code{rsq}}{Best R² achieved on holdout set}
#'   }
#'
#' @details
#' Uses `furrr::future_pmap()` with parallel execution to test all blending
#' configurations. Internally calls `blend_predictions()` and `fit_members()` for
#' each configuration and evaluates predictions using `yardstick::rsq()`.
#'
#' @seealso
#'   \code{\link[stacks]{blend_predictions}},
#'   \code{\link[stacks]{fit_members}},
#'   \code{\link[yardstick]{rsq}},
#'   \code{\link[furrr]{future_pmap}}
#'
#' @examples
#' \dontrun{
#' tune_blend(model_stack = my_stack,
#'            test_data   = holdout_samples)
#' }
#'
#' @importFrom cli cli_alert_success
#' @importFrom dplyr mutate filter pull bind_rows arrange slice
#' @importFrom furrr future_pmap furrr_options
#' @importFrom future plan sequential multisession
#' @importFrom glue glue
#' @importFrom parallel detectCores
#' @importFrom progressr handlers with_progress progressor
#' @importFrom stacks blend_predictions fit_members
#' @importFrom tibble tibble
#' @importFrom yardstick rsq metric_set metrics
#'
#'
#' @keywords internal


tune_blend <- function(model_stack,
                       test_data,
                       penalty_grid  = 10^(seq(-5, -0, length.out = 25)),
                       mixture_grid  = seq(0, 1, by = 0.1),
                       negative_grid = c(TRUE, FALSE),
                       verbose       = TRUE){

  requireNamespace("plsmod", quietly = TRUE)


  on.exit(future::plan(sequential), add = TRUE)

  tuning_grid <- expand.grid(penalty         = penalty_grid,
                             mixture         = mixture_grid,
                             non_negative    = negative_grid)

  future::plan(multisession, workers = parallel::detectCores() - 2)

  cli::cli_alert_success("Initialized parallel workflow with {.val {parallel::detectCores() - 2}} workers.")

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
