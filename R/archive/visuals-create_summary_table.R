#' Generate Summary Table of Candidate and Ensemble Model Metrics
#'
#' Produces a tidy `tibble` summarizing performance metrics (R², RMSE, MAE)
#' across all candidate models and the final ensemble. This function extracts
#' key model metadata—including model type, response transformation, spectral
#' preprocessing, and covariate grouping—from workflow IDs and assembles them
#' into a unified results table for comparison.
#'
#' @param ensemble_metrics A single-row `tibble` or named list containing the ensemble
#'        model's evaluation metrics. Must include `rsq`, `rmse`, and `mae`.
#' @param candidate_metrics A `tibble` containing candidate model results, including a
#'        `Model` column with workflow IDs and metric columns (`rsq`, `rmse`, `mae`).
#'
#' @return A `tibble` with one row per model (candidate or ensemble), and columns:
#' \describe{
#'   \item{ModelType}{Descriptive name of the model type (e.g., "Random Forest").}
#'   \item{Transformation}{Response transformation applied (e.g., "Log", "None").}
#'   \item{Preprocessing}{Spectral preprocessing used (e.g., "SNV + Derivative 1").}
#'   \item{Covariates}{Covariate specification parsed from model ID.}
#'   \item{rsq}{R-squared (coefficient of determination).}
#'   \item{rmse}{Root Mean Squared Error.}
#'   \item{mae}{Mean Absolute Error.}
#'   \item{Group}{Model group: "Candidate Models" or "Ensemble".}
#' }
#'
#' @details
#' This function assumes that model names follow a structured naming convention
#' (e.g., `<index>_<model>_<transformation>_<preprocessing>_<covariates>`).
#' It uses string parsing to infer model metadata from these identifiers.
#'
#' @seealso \code{\link{clean_workflow_id}}, \code{\link{evaluate_final_models}}
#'
#' @examples
#' \dontrun{
#' create_summary_table(ensemble_metrics = best_stack_metrics,
#'                      candidate_metrics = best_candidate_metrics)
#' }
#'
#' @importFrom dplyr mutate case_when select bind_rows
#' @importFrom stringr str_detect str_split_i str_count
#' @importFrom tibble tibble
#' @keywords internal
#' @export


create_summary_table <- function(ensemble_metrics,
                                 candidate_metrics){

  candidate_metrics %>%
    dplyr::mutate(ModelType = dplyr::case_when(str_detect(Model, "random_forest") ~ "Random Forest",
                                               str_detect(Model, "cubist")        ~ "Cubist",
                                               str_detect(Model, "xgboost")       ~ "XGBoost",
                                               str_detect(Model, "lightgbm")      ~ "LightGBM",
                                               str_detect(Model, "elastic_net")   ~ "Elastic Net",
                                               str_detect(Model, "svm_rbf")       ~ "SVM (RBF)",
                                               str_detect(Model, "mars")          ~ "MARS",
                                               str_detect(Model, "plsr")          ~ "PLSR",
                                               str_detect(Model, "mlp_nn")        ~ "MLP Neural Net",
                                               TRUE ~ Model),
                  Transformation = case_when(str_detect(Model, "NoTrans") ~ "None",
                                             str_detect(Model, "Log")     ~ "Log",
                                             str_detect(Model, "Sqrt")    ~ "Square Root",
                                             TRUE ~ NA_character_),
                  Preprocessing = case_when(str_detect(Model, "snv_deriv2") ~ "SNV + Derivative 2",
                                            str_detect(Model, "snv_deriv1") ~ "SNV + Derivative 1",
                                            str_detect(Model, "msc_deriv1") ~ "MSC + Derivative 1",
                                            str_detect(Model, "deriv2")     ~ "Derivative 2",
                                            str_detect(Model, "deriv1")     ~ "Derivative 1",
                                            str_detect(Model, "snv")        ~ "SNV",
                                            str_detect(Model, "msc")        ~ "MSC",
                                            str_detect(Model, "sg")         ~ "Savitzky-Golay",
                                            str_detect(Model, "raw")        ~ "Raw",
                                            TRUE ~ NA_character_),
                  check      = stringr::str_count(Model, "_"),
                  Covariates = case_when(check == 3 ~ stringr::str_split_i(Model, "_", i = 4),
                                         check == 4 ~ stringr::str_split_i(Model, "_", i = 5),
                                         check == 5 ~ stringr::str_split_i(Model, "_", i = 6),
                                         check == 6 ~ stringr::str_split_i(Model, "_", i = 8),
                                         check == 7 ~ stringr::str_split_i(Model, "_", i = 8)),
    Group = "Candidate Models") %>%
    dplyr::select(ModelType,
                  Transformation,
                  Preprocessing,
                  Covariates,
                  rsq,
                  rmse,
                  mae,
                  Group) -> candidate_table

  tibble::tibble(
               ModelType    = "Ensemble Model",
               Transformation = NA,
               Preprocessing  = NA,
               Covariates     = NA,
               rsq            = ensemble_metrics$rsq,
               rmse           = ensemble_metrics$rmse,
               mae            = ensemble_metrics$mae,
               Group          = "Ensemble") -> ensemble_row

  summary_table <- dplyr::bind_rows(candidate_table,
                                    ensemble_row)

  return(summary_table)

}
