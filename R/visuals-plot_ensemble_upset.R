#' Plot an UpSet-style summary of top-performing ensemble workflows
#'
#' This function visualizes the intersection of modeling components (e.g., model type,
#' preprocessing, transformations, covariate sets) across the top-N performing workflows
#' from a batch model evaluation. It generates a two-part composite plot: a bar chart
#' of performance metric values and a matrix-style intersection plot indicating which
#' components are used by which workflows. Optionally includes an overlay for ensemble performance.
#'
#' @param results_dir Character. Path to the directory containing `batch_summary_...qs`.
#' @param ensemble_results Optional list. A fitted `stacks` ensemble object with `$evaluation_metrics`.
#' @param num_models Integer. Number of top-performing workflows to include (default: 30).
#' @param metric Character. Performance metric to plot and rank models by (e.g., `"rsq"`, `"rmse"`).
#' @param components Character vector. Workflow components to include in the intersection matrix.
#'        Must be a subset of: `"Model"`, `"Preprocessing"`, `"Transformation"`, `"Covariates"`.
#' @param collapse_rare_traits Logical. If `TRUE`, component values used in <10% of top workflows
#'        will be collapsed into `"Other {category}"` (default: `TRUE`).
#' @param simplify Logical. If `TRUE`, components are simplified into semantic groupings
#'        (e.g., "Tree-Based", "Standardized"). Cannot be used with `collapse_rare_traits = TRUE`.
#' @param custom_metric Character (optional). Custom label for the y-axis if `metric = "custom"`.
#'
#' @return A `patchwork` composite plot object showing performance bars and component intersections.
#'         Use `print()` or `ggsave()` to visualize or export.
#'
#' @details
#' This function is designed for interactive exploration of workflow intersections and performance.
#' It uses internally defined component parsing logic and optionally maps components to semantic
#' groups (model family, preprocessing type, etc.). An ensemble overlay is drawn as a dashed line
#' if `ensemble_results` is provided.
#'
#' All parsing is based on `wflow_id` naming conventions. Ensure consistent naming across workflows.
#'
#' @section Required Packages:
#' This function requires the following packages to be installed and available:
#'
#' @importFrom cli cli_abort
#' @importFrom dplyr filter select distinct mutate case_when pull arrange desc slice left_join group_by ungroup summarise n count rename
#' @importFrom forcats fct_relevel
#' @importFrom fs dir_ls
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_point geom_hline geom_text geom_segment scale_shape_manual scale_y_discrete coord_cartesian labs theme theme_minimal element_blank element_text element_rect annotate
#' @importFrom patchwork wrap_plots
#' @importFrom purrr pluck
#' @importFrom qs qread
#' @importFrom rlang sym eval_tidy
#' @importFrom stringr str_detect str_split_i str_count
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tidyselect all_of
#'
#' @seealso \code{\link{run_batch_evaluation}}, \code{\link{evaluate_stacks}}, \code{\link{plot_metric_summary}}
#'
#' @export


plot_ensemble_upset <- function(results_dir,
                                ensemble_results     = NULL,
                                num_models           = 30,
                                metric               = "rsq",
                                components           = c("Model",
                                                         "Transformation",
                                                         "Preprocessing",
                                                         "Covariates"),
                                collapse_rare_traits = TRUE,
                                simplify             = FALSE,
                                custom_metric        = NULL){

  ## ---------------------------------------------------------------------------
  ## Step 0: Data validation
  ## ---------------------------------------------------------------------------

  if(isTRUE(collapse_rare_traits) & isTRUE(simplify)){
    cli::cli_abort("collapse_rare_traits and simplify cannot both be TRUE. Please set one to FALSE.")
  }

  if (!dir.exists(results_dir)) cli::cli_abort("Provided results_dir does not exist.")

  if (!is.character(metric) || length(metric) != 1) cli::cli_abort("Metric must be a single character string.")

  if (!all(components %in% c("Model", "Preprocessing", "Transformation", "Covariates"))) {
    cli::cli_abort("Unknown component(s) specified: {components[!components %in% ...]}")
  }


  ## ---------------------------------------------------------------------------
  ## Step 1: Load the batch summary results
  ## ---------------------------------------------------------------------------

  safely_execute(expr = {fs::dir_ls(path   = results_dir,
                                    regexp = "batch_summary.*\\.qs$") %>%
                          .[1] -> batch_summary_path

                          qs::qread(batch_summary_path)},
    default_value = NULL,
    error_message = "Error loading batch results file -- check that batch_summary_...qs exists.") -> batch_summary_safe

  batch_summary <- batch_summary_safe$result

  if(is.null(batch_summary)){

    cli::cli_abort("Batch summary returned NULL-- check that file exists in results directory.")

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Select the top workflows based on chosen metric
  ## ---------------------------------------------------------------------------

  batch_summary %>%
    dplyr::filter(status == "success") %>%
    dplyr::select(-row,
                  -config_desc,
                  -saved_path,
                  -error_log_path,
                  -error_message,
                  -status) %>%
    dplyr::distinct(wflow_id,
                    .keep_all = TRUE) -> summary_df

  if(metric == "rsq"){

    summary_df %>%
      dplyr::arrange(desc(rlang::eval_tidy(rlang::sym(metric)))) %>%
      dplyr::slice(1:num_models) -> summary_df

  } else {

    summary_df %>%
      dplyr::arrange(rlang::eval_tidy(rlang::sym(metric))) %>%
      dplyr::slice(1:num_models) -> summary_df

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Extract workflow components.
  ## ---------------------------------------------------------------------------

  summary_df %>%
    dplyr::mutate(Model          = dplyr::case_when(stringr::str_detect(wflow_id, "random_forest") ~ "Random Forest",
                                                    stringr::str_detect(wflow_id, "cubist")        ~ "Cubist",
                                                    stringr::str_detect(wflow_id, "xgboost")       ~ "XGBoost",
                                                    stringr::str_detect(wflow_id, "lightgbm")      ~ "LightGBM",
                                                    stringr::str_detect(wflow_id, "elastic_net")   ~ "Elastic Net",
                                                    stringr::str_detect(wflow_id, "svm_rbf")       ~ "SVM (RBF)",
                                                    stringr::str_detect(wflow_id, "mars")          ~ "MARS",
                                                    stringr::str_detect(wflow_id, "plsr")          ~ "PLSR",
                                                    stringr::str_detect(wflow_id, "mlp_nn")        ~ "MLP Neural Net",
                                                    TRUE                                           ~ wflow_id),
                  Transformation = dplyr::case_when(stringr::str_detect(wflow_id, "NoTrans")       ~ "None",
                                                    stringr::str_detect(wflow_id, "Log")           ~ "Log",
                                                    stringr::str_detect(wflow_id, "Sqrt")          ~ "Square Root",
                                                    TRUE                                           ~ NA_character_),
                  Preprocessing  = dplyr::case_when(stringr::str_detect(wflow_id, "SNVD2")    ~ "SNV + Derivative 2",
                                                    stringr::str_detect(wflow_id, "SNVD1")    ~ "SNV + Derivative 1",
                                                    stringr::str_detect(wflow_id, "MSCD1")    ~ "MSC + Derivative 1",
                                                    stringr::str_detect(wflow_id, "D2")        ~ "Derivative 2",
                                                    stringr::str_detect(wflow_id, "D1")        ~ "Derivative 1",
                                                    stringr::str_detect(wflow_id, "SNV")           ~ "SNV",
                                                    stringr::str_detect(wflow_id, "MSC")           ~ "MSC",
                                                    stringr::str_detect(wflow_id, "SG")            ~ "Savitzky-Golay",
                                                    stringr::str_detect(wflow_id, "Raw")           ~ "Raw",
                                                    TRUE                                           ~ NA_character_),
                  check          = stringr::str_count(wflow_id, "_"),
                  Covariates     = dplyr::case_when(check == 3 ~ stringr::str_split_i(wflow_id, "_", i = 4),
                                                    check == 4 ~ stringr::str_split_i(wflow_id, "_", i = 5),
                                                    check == 5 ~ stringr::str_split_i(wflow_id, "_", i = 6),
                                                    check == 6 ~ stringr::str_split_i(wflow_id, "_", i = 7),
                                                    check == 7 ~ stringr::str_split_i(wflow_id, "_", i = 8)),
                  Covariates     = ifelse(Covariates == "", "No Covariates", Covariates)) %>%
    dplyr::select(wflow_id,
                  all_of(components),
                  metric) -> summary_df

  ## ---------------------------------------------------------------------------
  ## Step 3: Optionally simplify the components to semantic groups
  ## ---------------------------------------------------------------------------

  if(simplify){

  ## This is brittle.

  tibble::tibble(Model = c("Random Forest",
                           "Cubist",
                           "XGBoost",
                           "LightGBM",
                           "Elastic Net",
                           "SVM (RBF)",
                           "MARS",
                           "PLSR",
                           "MLP Neural Net"),
         Model_Group   = c("Tree-Based",
                           "Tree-Based",
                           "Tree-Based",
                           "Tree-Based",
                           "Linear",
                           "Kernel-Based",
                           "Basis Expansion",
                           "Linear",
                           "Neural Network")) -> model_semantics

    tibble::tibble(Preprocessing       = c("Raw",
                                           "SNV",
                                           "MSC",
                                           "Savitzky-Golay",
                                           "Derivative 1",
                                           "Derivative 2",
                                           "SNV + Derivative 1",
                                           "SNV + Derivative 2",
                                           "MSC + Derivative 1"),
                   Preprocessing_Group = c("Raw",
                                           "Standardized",
                                           "Standardized",
                                           "Standardized",
                                           "Derivatives",
                                           "Derivatives",
                                           "Combined",
                                           "Combined",
                                           "Combined")) -> preproc_semantics


    summary_df %>%
      dplyr::left_join(model_semantics,   by = "Model") %>%
      dplyr::left_join(preproc_semantics, by = "Preprocessing") %>%
      dplyr::mutate(Covariates    = dplyr::case_when(stringr::str_detect(Covariates, "GDD|MAT|MAP|PET|AI|Precip_Seasonality") &
                                                     stringr::str_detect(Covariates, "Clay|pH|CEC|Sand|Nitrogen|Silt")          ~ "Soil + Climate",
                                                     stringr::str_detect(Covariates, "Clay|pH|CEC|Sand|Nitrogen|Silt")          ~ "Soil",
                                                     stringr::str_detect(Covariates, "GDD|MAT|MAP|PET|AI|Precip_Seasonality")   ~ "Climate",
                                                     TRUE ~ "No Covariates"),
                    Model         = Model_Group,
                    Preprocessing = Preprocessing_Group) %>%
      dplyr::select(wflow_id,
                    tidyselect::all_of(components),
                    tidyselect::all_of(metric)) -> summary_df

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Create the metric plot
  ## ---------------------------------------------------------------------------

  ## Dynamic y axis label ------------------------------------------------------

  switch(metric,
         "rsq"  = metric_label <- "R-Squared",
         "rmse" = metric_label <- "RMSE",
         "mae"  = metric_label <- "MAE",
         "mse"  = metric_label <- "MSE",
         "mape" = metric_label <- "MAPE",
         "r2"   = metric_label <- "R-Squared",
         "custom" = {
           if(is.null(custom_metric)){
             cli::cli_abort("Please provide a custom metric name.")
           } else {
             metric_label <- custom_metric
           }
         })

  ## Dynamic y axis scale ------------------------------------------------------

  tibble(range = max(summary_df %>%  pull(!!rlang::sym(metric))) - min(summary_df %>%  pull(!!rlang::sym(metric))),
         ymax  = max(summary_df %>%  pull(!!rlang::sym(metric))) + 0.1 * range,
         ymin  = min(summary_df %>%  pull(!!rlang::sym(metric))) - 0.1 * range) -> y_scale


  ## Top Plot ------------------------------------------------------------------

  ggplot2::ggplot(data = summary_df,
                  ggplot2::aes(x = reorder(wflow_id,
                                           -rlang::eval_tidy(rlang::sym(metric))),
                               y = rlang::eval_tidy(rlang::sym(metric)))) +
    ggplot2::geom_bar(fill = "#818D5D",
                      stat = "identity",
                      color = "black") +
    ggplot2::labs(y = metric_label) +
    ggplot2::coord_cartesian(ylim = c(y_scale$ymin, y_scale$ymax)) +
    ggplot2::theme(axis.title.x    = ggplot2::element_blank(),
                   axis.title.y    = ggplot2::element_text(size  = 16,
                                                           face  = "bold",
                                                           color = "black"),
                   axis.text.x     = ggplot2::element_blank(),
                   axis.ticks      = ggplot2::element_blank(),
                   axis.text.y     = ggplot2::element_text(size  = 12,
                                                           color = "black"),
                   panel.background = ggplot2::element_rect(fill  = "white",
                                                            color = "black"),
                   legend.position = "none") -> metric_plot


  ## Optionally add ensemble ---------------------------------------------------

  if(!is.null(ensemble_results)){

    ensemble_results %>%
      purrr::pluck(., "evaluation_metrics") %>%
      dplyr::filter(.metric == metric) %>%
      dplyr::pull(.estimate) -> ensemble_metric

    if(ensemble_metric < y_scale$ymin){

      y_scale$ymin <- ensemble_metric - 0.1 * y_scale$range

    }

    if(ensemble_metric > y_scale$ymax){

      y_scale$ymax <- ensemble_metric - 0.1 * y_scale$range

    }

    suppressMessages({
    metric_plot +
      ggplot2::geom_hline(yintercept = ensemble_metric,
                          linetype = 2,
                          color    = "red",
                          linewidth = 1.25) +
      ggplot2::coord_cartesian(ylim = c(y_scale$ymin, y_scale$ymax)) +
      ggplot2::annotate(geom = "text",
                        x = nrow(summary_df),
                        y = ensemble_metric + 0.1 * y_scale$range,
                        label = paste0("Ensemble: ", round(ensemble_metric, 3)),
                        hjust = 1,
                        size = 5) -> metric_plot
    })

  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Create a custom order for the workflow components
  ## ---------------------------------------------------------------------------

  summary_df %>%
    dplyr::select(wflow_id,
                  tidyselect::all_of(metric),
                  tidyselect::all_of(components)) %>%
    tidyr::pivot_longer(cols      = tidyselect::all_of(components),
                        names_to  = "category",
                        values_to = "component") %>%
    dplyr::filter(!is.na(component)) -> summary_long


  if(collapse_rare_traits) {

    summary_long %>%
      dplyr::group_by(category, component) %>%
      dplyr::mutate(n = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(component = as.character(component),
                    component = ifelse(n < 0.1 * num_models,
                                       paste0("Other ", category),
                                       component)) %>%
      dplyr::select(-n) -> summary_long

  }


  summary_long %>%
    dplyr::group_by(category, component) %>%
    dplyr::summarise(mean_metric = max(!!rlang::sym(metric), na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(category) %>%
    dplyr::arrange(dplyr::desc(mean_metric), .by_group = TRUE) %>%
    dplyr::mutate(comp_order = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cat_order = case_when(category == "Model" ~ 1,
                                        category == "Preprocessing" ~ 2,
                                        category == "Transformation" ~ 3,
                                        category == "Covariates" ~ 4)) %>%
    dplyr::arrange(cat_order, comp_order) %>%
    dplyr::mutate(final_order = dplyr::row_number(),
                  component   = as.character(component)) %>%
    dplyr::select(category,
                  component,
                  final_order) -> ranked_df


  ## ---------------------------------------------------------------------------
  ## Step 4: Create the matrix dataframe
  ## ---------------------------------------------------------------------------

  summary_long %>%
    dplyr::filter(!is.na(component)) %>%
    dplyr::mutate(component = forcats::fct_relevel(component, ranked_df$component))-> matrix_df


  ## ---------------------------------------------------------------------------
  ## Step 6: Create a df for visually segmenting the matrix
  ## ---------------------------------------------------------------------------

  component_levels <- levels(matrix_df$component)

  matrix_df %>%
    dplyr::distinct(component, category) %>%
    dplyr::count(category, name = "n") %>%
    dplyr::mutate(cat_order = case_when(category == "Model"           ~ 1,
                                        category == "Preprocessing"   ~ 2,
                                        category == "Transformation"  ~ 3,
                                        category == "Covariates"      ~ 4)) %>%
    dplyr::arrange(cat_order) %>%
    dplyr::select(-cat_order) %>%
    dplyr::mutate(y = cumsum(n),
                  y = length(component_levels) - y + 0.5,
                  yend = y,
                  x = 0.5,
                  xend = length(unique(matrix_df$wflow_id)) + 0.5) %>%
    dplyr::filter(category != "Covariates") -> segments


  ## ---------------------------------------------------------------------------
  ## Step 6: Create the matrix plot
  ## ---------------------------------------------------------------------------

  ggplot(matrix_df,
         aes(x = reorder(wflow_id,
                         -rlang::eval_tidy(rlang::sym(metric))),
             y = component)) +
    ggplot2::geom_line(ggplot2::aes(group = wflow_id),
                       color = "grey80") +
    ggplot2::geom_point(ggplot2::aes(shape = category),
                        size  = 4,
                        fill  = "#818D5D",
                        color = "black") +
    ggplot2::geom_segment(data        = segments,
                          ggplot2::aes(x    = x,
                                       xend = xend,
                                       y    = y,
                                       yend = yend),
                          inherit.aes = TRUE,
                          color       = "grey30",
                          linetype    = "dashed",
                          linewidth   = 0.4) +
    ggplot2::scale_shape_manual(values = c(Model             = 21,
                                              Transformation = 22,
                                              Preprocessing  = 23,
                                              Covariates     = 24)) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.x    = ggplot2::element_blank(),
                   axis.title.y    = ggplot2::element_blank(),
                   axis.text.x     = ggplot2::element_blank(),
                   axis.ticks      = ggplot2::element_blank(),
                   axis.text.y     = ggplot2::element_text(size  = 10,
                                                           color = "black"),
                   panel.grid       = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.position = "none") -> matrix_plot

  ## ---------------------------------------------------------------------------
  ## Step 7: Finalize plot and return
  ## ---------------------------------------------------------------------------

  patchwork::wrap_plots(metric_plot,
                        matrix_plot,
                        ncol    = 1,
                        heights = c(0.4, 0.6)) -> final_plot

  return(final_plot)

}
