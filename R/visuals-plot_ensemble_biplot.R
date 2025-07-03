#' Plot Ensemble Predictions and Model Weights
#'
#' Visualizes the performance of a stacked ensemble by generating:
#' (1) a predicted vs. observed plot for holdout data and
#' (2) a bar chart of model weights in the final stack.
#'
#' @param ensemble_results A named list output from `build_ensemble_stack()`, containing
#'   at minimum: `predictions` (a tibble) and `model_stack` (a fitted stack object).
#' @param include_candidate_models Logical. If `TRUE`, overlays individual model predictions on the
#'   observed vs. predicted plot (not yet implemented).
#' @param point_color Optional. Fill color for prediction points and bars. Default is a dark orange.
#'
#' @return A named list of two `ggplot2` objects:
#' \itemize{
#'   \item `Obs_vs_Pred`: Scatterplot of observed vs. predicted holdout values
#'   \item `Model_Weights`: Horizontal barplot of model weights from the stacked ensemble
#' }
#'
#' @examples
#' \dontrun{
#' results <- build_ensemble_stack(...)
#' plots <- plot_ensemble_results(results)
#' print(plots$Obs_vs_Pred)
#' print(plots$Model_Weights)
#' }
#'
#' @importFrom dplyr filter mutate case_when select bind_rows
#' @importFrom tidyr pivot_wider
#' @importFrom ggplot2 ggplot aes geom_abline geom_point geom_smooth
#'             scale_x_continuous scale_y_continuous scale_shape_discrete
#'             theme_minimal theme element_rect element_text element_blank
#' @importFrom stringr str_detect
#' @importFrom gt gt cols_label fmt_number tab_header
#' @importFrom tibble tibble
#' @import patchwork
#' @export

plot_ensemble_biplot <- function(ensemble_results){


  ## ---------------------------------------------------------------------------
  ## Step 1: Get plotting data
  ## ---------------------------------------------------------------------------

  ensemble_predictions <- ensemble_results$predictions

  ensemble_results$evaluation_metrics %>%
    dplyr::filter(.metric %in% c("rmse",
                                 "rsq",
                                 "mae")) %>%
    dplyr::mutate(.estimate = round(.estimate, 3)) %>%
    tidyr::pivot_wider(names_from  = .metric,
                       values_from = .estimate) -> ensemble_metrics

  candidate_preds    <- ensemble_results$candidate_predictions
  candidate_metrics  <- ensemble_results$candidate_metrics

  ## ---------------------------------------------------------------------------
  ## Step 2: Scales housekeeping
  ## ---------------------------------------------------------------------------

  lower_limts <- min(c(ensemble_predictions$Observed,
                       ensemble_predictions$Predicted))

  lower_limts <- ifelse(lower_limts - (0.25 * lower_limts) > 0,
                        lower_limts - (0.25 * lower_limts),
                        0)

  upper_limits <- max(c(ensemble_predictions$Observed,
                        ensemble_predictions$Predicted))

  upper_limits <- upper_limits + (0.25 * upper_limits)


  ## ---------------------------------------------------------------------------
  ## Step 3: Create summary tables
  ## ---------------------------------------------------------------------------

  create_summary_table(ensemble_metrics  = ensemble_metrics,
                       candidate_metrics = candidate_metrics) -> summary_table

#
# Define shape and color maps
# model_ids <- summary_table$Model
# shape_map <- setNames(c(1:(length(model_ids) - 1), 21), model_ids)
# color_map <- setNames(c(rep("#818D5D", length(model_ids) - 1), "#9D4920"), model_ids)
#
# Store model ID in the "Shape" column for dynamic plotting
# summary_table <- summary_table %>%
#   dplyr::mutate(Shape = Model)
#
# Plotting function that generates ggplot icons for each model ID
# create_icon_plot <- function(model_id) {
#   shape <- shape_map[[model_id]]
#   color <- color_map[[model_id]]
#
#   df <- tibble(x = 1,
#                y = 1)
#
#   ggplot2::ggplot(data = df,
#                   ggplot2::aes(x = x,
#                                y = y)) +
#     ggplot2::geom_point(shape = shape,
#                         size = 50,
#                         color = color,
#                         fill = color,
#                         stroke = 10) +
#     ggplot2::theme_void() +
#     ggplot2::coord_fixed()
# }
#
#     summary_table %>%
#       dplyr::mutate(Shape = Model) -> summary_table
#

  ## ---------------------------------------------------------------------------
  ## Step 4: Create gt:: table.
  ## ---------------------------------------------------------------------------

    gt::gt(summary_table,
           groupname_col = "Group") %>%
      # gt::text_transform(locations = gt::cells_body(columns = "Shape"),
      #                    fn = function(model_ids) {purrr::map(model_ids, create_icon_plot) %>%
      #                                               gt::ggplot_image(height = 50, aspect_ratio = 1)
      #                      })%>%
      gt::cols_label(rsq   = "R²",
                     rmse  = "RMSE",
                    #Shape = ""
                     mae   = "MAE") %>%
      gt::fmt_number(columns  = where(is.numeric),
                     decimals = 3) %>%
      gt::tab_header(title = "Model Performance Summary")-> table

    ## -------------------------------------------------------------------------

    ggplot2::ggplot(data  = ensemble_predictions,
                   ggplot2::aes(x = Observed,
                       y = Predicted)) +
      ggplot2::geom_abline(slope     = 1,
                          intercept = 0,
                          lty       = 2,
                          alpha     = 0.5) +
      ggplot2::geom_point(data     = candidate_preds,
                ggplot2::aes(x     = Observed,
                             y     = Predicted,
                             shape = Model),
                         size      = 2,
                         alpha     = 0.75,
                         color     = "#818D5D") +
      ggplot2::scale_shape_discrete(name = "Model") +
      ggplot2::geom_smooth(method = "lm",
                          se     = FALSE,
                          color  = "#9D4920",
                          lty    = 2) +
      ggplot2::geom_point(shape = 21,
                         size  = 4,
                         fill  = "#9D4920") +
      ggplot2::scale_x_continuous(limits = c(lower_limts, upper_limits)) +
      ggplot2::scale_y_continuous(limits = c(lower_limts, upper_limits)) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(panel.border          = ggplot2::element_rect(fill = "transparent", color = "black", linewidth = 1),
                    panel.grid             = ggplot2::element_blank(),
                    panel.background       = ggplot2::element_rect(fill = "white"),
                    plot.background        = ggplot2::element_rect(fill = "white"),
                    axis.title.y           = ggplot2::element_text(color = "black", size = 15, angle = 90, face = "bold"),
                    axis.title.x           = ggplot2::element_text(color = "black", size = 15,face = "bold"),
                    axis.text              = ggplot2::element_text(color = "black", size = 15),
                    strip.text             = ggplot2::element_text(size = 15, face = "bold"),
                    legend.position        = "none") -> results_plot_partial




    results_plot_partial/table + patchwork::plot_layout(nrow = 2,
                                                        heights = c(3, 1)) -> results_plot
  return(list(Obs_vs_Pred   = results_plot))

}
