transform_sankey_data <- function(results_dir,

                                  num_models           = 30,
                                  metric               = "rsq",
                                  collapse_rare_traits = TRUE,
                                  simplify             = FALSE){

  ## ---------------------------------------------------------------------------
  ## Step 0: Validate inputs
  ## ---------------------------------------------------------------------------


  ## ---------------------------------------------------------------------------
  ## Step 1: Futz with the input data
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


  ensemble_results %>%
    dplyr::filter(status == "success") %>%
    dplyr::select(-row,
                  -config_desc,
                  -saved_path,
                  -error_log_path,
                  -error_message,
                  -status) %>%
    dplyr::distinct(wflow_id,
                    .keep_all = TRUE) -> ensemble_df

  if(metric == "rsq"){

    ensemble_df %>%
      dplyr::arrange(desc(rlang::eval_tidy(rlang::sym(metric)))) %>%
      dplyr::slice(1:num_models) %>%
      dplyr::mutate(path_id = dplyr::row_number()) -> ensemble_df

    } else {

    ensemble_df %>%
      dplyr::arrange(rlang::eval_tidy(rlang::sym(metric))) %>%
      dplyr::slice(1:num_models) %>%
      dplyr::mutate(path_id = dplyr::row_number()) -> ensemble_df

    }

  ## ---------------------------------------------------------------------------
  ## Step 2: Extract the traits from the wflow_id
  ## ---------------------------------------------------------------------------

  ensemble_df %>%
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
                  Preprocessing  = dplyr::case_when(stringr::str_detect(wflow_id, "snv_deriv2")    ~ "SNV + Derivative 2",
                                                    stringr::str_detect(wflow_id, "snv_deriv1")    ~ "SNV + Derivative 1",
                                                    stringr::str_detect(wflow_id, "msc_deriv1")    ~ "MSC + Derivative 1",
                                                    stringr::str_detect(wflow_id, "deriv2")        ~ "Derivative 2",
                                                    stringr::str_detect(wflow_id, "deriv1")        ~ "Derivative 1",
                                                    stringr::str_detect(wflow_id, "snv")           ~ "SNV",
                                                    stringr::str_detect(wflow_id, "msc")           ~ "MSC",
                                                    stringr::str_detect(wflow_id, "sg")            ~ "Savitzky-Golay",
                                                    stringr::str_detect(wflow_id, "raw")           ~ "Raw",
                                                    TRUE                                           ~ NA_character_),
                  check          = stringr::str_count(wflow_id, "_"),
                  Covariates     = dplyr::case_when(check == 3 ~ stringr::str_split_i(wflow_id, "_", i = 4),
                                                    check == 4 ~ stringr::str_split_i(wflow_id, "_", i = 5),
                                                    check == 5 ~ stringr::str_split_i(wflow_id, "_", i = 6),
                                                    check == 6 ~ stringr::str_split_i(wflow_id, "_", i = 7),
                                                    check == 7 ~ stringr::str_split_i(wflow_id, "_", i = 8)),
                  Covariates     = ifelse(Covariates == "", "No Covariates", Covariates)) -> ensemble_df

  ## ---------------------------------------------------------------------------
  ## Step 3: Dynamically build performance buckets
  ## ---------------------------------------------------------------------------

  assign_performance_bucket <- function(metric_value,
                                        metric_name) {

    list(rsq   = "maximize",
         rrmse = "minimize",
         rmse  = "minimize",
         mae   = "minimize") -> metric_directions

    direction <- metric_directions[[metric_name]]

    if (direction == "maximize") {

      return(dplyr::case_when(metric_value >= quantile(metric_value, 0.665, na.rm = TRUE) ~ "Top",
                              metric_value >= quantile(metric_value, 0.25, na.rm = TRUE) ~ "Mid",
                              TRUE                                     ~ "Low"))

    } else if (direction == "minimize") {

      return(dplyr::case_when(metric_value <= quantile(metric_value, 0.25, na.rm = TRUE) ~ "Top",
                              metric_value <= quantile(metric_value, 0.665, na.rm = TRUE) ~ "Mid",
                              TRUE                                     ~ "Low"))
    } else {

      cli::cli_alert_warning("Unknown direction for metric: {metric_name}. Assuming minimization.")

      return(dplyr::case_when(metric_value <= quantile(metric_value, 0.25, na.rm = TRUE) ~ "Top",
                              metric_value <= quantile(metric_value, 0.665, na.rm = TRUE) ~ "Mid",
                              TRUE                                     ~ "Low"))
    }
  }

  ensemble_df %>%
    dplyr::mutate(Performance = factor(assign_performance_bucket(metric_value = rlang::eval_tidy(rlang::sym(metric)),
                                                                 metric_name  = metric),
                                       levels  = c("Top", "Mid", "Low")),
                  Performance = forcats::fct_relevel(Performance, "Mid", "Low", "Top")) -> ensemble_df

  ## ---------------------------------------------------------------------------
  ## Step 4: Pivot the data longer
  ## ---------------------------------------------------------------------------

  ensemble_df %>%
    tidyr::pivot_longer(cols      = c(Model,
                                      Transformation,
                                      Preprocessing,
                                      Covariates,
                                      Performance),
                        names_to  = "Stage",
                        values_to = "Trait") %>%
    dplyr::mutate(Stage = factor(Stage,
                                 levels = c("Model",
                                            "Transformation",
                                            "Preprocessing",
                                            "Covariates",
                                            "Performance"))) %>%
    select(wflow_id,
           Stage,
           Trait,
           all_of(metric)) -> ensemble_long

  ## ---------------------------------------------------------------------------
  ## Step 5: Optionally collapse the rare traits into "Other"
  ## ---------------------------------------------------------------------------

  if(collapse_rare_traits) {

    ensemble_long %>%
      dplyr::group_by(Stage, Trait) %>%
      dplyr::mutate(n = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Trait = as.character(Trait),
                    Trait = ifelse(n < 0.1 * num_models & Stage != "Performance",
                                   "Other",
                                   Trait),
                    Trait = as.factor(Trait)) %>%
      dplyr::select(-n) -> ensemble_long

  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Reorder the traits by frequency
  ## ---------------------------------------------------------------------------

  ## ---------------------------------------------------------------------------
  ## Step 7: Terminate bad paths early on.
  ## ---------------------------------------------------------------------------

  ensemble_long %>%
    dplyr::mutate(Trait = case_when(Stage == "Performance" & Trait != "Top" ~ NA,
                                    TRUE ~ Trait)) %>%
    dplyr::group_by(wflow_id) %>%
    dplyr::mutate(Highlight = dplyr::case_when(any(Stage == "Performance" & Trait == "Top") ~ "top_model",
                                               TRUE ~ "other")) %>%
    dplyr::ungroup() -> alluvial_df

  ## ---------------------------------------------------------------------------
  ## Step 8: Make the silly plot
  ## ---------------------------------------------------------------------------

  ggplot2::ggplot(alluvial_df,
                  ggplot2::aes(x        = Stage,
                               stratum  = Trait,
                               alluvium = wflow_id,
                               y        = 1)) +
    ggalluvial::geom_flow(ggplot2::aes(fill  = Highlight,
                                       color = Highlight),
                          stat  = "alluvium",
                          alpha = 0.9,
                          na.rm = TRUE) +
    ggalluvial::geom_stratum(color = "black",
                             na.rm = TRUE) +
    ggplot2::scale_color_manual(values = c("top_model" = "black",
                                           "other"     = "grey70")) +
    ggplot2::scale_fill_manual(values = c("top_model" = "#818D5D",
                                          "other"     = "lightgrey")) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::scale_x_discrete(expand = c(0,0)) +
    ggplot2::geom_text(stat = "stratum",
                       aes(label = after_stat(stratum)),
                       angle = 270,
                       size  = 4,
                       fontface  = "bold",
                       color = "black",
                       na.rm = TRUE) +
    ggplot2::theme(axis.title      = ggplot2::element_blank(),
                   axis.text.y     = ggplot2::element_blank(),
                   axis.ticks      = ggplot2::element_blank(),
                   axis.text.x     = ggplot2::element_text(size  = 14,
                                                           face  = "bold",
                                                           color = "black",
                                                           angle = 45,
                                                           vjust = 1,
                                                           hjust = 1),
                   panel.background = ggplot2::element_blank(),
                   legend.position = "none")

