#' Predict Soil Covariates from MIR Spectra Using Clustered Cubist Models
#'
#' Predicts soil covariate values (e.g., Sand, pH) from mid-infrared (MIR) spectra using
#' a clustered modeling workflow built on OSSL data. Includes PCA-based dimensionality reduction,
#' k-means clustering, per-cluster Cubist model calibration, prediction on new data, and optional
#' retrieval of cached predictions.
#'
#' @param covariates A character vector of covariate names to predict (e.g., `"Sand"`, `"pH"`).
#'   Covariates must be present in the OSSL training dataset.
#' @param input_data A `data.frame` or `tibble` containing wide-format MIR spectra.
#'   Must include a `Sample_ID` column and numeric wavenumber columns ranging from `600` to `4000` (2 cm⁻¹ interval).
#'   A `Project` column is also expected for cache partitioning.
#' @param verbose Logical. If `TRUE`, prints progress updates via `cli::cli_*()` functions. Defaults to `TRUE`.
#' @param refresh Logical. If `TRUE`, forces re-calculation of predictions and bypasses cached results. Defaults to `FALSE`.
#' @param cache_dir Character path. Directory where project- and covariate-specific predictions are cached using `qs::qsave()`.
#'   Defaults to `tools::R_user_dir("horizons", "cache")`.
#'
#' @return A named `list` with two elements:
#' \itemize{
#'   \item \strong{Predicted_Values}: A `tibble` with one row per sample and one column per predicted covariate.
#'     Includes `Project` and `Sample_ID`.
#'   \item \strong{Evaluation_Statistics}: A `tibble` of model evaluation metrics for the hold-out data
#'     (e.g., RMSE, R², CCC). If predictions are loaded from cache, a placeholder message is returned instead.
#' }
#'
#' @details
#' This function implements a hybrid local-global modeling strategy:
#' \enumerate{
#'   \item Smooth and normalize input MIR spectra using Savitzky-Golay (SG0) + SNV.
#'   \item Download and preprocess topsoil OSSL data for training.
#'   \item Perform PCA on OSSL data, then apply projection to new samples.
#'   \item Cluster samples using k-means on PCA scores.
#'   \item Create representative training subsets for each cluster using proximity filtering.
#'   \item Fit Cubist models per covariate per cluster using grid + Bayesian tuning.
#'   \item Predict covariate values for each cluster subset and evaluate performance using a 5% hold-out.
#'   \item Save new predictions to disk and merge with cached predictions.
#' }
#'
#' If all requested project-covariate pairs are already cached and `refresh = FALSE`, the function
#' returns only cached predictions and skips modeling. Otherwise, only missing pairs are recalculated.
#'
#' @examples
#' \dontrun{
#' preds <- predict_covariates(
#'   covariates = c("Sand", "pH"),
#'   input_data = my_mir_data,
#'   refresh = TRUE
#' )
#'
#' preds$Predicted_Values
#' preds$Evaluation_Statistics
#' }
#'
#' @importFrom dplyr select filter mutate rename bind_rows count inner_join group_by summarise ungroup starts_with
#' @importFrom purrr map map2 pmap walk compact cross_df
#' @importFrom tidyr pivot_longer pivot_wider drop_na
#' @importFrom tibble tibble as_tibble
#' @importFrom stringr str_detect str_split_i
#' @importFrom cli cli_alert_info cli_alert_danger cli_alert_warning cli_alert_success cli_progress_step cli_progress_done
#' @importFrom stats predict quantile
#' @importFrom prospectr savitzkyGolay standardNormalVariate
#' @importFrom rsample initial_split training testing
#' @importFrom glue glue
#' @importFrom qs qread qsave
#' @export


predict_covariates <- function(covariates,
                               input_data,
                               verbose   = TRUE,
                               refresh   = FALSE,
                               cache_dir = tools::R_user_dir("horizons", "cache")) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation
  ## ---------------------------------------------------------------------------

    ## ---------------------------------------------------------------------------
    ## Input Validation: Check input_data is a data frame
    ## ---------------------------------------------------------------------------

    if (!is.data.frame(input_data)) {

      cli::cli_alert_danger("Input {.arg input_data} must be a {.cls data.frame}.")
      stop("Aborting: Invalid `input_data` format.")

     }

    ## ---------------------------------------------------------------------------
    ## Input Validation: Check Sample_ID column
    ## ---------------------------------------------------------------------------

    if (!"Sample_ID" %in% names(input_data)) {

       cli::cli_alert_danger("Input {.arg input_data} must contain a {.field Sample_ID} column.")
      stop("Aborting: Missing `Sample_ID` in `input_data`.")

    }

    ## ---------------------------------------------------------------------------
    ## Input Validation: Check spectral columns exist
    ## ---------------------------------------------------------------------------

    if (!any(stringr::str_detect(names(input_data), "^\\d{3,4}$"))) {

      cli::cli_alert_danger("No spectral columns detected in {.arg input_data}.")
      cli::cli_alert_info("Expected numeric wavenumber columns like {.val '600'}, {.val '602'}, ..., {.val '4000'}.")
      stop("Aborting: Spectral input check failed.")

    }

    ## ---------------------------------------------------------------------------
    ## Check for duplicate Sample_IDs in input
    ## ---------------------------------------------------------------------------

   dup_check <- input_data %>%
      dplyr::count(Sample_ID) %>%
      dplyr::filter(n > 1)

    if (nrow(dup_check) > 0) {
      cli::cli_alert_warning("Duplicate Sample_IDs detected in input data. This may cause mapping conflicts.")
      print(dup_check)
    }

   ## ---------------------------------------------------------------------------
   ## Step 1: Check for cached covariates
   ## ---------------------------------------------------------------------------

   build_cache_path <- function(project,
                                covariate,
                                cache_dir) {

     proj_slug <- janitor::make_clean_names(project)

     file.path(cache_dir, proj_slug, paste0(covariate, ".qs"))

   }

   if(verbose) cli::cli_alert_success("Starting prediction of covariates.")

   get_cached_covariates <- function(project, covariate,
                                     cache_dir,
                                     refresh = FALSE) {

     path <- build_cache_path(project, covariate, cache_dir)

     if (!refresh && file.exists(path)) {
       cli::cli_alert_info("Cache hit: {.val {project}} — {.val {covariate}}")
       return(tryCatch(qs::qread(path), error = function(e) NULL))
     } else {
       if (!file.exists(path)) {
         cli::cli_alert_info("Cache miss: {.val {project}} — {.val {covariate}}")
       } else if (refresh) {
         cli::cli_alert_info("Refresh requested: Skipping cache for {.val {project}} — {.val {covariate}}")
       }
       return(NULL)
     }
   }

   project_ids <- unique(input_data$Project)

   cov_lookup <- expand.grid(
     project   = project_ids,
     covariate = covariates,
     stringsAsFactors = FALSE
   )

   if(verbose) cli::cli_alert_success("Checking cache for {.val {nrow(cov_lookup)}} project-covariate pairs")

   cov_lookup <- cov_lookup %>%
     dplyr::mutate(cache_result = purrr::map2(
       .x = project,
       .y = covariate,
       .f = ~get_cached_covariates(.x, .y, cache_dir, refresh = refresh)))

   cov_lookup <- cov_lookup %>%
     dplyr::mutate(missing = purrr::map_lgl(cache_result, is.null))

   covs_to_predict <- dplyr::filter(cov_lookup, missing)
   predicted_covs  <- dplyr::filter(cov_lookup, !missing)

   if(verbose) cli::cli_progress_step("Cached: {.val {nrow(predicted_covs)}} | To predict: {.val {nrow(covs_to_predict)}}")

   ## ---------------------------------------------------------------------------

   input_data <- input_data %>%
     dplyr::filter(Project %in% unique(covs_to_predict$project))

   if (nrow(covs_to_predict) == 0) {

     if(verbose) cli::cli_alert_success("All requested covariates found in cache. Skipping model prediction.")

     cached_predictions <- dplyr::bind_rows(predicted_covs$cache_result)

     long_predictions <- cached_predictions %>%
       tidyr::pivot_longer(cols = all_of(covariates),
                           names_to = "Covariate",
                           values_to = "Predicted_Values") %>%
       dplyr::group_by(Project, Sample_ID, Covariate) %>%
       dplyr::summarise(Predicted_Values = dplyr::first(na.omit(Predicted_Values)),
                        .groups = "drop")

     final_predictions <- long_predictions %>%
       tidyr::pivot_wider(names_from = Covariate,
                          values_from = Predicted_Values) %>%
       dplyr::select(Project, Sample_ID, all_of(covariates))

     evaluation_stats <- "Run with refresh = TRUE to get a new set of eval stats."

     if (verbose) cli::cli_progress_done()

     return(list(Predicted_Values      = final_predictions,
                 Evaluation_Statistics = evaluation_stats))
   }

   covariates <- unique(covs_to_predict$covariate)

  ## ---------------------------------------------------------------------------
  ## Step 1.5: Smooth and normalize the data
  ## ---------------------------------------------------------------------------

     input_data %>%
       dplyr::select(`600`:`4000`) %>%
       prospectr::savitzkyGolay(X = .,
                             m = 0,
                             p = 1,
                             w = 9) %>%
    prospectr::standardNormalVariate(X = .) %>%
    tibble::as_tibble() %>%
    dplyr::bind_cols(input_data %>% select(-c(`600`:`4000`)),
                     .) -> input_data

  ## ---------------------------------------------------------------------------
  ## Step 2: Load the OSSL data
  ## ---------------------------------------------------------------------------

  safely_execute(expr          = {download_ossl_data(covariates = covariates,
                                                     max_samples = NULL)},
                 default_value = NULL,
                 error_message = "Error downloading OSSL data for {covariates}") -> training_data_safe

  training_data <- training_data_safe$result

  if(is.null(training_data)) {
    cli::cli_abort("Aborting: OSSL data loading failed.")
  }

  if(verbose) cli::cli_progress_step("OSSL data loaded and processed.")

  ## ---------------------------------------------------------------------------
  ## Step 3: Reduce dimensions
  ## ---------------------------------------------------------------------------

  safely_execute(expr          = {reduce_dimensions_pca(training_data = training_data,
                                                        new_data      = input_data)},
                 default_value = NULL,
                 error_message = "Error during dimensionality reduction") -> reduced_dimensions_data_safe

  reduced_dimensions_data <- reduced_dimensions_data_safe$result

  if(is.null(reduced_dimensions_data)) {
    cli::cli_abort("Aborting: Dimensionality reduction failed.")
  }

  ## ---------------------------------------------------------------------------

  training_data <- reduced_dimensions_data$training_data
  input_data    <- reduced_dimensions_data$new_data

  if(verbose) cli::cli_progress_step("Dimensionality reduction complete.")

  ## ---------------------------------------------------------------------------
  ## Step 4: Cluster the new data
  ## ---------------------------------------------------------------------------

  if(verbose) cli::cli_progress_step("Running clustering analysis...")

  safely_execute(expr          = {cluster_spectral_data(input_data = input_data)},
                 default_value = NULL,
                 error_message = "Error during clustering of input data") -> clustering_results_safe

  clustering_results <- clustering_results_safe$result

  if(is.null(clustering_results)) {
    cli::cli_abort("Aborting: Clustering analysis of input data failed.")
  }

  ## ---------------------------------------------------------------------------

  input_data    <- clustering_results$input_data
  pca_model     <- clustering_results$pca_model
  kmeans_model  <- clustering_results$kmeans_model
  n_components  <- clustering_results$ncomp

  if(verbose) cli::cli_progress_step("Clustering analysis complete.")

  ## ---------------------------------------------------------------------------
  ## Step 5: Build per-cluster training subsets
  ## ---------------------------------------------------------------------------

  if(verbose)  cli::cli_progress_step("Segmenting data...")

  safely_execute(expr          = {create_clustered_subsets(training_data = training_data,
                                                           pca_model     = pca_model,
                                                           kmeans_model  = kmeans_model,
                                                           n_components  = n_components,
                                                           coverage      = 0.80)},
                 default_value = NULL,
                 error_message = "Error during training data segmentation.") -> training_data_clustered_safe

  training_data_clustered <- training_data_clustered_safe$result

  if (is.null(training_data_clustered)) {
    cli::cli_abort("Aborting: Training data segmentation failed.")
  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Split training data into train/holdout
  ## ---------------------------------------------------------------------------

  splits        <- purrr::map(training_data_clustered, ~ rsample::initial_split(.x, prop = 0.95, strata = NULL))
  training_data <- purrr::map(splits, rsample::training)
  holdout_data  <- purrr::map(splits, rsample::testing)

  ## ---------------------------------------------------------------------------
  ## Step 7: Prepare holdout data for evaluation
  ## ---------------------------------------------------------------------------

  holdout_data %>%
    dplyr::bind_rows(.id = "Cluster") %>%
    dplyr::mutate(Cluster      = stringr::str_split_i(Cluster, "_", 2),
                  Cluster      = as.numeric(Cluster),
                  Sample_Index = as.character(Sample_Index)) -> holdout_data

  holdout_data %>%
    dplyr::select(-Cluster,
                  -starts_with("Dim.")) %>%
    dplyr::rename(Sample_ID = Sample_Index) %>%
    dplyr::select(Sample_ID,
                  all_of(covariates)) -> holdout_measurements

   holdout_data %>%
    dplyr::select(Cluster,
                  Sample_Index,
                  starts_with("Dim.")) %>%
    dplyr::mutate(Source = "Holdout",
                  .before = 2) %>%
    dplyr::rename(Sample_ID = Sample_Index) -> holdout_spectra

   if(verbose) cli::cli_progress_step("Training data and holdouts created.")

  ## ---------------------------------------------------------------------------
  ## Step 8: Train Cubist models across clusters and covariates
  ## ---------------------------------------------------------------------------

  purrr::cross_df(list(cluster   = names(training_data),
                       covariate = covariates)) %>%
    dplyr::mutate(training_subset = purrr::map(cluster, ~ training_data[[.x]])) -> model_grid

   if(verbose) cli::cli_progress_step("Setting up Cubist models...")

  Calibrated_Models <- purrr::pmap(model_grid,
                                   function(covariate,
                                            cluster,
                                            training_subset) {

    if(verbose) cli::cli_progress_step("Calibrating model for: {covariate} ({cluster})")

    calibrated_model <- fit_cubist_model(input_data = training_subset,
                                         covariate  = covariate,
                                         verbose    = verbose)

    if(is.null(calibrated_model)) {
      cli::cli_alert_danger("Model calibration failed for {covariate} in {cluster}. Skipping.")
      return(NULL)
    }

    list(outcome     = covariate,
         cluster     = cluster,
         model       = calibrated_model$Model,
         best_params = calibrated_model$Best_Parameters,
         evaluation  = calibrated_model$Evaluation)
  })

  Calibrated_Models <- purrr::compact(Calibrated_Models)

  if (length(Calibrated_Models) == 0) {
    cli::cli_abort("Aborting: No Cubist models were successfully calibrated.")
  }

  if(verbose) cli::cli_progress_step("Cubist models calibrated.")

  ## ---------------------------------------------------------------------------
  ## Step 9: Predict covariates for new samples
  ## ---------------------------------------------------------------------------

  input_data %>%
    dplyr::mutate(Source = "Unknown", .before = 2) %>%
    dplyr::bind_rows(holdout_spectra) -> input_data

  predictions <- purrr::map(Calibrated_Models, function(model_info) {

    input_data %>%
      dplyr::filter(Cluster == as.numeric(stringr::str_split_i(model_info$cluster, "_", 2))) -> input_data_local

    if (nrow(input_data_local) == 0) return (NULL)

     safely_execute(expr          = {predict(object   = model_info$model,
                                             new_data = input_data_local)},
                    default_value = NULL,
                    error_message = "Prediction failed for {model_info$outcome} in {model_info$cluster}") -> preds_safe

     preds <- preds_safe$result

    if(is.null(preds)) {
      return(NULL)
    }

    input_data_local %>%
      dplyr::select(Project,
                    Sample_ID,
                    Cluster,
                    Source) %>%
      dplyr::mutate(Predicted_Values = preds$.pred,
                    Covariate        = model_info$outcome)

    }) %>%  dplyr::bind_rows() %>%
            dplyr::group_by(Project,
                            Sample_ID,
                            Covariate,
                            Source,
                            Cluster) %>%
            dplyr::ungroup()


  ## ---------------------------------------------------------------------------
  ## Step 10: Split into holdout and unknown
  ## ---------------------------------------------------------------------------

  predictions %>%
    dplyr::filter(Source == "Holdout") %>%
    dplyr::select(-Source,
                  -Cluster) %>%
    tidyr::pivot_wider(names_from  = Covariate,
                       values_from = Predicted_Values) %>%
    dplyr::select(Project,
                  Sample_ID,
           all_of(covariates)) -> holdout_predictions

  predictions %>%
    dplyr::filter(Source == "Unknown") %>%
    dplyr::select(-Source,
                  -Cluster) %>%
    tidyr::pivot_wider(names_from  = Covariate,
                       values_from = Predicted_Values) -> unknown_predictions

  ## ---------------------------------------------------------------------------
  ## Step 11: Evaluate holdout predictions
  ## ---------------------------------------------------------------------------

  safely_execute(expr          = {evaluate_predictions(measured_data = holdout_measurements,
                                                       modeled_data  = holdout_predictions)},
                 default_value = NULL,
                 error_message = "Error evaluating predictions for holdout data") -> evaluation_stats_safe

  evaluation_stats <- evaluation_stats_safe$result

  if(verbose) cli::cli_progress_step("Prediction and evaluation complete.")

  ## ---------------------------------------------------------------------------
  ## Step 12: Save to cache and combine with previous cached data.
  ## ---------------------------------------------------------------------------

  purrr::walk(.x = covariates,
              .f = function(cov) {
                purrr::walk(.x = unique(unknown_predictions$Project),
                            .f = function(proj){

                              cache_path <- build_cache_path(proj, cov, cache_dir)
                              dir.create(dirname(cache_path),
                                         showWarnings = FALSE,
                                         recursive    = TRUE)

                              unknown_predictions %>%
                                dplyr::filter(Project == proj) %>%
                                dplyr::select(Project,
                                              Sample_ID,
                                              !!cov) %>%
                                qs::qsave(cache_path)

                              if (verbose) cli::cli_alert_success("Cached predictions: {.val {proj}} — {.val {cov}}")
                            })
              })

  cached_predictions <- if (nrow(predicted_covs) > 0) {
    dplyr::bind_rows(predicted_covs$cache_result)
  } else {
    tibble::tibble()
  }

  all_predictions <- dplyr::bind_rows(cached_predictions, unknown_predictions)

  long_predictions <- all_predictions %>%
    dplyr::mutate(Source = "Final") %>%
    tidyr::pivot_longer(cols = covariates,
                        names_to = "Covariate",
                        values_to = "Predicted_Values") %>%
    dplyr::group_by(Project, Sample_ID, Covariate) %>%
    dplyr::summarise(Predicted_Values = dplyr::first(na.omit(Predicted_Values)),
                     .groups = "drop")

  final_predictions <- long_predictions %>%
    tidyr::pivot_wider(names_from = Covariate,
                       values_from = Predicted_Values) %>%
    dplyr::select(Project, Sample_ID, all_of(covariates))

  if(verbose) cli::cli_progress_step("Newly predicted variables saved to cache.")
  if(verbose) cli::cli_progress_done()


  ## ---------------------------------------------------------------------------
  ## Step 13: Return output
  ## ---------------------------------------------------------------------------

  return(list(Predicted_Values      = final_predictions,
              Evaluation_Statistics = evaluation_stats
  ))
}
