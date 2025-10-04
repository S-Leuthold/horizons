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
#' @param parallel Logical. Enable parallel processing for Cubist model training. Defaults to `FALSE` (safe for nested contexts).
#' @param n_workers Integer. Number of parallel workers for model fitting. If `NULL`, uses `min(10, detectCores()-1)` for safety.
#' @param allow_nested Logical. Allow parallel processing even when already in parallel context. Defaults to `FALSE` (recommended).
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

## ---------------------------------------------------------------------------
## Helper Functions (from main branch utils)
## Prefixed with old_ to avoid conflicts with current package
## ---------------------------------------------------------------------------

old_safely_execute <- function(expr,
                           default_value   = NULL,
                           error_message   = NULL,
                           log_error       = TRUE,
                           capture_trace   = FALSE,
                           trace_log_file  = NULL) {

  expr_quo <- rlang::enquo(expr)

  safe_eval <- purrr::safely(function() {
    rlang::eval_tidy(expr_quo, env = rlang::caller_env())
  }, otherwise = default_value, quiet = TRUE)

  result_list <- safe_eval()

  trace <- NULL

  if (!is.null(result_list$error)) {
    if (capture_trace) {
      trace <- tryCatch(
        if (exists("last_trace", asNamespace("rlang"), inherits = FALSE)) {
          rlang::last_trace()
        } else {
          rlang::trace_back()
        },
        error = function(e) NULL
      )

      if (!is.null(trace_log_file)) {
        try({
          cat(capture.output(print(trace)),
              file  = trace_log_file,
              sep    = "\n",
              append = TRUE)
        }, silent = TRUE)
      }
    }

    if (log_error) {
      msg <- if (!is.null(error_message)) {
        tryCatch(
          glue::glue(error_message, .envir = rlang::caller_env()),
          error = function(e) paste0(error_message, " (error in message: ", e$message, ")")
        )
      } else {
        "An error occurred"
      }

      cli::cli_warn("{msg}: {result_list$error$message}")
    }
  }

  return(list(result = result_list$result,
              error  = result_list$error,
              trace  = trace))
}

old_download_horizons_data <- function(force = FALSE, ask = TRUE) {
  cache_dir <- tools::R_user_dir("horizons", "cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  location_file <- file.path(cache_dir, "ossl_location_data.qs")
  lab_file      <- file.path(cache_dir, "ossl_lab_data.qs")
  mir_file      <- file.path(cache_dir, "ossl_mir_raw.qs")

  existing_files <- all(file.exists(c(location_file, lab_file, mir_file)))

  if (existing_files && !force) {
    cli::cli_alert_success("OSSL data already present in cache at {.path {cache_dir}}")
    return(invisible(list(location = location_file, lab = lab_file, mir = mir_file)))
  }

  if (ask) {
    confirm_title <- if (existing_files && force) {
      glue::glue("OSSL data already cached at {cache_dir}. Re-download now?")
    } else {
      glue::glue("The OSSL data (~1–2GB total) is missing in cache at {cache_dir}. Download now?")
    }

    utils::menu(c("Yes, download the data", "No, cancel"), title = confirm_title) -> response

    if (response != 1) {
      cli::cli_alert_info("Download canceled. No changes made.")
      return(invisible(NULL))
    }
  }

  cli::cli_progress_step("Downloading OSSL datasets from cloud storage")

  qs::qread_url("https://storage.googleapis.com/soilspec4gg-public/ossl_models/ossl_soilsite_L0_v1.2.qs") -> location_data
  qs::qread_url("https://storage.googleapis.com/soilspec4gg-public/ossl_models/ossl_soillab_L1_v1.2.qs") -> lab_data
  qs::qread_url("https://storage.googleapis.com/soilspec4gg-public/ossl_models/ossl_mir_L0_v1.2.qs") -> mir_data

  qs::qsave(location_data, location_file)
  qs::qsave(lab_data, lab_file)
  qs::qsave(mir_data, mir_file)

  cli::cli_alert_success("OSSL data downloaded and cached successfully")

  return(invisible(list(location = location_file, lab = lab_file, mir = mir_file)))
}

old_get_ossl_data_path <- function(type = c("location", "lab", "mir")) {
  type <- match.arg(type)
  cache_dir <- tools::R_user_dir("horizons", "cache")
  file_map  <- list(location = "ossl_location_data.qs",
                    lab      = "ossl_lab_data.qs",
                    mir      = "ossl_mir_raw.qs")
  file_path <- file.path(cache_dir, file_map[[type]])
  if (file.exists(file_path)) {
    return(file_path)
  } else {
    return(NULL)
  }
}

old_get_processed_mir_path <- function() {
  cache_dir <- tools::R_user_dir("horizons", "cache")
  processed_file <- file.path(cache_dir, "ossl_mir_processed.qs")
  if (file.exists(processed_file)) {
    return(processed_file)
  } else {
    return(NULL)
  }
}

## ---------------------------------------------------------------------------
## Main Prediction Function
## ---------------------------------------------------------------------------

predict_covariates <- function(covariates,
                               input_data,
                               verbose   = TRUE,
                               refresh   = FALSE,
                               cache_dir = tools::R_user_dir("horizons", "cache"),
                               parallel  = FALSE,
                               n_workers = NULL,
                               allow_nested = FALSE) {

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

  old_safely_execute(expr          = {download_ossl_data(covariates = covariates,
                                                     max_samples = NULL)},
                 default_value = NULL,
                 error_message = "Error downloading OSSL data for {covariates}") -> training_data_safe

  training_data <- training_data_safe$result

  if(is.null(training_data)) {
    cli::cli_abort("Aborting: OSSL data loading failed.")
  }

  if(verbose) cli::cli_progress_step("OSSL data loaded and processed.")

  ## DEBUG: Check what columns training_data has BEFORE PCA
  cli::cli_alert_info("DEBUG BEFORE PCA: training_data has {ncol(training_data)} columns")
  cli::cli_alert_info("DEBUG BEFORE PCA: First 20 columns: {paste(head(names(training_data), 20), collapse = ', ')}")
  cli::cli_alert_info("DEBUG BEFORE PCA: Covariate columns present? {paste(covariates[covariates %in% names(training_data)], collapse = ', ')}")

  ## ---------------------------------------------------------------------------
  ## Step 3: Reduce dimensions
  ## ---------------------------------------------------------------------------

  old_safely_execute(expr          = {reduce_dimensions_pca(training_data = training_data,
                                                        new_data      = input_data,
                                                        parallel      = parallel,
                                                        n_workers     = n_workers,
                                                        allow_nested  = allow_nested)},
                 default_value = NULL,
                 error_message = "Error during dimensionality reduction") -> reduced_dimensions_data_safe

  reduced_dimensions_data <- reduced_dimensions_data_safe$result

  if(is.null(reduced_dimensions_data)) {
    cli::cli_abort("Aborting: Dimensionality reduction failed.")
  }

  ## ---------------------------------------------------------------------------

  training_data <- reduced_dimensions_data$training_data
  input_data    <- reduced_dimensions_data$new_data

  ## DEBUG: Check what columns training_data has AFTER PCA
  cli::cli_alert_info("DEBUG AFTER PCA: training_data has {ncol(training_data)} columns")
  cli::cli_alert_info("DEBUG AFTER PCA: First 20 columns: {paste(head(names(training_data), 20), collapse = ', ')}")
  cli::cli_alert_info("DEBUG AFTER PCA: Covariate columns present? {paste(covariates[covariates %in% names(training_data)], collapse = ', ')}")

  if(verbose) cli::cli_progress_step("Dimensionality reduction complete.")

  ## ---------------------------------------------------------------------------
  ## Step 4: Cluster the new data
  ## ---------------------------------------------------------------------------

  if(verbose) cli::cli_progress_step("Running clustering analysis...")

  old_safely_execute(expr          = {cluster_spectral_data(input_data    = input_data,
                                                        parallel      = parallel,
                                                        n_workers     = n_workers,
                                                        allow_nested  = allow_nested)},
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

  old_safely_execute(expr          = {create_clustered_subsets(training_data = training_data,
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

  # Diagnostic: Show cluster sizes and columns
  for (i in seq_along(training_data_clustered)) {
    cli::cli_alert_info("DEBUG CLUSTER: Cluster_{i} has {nrow(training_data_clustered[[i]])} samples, {ncol(training_data_clustered[[i]])} columns")
    cli::cli_alert_info("DEBUG CLUSTER: Cluster_{i} last 5 columns: {paste(tail(names(training_data_clustered[[i]]), 5), collapse = ', ')}")
  }

  splits        <- purrr::map(training_data_clustered, ~ rsample::initial_split(.x, prop = 0.95, strata = NULL))
  training_data <- purrr::map(splits, rsample::training)
  holdout_data  <- purrr::map(splits, rsample::testing)

  # Diagnostic: Show training sizes after split
  for (i in seq_along(training_data)) {
    cli::cli_alert_info("DEBUG CLUSTER: Cluster_{i} training set: {nrow(training_data[[i]])} samples, {ncol(training_data[[i]])} columns")
    cli::cli_alert_info("DEBUG CLUSTER: Cluster_{i} training columns containing clay/ph: {paste(names(training_data[[i]])[grepl('clay|ph', names(training_data[[i]]), ignore.case = TRUE)], collapse = ', ')}")
  }

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

    calibrated_model <- fit_cubist_model(input_data    = training_subset,
                                         covariate     = covariate,
                                         verbose       = verbose,
                                         parallel      = parallel,
                                         n_workers     = n_workers,
                                         allow_nested  = allow_nested)

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

     old_safely_execute(expr          = {predict(object   = model_info$model,
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

  old_safely_execute(expr          = {evaluate_predictions(measured_data = holdout_measurements,
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
#' Cluster Mid-Infrared Spectra via PCA and K-Means
#'
#' Reduces high-dimensional mid-infrared (MIR) spectral data using principal component analysis (PCA),
#' then performs k-means clustering on the PCA scores. The optimal number of clusters is selected using
#' silhouette analysis, with a maximum cap of three clusters to preserve interpretability and modeling tractability.
#'
#' @param input_data A `tibble` containing MIR spectra for multiple samples. Must include numeric columns
#'   representing wavenumber features (e.g., `Dim.600`, `Dim.602`, ...). A column named `Sample_ID` is
#'   recommended but not required. Any non-spectral columns are preserved in the output.
#' @param parallel Logical. Enable parallel processing for clustering operations. Defaults to `FALSE` (safe for nested contexts).
#' @param n_workers Integer. Number of parallel workers. If `NULL`, uses `min(10, detectCores()-1)` for safety.
#' @param allow_nested Logical. Allow parallel processing even when already in parallel context. Defaults to `FALSE` (recommended).
#'
#' @return A `list` with the following components:
#' \itemize{
#'   \item \strong{input_data}: A `tibble` identical to the input but with a new `Cluster` column indicating cluster membership.
#'   \item \strong{pca_model}: A `prcomp` object containing the PCA model used for dimensionality reduction.
#'   \item \strong{kmeans_model}: A `kmeans` object fit to the retained PCA scores.
#'   \item \strong{ncomp}: An integer specifying the number of principal components retained (those explaining ≥99.5% of variance).
#' }
#'
#' @details
#' Spectral data is first scaled and projected into PCA space. The number of retained components
#' is selected to capture 99.5% of the variance. K-means clustering is then applied across a range
#' of candidate `k` values (2–20), and the silhouette score is used to identify the optimal cluster count.
#' For computational efficiency and downstream model interpretability, the number of clusters is capped at 3.
#'
#' This clustering routine is intended to support local model calibration in ensemble workflows.
#'
#' @examples
#' \dontrun{
#' clustered <- cluster_spectral_data(my_spectral_data)
#' head(clustered$input_data)
#' }
#'
#' @importFrom dplyr select mutate starts_with everything
#' @importFrom purrr map_dbl
#' @importFrom tibble as_tibble
#' @importFrom stats prcomp dist kmeans
#' @importFrom cluster silhouette
#' @importFrom glue glue
#' @importFrom cli cli_alert_success
#'
#' @export


cluster_spectral_data <- function(input_data,
                                  parallel = FALSE,
                                  n_workers = NULL,
                                  allow_nested = FALSE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: PCA Preparation — scale and reduce
  ## ---------------------------------------------------------------------------

  scaled_data <- input_data %>%
                  dplyr::select(dplyr::starts_with("Dim.")) %>%
                  scale() %>%
                  as.matrix()

  pca_model <- stats::prcomp(x      = scaled_data,
                             center = TRUE,
                             scale. = FALSE)

  ## ---------------------------------------------------------------------------
  ## Step 2: Determine number of components to retain (99.5% variance)
  ## ---------------------------------------------------------------------------

  n_components <- which(cumsum(pca_model$sdev^2) / sum(pca_model$sdev^2) >= 0.995)[1]

  pca_scores <- tibble::as_tibble(pca_model$x[, 1:n_components])

  ## ---------------------------------------------------------------------------
  ## Step 3: Identify optimal number of clusters via silhouette score
  ## ---------------------------------------------------------------------------

  k_range <- 2:20

  silhouette_scores <- purrr::map_dbl(k_range,
                                      function(k) {

    set.seed(0307)

    stats::kmeans(x       = pca_scores,
                  centers = k,
                  nstart  = 25) -> kmeans_model

    cluster_assignments <- as.integer(kmeans_model$cluster)

    cluster::silhouette(x    = cluster_assignments,
                        dist = stats::dist(pca_scores))[, 3] %>%
      mean(na.rm = TRUE)
  })

  optimal_k <- k_range[which.max(silhouette_scores)]

  if (optimal_k > 5) {

    cli::cli_alert_success("Optimal number of clusters is too big. Setting to 3 or we'll be here for a year.")
    optimal_k <- 3

  } else {

    cli::cli_alert_success(glue::glue("Optimal number of clusters: {optimal_k}"))

  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Run final k-means and attach cluster column
  ## ---------------------------------------------------------------------------

  stats::kmeans(x      = pca_scores,
                centers = optimal_k,
                nstart = 25) -> kmeans_model

  input_data %>%
    dplyr::mutate(Cluster = kmeans_model$cluster,
                  .before = dplyr::everything()) -> clustered_data

  ## ---------------------------------------------------------------------------
  ## Step 5: Return result
  ## ---------------------------------------------------------------------------

  return(list(
    input_data    = clustered_data,
    pca_model     = pca_model,
    kmeans_model  = kmeans_model,
    ncomp         = n_components
  ))
}




## ============================================================================


#' Create Clustered Training Subsets from PCA Scores
#'
#' Projects training samples into PCA space, assigns them to clusters based on
#' proximity to k-means centroids, and selects a representative subset from each cluster.
#' The most central samples—defined by Euclidean distance in PCA space—are retained according to
#' a user-specified coverage threshold.
#'
#' @param training_data A `tibble` of MIR spectral data already projected into principal component space
#'   (i.e., columns named `Dim.1`, `Dim.2`, ...). All non-spectral columns are retained in the output.
#' @param pca_model A PCA model object returned by `stats::prcomp()` used for projection into PCA space.
#' @param kmeans_model A fitted `kmeans` object whose cluster centers define proximity for sample selection.
#' @param n_components Integer. Number of PCA components to use during projection (e.g., 50).
#' @param coverage Numeric (between 0 and 1). Proportion of samples per cluster to retain based on distance
#'   to the cluster centroid. Defaults to `0.8`.
#'
#' @return A named `list` of `tibble`s. Each element represents a training subset corresponding to one cluster
#'   (e.g., `"Cluster_1"`, `"Cluster_2"`, ...), retaining only the samples closest to the centroid in PCA space.
#'
#' @details
#' This function is designed to facilitate localized model calibration by filtering training samples
#' based on their similarity to cluster centers. This approach reduces outlier influence and computational
#' burden, particularly when paired with local model ensembles. Coverage values near `1.0` retain most samples,
#' while lower values emphasize only the most representative.
#'
#' Cluster assignments are made using nearest-centroid classification in PCA space.
#' Euclidean distances are used for all calculations.
#'
#' @examples
#' \dontrun{
#' training_subsets <- create_clustered_subsets(
#'   training_data = my_training_data,
#'   pca_model     = my_pca_model,
#'   kmeans_model  = my_kmeans_model,
#'   n_components  = 50,
#'   coverage      = 0.8
#' )
#' }
#'
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom tibble as_tibble
#' @importFrom stats predict quantile
#' @importFrom cli cli_alert_success
#' @export


create_clustered_subsets <- function(training_data,
                                     pca_model,
                                     kmeans_model,
                                     n_components,
                                     coverage = 0.8) {

  cli::cli_alert_info("DEBUG SUBSET: Input training_data has {ncol(training_data)} columns")
  cli::cli_alert_info("DEBUG SUBSET: First 20 columns: {paste(head(names(training_data), 20), collapse = ', ')}")
  cli::cli_alert_info("DEBUG SUBSET: Last 10 columns: {paste(tail(names(training_data), 10), collapse = ', ')}")
  cli::cli_alert_info("DEBUG SUBSET: Columns containing 'clay' or 'ph': {paste(names(training_data)[grepl('clay|ph', names(training_data), ignore.case = TRUE)], collapse = ', ')}")

  ## ---------------------------------------------------------------------------
  ## Step 1: Project training data into PCA space
  ## ---------------------------------------------------------------------------

  training_data %>%
    dplyr::select(tidyselect::starts_with("Dim.")) %>%
    scale() %>%
    as.matrix() -> scaled_data

  stats::predict(pca_model,
                 newdata = scaled_data)[, 1:n_components] %>%
    tibble::as_tibble() -> pca_scores

  ## ---------------------------------------------------------------------------
  ## Step 2: Assign training samples to clusters
  ## ---------------------------------------------------------------------------

  apply(pca_scores, 1, function(x) {
    apply(kmeans_model$centers, 1, function(center) sqrt(sum((x - center)^2))) %>%
      which.min()
  }) -> assigned_clusters

  ## ---------------------------------------------------------------------------
  ## Step 3: Build cluster-specific subsets
  ## ---------------------------------------------------------------------------

  purrr::map(unique(assigned_clusters), function(cluster_id) {

    cluster_indices <- which(assigned_clusters == cluster_id)

    cluster_scores <- pca_scores[cluster_indices, ]

    center <- kmeans_model$centers[cluster_id, ]

    distances <- apply(cluster_scores, 1, function(x) sqrt(sum((x - center)^2)))

    threshold <- stats::quantile(distances, probs = coverage)

    selected_indices <- cluster_indices[which(distances <= threshold)]

    training_data[selected_indices, ]

  }) -> training_subsets

  ## ---------------------------------------------------------------------------
  ## Step 4: Return named list
  ## ---------------------------------------------------------------------------

  names(training_subsets) <- paste0("Cluster_", seq_along(training_subsets))

  cli::cli_alert_success("Created {.val {length(training_subsets)}} clustered training subsets.")

  return(training_subsets)

}


## ============================================================================


#' Fit a Cubist Model Using PCA-Transformed Spectral Data
#'
#' Builds, tunes, and evaluates a Cubist model to predict a single soil covariate
#' using PCA-transformed MIR spectra. The function applies a max entropy grid search
#' followed by Bayesian optimization to tune hyperparameters, then fits the final model
#' and returns both performance metrics and workflow objects.
#'
#' @param input_data A `tibble` or `data.frame` containing PCA-transformed predictors
#'   (`Dim.1`, `Dim.2`, ..., `Dim.n`) and one numeric column corresponding to the
#'   covariate to be modeled. All rows with `NA` in the response are removed.
#' @param covariate A character string. Name of the column to use as the response variable (e.g., `"Sand"`, `"pH"`).
#' @param verbose Logical. If `TRUE`, prints progress messages using `cli::cli_*()` during model training. Defaults to `FALSE`.
#' @param parallel Logical. Enable parallel processing for hyperparameter tuning. Defaults to `FALSE` (safe for nested contexts).
#' @param n_workers Integer. Number of parallel workers for tuning. If `NULL`, uses `min(10, detectCores()-1)` for safety.
#' @param allow_nested Logical. Allow parallel processing even when already in parallel context. Defaults to `FALSE` (recommended).
#'
#' @return A named `list` with the following components:
#' \itemize{
#'   \item \strong{Model}: A fitted Cubist workflow (`workflow`) trained on the full training set.
#'   \item \strong{Best_Parameters}: A `tibble` containing the best hyperparameter configuration selected via Bayesian optimization.
#'   \item \strong{Evaluation}: A `tibble` of evaluation metrics (e.g., RMSE, R², CCC) computed on the hold-out set using `soilspec::eval()`.
#' }
#'
#' @details
#' The modeling pipeline follows three main stages:
#' \enumerate{
#'   \item Stratified data split into training/testing sets.
#'   \item Max entropy grid search for tuning `committees`, `neighbors`, and `max_rules` using `tune::tune_grid()`.
#'   \item Bayesian optimization using `tune::tune_bayes()` for refinement.
#' }
#' The model is finalized with `tune::finalize_workflow()` and fitted to the training set.
#' Performance metrics are computed on the hold-out set using `tune::last_fit()` and `soilspec::eval()`.
#'
#' Parallel tuning is enabled with `future::plan(multisession)` and automatically reset afterward.
#' All error handling is wrapped with `safely_execute()` for fault-tolerant orchestration.
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(
#'   Dim.1 = rnorm(100),
#'   Dim.2 = rnorm(100),
#'   Dim.3 = rnorm(100),
#'   Sand  = runif(100, 50, 80)
#' )
#'
#' result <- fit_cubist_model(input_data = df, covariate = "Sand", verbose = TRUE)
#' result$Evaluation
#' }
#'
#' @seealso
#' \code{\link{predict_covariates}}, \code{\link{evaluate_predictions}}, \code{\link{reduce_dimensions_pca}}
#'
#' @importFrom dplyr select rename mutate bind_rows starts_with
#' @importFrom purrr map
#' @importFrom tidyr drop_na
#' @importFrom tibble tibble as_tibble
#' @importFrom stats quantile
#' @importFrom rsample initial_split training testing vfold_cv
#' @importFrom dials grid_space_filling neighbors
#' @importFrom rules committees max_rules
#' @importFrom parsnip cubist_rules set_engine set_mode fit
#' @importFrom workflows workflow add_model add_formula
#' @importFrom tune tune_grid tune_bayes control_grid control_bayes select_best finalize_workflow last_fit collect_predictions
#' @importFrom future plan multisession sequential
#' @importFrom glue glue
#' @importFrom cli cli_progress_step cli_alert_danger cli_alert_warning
#' @export


fit_cubist_model <- function(input_data,
                             covariate,
                             verbose,
                             parallel = FALSE,
                             n_workers = NULL,
                             allow_nested = FALSE) {

  ## ---------------------------------------------------------------------------
  ## Step 0: Data validation
  ## ---------------------------------------------------------------------------

  if(verbose) cli::cli_progress_step("Preparing training data for covariate {.val {covariate}}")

  cli::cli_alert_info("DEBUG FIT: Looking for covariate '{covariate}' in columns: {paste(head(colnames(input_data), 20), collapse = ', ')}")

  covariate_name <- grep(covariate, colnames(input_data), value = TRUE)

  cli::cli_alert_info("DEBUG FIT: grep found: {paste(covariate_name, collapse = ', ')}")

  if (length(covariate_name) == 0) {
    cli::cli_alert_danger("Covariate '{covariate}' not found in input_data.")
    return(NULL)
  }

  input_data$Response <- input_data[[covariate_name]]

  input_data %>%
    dplyr::select(Response,
                  dplyr::starts_with("Dim.")) %>%
    tidyr::drop_na() -> input_data

  if (nrow(input_data) == 0) {
    cli::cli_alert_warning("Input data for covariate '{covariate}' is empty after dropping NAs. Cannot train model.")
    return(NULL)
  }

  ## ---------------------------------------------------------------------------
  ## Step 1: Data Preparation
  ## ---------------------------------------------------------------------------

  set.seed(0307)

  old_safely_execute(expr          = {rsample::initial_split(input_data, strata = Response)},
                 default_value = NULL,
                 error_message = glue::glue("Failed to create initial data split for {covariate}")) -> split_data_safe

  split_data <- split_data_safe$result

  if (is.null(split_data)) return(NULL)

  Train_Data <- rsample::training(split_data)
  Test_Data  <- rsample::testing(split_data)


  old_safely_execute(expr          = {rsample::vfold_cv(Train_Data, v = 10, strata = Response)},
                 default_value = NULL,
                 error_message = glue::glue("Failed to create CV folds for {covariate}")) -> CV_Folds_safe

  CV_Folds <- CV_Folds_safe$result

  if (is.null(CV_Folds)) return(NULL)

  ## ---------------------------------------------------------------------------
  ## Step 2: Define and Tune Cubist Model
  ## ---------------------------------------------------------------------------

    ## ---------------------------------------------------------------------------
    ## Stage 1: Define cubist model specifications
    ## ---------------------------------------------------------------------------

    parsnip::cubist_rules(committees = tune::tune(),
                          neighbors  = tune::tune(),
                          max_rules  = tune::tune()) %>%
      parsnip::set_engine("Cubist") %>%
      parsnip::set_mode("regression") -> model_spec

    workflows::workflow() %>%
      workflows::add_model(model_spec) %>%
      workflows::add_formula(Response ~ .) -> wf

    ## ---------------------------------------------------------------------------
    ## Stage 2: Initial Grid Search for Hyperparameter Tuning
    ## ---------------------------------------------------------------------------

    ## Step 2.1: Configure parallel processing with safety controls -----------

    # Determine safe worker count
    if (is.null(n_workers)) {
      max_cores <- parallel::detectCores(logical = TRUE)
      n_workers <- pmax(1, pmin(max_cores - 1, 10))  # Cap at 10 for safety
    }

    # Check for nested parallelization using number of workers (simple and reliable)
    current_workers <- future::nbrOfWorkers()
    if (!allow_nested && current_workers > 1) {
      if(verbose) cli::cli_alert_warning("Nested parallelization detected ({current_workers} workers active). Setting parallel=FALSE for safety")
      parallel <- FALSE
    }

    # Set parallel plan with proper cleanup
    if (parallel && n_workers > 1) {
      old_plan <- future::plan()
      on.exit(future::plan(old_plan), add = TRUE)
      
      old_safely_execute(expr          = {future::plan(future::multisession, workers = n_workers)},
                     default_value = NULL,
                     error_message = glue::glue("Failed to set parallel plan for {covariate} tuning"))
    } else {
      if(verbose) cli::cli_alert_info("Using sequential processing for {.val {covariate}} (parallel={parallel}, n_workers={n_workers})")
    }

    dials::grid_space_filling(rules::committees(range = c(2L, 20L)),
                              dials::neighbors(range = c(2L, 9L)),
                              dials::max_rules(),
                              size = 5,
                              type = "max_entropy") -> grid

    if(verbose) cli::cli_progress_step("Running grid search for {.val {covariate}}")

    old_safely_execute(expr          = {tune::tune_grid(object    = wf,
                                   resamples = CV_Folds,
                                   grid      = grid,
                                   control   = tune::control_grid(allow_par = TRUE))},
                   default_value = NULL,
                   error_message = glue::glue("Grid tuning faliled for {covariate}")) -> grid_res_safe

    grid_res <- grid_res_safe$result

    if(is.null(grid_res)){
      old_safely_execute(expr = {future::plan(sequential)})
      return(NULL)
    }

    ## ---------------------------------------------------------------------------
    ## Stage 3: Bayesian Optimization for Hyperparameter Tuning
    ## ---------------------------------------------------------------------------

    if(verbose) cli::cli_progress_step("Running Bayesian optimization for {.val {covariate}}")

    old_safely_execute(expr        = {tune::tune_bayes(object    = wf,
                                                   resamples = CV_Folds,
                                                   initial   = grid_res,
                                                   iter      = 2,
                                                   control   = tune::control_bayes(allow_par = TRUE))},
                   default_value = NULL,
                   error_message = glue::glue("Bayesian tuning failed for {covariate}")) -> bayes_res_safe

    bayes_res <- bayes_res_safe$result

    if(is.null(bayes_res)){
      old_safely_execute(expr = {future::plan(sequential)})
      return(NULL)
    }

    ## ---------------------------------------------------------------------------
    ## Stage 4: Finalizing workflow
    ## ---------------------------------------------------------------------------

    cli::cli_progress_step("Finalizing workflow for {.val {covariate}}")

    best_params <- tune::select_best(bayes_res,
                                     metric = "rmse")

    final_wf    <- tune::finalize_workflow(wf,
                                           best_params)

  ## ---------------------------------------------------------------------------
  ## Step 3: Final Fit and Evaluation
  ## ---------------------------------------------------------------------------

  if(verbose) cli::cli_progress_step("Evaluating final model for {.val {covariate}}")

  old_safely_execute(expr          = {final_wf %>%
                                    tune::last_fit(split_data) %>%
                                    tune::collect_predictions() %>%
                                    dplyr::rename(Predicted = .pred) %>%
                                    drop_na() %>%
                                    soilspec::eval(pred = .$Predicted,
                                                   obs  = .$Response,
                                                   obj  = "quant")},
                 default_value = NULL,
                 error_message = ("Failed to perform last_fit() or collect_predictions() for the current model.")) -> eval_safe

  eval <- eval_safe$result

  if (is.null(eval)) return(NULL)

  old_safely_execute(expr          = {final_wf %>% parsnip::fit(Train_Data)},
                 default_value = NULL,
                 error_message = glue::glue("Failed to fit final workflow for {covariate}")) -> fitted_model_safe

  fitted_model <- fitted_model_safe$result

  if(is.null(fitted_model)) return(NULL)

  return(list(Model           = fitted_model,
              Best_Parameters = best_params,
              Evaluation      = eval))

}


## ============================================================================


#' Download and Preprocess OSSL Spectral and Covariate Data
#'
#' Retrieves topsoil mid-infrared (MIR) spectral data and associated covariate measurements
#' from the Open Soil Spectroscopy Library (OSSL). Applies Savitzky-Golay smoothing followed by
#' standard normal variate (SNV) preprocessing to the spectra, reshapes and filters lab data,
#' and joins all components into a unified dataset. Final outputs are cached for reuse.
#'
#' @param covariates A character vector of covariate names to retrieve (e.g., `"Sand"`, `"pH"`, `"SOC"`).
#'   Covariate names must match those defined in the internal OSSL variable dictionary.
#' @param window_size Integer. Width of the Savitzky-Golay smoothing window (must be odd). Defaults to `9`.
#' @param max_samples Optional integer. If supplied, limits the number of samples processed—useful for debugging or testing.
#'   If `NULL`, all available samples are used.
#' @param bounding_box Currently unused. Placeholder for future spatial subsetting based on bounding box coordinates.
#' @param parallel Logical. Enable parallel processing for spectral preprocessing. Defaults to `FALSE` (safe for nested contexts).
#' @param n_workers Integer. Number of parallel workers. If `NULL`, uses `min(10, detectCores()-1)` for safety.
#' @param allow_nested Logical. Allow parallel processing even when already in parallel context. Defaults to `FALSE` (recommended).
#'
#' @return A `tibble` containing:
#' \itemize{
#'   \item Preprocessed MIR spectra: SNV-SG0 transformed reflectance values at 2 cm⁻¹ resolution between 600–4000 cm⁻¹.
#'   \item Covariate data: Wide-format table of measured values for requested soil properties.
#'   \item Sample index: A sequential index column for internal referencing.
#' }
#'
#' @details
#' This function follows a multi-step pipeline:
#' \enumerate{
#'   \item Read and filter the internal OSSL variable dictionary.
#'   \item Load cached raw data files (location, lab, and MIR) from disk using `qs::qread()`.
#'   \item Filter and reshape metadata to include only topsoil layers and desired covariates.
#'   \item Load and optionally subsample MIR spectra.
#'   \item Apply parallelized SNV and Savitzky-Golay preprocessing via `furrr::future_map()`.
#'   \item Cache the resulting processed spectra using `qs::qsave()` for future reuse.
#'   \item Join processed spectra with lab data by `Layer_ID`.
#' }
#'
#' If previously processed MIR spectra exist in cache, they are loaded automatically to avoid reprocessing.
#' Progress and errors are communicated through `cli`-based messaging and `safely_execute()` wrappers.
#'
#' @examples
#' \dontrun{
#' # Download and preprocess OSSL data for pH and Sand
#' ossl_data <- download_ossl_data(covariates = c("pH", "Sand"))
#'
#' # Preview structure
#' glimpse(ossl_data)
#' }
#'
#' @seealso
#' \code{\link{predict_covariates}}, \code{\link{create_input_data}}
#'
#' @importFrom dplyr filter select distinct mutate rename group_by ungroup inner_join starts_with row_number
#' @importFrom purrr map map_chr map_dfr pluck
#' @importFrom tidyr pivot_longer pivot_wider drop_na
#' @importFrom tibble tibble as_tibble
#' @importFrom stats quantile
#' @importFrom readr read_csv
#' @importFrom qs qread qread_url qsave
#' @importFrom future plan multisession sequential
#' @importFrom furrr future_map furrr_options
#' @importFrom prospectr savitzkyGolay standardNormalVariate
#' @importFrom progressr with_progress handlers progressor
#' @importFrom here here
#' @importFrom glue glue
#' @importFrom cli cli_progress_message cli_progress_step cli_alert_success cli_alert_danger cli_alert_info cli_abort
#' @importFrom stringr str_split_i
#' @export


download_ossl_data <- function(covariates,
                               window_size = 9,
                               max_samples = NULL,
                               parallel = FALSE,
                               n_workers = NULL,
                               allow_nested = FALSE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Read the OSSL data dictionary
  ## ---------------------------------------------------------------------------

  ## Load from package sysdata if available (when load_all() was called)
  if (!exists("OSSL_Data_Dictionary", envir = .GlobalEnv)) {
    ## If not in global env, try to load from horizons package sysdata
    if (file.exists("R/sysdata.rda")) {
      load("R/sysdata.rda", envir = environment())
    } else {
      cli::cli_abort("Aborting: OSSL_Data_Dictionary not found. Run devtools::load_all() first.")
    }
  } else {
    OSSL_Data_Dictionary <- get("OSSL_Data_Dictionary", envir = .GlobalEnv)
  }

  OSSL_Data_Dictionary %>%
    dplyr::filter(Sam_Include == TRUE) -> OSSL_Data_Dictionary

  if (is.null(OSSL_Data_Dictionary) || nrow(OSSL_Data_Dictionary) == 0) {
    cli::cli_abort("Aborting: Failed to read OSSL data dictionary.")
  }


  ## ---------------------------------------------------------------------------
  ## Step 2: Ensure required data is downloaded
  ## ---------------------------------------------------------------------------

  old_safely_execute(expr          = {old_download_horizons_data(force = FALSE,
                                                         ask   = TRUE)},
                 default_value = NULL,
                 log_error     = FALSE,
                 error_message = "Failed to download the required OSSL data") -> download_result_safe

  download_result <- download_result_safe$result

  if (is.null(download_result)) {
    cli::cli_abort("Aborting: Unable to download or locate required OSSL data.")
  }

  cli::cli_progress_step("Required OSSL raw data is available.")

  ## ---------------------------------------------------------------------------
  ## Step 3: Load data from cache
  ## ---------------------------------------------------------------------------

  old_safely_execute(expr          = {qs::qread(old_get_ossl_data_path("location"))},
                 default_value = NULL,
                 error_message = "Failed to load the required OSSL location data from cache")   -> location_data_safe

  old_safely_execute(expr          = {qs::qread(old_get_ossl_data_path("lab"))},
                 default_value = NULL,
                 error_message = "Failed to load the required OSSL laboratory data from cache") -> lab_data_safe

  old_safely_execute(expr          = {qs::qread(old_get_ossl_data_path("mir"))},
                 default_value = NULL,
                 error_message = "Failed to load the required OSSL MIR data from cache")        -> mir_data_safe

  location_data <- location_data_safe$result
  lab_data      <- lab_data_safe$result
  mir_data      <- mir_data_safe$result

  if(is.null(location_data) || is.null(lab_data) || is.null(mir_data)) {
    cli::cli_abort("Aborting: One or more required OSSL datasets failed to load from cache.")
  }

  cli::cli_progress_step("Required datasets successfully loaded from cache.")

  ## ---------------------------------------------------------------------------
  ## Step 4: Subset and clean location data
  ## ---------------------------------------------------------------------------

  old_safely_execute(expr          = {location_data %>%
                                   dplyr::filter(dataset.code_ascii_txt == "KSSL.SSL") %>%
                                   dplyr::select(Layer_ID  = id.layer_uuid_txt,
                                                 Longitude = longitude.point_wgs84_dd,
                                                 Latitude  = latitude.point_wgs84_dd,
                                                Top_Depth = layer.upper.depth_usda_cm) %>%
                                   dplyr::filter(Top_Depth == 0)},
                 default_value = NULL,
                 error_message = "Failed to subset/clean OSSL location metadata") -> location_data_safe

  location_data <- location_data_safe$result

  if(is.null(location_data)){
    cli::cli_abort("Aborting: Issues with subsetting or cleaning OSSL location metadata.")
  }

  cli::cli_alert_info("DEBUG: Location data after filtering: {nrow(location_data)} rows")
  cli::cli_progress_step("Location data processed and ready to go.")

  ## ---------------------------------------------------------------------------
  ## Step 5: Subset and reshape lab data
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Downloading and joining lab measurements")

  ## DEBUG: Break down the lab data pipeline step by step
  lab_temp1 <- lab_data %>%
    dplyr::distinct() %>%
    dplyr::rename(Layer_ID = id.layer_uuid_txt) %>%
    dplyr::filter(Layer_ID %in% location_data$Layer_ID)
  cli::cli_alert_info("DEBUG LAB: After location filter: {nrow(lab_temp1)} rows")

  lab_temp2 <- lab_temp1 %>%
    dplyr::select(-dataset.code_ascii_txt, -efferv_usda.a479_class) %>%
    tidyr::pivot_longer(cols = -Layer_ID, names_to = "ossl_name_level1", values_to = "Measured_Value")
  cli::cli_alert_info("DEBUG LAB: After pivot_longer: {nrow(lab_temp2)} rows")

  lab_temp3 <- lab_temp2 %>%
    dplyr::left_join(OSSL_Data_Dictionary, by = "ossl_name_level1")
  cli::cli_alert_info("DEBUG LAB: After dictionary join: {nrow(lab_temp3)} rows")
  cli::cli_alert_info("DEBUG LAB: How many NAs in 'analyte' column: {sum(is.na(lab_temp3$analyte))}")

  lab_temp4 <- lab_temp3 %>%
    dplyr::select(Layer_ID, analyte, target_unit, Measured_Value) %>%
    tidyr::drop_na()
  cli::cli_alert_info("DEBUG LAB: After drop_na: {nrow(lab_temp4)} rows")

  lab_temp5 <- lab_temp4 %>%
    dplyr::mutate(Analyte_Base = stringr::str_split_i(analyte, ",", 1))

  all_analytes <- sort(unique(lab_temp5$Analyte_Base))
  cli::cli_alert_info("DEBUG LAB: Found {length(all_analytes)} unique analytes total")
  cli::cli_alert_info("DEBUG LAB: First 10: {paste(all_analytes[1:min(10, length(all_analytes))], collapse = ', ')}")

  # Check for clay-related analytes
  clay_analytes <- all_analytes[grepl("clay|Clay|CLAY", all_analytes, ignore.case = TRUE)]
  if (length(clay_analytes) > 0) {
    cli::cli_alert_info("DEBUG LAB: Clay-related analytes: {paste(clay_analytes, collapse = ', ')}")
  } else {
    cli::cli_alert_warning("DEBUG LAB: NO clay analytes found!")
  }

  cli::cli_alert_info("DEBUG LAB: Covariates requested: {paste(covariates, collapse = ', ')}")

  # Fix: Make case-insensitive matching AND preserve user's requested case
  lab_temp6 <- lab_temp5 %>%
    dplyr::filter(tolower(Analyte_Base) %in% tolower(covariates))
  cli::cli_alert_info("DEBUG LAB: After covariate filter (case-insensitive): {nrow(lab_temp6)} rows")

  # Create a lookup to map OSSL names back to user's requested case
  case_lookup <- setNames(covariates, tolower(covariates))

  old_safely_execute(expr          = {lab_temp6 %>%
                                   dplyr::mutate(final_variable = case_lookup[tolower(Analyte_Base)]) %>%
                                   dplyr::select(Layer_ID,
                                                 final_variable,
                                                 Measured_Value) %>%
                                   tidyr::pivot_wider(names_from  = final_variable,
                                                      values_from = Measured_Value)},
                 default_value = NULL,
                 error_message = "Failed to process OSSL lab measurements") -> lab_data_safe

  lab_data <- lab_data_safe$result

  if (is.null(lab_data)) {
    cli::cli_abort("Aborting: OSSL lab measurements were corrupted at some point.")
  }

  cli::cli_alert_info("DEBUG: Lab data after processing: {nrow(lab_data)} rows, {ncol(lab_data)} cols")
  cli::cli_alert_info("DEBUG: Lab data columns: {paste(names(lab_data), collapse = ', ')}")

  ## ---------------------------------------------------------------------------
  ## Step 6: Load OSSL MIR spectra from local cache if available.
  ## ---------------------------------------------------------------------------

  old_safely_execute(expr          = {old_get_processed_mir_path()},
                 default_value = NULL,
                 error_message = "Failed to get the path for processed  MIR spectra") -> processed_mir_path_safe

  processed_mir_path <- processed_mir_path_safe$result

  processed_mir <- NULL

  if (!is.null(processed_mir_path)) {

    cli::cli_progress_step("Loading processed MIR spectra from the cache at {processed_mir_path}")

    old_safely_execute(expr          = {qs::qread(processed_mir_path)},
                   default_value = NULL,
                   error_message = glue::glue("Failed to read processed MIR data from {processed_mir_path}")) -> processed_mir_safe

    processed_mir <- processed_mir_safe$result

    if (!is.null(processed_mir)) {
      cli::cli_progress_step("Successfully loaded processed MIR data from cache.")
    }

  }


  ## ---------------------------------------------------------------------------
  ## Step 7: If necessary, process raw OSSL MIR spectra
  ## ---------------------------------------------------------------------------

  if(is.null(processed_mir)) {

    cli::cli_progress_step("No cached processed MIR spectra found. Processing raw OSSL MIR spectra.")
    cli::cli_progress_step("Filtering and reshaping MIR data.")

    old_safely_execute(expr          = {mir_data %>%
                                      dplyr::rename(Layer_ID = id.layer_uuid_txt) %>%
                                      dplyr::filter(scan.mir.model.name_utf8_txt == "Bruker Vertex 70 with HTS-XT accessory",
                                                    Layer_ID %in% location_data$Layer_ID) %>%
                                      dplyr::select(Layer_ID,
                                      dplyr::starts_with("scan_mir.")) %>%
                                      tidyr::pivot_longer(cols      = -Layer_ID,
                                                          names_to  = "Wavenumber",
                                                          values_to = "Absorbance") %>%
                                      dplyr::mutate(Wavenumber = stringr::str_split_i(Wavenumber, "\\.", 2),
                                      Wavenumber = stringr::str_split_i(Wavenumber, "_", 1))},
                  default_value = NULL,
                   error_message = "Failed to filter/select/pivot the raw OSSL MIR spectra") -> mir_data_safe

    mir_data <- mir_data_safe$result


  if(is.null(mir_data)){
    cli::cli_abort("Aborting: Raw OSSL MIR spectra preprocessing failed.")
  }

  ## ---------------------------------------------------------------------------

  mir_split <- split(mir_data, mir_data$Layer_ID)

  if (!is.null(max_samples)) {
    sampled_layer_ids <- sample(names(mir_split), size = min(max_samples, length(mir_split)))
    mir_split <- mir_split[sampled_layer_ids]
    cli::cli_alert_info("Processing only {length(mir_split)} samples for testing/debugging.")
  }

  ## ---------------------------------------------------------------------------
  ## Parallel processing setup with safety controls
  ## ---------------------------------------------------------------------------
  
  # Store original plan for cleanup
  original_plan <- future::plan()
  on.exit(future::plan(original_plan), add = TRUE)
  
  if (parallel) {
    # Check for nested parallelization using number of workers (simple and reliable)
    current_workers <- future::nbrOfWorkers()
    if (!allow_nested && current_workers > 1) {
      cli::cli_alert_warning("Nested parallelization detected in download_ossl_data ({current_workers} workers active). Setting parallel=FALSE for safety")
      parallel <- FALSE
    }
  }
  
  if (parallel) {
    # Set worker count with safety cap
    if (is.null(n_workers)) {
      available_cores <- parallel::detectCores(logical = TRUE)
      n_workers <- min(10, available_cores - 1)
      cli::cli_alert_info("Using {n_workers} workers for MIR spectral preprocessing (capped for safety)")
    } else {
      # Still apply safety cap to user-specified workers
      n_workers <- min(n_workers, 10)
      if (n_workers != n_workers) {
        cli::cli_alert_warning("Worker count capped at 10 for safety (requested: {n_workers})")
      }
    }
    
    old_safely_execute(expr          = {future::plan(future::multisession,
                                                 workers = n_workers)},
                   default_value = NULL,
                   error_message = "Failed to set parallel plan for processing")
  } else {
    old_safely_execute(expr          = {future::plan(sequential)},
                   default_value = NULL,
                   error_message = "Failed to set sequential plan for processing")
  }

  cli::cli_progress_step("Applying SNV and SG transformation to MIR data.")

  progressr::handlers("txtprogressbar")

  old_safely_execute(expr = {progressr::with_progress({

                          p <- progressr::progressor(along = mir_split)

                         furrr::future_map(mir_split,
                                           function(sample_data)
                                           {

                                             p(sprintf("Processing %s", unique(sample_data$Layer_ID)))

                                             sample_data <- dplyr::arrange(sample_data,
                                                                           as.numeric(Wavenumber))

                                             Start_Row <- 1 + ((window_size - 1) / 2)
                                             End_Row   <- nrow(sample_data) - ((window_size - 1) / 2)

                                             sample_data$Absorbance %>%
                                               as.matrix() %>%
                                               t() %>%
                                               prospectr::savitzkyGolay(m = 0,
                                                                        p = 1,
                                                                        w = window_size) %>%
                                               prospectr::standardNormalVariate() %>%
                                               t() %>%
                                               tibble::as_tibble(.name_repair = ~ "SNV_SG0_Absorbance") -> spectra_matrix

                                             dplyr::bind_cols(sample_data %>% dplyr::select(-Absorbance) %>% dplyr::slice(Start_Row:End_Row),
                                                              spectra_matrix) %>%
                                               tidyr::pivot_wider(names_from = Wavenumber, values_from = SNV_SG0_Absorbance)

                                           },
                                           .options = furrr::furrr_options(seed = TRUE)) %>%
                           purrr::list_rbind()
                       })},
                 default_value = NULL,
                 error_message = "Failed during parallel SNV/SG transformation of OSSL MIR spectra") -> processed_mir_safe

  processed_mir <- processed_mir_safe$result

  if (is.null(processed_mir)) {
    cli::cli_abort("Aborting: OSSL MIR spectra processing failed.")
  }

  qs::qsave(processed_mir, file.path(tools::R_user_dir("horizons", "cache"), "ossl_mir_processed.qs"))
  cli::cli_alert_success("Processed MIR data cached for future use.")

  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("OSSL MIR data processed.")
  cli::cli_alert_info("DEBUG: Processed MIR data: {nrow(processed_mir)} rows, {ncol(processed_mir)} cols")

 }
  ## ---------------------------------------------------------------------------

  cli::cli_alert_info("DEBUG: Before join - Lab: {nrow(lab_data)} rows, MIR: {nrow(processed_mir)} rows")

  old_safely_execute(expr       = {dplyr::inner_join(lab_data,
                                                 processed_mir,
                                                 by = "Layer_ID") %>%
                                dplyr::select(-Layer_ID) %>%
                                dplyr::mutate(Sample_Index = dplyr::row_number(),
                                              .before = dplyr::everything())},
              default_value = NULL,
              error_message = "Failed to join lab data with processed MIR spectra") -> OSSL_Data_safe

  OSSL_Data <- OSSL_Data_safe$result

  if(is.null(OSSL_Data)) {
    cli::cli_abort("Aborting: Joining lab data with processed MIR spectra failed.")
  }

  cli::cli_alert_info("DEBUG: Final joined OSSL_Data: {nrow(OSSL_Data)} rows, {ncol(OSSL_Data)} cols")

  ## ---------------------------------------------------------------------------

  cli::cli_alert_success("OSSL data download and preprocessing complete!")


  return(OSSL_Data)

}



## ============================================================================


#' Reduce Spectral Dimensionality Using PCA
#'
#' Performs principal component analysis (PCA) on training MIR spectral data, and projects both
#' training and new samples into the same reduced-dimensional space. PCA is applied using
#' `FactoMineR::PCA()` with scaling enabled, retaining up to 80 components.
#'
#' @param training_data A `data.frame` or `tibble` containing training MIR spectra with numeric
#'   wavenumber columns (typically `608`–`3992`) and any number of metadata columns (e.g., `Sample_ID`, `Project`).
#' @param new_data A `data.frame` or `tibble` containing new MIR spectra to be projected into PCA space.
#'   Must have the same spectral columns as `training_data`.
#' @param parallel Logical. Enable parallel processing for PCA operations. Defaults to `FALSE` (safe for nested contexts).
#' @param n_workers Integer. Number of parallel workers. If `NULL`, uses `min(10, detectCores()-1)` for safety.
#' @param allow_nested Logical. Allow parallel processing even when already in parallel context. Defaults to `FALSE` (recommended).
#'
#' @return A named `list` with two elements:
#' \itemize{
#'   \item \strong{training_data}: A `tibble` containing the PCA-transformed training data joined to its original metadata.
#'   \item \strong{new_data}: A `tibble` containing the PCA-transformed new data joined to its original metadata.
#' }
#'
#' @details
#' PCA is fit to the spectral columns of `training_data` using `FactoMineR::PCA()` with `scale.unit = TRUE`
#' and `ncp = 80`. The resulting PCA object is used to project `new_data` via `stats::predict()` to ensure
#' dimensional consistency across datasets.
#'
#' Spectral columns should be numeric and represent evenly spaced wavenumbers between ~600–4000 cm⁻¹.
#' All non-spectral columns are preserved and returned alongside the transformed scores.
#'
#' This function is intended for dimensionality reduction prior to clustering or model calibration
#' in MIR-based soil property prediction workflows.
#'
#' @examples
#' \dontrun{
#' reduced <- reduce_dimensions_pca(training_data = ossl_data, new_data = unknown_data)
#'
#' names(reduced)
#' head(reduced$training_data)
#' head(reduced$new_data)
#' }
#'
#' @importFrom dplyr select bind_cols
#' @importFrom tibble as_tibble
#' @importFrom FactoMineR PCA
#' @importFrom stats predict
#' @importFrom cli cli_progress_step
#' @export

reduce_dimensions_pca <- function(training_data,
                                  new_data,
                                  parallel = FALSE,
                                  n_workers = NULL,
                                  allow_nested = FALSE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Run PCA on training spectral columns
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Fitting PCA on training data.")

  ## DIAGNOSTIC: Check what columns we actually have
  spectral_cols <- names(training_data)[grepl("^[0-9]+$", names(training_data))]
  cli::cli_alert_info("DIAGNOSTIC: training_data dimensions: {nrow(training_data)} rows x {ncol(training_data)} cols")
  cli::cli_alert_info("DIAGNOSTIC: Found {length(spectral_cols)} spectral columns")
  cli::cli_alert_info("DIAGNOSTIC: First 10 column names: {paste(head(names(training_data), 10), collapse = ', ')}")

  if (length(spectral_cols) > 0) {
    spectral_nums <- as.numeric(spectral_cols)
    cli::cli_alert_info("DIAGNOSTIC: Spectral range: {min(spectral_nums)} to {max(spectral_nums)}")
    cli::cli_alert_info("DIAGNOSTIC: First 5 spectral cols: {paste(head(sort(spectral_cols), 5), collapse = ', ')}")

    # Try the selection and see what we get
    test_select <- dplyr::select(training_data, dplyr::all_of(spectral_cols))
    cli::cli_alert_info("DIAGNOSTIC: After selection: {nrow(test_select)} rows x {ncol(test_select)} cols")
  } else {
    cli::cli_alert_warning("No numeric spectral columns found!")
  }

  old_safely_execute(expr          = {FactoMineR::PCA(X          = dplyr::select(training_data, dplyr::all_of(spectral_cols)),
                                                  scale.unit = TRUE,
                                                  graph      = FALSE,
                                                  ncp        = 80)},
                 default_value = NULL,
                 error_message = "Failed to perform PCA on training data") -> Training_PCA_safe

  Training_PCA <- Training_PCA_safe$result

  if(is.null(Training_PCA)) {
    return(NULL)
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Project training data into PCA space and join metadata
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Extracting PCA scores for training data.")

  old_safely_execute(expr          = {tibble::as_tibble(Training_PCA$ind$coord) %>%
                                   dplyr::bind_cols(dplyr::select(training_data, -dplyr::all_of(spectral_cols)))},
                 default_value = NULL,
                 error_message = "Failed to extract or combine training PCA scores") -> training_scores_safe

  training_scores <- training_scores_safe$result

  if(is.null(training_scores)) {
    return(NULL)
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Project new data using the same PCA and join metadata
  ## ---------------------------------------------------------------------------

  cli::cli_progress_step("Predicting PCA scores for new data.")

  old_safely_execute(expr         = {predict(object  = Training_PCA,
                                         newdata = dplyr::select(new_data, dplyr::all_of(spectral_cols)))$coord %>%
                                                    tibble::as_tibble() %>%
                                                    dplyr::bind_cols(dplyr::select(new_data, -dplyr::all_of(spectral_cols)))},
                 default_value = NULL,
                 error_message = "Failed to project new data into PCA space") -> new_scores_safe

  new_scores <- new_scores_safe$result

  if(is.null(new_scores)){
    return(NULL)
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Return as list
  ## ---------------------------------------------------------------------------

  return(list(training_data = training_scores,
              new_data      = new_scores
  ))
}



## ============================================================================


#' Evaluate Predicted Covariates Against Measured Values
#'
#' Compares predicted and observed covariate values by `Sample_ID`, computes evaluation metrics
#' for each covariate independently, and returns a tidy summary of model performance. Metrics include
#' RMSE, R², CCC, RPIQ, and others via `soilspec::eval()`. Errors or non-numeric columns are gracefully skipped.
#'
#' @param measured_data A `data.frame` or `tibble` containing observed covariate values.
#'   Must include a `Sample_ID` column and one or more numeric columns representing measured covariates.
#' @param modeled_data A `data.frame` or `tibble` containing predicted covariate values.
#'   Must also include `Sample_ID` and matching column names for predicted covariates.
#'
#' @return A `tibble` summarizing evaluation metrics for each covariate. Includes columns:
#' \itemize{
#'   \item \strong{Covariate}: Name of the covariate being evaluated
#'   \item \strong{RMSE}: Root Mean Squared Error
#'   \item \strong{R2}: Coefficient of Determination
#'   \item \strong{CCC}: Concordance Correlation Coefficient
#'   \item \strong{RPIQ}: Ratio of Performance to Interquartile Distance
#'   \item Other metrics computed by `soilspec::eval()`
#' }
#'
#' @details
#' The function performs an inner join on `Sample_ID`, appending `_measured` and `_modeled` suffixes
#' to corresponding columns. Only numeric covariates with valid observed and predicted values are evaluated.
#' Any covariate with missing data or evaluation errors is skipped, with a warning logged via `cli::cli_alert_warning()`.
#'
#' Metrics are computed using `soilspec::eval()` with `obj = "quant"` and `na.rm = TRUE`.
#' This function is especially useful for assessing external validation or covariate prediction workflows.
#'
#' @examples
#' \dontrun{
#' results <- evaluate_predictions(
#'   measured_data = my_measured_data,
#'   modeled_data  = my_predicted_data
#' )
#' }
#'
#' @seealso
#' \code{\link[soilspec]{eval}}
#'
#' @importFrom dplyr distinct inner_join mutate bind_rows
#' @importFrom purrr map
#' @importFrom tidyr drop_na
#' @importFrom tibble tibble as_tibble
#' @importFrom cli cli_alert_warning
#' @export

evaluate_predictions <- function(measured_data,
                                 modeled_data) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Ensure unique entries by Sample_ID
  ## ---------------------------------------------------------------------------

  measured_data <- dplyr::distinct(measured_data, Sample_ID, .keep_all = TRUE)
  modeled_data  <- dplyr::distinct(modeled_data,  Sample_ID, .keep_all = TRUE)

  ## ---------------------------------------------------------------------------
  ## Step 2: Join modeled and measured data
  ## ---------------------------------------------------------------------------

  dplyr::inner_join(measured_data,
                    modeled_data,
                    by     = "Sample_ID",
                    suffix = c("_measured", "_modeled")) -> eval_data

  ## ---------------------------------------------------------------------------
  ## Step 3: Evaluate each covariate independently
  ## ---------------------------------------------------------------------------

  covariate_names <- setdiff(names(measured_data), "Sample_ID")

  purrr::map(covariate_names, function(covar) {

  observed  <- eval_data[[paste0(covar, "_measured")]]
  predicted <- eval_data[[paste0(covar, "_modeled")]]

  # Skip if either column is missing or non-numeric

  if (is.null(observed) || is.null(predicted)) return(NULL)

  if (!is.numeric(observed) || !is.numeric(predicted)) return(NULL)

  tibble::tibble(observed  = observed,
                 predicted = predicted) %>%
        tidyr::drop_na() -> eval_combo

  tryCatch({
    soilspec::eval(pred = eval_combo$predicted,
                   obs  = eval_combo$observed,
                   obj  = "quant",
                   na.rm = TRUE) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(Covariate = covar, .before = 1)

  }, error = function(e) {

    cli::cli_alert_warning("Skipping covariate {.val {covar}} due to error: {e$message}")
    return(NULL)

    })

}) %>% dplyr::bind_rows() -> results

  return(results)
}


## ============================================================================


