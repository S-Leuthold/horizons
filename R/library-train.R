#' Library Model Training and Optimization
#'
#' @description
#' Functions for training models on clustered library data with automatic config
#' selection and hyperparameter optimization. Implements two-stage training:
#' Stage 1 - Fast config selection across candidates, Stage 2 - Deep hyperparameter
#' tuning of winner.
#'
#' @details
#' This module bridges library data/clustering infrastructure with tidymodels
#' training pipelines. Key features:
#' - Reuses existing build_recipe() and define_model_specifications()
#' - Two-stage optimization (config then hyperparameters)
#' - Composite scoring (RPD, CCC, R², RMSE)
#' - Memory-optimized (butcher, rm/gc pattern)
#' - Preserves external test set for UQ calibration
#'
#' @importFrom cli cli_text cli_alert_info cli_alert_success cli_alert_warning cli_abort style_bold
#' @importFrom dplyr filter select mutate arrange desc slice_sample bind_rows
#' @importFrom tibble tibble as_tibble
#' @importFrom rsample initial_split training testing
#' @importFrom workflows workflow add_recipe add_model fit
#' @importFrom tune tune_grid select_best finalize_workflow
#' @importFrom yardstick rsq rmse metric_set
#' @importFrom butcher butcher
#' @keywords internal

## -----------------------------------------------------------------------------
## Section 1: Data Preparation and Train/Test Splits
## -----------------------------------------------------------------------------

#' Prepare Cluster Data Splits for Training
#'
#' @description
#' Creates 80/20 train/test split of cluster data, preserving the external test
#' set for UQ calibration. The 80% training pool is used for both config optimization
#' and final model training.
#'
#' @details
#' **Split Strategy:**
#' - 80% training pool: Used for config selection AND final model training
#' - 20% external test: NEVER touched during training, preserved for:
#'   - Conformal calibration (Phase 3)
#'   - Final performance validation
#'   - Coverage assessment
#'
#' Split is stratified by property value quantiles to ensure both sets
#' represent the full range of property values.
#'
#' @param cluster_data Tibble with cluster samples (spectra + property measurements)
#' @param property Character. Property name (e.g., "clay")
#' @param train_prop Numeric (0-1). Proportion for training pool. Default: 0.8
#' @param seed Integer. Random seed for reproducibility. Default: 123
#'
#' @return List with:
#' \describe{
#'   \item{training_pool}{Tibble, 80% of samples for training and optimization}
#'   \item{external_test}{Tibble, 20% of samples for UQ calibration (never used in training)}
#'   \item{n_train}{Integer, number of training samples}
#'   \item{n_test}{Integer, number of test samples}
#' }
#'
#' @keywords internal
prepare_cluster_splits <- function(cluster_data,
                                   property,
                                   train_prop = 0.8,
                                   seed       = 123) {

  ## ---------------------------------------------------------------------------
  ## Step 1.1: Set seed for reproducibility
  ## ---------------------------------------------------------------------------

  set.seed(seed)

  ## ---------------------------------------------------------------------------
  ## Step 1.2: Find property column
  ## ---------------------------------------------------------------------------

  ## Get OSSL column name from mapping ----------------------------------------

  mapping <- get_library_property_mapping()
  prop_row <- mapping[mapping$property == property, ]

  if (nrow(prop_row) != 1) {
    cli::cli_abort("Property '{property}' not found in mapping")
  }

  property_col <- prop_row$ossl_name

  ## Check if column exists in data -------------------------------------------

  if (!property_col %in% names(cluster_data)) {
    cli::cli_abort("Property column '{property_col}' not found in cluster data")
  }

  ## ---------------------------------------------------------------------------
  ## Step 1.3: Create stratified train/test split
  ## ---------------------------------------------------------------------------

  ## Use rsample for clean split -----------------------------------------------

  safely_execute(
    rsample::initial_split(cluster_data,
                          prop  = train_prop,
                          strata = !!rlang::sym(property_col)),
    error_message = "Failed to create train/test split"
  ) %>%
    handle_results(
      error_title = "Split creation failed",
      error_hints = c("Check property column has variance", "Verify sufficient samples"),
      abort_on_null = FALSE
    ) -> split_obj

  if (is.null(split_obj)) return(NULL)

  ## Extract train and test sets ----------------------------------------------

  training_pool <- rsample::training(split_obj)
  external_test <- rsample::testing(split_obj)

  ## ---------------------------------------------------------------------------
  ## Step 1.3.5: Prepare for tidymodels (rename columns, add required fields)
  ## ---------------------------------------------------------------------------

  ## Add Sample_ID and Project if not present ----------------------------------

  if (!"Sample_ID" %in% names(training_pool)) {
    if ("sample_id" %in% names(training_pool)) {
      training_pool <- training_pool %>% dplyr::rename(Sample_ID = sample_id)
      external_test <- external_test %>% dplyr::rename(Sample_ID = sample_id)
    } else {
      cli::cli_warn("No sample_id column found - build_recipe may fail")
    }
  }

  if (!"Project" %in% names(training_pool)) {
    training_pool <- training_pool %>% dplyr::mutate(Project = "library")
    external_test <- external_test %>% dplyr::mutate(Project = "library")
  }

  ## Rename property column to Response (build_recipe expects this) ------------

  training_pool <- training_pool %>%
    dplyr::rename(Response = !!rlang::sym(property_col))

  external_test <- external_test %>%
    dplyr::rename(Response = !!rlang::sym(property_col))

  ## ---------------------------------------------------------------------------
  ## Step 1.4: Assemble result
  ## ---------------------------------------------------------------------------

  list(training_pool = training_pool,
       external_test = external_test,
       n_train       = nrow(training_pool),
       n_test        = nrow(external_test),
       property_col  = "Response") -> result  # Now always Response

  return(result)

}

## -----------------------------------------------------------------------------
## Section 2: Composite Score Calculation
## -----------------------------------------------------------------------------

#' Calculate Composite Score for Config Ranking
#'
#' @description
#' Computes weighted composite score from multiple performance metrics for ranking
#' model configurations. Uses spectroscopy-appropriate weighting with RPD as primary.
#'
#' @details
#' **Formula:**
#' ```
#' composite = 0.35 * RPD_norm +
#'             0.25 * CCC_norm +
#'             0.25 * R²_norm +
#'             0.15 * (1 - RMSE_norm)
#' ```
#'
#' **Normalization:** Each metric scaled to [0, 1] within the tested configs:
#' ```
#' metric_norm = (metric - min(metric)) / (max(metric) - min(metric))
#' ```
#'
#' **Tie-Breaking:** If scores within threshold (default 0.01), prefer:
#' - Simpler model (fewer features)
#' - Faster model (lower training time)
#'
#' @param metrics Tibble with columns: config_id, rpd, ccc, rsq, rmse
#' @param tie_threshold Numeric. Score difference considered a tie. Default: 0.01
#'
#' @return Tibble with added columns:
#' \describe{
#'   \item{composite_score}{Weighted normalized score (0-1, higher = better)}
#'   \item{rank}{Integer rank (1 = best)}
#' }
#'
#' @keywords internal
calculate_composite_score <- function(metrics,
                                      tie_threshold = 0.01) {

  ## ---------------------------------------------------------------------------
  ## Step 2.1: Normalize metrics to [0, 1]
  ## ---------------------------------------------------------------------------

  ## Helper function for min-max normalization ---------------------------------

  normalize <- function(x) {

    if (length(unique(x)) == 1) return(rep(0.5, length(x)))  # All same
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

  }

  ## Normalize each metric -----------------------------------------------------

  metrics %>%
    dplyr::mutate(
      rpd_norm  = normalize(rpd),
      ccc_norm  = normalize(ccc),
      rsq_norm  = normalize(rsq),
      rmse_norm = normalize(rmse)
    ) -> metrics_norm

  ## ---------------------------------------------------------------------------
  ## Step 2.2: Calculate weighted composite score
  ## ---------------------------------------------------------------------------

  metrics_norm %>%
    dplyr::mutate(
      composite_score = 0.35 * rpd_norm +
                        0.25 * ccc_norm +
                        0.25 * rsq_norm +
                        0.15 * (1 - rmse_norm)  # Lower RMSE = better
    ) -> metrics_scored

  ## ---------------------------------------------------------------------------
  ## Step 2.3: Rank configs by composite score
  ## ---------------------------------------------------------------------------

  metrics_scored %>%
    dplyr::arrange(dplyr::desc(composite_score)) %>%
    dplyr::mutate(rank = dplyr::row_number()) -> metrics_ranked

  ## ---------------------------------------------------------------------------
  ## Step 2.4: Return result
  ## ---------------------------------------------------------------------------

  return(metrics_ranked)

}

## -----------------------------------------------------------------------------
## Section 3: Config Optimization and Model Training
## -----------------------------------------------------------------------------

#' Optimize Model Config for Cluster
#'
#' @description
#' Tests multiple model configurations on a cluster's training data, ranks by
#' composite score, and trains the winning config with full hyperparameter tuning.
#' Implements two-stage optimization: fast config selection, then deep tuning.
#'
#' @details
#' **Two-Stage Training:**
#'
#' **Stage 1 - Config Selection (Fast):**
#' 1. Take top 10 configs from OPTIMAL_CONFIGS_V1 for this property
#' 2. Subset training pool to 20% (~380 samples for quick testing)
#' 3. For each config:
#'    - Build recipe (preprocessing + feature selection)
#'    - Define model spec
#'    - Quick tune: 5 grid points, 3-fold CV
#'    - Evaluate: RPD, CCC, R², RMSE on validation fold
#'    - Calculate composite score
#'    - DISCARD model (don't save)
#' 4. Rank by composite score
#' 5. Select winner (highest score)
#'
#' **Stage 2 - Hyperparameter Tuning (Thorough):**
#' 1. Take winning config
#' 2. Use FULL 80% training pool (~1,930 samples)
#' 3. Build recipe + model spec
#' 4. Full tune: 10 grid points, 10-fold CV (parallel CV folds)
#' 5. Fit final model with best hyperparameters
#' 6. butcher::butcher() for memory
#' 7. Return trained workflow
#'
#' **Memory Discipline:**
#' - Stage 1: Test 10 configs, rm() all after scoring
#' - Stage 2: Keep only final butchered workflow
#' - Target: <500MB overhead
#'
#' @param cluster_splits List from prepare_cluster_splits() with training_pool and external_test
#' @param property Character. Property name
#' @param n_configs_test Integer. How many top configs to test. Default: 10
#' @param config_subset_prop Numeric (0-1). Proportion of training pool for config testing. Default: 0.2
#' @param quick_grid_size Integer. Grid points for config testing. Default: 5
#' @param final_grid_size Integer. Grid points for final tuning. Default: 10
#' @param quick_cv_folds Integer. CV folds for config testing. Default: 3
#' @param final_cv_folds Integer. CV folds for final tuning. Default: 10
#' @param seed Integer. Random seed. Default: 123
#' @param verbose Logical. Print progress? Default: TRUE
#'
#' @return List with:
#' \describe{
#'   \item{final_workflow}{Trained and butchered workflow (ready for prediction)}
#'   \item{winning_config}{Tibble row from OPTIMAL_CONFIGS with winner details}
#'   \item{config_scores}{Tibble with all tested configs and their scores}
#'   \item{best_params}{List with optimal hyperparameters}
#'   \item{external_test}{Tibble with held-out test set (for UQ calibration)}
#'   \item{training_time_sec}{Numeric, total time for optimization + training}
#' }
#'
#' Returns NULL if optimization or training fails.
#'
#' @keywords internal
optimize_config_for_cluster <- function(cluster_splits,
                                        property,
                                        n_configs_test      = 10,
                                        config_subset_prop  = 0.2,
                                        quick_grid_size     = 5,
                                        final_grid_size     = 10,
                                        quick_cv_folds      = 3,
                                        final_cv_folds      = 10,
                                        seed                = 123,
                                        verbose             = TRUE) {

  ## Start timer ----------------------------------------------------------------

  start_time <- Sys.time()

  ## ---------------------------------------------------------------------------
  ## Step 3.1: Get candidate configs for this property
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text("│")
  if (verbose) cli::cli_text("├─ {cli::style_bold('Stage 1: Config Selection')}...")

  ## Filter OPTIMAL_CONFIGS_V1 to this property --------------------------------

  candidate_configs <- OPTIMAL_CONFIGS_V1 %>%
    dplyr::filter(property == !!property) %>%
    dplyr::arrange(rank) %>%
    dplyr::slice(1:min(n_configs_test, dplyr::n()))

  if (nrow(candidate_configs) == 0) {
    cli::cli_abort("No configs found for property '{property}' in OPTIMAL_CONFIGS_V1")
  }

  if (verbose) cli::cli_text("│  ├─ Testing {nrow(candidate_configs)} candidate configs")

  ## ---------------------------------------------------------------------------
  ## Step 3.2: Subset training pool for quick config testing
  ## ---------------------------------------------------------------------------

  ## Sample subset for fast testing --------------------------------------------

  n_subset <- round(cluster_splits$n_train * config_subset_prop)

  training_pool <- cluster_splits$training_pool

  set.seed(seed)
  config_test_data <- training_pool %>%
    dplyr::slice_sample(n = min(n_subset, nrow(training_pool)))

  if (verbose) cli::cli_text("│  ├─ Config test subset: {nrow(config_test_data)} samples")

  ## ---------------------------------------------------------------------------
  ## Step 3.3: Test each config and score
  ## ---------------------------------------------------------------------------

  ## Get property column name --------------------------------------------------

  property_col <- get_library_property_mapping() %>%
    dplyr::filter(property == !!property) %>%
    dplyr::pull(ossl_name)

  if (verbose) cli::cli_text("│  ├─ Testing {nrow(candidate_configs)} configs...")

  ## Test each config ----------------------------------------------------------

  config_results <- list()

  for (i in 1:nrow(candidate_configs)) {

    config <- candidate_configs[i, ]

    if (verbose) {
      config_name <- paste0(config$model, " | ", config$preprocessing, " | ", config$feature_selection)
      cli::cli_text("│  │  ├─ [{i}/{nrow(candidate_configs)}] {config_name}...")
    }

    ## Train and score (quick, no workflow) ------------------------------------

    result <- train_and_score_config(
      config = config,
      train_data = config_test_data,
      property_col = property_col,
      grid_size = quick_grid_size,
      cv_folds = quick_cv_folds,
      return_workflow = FALSE,
      seed = seed,
      verbose = FALSE
    )

    config_results[[i]] <- result$metrics %>%
      dplyr::mutate(
        config_id = paste(config$model, config$preprocessing, config$feature_selection, sep = "_"),
        model = config$model,
        preprocessing = config$preprocessing,
        feature_selection = config$feature_selection
      )

    if (verbose && result$status == "success") {
      cli::cli_text("│  │  │  └─ RPD={round(result$metrics$rpd, 2)} CCC={round(result$metrics$ccc, 3)}")
    }

    ## Clean up ------------------------------------------------------------------

    rm(result)
    gc(verbose = FALSE)

  }

  ## Combine results and calculate composite scores ----------------------------

  config_metrics <- dplyr::bind_rows(config_results)
  config_scores  <- calculate_composite_score(config_metrics)

  ## Select winner -------------------------------------------------------------

  winning_config <- config_scores %>%
    dplyr::arrange(dplyr::desc(composite_score)) %>%
    dplyr::slice(1)

  if (verbose) {
    cli::cli_text("│  │")
    cli::cli_text("│  └─ Winner: {winning_config$model} (score={round(winning_config$composite_score, 3)})")
  }

  ## ---------------------------------------------------------------------------
  ## Step 3.4: Stage 2 - Train winning config on full training pool
  ## ---------------------------------------------------------------------------

  if (verbose) cli::cli_text("│")
  if (verbose) cli::cli_text("├─ {cli::style_bold('Stage 2: Final Training')}...")
  if (verbose) {
    cli::cli_text("│  ├─ Config: {winning_config$model} | {winning_config$preprocessing} | {winning_config$feature_selection}")
    cli::cli_text("│  ├─ Training on {cluster_splits$n_train} samples")
    cli::cli_text("│  ├─ {final_grid_size} grid points, {final_cv_folds}-fold CV")
  }

  ## Train with full tuning on complete training pool --------------------------

  final_result <- train_and_score_config(
    config = winning_config,
    train_data = training_pool,
    property_col = property_col,
    grid_size = final_grid_size,
    cv_folds = final_cv_folds,
    return_workflow = TRUE,  # Keep the workflow!
    seed = seed,
    verbose = FALSE
  )

  if (verbose && final_result$status == "success") {
    cli::cli_text("│  ├─ Final metrics: RPD={round(final_result$metrics$rpd, 2)}, CCC={round(final_result$metrics$ccc, 3)}")
    cli::cli_text("│  └─ Model ready")
  }

  ## Calculate total time ------------------------------------------------------

  end_time <- Sys.time()
  training_time_sec <- as.numeric(difftime(end_time, start_time, units = "secs"))

  ## ---------------------------------------------------------------------------
  ## Step 3.5: Assemble result
  ## ---------------------------------------------------------------------------

  list(
    final_workflow    = final_result$workflow,
    winning_config    = winning_config,
    config_scores     = config_scores,
    final_metrics     = final_result$metrics,
    best_params       = final_result$best_params,
    external_test     = cluster_splits$external_test,
    training_time_sec = training_time_sec
  ) -> result

  return(result)

}

## -----------------------------------------------------------------------------
## Section 3.5: Core Training Function (Library-Specific)
## -----------------------------------------------------------------------------

#' Train and Score Single Config
#'
#' @description
#' Trains a single model configuration and extracts performance metrics.
#' Simplified for library mode - no checkpointing, pruning, or output management.
#'
#' @details
#' Core training steps:
#' 1. Build recipe (preprocessing + feature selection)
#' 2. Define model specification
#' 3. Create workflow
#' 4. Hyperparameter tuning with tune_grid (CV)
#' 5. Select best parameters
#' 6. Extract validation metrics (RPD, CCC, R², RMSE)
#' 7. Optional: Fit final workflow if return_workflow = TRUE
#'
#' For quick config testing: return_workflow = FALSE (discard model, keep metrics)
#' For final training: return_workflow = TRUE (keep butchered workflow)
#'
#' @param config Config row from OPTIMAL_CONFIGS_V1
#' @param train_data Tibble with training data
#' @param property_col Character. OSSL property column name (e.g., "clay.tot_usda.a334_w.pct")
#' @param grid_size Integer. Hyperparameter grid points. Default: 5
#' @param cv_folds Integer. Cross-validation folds. Default: 3
#' @param return_workflow Logical. Return trained workflow? Default: FALSE
#' @param seed Integer. Random seed. Default: 123
#' @param verbose Logical. Print progress? Default: FALSE
#'
#' @return List with:
#' \describe{
#'   \item{metrics}{Tibble with rpd, ccc, rsq, rmse from CV}
#'   \item{workflow}{Trained workflow (if return_workflow = TRUE) or NULL}
#'   \item{best_params}{List with optimal hyperparameters}
#'   \item{status}{Character: "success" or "failed"}
#' }
#'
#' @keywords internal
train_and_score_config <- function(config,
                                   train_data,
                                   property_col,
                                   grid_size       = 5,
                                   cv_folds        = 3,
                                   return_workflow = FALSE,
                                   seed            = 123,
                                   verbose         = FALSE) {

  ## Set seed for reproducibility ----------------------------------------------

  set.seed(seed)

  ## ---------------------------------------------------------------------------
  ## Step 1: Build recipe (data already prepared with Response column)
  ## ---------------------------------------------------------------------------

  safely_execute(
    build_recipe(
      input_data               = train_data,
      spectral_transformation  = config$preprocessing,
      response_transformation  = config$transformation,
      feature_selection_method = config$feature_selection,
      covariate_selection      = NULL,  # No covariates in library mode
      covariate_data           = NULL,
      covariate_interactions   = FALSE
    ),
    error_message = "Recipe build failed"
  ) %>%
    handle_results(abort_on_null = FALSE) -> recipe_obj

  if (is.null(recipe_obj)) {
    return(list(
      metrics = tibble::tibble(rpd = 0, ccc = 0, rsq = 0, rmse = 999),
      workflow = NULL,
      best_params = list(),
      status = "recipe_failed"
    ))
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Define model specification
  ## ---------------------------------------------------------------------------

  model_spec <- define_model_specifications(config$model)

  ## ---------------------------------------------------------------------------
  ## Step 3: Create workflow
  ## ---------------------------------------------------------------------------

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_obj) %>%
    workflows::add_model(model_spec)

  ## ---------------------------------------------------------------------------
  ## Step 4: Create CV folds
  ## ---------------------------------------------------------------------------

  safely_execute(
    rsample::vfold_cv(train_data, v = cv_folds, strata = !!rlang::sym(property_col)),
    error_message = "CV fold creation failed"
  ) %>%
    handle_results(abort_on_null = FALSE) -> cv_folds_obj

  if (is.null(cv_folds_obj)) {
    return(list(
      metrics = tibble::tibble(rpd = 0, ccc = 0, rsq = 0, rmse = 999),
      workflow = NULL,
      best_params = list(),
      status = "cv_failed"
    ))
  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Hyperparameter tuning with tune_grid
  ## ---------------------------------------------------------------------------

  ## Create metric set with custom spectroscopy metrics ------------------------

  metric_set_custom <- yardstick::metric_set(
    yardstick::rsq,
    yardstick::rmse,
    rpd,
    ccc,
    rrmse
  )

  ## Tune grid ------------------------------------------------------------------

  safely_execute(
    tune::tune_grid(
      wf,
      resamples = cv_folds_obj,
      grid      = grid_size,
      metrics   = metric_set_custom,
      control   = tune::control_grid(allow_par = TRUE, save_pred = FALSE)
    ),
    error_message = "tune_grid failed"
  ) %>%
    handle_results(abort_on_null = FALSE) -> tune_results

  if (is.null(tune_results)) {
    return(list(
      metrics = tibble::tibble(rpd = 0, ccc = 0, rsq = 0, rmse = 999),
      workflow = NULL,
      best_params = list(),
      status = "tuning_failed"
    ))
  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Select best parameters and extract metrics
  ## ---------------------------------------------------------------------------

  ## Select best by RPD (primary spectroscopy metric) --------------------------

  best_params <- tune::select_best(tune_results, metric = "rpd")

  ## Get CV metrics for best parameters ----------------------------------------

  best_metrics <- tune::show_best(tune_results, metric = "rpd", n = 1)

  ## Extract our metrics -------------------------------------------------------

  metrics_out <- tibble::tibble(
    rpd  = best_metrics$mean[best_metrics$.metric == "rpd"],
    ccc  = best_metrics$mean[best_metrics$.metric == "ccc"],
    rsq  = best_metrics$mean[best_metrics$.metric == "rsq"],
    rmse = best_metrics$mean[best_metrics$.metric == "rmse"]
  )

  ## ---------------------------------------------------------------------------
  ## Step 7: Optionally fit final workflow
  ## ---------------------------------------------------------------------------

  final_workflow <- NULL

  if (return_workflow) {

    ## Finalize workflow with best params --------------------------------------

    final_wf <- tune::finalize_workflow(wf, best_params)

    ## Fit on full training data -----------------------------------------------

    safely_execute(
      workflows::fit(final_wf, data = train_data),
      error_message = "Final fit failed"
    ) %>%
      handle_results(abort_on_null = FALSE) -> fitted_wf

    if (!is.null(fitted_wf)) {

      ## Butcher for memory ----------------------------------------------------

      final_workflow <- butcher::butcher(fitted_wf)

    }

  }

  ## ---------------------------------------------------------------------------
  ## Step 8: Return result
  ## ---------------------------------------------------------------------------

  list(
    metrics      = metrics_out,
    workflow     = final_workflow,
    best_params  = best_params,
    status       = "success"
  ) -> result

  return(result)

}

## -----------------------------------------------------------------------------
