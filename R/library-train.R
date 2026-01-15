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
#' @param ilr_coordinate Integer. For texture properties, which ILR coordinate to use as Response (1 or 2).
#'   NULL for non-texture properties. When texture property requested, function will:
#'   - Apply ILR transformation to all 3 texture components
#'   - Use ilr_coordinate to select which coordinate becomes Response column
#'   - Both coordinates will be present in data for potential dual training
#' @param train_prop Numeric (0-1). Proportion for training pool. Default: 0.8
#' @param seed Integer. Random seed for reproducibility. Default: 123
#'
#' @return List with:
#' \describe{
#'   \item{training_pool}{Tibble, 80% of samples for training and optimization}
#'   \item{external_test}{Tibble, 20% of samples for UQ calibration (never used in training)}
#'   \item{n_train}{Integer, number of training samples}
#'   \item{n_test}{Integer, number of test samples}
#'   \item{ilr_coordinate}{Integer or NULL, tracks which ILR coordinate if texture}
#' }
#'
#' @keywords internal
prepare_cluster_splits <- function(cluster_data,
                                   property,
                                   ilr_coordinate = NULL,
                                   train_prop     = 0.8,
                                   seed           = 123) {

  ## ---------------------------------------------------------------------------
  ## Step 1.1: Set seed for reproducibility
  ## ---------------------------------------------------------------------------

  set.seed(seed)

  ## ---------------------------------------------------------------------------
  ## Step 1.2: Handle texture properties (ILR transformation)
  ## ---------------------------------------------------------------------------

  if (is_texture_property(property)) {

    ## Texture requires ILR transformation --------------------------------------

    ## Validate ilr_coordinate parameter ----------------------------------------

    if (is.null(ilr_coordinate) || !ilr_coordinate %in% c(1, 2)) {
      cli::cli_abort(
        "Texture properties require ilr_coordinate = 1 or 2",
        "i" = "Must specify which ILR coordinate to use as Response"
      )
    }

    ## Get all three texture column names ---------------------------------------

    texture_mapping <- get_library_property_mapping() %>%
      dplyr::filter(property %in% c("sand", "silt", "clay"))

    sand_col <- texture_mapping$ossl_name[texture_mapping$property == "sand"]
    silt_col <- texture_mapping$ossl_name[texture_mapping$property == "silt"]
    clay_col <- texture_mapping$ossl_name[texture_mapping$property == "clay"]

    ## Verify all columns exist -------------------------------------------------

    required_cols <- c(sand_col, silt_col, clay_col)
    missing_cols  <- setdiff(required_cols, names(cluster_data))

    if (length(missing_cols) > 0) {
      cli::cli_abort(
        "Missing texture column{?s}: {paste(missing_cols, collapse = ', ')}",
        "i" = "load_ossl_raw() should fetch all texture columns for texture properties"
      )
    }

    ## Validate texture measurements --------------------------------------------

    sand_vals <- cluster_data[[sand_col]]
    silt_vals <- cluster_data[[silt_col]]
    clay_vals <- cluster_data[[clay_col]]

    ## Check non-negativity -----------------------------------------------------

    if (any(sand_vals < 0, na.rm = TRUE) ||
        any(silt_vals < 0, na.rm = TRUE) ||
        any(clay_vals < 0, na.rm = TRUE)) {
      cli::cli_abort("Texture values contain negative numbers - check OSSL data quality")
    }

    ## Check mass balance (OSSL stores as % not g/kg, so sum should be ~100) ----

    total <- sand_vals + silt_vals + clay_vals
    mean_total <- mean(total, na.rm = TRUE)
    max_dev <- max(abs(total - mean_total), na.rm = TRUE)

    ## Detect units: if mean ~100, it's %, if mean ~1000, it's g/kg -------------

    if (mean_total > 500) {
      ## g/kg units (sum should be ~1000)
      expected_sum <- 1000
      severe_threshold <- 50
    } else {
      ## Percentage units (sum should be ~100)
      expected_sum <- 100
      severe_threshold <- 5
    }

    max_dev_from_expected <- max(abs(total - expected_sum), na.rm = TRUE)

    if (max_dev_from_expected > severe_threshold) {
      cli::cli_warn(
        "Texture sum deviation: {round(max_dev_from_expected, 2)} from expected {expected_sum}",
        "i" = "ILR will normalize - continuing anyway"
      )
    }

    ## Apply ILR transformation -------------------------------------------------

    ilr_coords <- texture_to_ilr(
      sand = cluster_data[[sand_col]],
      silt = cluster_data[[silt_col]],
      clay = cluster_data[[clay_col]],
      as_proportions = FALSE  # OSSL data in g/kg
    )

    ## Add ILR coordinates to cluster data --------------------------------------

    cluster_data$ilr_1 <- ilr_coords[, 1]
    cluster_data$ilr_2 <- ilr_coords[, 2]

    ## Response will be the specified ILR coordinate ----------------------------

    property_col <- paste0("ilr_", ilr_coordinate)

  } else {

    ## Non-texture: standard property column handling ---------------------------

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

  ## If Response already exists, remove it first (avoid duplicate column) ------

  if ("Response" %in% names(training_pool) && property_col != "Response") {
    training_pool <- training_pool %>% dplyr::select(-Response)
    external_test <- external_test %>% dplyr::select(-Response)
  }

  ## Now safely rename property column → Response ------------------------------

  if (property_col != "Response") {
    training_pool <- training_pool %>%
      dplyr::rename(Response = !!rlang::sym(property_col))

    external_test <- external_test %>%
      dplyr::rename(Response = !!rlang::sym(property_col))
  }

  ## Remove unused ILR coordinate to prevent data leakage ---------------------

  if (is_texture_property(property) && !is.null(ilr_coordinate)) {

    ## Remove the OTHER coordinate (keep only the one being trained)
    other_coord <- if (ilr_coordinate == 1) "ilr_2" else "ilr_1"

    if (other_coord %in% names(training_pool)) {
      training_pool <- training_pool %>% dplyr::select(-!!other_coord)
      external_test <- external_test %>% dplyr::select(-!!other_coord)
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 1.4: Assemble result
  ## ---------------------------------------------------------------------------

  list(training_pool  = training_pool,
       external_test  = external_test,
       n_train        = nrow(training_pool),
       n_test         = nrow(external_test),
       property_col   = "Response",          # Always Response after renaming
       ilr_coordinate = ilr_coordinate) -> result  # Track ILR coord if texture

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
#' **Normalization:** Each metric scaled to \[0, 1\] within the tested configs:
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
                                        allow_par           = TRUE,
                                        n_workers           = 4,
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
    dplyr::filter(model != "plsr") %>%  # TEMPORARY: Exclude PLSR (upstream bug)
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
      model_name <- get_readable_model_name(as.character(config$model))
      preproc_name <- get_readable_preprocessing_name(as.character(config$preprocessing))
      feature_name <- get_readable_feature_selection_name(as.character(config$feature_selection))
      cli::cli_text("│  │  ├─ [{i}/{nrow(candidate_configs)}] {model_name} + {preproc_name} + {feature_name}...")
    }

    ## Train and score (quick, no workflow) ------------------------------------

    result <- train_and_score_config(
      config = config,
      train_data = config_test_data,
      property_col = property_col,
      grid_size = quick_grid_size,
      cv_folds = quick_cv_folds,
      return_workflow = FALSE,
      allow_par = allow_par,
      n_workers = n_workers,
      seed = seed,
      verbose = FALSE
    )

    config_results[[i]] <- result$metrics %>%
      dplyr::mutate(
        config_id = paste(config$model, config$preprocessing, config$feature_selection, sep = "_"),
        model = as.character(config$model),
        preprocessing = as.character(config$preprocessing),
        transformation = as.character(config$transformation),
        feature_selection = as.character(config$feature_selection)
      )

    if (verbose && result$status == "success") {
      cli::cli_text("│  │  │  └─ RPD={round(result$metrics$rpd, 2)} CCC={round(result$metrics$ccc, 3)}")
    }

    ## Aggressive cleanup after each config test --------------------------------

    if (exists("result") && !is.null(result$workflow)) {
      result$workflow <- butcher::butcher(result$workflow)
    }

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
    cli::cli_text("│  ├─ {cli::style_bold('Config rankings')} (top {min(5, nrow(config_scores))}):")

    top_configs <- config_scores %>%
      dplyr::arrange(dplyr::desc(composite_score)) %>%
      dplyr::slice(1:min(5, dplyr::n()))

    for (i in 1:nrow(top_configs)) {
      cfg <- top_configs[i, ]
      model_name <- get_readable_model_name(as.character(cfg$model))
      preproc_name <- get_readable_preprocessing_name(as.character(cfg$preprocessing))
      cli::cli_text("│  │  #{i}: {model_name} + {preproc_name} (RPD={round(cfg$rpd, 2)}, score={round(cfg$composite_score, 2)})")
    }

    winner_name <- get_readable_model_name(as.character(winning_config$model))

    cli::cli_text("│  │")
    cli::cli_text("│  └─ Winner: {winner_name}")
  }

  ## ---------------------------------------------------------------------------
  ## Step 3.4: Stage 2 - Train winning config on full training pool
  ## ---------------------------------------------------------------------------

  ## Free Stage 1 intermediate results before Stage 2 --------------------------

  rm(config_results, config_metrics, config_test_data)
  gc(verbose = FALSE)

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
    allow_par = allow_par,
    n_workers = n_workers,
    seed = seed,
    verbose = FALSE
  )

  ## Validate training succeeded -----------------------------------------------

  if (final_result$status != "success") {
    cli::cli_abort(
      "Final model training failed for winning config",
      "i" = "Config: {winning_config$model} | {winning_config$preprocessing}",
      "i" = "Status: {final_result$status}",
      "i" = "Check if cluster has sufficient samples or try different configs"
    )
  }

  if (is.null(final_result$workflow)) {
    cli::cli_abort(
      "Training succeeded but workflow is NULL",
      "i" = "This is a bug - report to maintainer"
    )
  }

  if (verbose) {
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
#' @param resamples Optional rsample object. Pre-computed CV folds for shared training.
#'   If NULL (default), folds are created internally using cv_folds.
#' @param return_workflow Logical. Return trained workflow? Default: FALSE
#' @param allow_par Logical. Allow parallel processing during tuning? Default: TRUE.
#' @param n_workers Integer. Number of parallel workers. Default: 4.
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
                                   resamples       = NULL,  # NEW: Accept pre-made CV folds for shared training
                                   return_workflow = FALSE,
                                   allow_par       = TRUE,
                                   n_workers       = 4,
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
      spectral_transformation  = as.character(config$preprocessing),
      response_transformation  = as.character(config$transformation),
      feature_selection_method = as.character(config$feature_selection),
      covariate_selection      = NULL,  # No covariates in library mode
      covariate_data           = NULL,
      covariate_interactions   = FALSE
    ),
    error_message = "Recipe build failed"
  ) %>%
    handle_results(
      error_title = "Recipe build failed",
      error_hints = c(
        "Config: {as.character(config$model)} | {as.character(config$preprocessing)}",
        "Data structure issue - check Sample_ID, Project, Response columns"
      ),
      abort_on_null = FALSE
    ) -> recipe_obj

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

  model_spec <- define_model_specifications(as.character(config$model))

  ## ---------------------------------------------------------------------------
  ## Step 3: Create workflow
  ## ---------------------------------------------------------------------------

  wf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_obj) %>%
    workflows::add_model(model_spec)

  ## ---------------------------------------------------------------------------
  ## Step 4: Create CV folds (or use provided folds for shared training)
  ## ---------------------------------------------------------------------------

  if (is.null(resamples)) {

    ## Create new CV folds if not provided ---------------------------------------

    safely_execute(
      rsample::vfold_cv(train_data, v = cv_folds, strata = Response),
      error_message = "CV fold creation failed"
    ) %>%
      handle_results(
        error_title = "CV fold creation failed",
        error_hints = c(
          "May need more samples ({nrow(train_data)} for {cv_folds}-fold CV)",
          "Try reducing cv_folds or increasing cluster size"
        ),
        abort_on_null = FALSE
      ) -> cv_folds_obj

    if (is.null(cv_folds_obj)) {
      return(list(
        metrics = tibble::tibble(rpd = 0, ccc = 0, rsq = 0, rmse = 999),
        workflow = NULL,
        best_params = list(),
        status = "cv_failed"
      ))
    }

  } else {

    ## Use provided CV folds (for shared training with quantile models) ----------

    cv_folds_obj <- resamples

  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Finalize mtry if needed (random forest hyperparameter)
  ## ---------------------------------------------------------------------------

  ## Check if mtry is part of the parameter set --------------------------------

  param_set <- workflows::extract_parameter_set_dials(wf)

  ## Bake the data to get the final dataset, then finalize the param set ------

  if ("mtry" %in% param_set$name) {

    recipe_obj %>%
      recipes::prep() %>%
      recipes::bake(new_data = NULL) %>%
      dplyr::select(-Response) -> eval_data

    param_set <- dials::finalize(param_set, eval_data)

    # Explicit cleanup to prevent memory accumulation
    rm(eval_data)
    invisible(gc(verbose = FALSE))

  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Setup parallel backend (if requested)
  ## ---------------------------------------------------------------------------

  if (allow_par && n_workers > 1) {

    is_linux <- .Platform$OS.type == "unix" && Sys.info()["sysname"] == "Linux"

    if (is_linux) {

      ## Linux: Use doMC (fork-based) -------------------------------------------

      options(mc.cores = n_workers, mc.preschedule = FALSE)
      RNGkind("L'Ecuyer-CMRG")
      set.seed(seed)

    } else {

      ## macOS/Windows: Use future (PSOCK-based) --------------------------------

      ## Increase globals size limit for large spectral datasets
      options(future.globals.maxSize = 8 * 1024^3)  # 8 GB

      if (!inherits(future::plan(), "sequential")) {
        ## User already set up parallel - respect it
      } else {
        future::plan(future::multisession, workers = n_workers)
        on.exit(future::plan(future::sequential), add = TRUE)
      }
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 7: Hyperparameter tuning with tune_grid
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
    suppressMessages(suppressWarnings(
      tune::tune_grid(
        object     = wf,
        resamples  = cv_folds_obj,
        grid       = grid_size,
        metrics    = metric_set_custom,
        param_info = param_set,  # Pass finalized params
        control    = tune::control_grid(allow_par = TRUE, save_pred = TRUE, verbose = FALSE)  # Save for residual UQ
      )
    )),
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

  ## Extract out-of-fold predictions for residual UQ ---------------------------

  cv_predictions <- tune::collect_predictions(tune_results) %>%
    dplyr::inner_join(best_params, by = names(best_params)) %>%
    dplyr::select(.row, Response, .pred) %>%
    dplyr::arrange(.row)

  ## Get CV metrics using collect_metrics (long format) ------------------------

  all_metrics <- tune::collect_metrics(tune_results)

  ## Filter to best params and extract each metric -----------------------------

  metrics_out <- tibble::tibble(
    rpd  = all_metrics %>% dplyr::filter(.metric == "rpd") %>% dplyr::slice(1) %>% dplyr::pull(mean),
    ccc  = all_metrics %>% dplyr::filter(.metric == "ccc") %>% dplyr::slice(1) %>% dplyr::pull(mean),
    rsq  = all_metrics %>% dplyr::filter(.metric == "rsq") %>% dplyr::slice(1) %>% dplyr::pull(mean),
    rmse = all_metrics %>% dplyr::filter(.metric == "rmse") %>% dplyr::slice(1) %>% dplyr::pull(mean)
  )

  ## Handle case where metrics are missing (failed CV) -------------------------

  if (nrow(metrics_out) == 0 || all(is.na(metrics_out))) {
    metrics_out <- tibble::tibble(rpd = NA_real_, ccc = NA_real_, rsq = NA_real_, rmse = NA_real_)
  }

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
    metrics        = metrics_out,
    workflow       = final_workflow,
    best_params    = best_params,
    cv_predictions = cv_predictions,  # OOF predictions for unbiased residuals
    status         = "success"
  ) -> result

  return(result)

}

## -----------------------------------------------------------------------------
