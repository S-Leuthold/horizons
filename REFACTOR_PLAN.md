# Clustered Local Models Integration Plan

## Executive Summary

**Goal**: Replace global Kennard-Stone approach with clustered local models approach
**Reason**: 9-10% improvement in R² (0.96 vs 0.87 for clay, 0.91 vs 0.84 for pH)
**Impact**: Core covariate prediction system - medium risk, high reward
**Timeline**: ~6-8 hours focused work

---

## Current Architecture (NEW - To Be Replaced)

### File Structure:
```
R/covariates-orchestrator.R        ← Entry point (fetch_covariates)
R/covariates-soil.R                ← Main logic (predict_soil_covariates)
R/covariates-data.R                ← OSSL data loading
R/covariates-similarity.R          ← Kennard-Stone selection (DELETE THIS)
R/covariates-cubist.R              ← Model fitting (ADAPT THIS)
R/covariates-climate.R             ← Climate data (unchanged)
```

### Current Flow:
```
fetch_covariates()
  └─> predict_soil_covariates()
        ├─> get_processed_ossl_training_data()  [returns PCA model + scores with covariates]
        ├─> preprocess_mir_spectra(input_data)
        ├─> project_spectra_to_pca(input_data)
        ├─> select_global_training_set()        [KENNARD-STONE - REMOVE]
        │     ├─> cluster_unknown_samples()     [clusters for stratification]
        │     ├─> stratified_kennard_stone()    [selects samples per cluster]
        │     └─> returns: train_data, val_data (single split)
        │
        └─> Loop: for each covariate
              ├─> fit_cubist_soil_model()       [ONE global model]
              └─> predict on all unknowns       [same model for everyone]
```

### Current Data Structures:
```r
# After select_global_training_set():
train_data: tibble(Dim.1, Dim.2, ..., Dim.N, clay, ph, oc, ...)  # 8,903 rows
val_data:   tibble(Dim.1, Dim.2, ..., Dim.N, clay, ph, oc, ...)  # 1,572 rows

# Models stored as:
models[[covariate]] → single Cubist model
```

---

## Target Architecture (OLD - To Be Integrated)

### New File Structure:
```
R/covariates-orchestrator.R        ← Entry point (unchanged API)
R/covariates-soil.R                ← Main logic (REFACTOR)
R/covariates-data.R                ← OSSL data loading (unchanged)
R/covariates-clustering.R          ← NEW FILE (clustering logic)
R/covariates-cubist.R              ← Model fitting (minor adaptation)
R/covariates-climate.R             ← Climate data (unchanged)
```

### Target Flow:
```
fetch_covariates()
  └─> predict_soil_covariates()
        ├─> get_processed_ossl_training_data()  [same as before]
        ├─> preprocess_mir_spectra(input_data)
        ├─> project_spectra_to_pca(input_data)
        ├─> cluster_ossl_samples()              [NEW - K-means on OSSL]
        ├─> assign_unknowns_to_clusters()       [NEW - match unknowns to clusters]
        ├─> create_clustered_subsets()          [NEW - partition OSSL by cluster]
        │     └─> returns: list of (train_data, val_data) per cluster
        │
        └─> Nested Loop: for each cluster, for each covariate
              ├─> fit_cubist_soil_model()       [MULTIPLE local models]
              └─> Route predictions              [cluster-specific models]
```

### Target Data Structures:
```r
# After create_clustered_subsets():
training_subsets: list(
  Cluster_1 = list(train = tibble(...), val = tibble(...)),  # 7,896 rows
  Cluster_2 = list(train = tibble(...), val = tibble(...))   # 1,753 rows
)

# Cluster assignments for unknowns:
cluster_assignments: integer vector [1, 1, 2, 1, 2, ...]  # length = n_unknowns

# Models stored as:
models[[cluster_id]][[covariate]] → nested list of Cubist models
```

---

## Dependency Analysis

### Functions to Extract from OLD Code:

**From `benchmark_covariate_methods/OLD_clustered_approach.R`:**

1. `cluster_spectral_data()` (lines ~1850-1950)
   - **Purpose**: K-means clustering with silhouette analysis
   - **Dependencies**: cluster package, stats::kmeans
   - **Inputs**: input_data (PCA scores)
   - **Outputs**: list(input_data, pca_model, kmeans_model, ncomp)

2. `create_clustered_subsets()` (lines ~880-943)
   - **Purpose**: Partition OSSL data by cluster assignments
   - **Dependencies**: dplyr, purrr
   - **Inputs**: training_data, pca_model, kmeans_model, n_components
   - **Outputs**: list of tibbles (one per cluster)

3. `reduce_dimensions_pca()` (lines ~1700-1786)
   - **Purpose**: Fit PCA on training, project training + new data
   - **Dependencies**: FactoMineR, dplyr
   - **NOTE**: Package already has `project_spectra_to_pca()` - may not need this

### Functions to Adapt:

**`fit_cubist_soil_model()` in `R/covariates-cubist.R`:**
- **Current**: Takes full train_data, fits ONE model
- **Target**: Takes cluster_train_data, fits per-cluster model
- **Change**: Minimal - function already works, just gets called more times

### Functions to Delete:

**Entire `R/covariates-similarity.R` file:**
- `select_global_training_set()` - Not needed
- `cluster_unknown_samples()` - Replaced by cluster_ossl_samples()
- `stratified_kennard_stone()` - Not needed

---

## Step-by-Step Implementation Plan

### PHASE 0: Preparation (30 minutes)

**0.1**: Create feature branch
```bash
git checkout -b feature/clustered-local-models
```

**0.2**: Run existing tests to establish baseline
```bash
Rscript -e "devtools::test(filter = 'covariates')"
```

**0.3**: Document current API behavior
```r
# Save example output from current system
results_old <- predict_soil_covariates(test_data, c("clay", "ph"))
saveRDS(results_old, "tests/fixtures/old_system_output.rds")
```

---

### PHASE 1: Extract & Create New Clustering Module (2 hours)

**1.1**: Create `R/covariates-clustering.R`

**Extract these functions:**
```r
#' Cluster OSSL Samples in PCA Space
#'
#' Performs K-means clustering on OSSL PCA scores with automatic
#' cluster number selection via silhouette analysis.
#'
#' @param ossl_pca_scores Tibble with OSSL PCA scores (Dim.1, Dim.2, ...)
#' @param max_clusters Integer. Maximum clusters to test (default: 10)
#' @param seed Integer. Random seed for reproducibility (default: 307)
#' @param verbose Logical. Print progress (default: TRUE)
#'
#' @return Named list:
#'   - kmeans_model: fitted kmeans object
#'   - n_clusters: optimal number of clusters
#'   - cluster_assignments: integer vector of assignments
#'
#' @keywords internal
cluster_ossl_samples <- function(ossl_pca_scores,
                                  max_clusters = 10,
                                  seed         = 307,
                                  verbose      = TRUE) {
  # Extract from OLD code lines 1850-1950
  # ... implementation ...
}

#' Assign Unknown Samples to OSSL Clusters
#'
#' Matches unknown samples to nearest OSSL cluster centroids
#' using Euclidean distance in PCA space.
#'
#' @param unknown_pca_scores Tibble with unknown PCA scores
#' @param cluster_model List from cluster_ossl_samples()
#' @param verbose Logical. Print progress (default: TRUE)
#'
#' @return Integer vector of cluster assignments (length = nrow(unknown_pca_scores))
#'
#' @keywords internal
assign_unknowns_to_clusters <- function(unknown_pca_scores,
                                         cluster_model,
                                         verbose = TRUE) {
  # New function - calculate distances to centroids
  # ... implementation ...
}

#' Create Clustered Training Subsets
#'
#' Partitions OSSL training data into cluster-specific subsets
#' with train/val splits.
#'
#' @param ossl_pca_scores Tibble with all OSSL PCA scores + covariates
#' @param cluster_model List from cluster_ossl_samples()
#' @param prop Numeric. Training proportion (default: 0.85)
#' @param verbose Logical. Print progress (default: TRUE)
#'
#' @return Named list of lists:
#'   Cluster_1 = list(train = tibble(...), val = tibble(...))
#'   Cluster_2 = list(train = tibble(...), val = tibble(...))
#'   ...
#'
#' @keywords internal
create_clustered_subsets <- function(ossl_pca_scores,
                                     cluster_model,
                                     prop    = 0.85,
                                     verbose = TRUE) {
  # Extract from OLD code lines 880-943
  # ... implementation ...
}
```

**Dependencies to add to DESCRIPTION:**
```
Imports:
  cluster,  # For silhouette analysis
  stats     # For kmeans (already imported)
```

**Testing strategy:**
- Test `cluster_ossl_samples()` with known data, verify reproducibility with seed
- Test `assign_unknowns_to_clusters()` correctness (should match nearest centroid)
- Test `create_clustered_subsets()` output structure and sizes

---

### PHASE 2: Refactor `R/covariates-soil.R` (2.5 hours)

**2.1**: Update `predict_soil_covariates()` function signature (NO CHANGE)

Keep existing API - internal implementation changes only.

**2.2**: Replace Kennard-Stone selection with clustering

**BEFORE (lines ~160-166):**
```r
select_global_training_set(unknown_pca_scores  = unknown_pca,
                           ossl_pca_scores     = ossl_result$pca_scores,
                           n_select            = n_similar,
                           prop                = prop,
                           relevance_threshold = 0.85,
                           verbose             = verbose) -> global_selection
```

**AFTER:**
```r
## ---------------------------------------------------------------------------
## Step 4: Cluster OSSL samples in PCA space
## ---------------------------------------------------------------------------

cluster_ossl_samples(ossl_pca_scores = ossl_result$pca_scores,
                     max_clusters    = 10,
                     verbose         = verbose) -> cluster_model

if (verbose) cli::cli_text("├─ Clustered OSSL into {cluster_model$n_clusters} groups")

## ---------------------------------------------------------------------------
## Step 5: Assign unknown samples to clusters
## ---------------------------------------------------------------------------

assign_unknowns_to_clusters(unknown_pca_scores = unknown_pca,
                             cluster_model      = cluster_model,
                             verbose            = verbose) -> cluster_assignments

## ---------------------------------------------------------------------------
## Step 6: Create cluster-specific training subsets
## ---------------------------------------------------------------------------

create_clustered_subsets(ossl_pca_scores = ossl_result$pca_scores,
                         cluster_model   = cluster_model,
                         prop            = prop,
                         verbose         = verbose) -> training_subsets
```

**2.3**: Update model training loop structure

**BEFORE (lines ~180-220):**
```r
global_models <- vector("list", length(covariates))
names(global_models) <- covariates

for (i in seq_along(covariates)) {
  covariate <- covariates[i]

  # Fit ONE global model
  fit_cubist_soil_model(train_data = global_selection$train_data,
                        val_data   = global_selection$val_data,
                        covariate  = covariate,
                        ...) -> model_result

  global_models[[covariate]] <- model_result
}
```

**AFTER:**
```r
## ---------------------------------------------------------------------------
## Step 7: Train cluster-specific models
## ---------------------------------------------------------------------------

local_models <- vector("list", cluster_model$n_clusters)
names(local_models) <- paste0("Cluster_", seq_len(cluster_model$n_clusters))

if (verbose) {
  cli::cli_text("")
  cli::cli_text("Training local models: {cluster_model$n_clusters} clusters × {length(covariates)} covariates")
}

for (cluster_id in seq_len(cluster_model$n_clusters)) {

  cluster_name <- paste0("Cluster_", cluster_id)
  cluster_data <- training_subsets[[cluster_name]]

  local_models[[cluster_name]] <- vector("list", length(covariates))
  names(local_models[[cluster_name]]) <- covariates

  for (i in seq_along(covariates)) {

    covariate <- covariates[i]

    if (verbose) {
      cli::cli_text("├─ [{cluster_id}/{cluster_model$n_clusters}] {toupper(cluster_name)} | [{i}/{length(covariates)}] {toupper(covariate)}")
      cli::cli_text("│  ├─ Training: {format(nrow(cluster_data$train), big.mark = ',')} samples")
    }

    # Fit LOCAL model for this cluster + covariate
    fit_cubist_soil_model(train_data     = cluster_data$train,
                          val_data       = cluster_data$val,
                          covariate      = covariate,
                          bayesian_iter  = bayesian_iter,
                          allow_par      = allow_par,
                          n_workers      = n_workers,
                          verbose        = verbose) -> model_result

    local_models[[cluster_name]][[covariate]] <- model_result
  }
}
```

**2.4**: Update prediction routing logic

**BEFORE:**
```r
# Apply same model to all unknowns
for (covariate in covariates) {
  predictions[[covariate]] <- predict(global_models[[covariate]]$workflow,
                                      new_data = unknown_pca)$.pred
}
```

**AFTER:**
```r
## ---------------------------------------------------------------------------
## Step 8: Generate predictions with cluster-specific routing
## ---------------------------------------------------------------------------

predictions <- tibble::tibble(Sample_ID = unknown_pca$Sample_ID)

for (covariate in covariates) {

  # Initialize prediction vector
  pred_values <- numeric(nrow(unknown_pca))

  # Route each unknown to its cluster-specific model
  for (cluster_id in seq_len(cluster_model$n_clusters)) {

    cluster_name <- paste0("Cluster_", cluster_id)
    cluster_mask <- cluster_assignments == cluster_id

    if (sum(cluster_mask) > 0) {

      cluster_unknowns <- unknown_pca[cluster_mask, ]
      cluster_model_obj <- local_models[[cluster_name]][[covariate]]$workflow

      pred_values[cluster_mask] <- predict(cluster_model_obj,
                                           new_data = cluster_unknowns)$.pred
    }
  }

  predictions[[covariate]] <- pred_values
}
```

**2.5**: Update return structure

**BEFORE:**
```r
return(list(
  predictions        = predictions,
  validation_metrics = validation_metrics,
  global_models      = global_models,
  selection_info     = list(...)
))
```

**AFTER:**
```r
return(list(
  predictions        = predictions,
  validation_metrics = validation_metrics,
  local_models       = local_models,                    # Changed
  cluster_info       = list(                            # New
    n_clusters         = cluster_model$n_clusters,
    cluster_assignments = cluster_assignments,
    cluster_sizes      = table(cluster_assignments)
  )
))
```

---

### PHASE 3: Update `R/covariates-cubist.R` (30 minutes)

**3.1**: Review `fit_cubist_soil_model()`

**Good news**: Function already works per-cluster! It just needs to be called multiple times.

**Minor change needed**: Update verbose output to show cluster context

**BEFORE:**
```r
if (verbose) {
  cli::cli_text("│  ├─ Preparing training data.")
}
```

**AFTER:**
```r
if (verbose) {
  cli::cli_text("│  │  ├─ Preparing training data.")  # Extra indent for nested display
}
```

---

### PHASE 4: Delete Old Code (15 minutes)

**4.1**: Delete `R/covariates-similarity.R`

```bash
git rm R/covariates-similarity.R
```

**4.2**: Remove exports from NAMESPACE (if any)

Check if any functions from similarity.R were exported:
```bash
grep -n "@export" R/covariates-similarity.R
```

If yes, rebuild documentation:
```r
devtools::document()
```

**4.3**: Remove from test files

```bash
grep -r "select_global_training_set\|stratified_kennard_stone" tests/
```

Delete or update any tests referencing removed functions.

---

### PHASE 5: Update Orchestrator (30 minutes)

**5.1**: Update `R/covariates-orchestrator.R`

**Minimal changes needed** - `fetch_covariates()` just calls `predict_soil_covariates()`, which we've already updated.

**Only change**: Update verbose output if needed to reflect clustering

**5.2**: Verify cache key generation

Check if cache keys need updating:
```r
# Current cache key (likely uses: project + covariate + params)
cache_key <- digest::digest(list(project, covariate, n_similar, ...))

# After refactor: Same key structure should work
# Clustering happens internally, doesn't change cache semantics
```

---

### PHASE 6: Testing & Validation (2 hours)

**6.1**: Unit tests for new clustering functions

Create `tests/testthat/test-covariates-clustering.R`:

```r
test_that("cluster_ossl_samples produces consistent results", {
  # Test with synthetic PCA scores
  pca_scores <- make_test_pca_scores(n = 1000)
  result <- cluster_ossl_samples(pca_scores, seed = 123)

  expect_equal(length(result$cluster_assignments), 1000)
  expect_true(result$n_clusters >= 2)
  expect_true(result$n_clusters <= 10)

  # Reproducibility
  result2 <- cluster_ossl_samples(pca_scores, seed = 123)
  expect_equal(result$cluster_assignments, result2$cluster_assignments)
})

test_that("assign_unknowns_to_clusters matches nearest", {
  # Test with known geometry
  ossl <- tibble(Dim.1 = c(0, 10), Dim.2 = c(0, 10))
  cluster_model <- list(
    kmeans_model = list(centers = matrix(c(0, 0, 10, 10), ncol = 2))
  )

  unknowns <- tibble(Dim.1 = c(1, 9), Dim.2 = c(1, 9))
  assignments <- assign_unknowns_to_clusters(unknowns, cluster_model)

  expect_equal(assignments, c(1, 2))
})

test_that("create_clustered_subsets preserves data", {
  pca_scores <- make_test_pca_scores(n = 1000)
  cluster_model <- cluster_ossl_samples(pca_scores)
  subsets <- create_clustered_subsets(pca_scores, cluster_model, prop = 0.8)

  # Check structure
  expect_type(subsets, "list")
  expect_true(all(c("train", "val") %in% names(subsets[[1]])))

  # Check no data loss
  total_rows <- sum(sapply(subsets, function(x) nrow(x$train) + nrow(x$val)))
  expect_equal(total_rows, nrow(pca_scores))
})
```

**6.2**: Integration test comparing OLD vs NEW

Create `tests/testthat/test-covariates-integration.R`:

```r
test_that("clustered approach produces valid predictions", {
  # Use real OSSL data subset
  test_data <- readRDS("tests/fixtures/test_spectra_10samples.rds")

  results <- predict_soil_covariates(
    input_data = test_data,
    covariates = c("clay", "ph"),
    n_similar = 5000,  # Small for speed
    bayesian_iter = 0,  # Skip Bayesian for speed
    verbose = FALSE
  )

  # Check structure
  expect_true("predictions" %in% names(results))
  expect_true("local_models" %in% names(results))
  expect_true("cluster_info" %in% names(results))

  # Check predictions exist
  expect_equal(nrow(results$predictions), 10)
  expect_true(all(c("Sample_ID", "clay", "ph") %in% names(results$predictions)))

  # Check no NAs
  expect_false(any(is.na(results$predictions$clay)))
  expect_false(any(is.na(results$predictions$ph)))

  # Check model structure
  expect_type(results$local_models, "list")
  expect_true(length(results$local_models) >= 2)  # At least 2 clusters
})

test_that("API compatibility maintained", {
  # Ensure return structure matches old system
  test_data <- readRDS("tests/fixtures/test_spectra_10samples.rds")
  results <- predict_soil_covariates(test_data, c("clay"))

  # Required fields
  expect_true(all(c("predictions", "validation_metrics") %in% names(results)))

  # Predictions format
  expect_s3_class(results$predictions, "tbl_df")
  expect_true("Sample_ID" %in% names(results$predictions))
})
```

**6.3**: Benchmark performance comparison

Run the full benchmark comparison again:
```r
source("benchmark_covariate_methods/compare_methods.R")
results <- compare_covariate_methods(test_data, c("clay", "ph"))

# Verify NEW (refactored) matches OLD performance
# Should see R² ~0.96 for clay, ~0.91 for pH
```

**6.4**: Visual inspection of predictions

```r
# Compare predictions visually
library(ggplot2)

old_preds <- readRDS("tests/fixtures/old_system_output.rds")
new_preds <- predict_soil_covariates(test_data, c("clay", "ph"))

comparison <- tibble(
  Sample_ID = new_preds$predictions$Sample_ID,
  clay_old = old_preds$predictions$clay,
  clay_new = new_preds$predictions$clay
)

ggplot(comparison, aes(clay_old, clay_new)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Old vs New Implementation",
       x = "Global Model Predictions",
       y = "Clustered Local Model Predictions")
```

---

### PHASE 7: Documentation Updates (1 hour)

**7.1**: Update `predict_soil_covariates()` documentation

```r
#' @description
#' Predicts soil covariate values from MIR spectra using OSSL training data and
#' clustered local Cubist models. The approach:
#' 1. Clusters OSSL samples in PCA space (K-means with silhouette selection)
#' 2. Assigns unknown samples to nearest cluster
#' 3. Trains specialized Cubist model per cluster
#' 4. Routes predictions to cluster-specific models
#'
#' This local modeling approach outperforms global models by 9-10% in R²,
#' achieving R² > 0.9 for most soil properties.
```

**7.2**: Update return value documentation

```r
#' @return Named list with:
#'   - predictions: Tibble with Sample_ID and predicted covariate columns
#'   - validation_metrics: Tibble with per-covariate validation performance
#'   - local_models: Nested list of Cubist models (cluster × covariate)
#'   - cluster_info: List with clustering details (n_clusters, assignments, sizes)
```

**7.3**: Add clustering function documentation

Ensure all new functions in `R/covariates-clustering.R` have complete roxygen2 docs with:
- `@description`
- `@param` for all parameters
- `@return` with detailed structure
- `@keywords internal` (not exported)
- `@examples` with \dontrun{} if helpful

**7.4**: Update NEWS.md

```md
# horizons 0.8.1 (Development)

## Major Changes

### Covariate Prediction Performance Improvement

- **BREAKING**: Internal covariate prediction now uses clustered local models instead of global Kennard-Stone approach
- **Performance**: 9-10% improvement in R² for clay (0.96 vs 0.87) and pH (0.91 vs 0.84)
- **Method**: K-means clustering of OSSL samples → local Cubist model per cluster
- **API**: External API unchanged - `predict_soil_covariates()` signature identical
- **Impact**: Users will see improved prediction accuracy with no code changes required

### Internal Changes

- Added `R/covariates-clustering.R` with clustering utilities
- Removed `R/covariates-similarity.R` (Kennard-Stone selection)
- Refactored `predict_soil_covariates()` for local modeling workflow
- Updated model storage from single global model to nested cluster × covariate structure

## Bug Fixes

- Fixed stratification bug in OLD Cubist model fitting (was stratifying on wrong column)
```

---

### PHASE 8: Final Validation & Merge (1 hour)

**8.1**: Run full test suite

```r
devtools::test()
```

**8.2**: Check package

```r
devtools::check()
```

**8.3**: Build and test package

```r
devtools::build()
devtools::install()

# Test in clean session
library(horizons)
test_data <- create_dataset(...)
results <- fetch_covariates(test_data, soil_covariates = c("clay", "ph"))
```

**8.4**: Performance regression test

```r
# Ensure no speed regression
bench::mark(
  clustered = predict_soil_covariates(test_data, c("clay", "ph")),
  times = 3,
  check = FALSE
)

# Should complete in reasonable time (~30min for 61 samples × 2 covariates)
```

**8.5**: Create pull request

```bash
git add .
git commit -m "Refactor: Replace global Kennard-Stone with clustered local models

- Adds R/covariates-clustering.R with K-means clustering utilities
- Refactors predict_soil_covariates() to use local models per cluster
- Removes R/covariates-similarity.R (Kennard-Stone selection)
- Performance improvement: R² increases from 0.87 to 0.96 (clay), 0.84 to 0.91 (pH)
- API unchanged: existing code continues to work
- Addresses #XXX"

git push origin feature/clustered-local-models
```

---

## Risk Assessment

### HIGH RISK:
- ✅ **API compatibility**: Mitigated by keeping function signatures identical
- ✅ **Data loss**: Mitigated by comprehensive testing

### MEDIUM RISK:
- ⚠️ **Cache invalidation**: Old cached results won't match new system
  - **Mitigation**: Increment internal cache version, auto-refresh
- ⚠️ **Cluster reproducibility**: K-means depends on random initialization
  - **Mitigation**: Always use fixed seed (307)

### LOW RISK:
- ✓ **Performance regression**: Local models faster (2 models vs 1 global)
- ✓ **Memory usage**: Similar memory footprint

---

## Rollback Strategy

If something goes wrong:

**Option 1: Quick revert**
```bash
git revert HEAD
git push
```

**Option 2: Keep both methods**
Add `method = c("clustered", "global")` parameter:
```r
predict_soil_covariates <- function(..., method = "clustered") {
  if (method == "global") {
    # Call old system (keep similarity.R)
  } else {
    # Call new system
  }
}
```

---

## Success Criteria

✅ All tests pass
✅ `devtools::check()` returns 0 errors, 0 warnings
✅ Benchmark shows R² ≥ 0.95 for clay, ≥ 0.90 for pH
✅ Predictions complete in <1 hour for 100 samples
✅ Documentation updated and complete
✅ API compatibility maintained (existing scripts run unchanged)

---

## Post-Refactor Tasks

1. Clean up debug messages (remove all `DEBUG:` lines)
2. Update vignette with clustering methodology
3. Add performance comparison to package documentation
4. Consider adding `plot_cluster_assignments()` diagnostic function
5. Benchmark on larger datasets (1000+ samples)

---

## Decisions Made

1. ✅ **Cache strategy**: Invalidate all old caches (clean break)
2. ✅ **Cluster number**: Fixed range (2-10), silhouette selection
3. ✅ **Error handling**: Not needed - OSSL always has sufficient samples
4. ✅ **Parallel strategy**: Keep current (within-model parallelization)

---

*Created: 2025-01-XX*
*Author: Sam Leuthold*
*Status: Ready for implementation*
