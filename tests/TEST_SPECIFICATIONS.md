# Test Specifications for Horizons Package

## Format Template

Each function should have test specifications in the following format:

```markdown
## test-[filename].R

### Function: `function_name()`

#### Test Group 1: [Group Name]

**Test 1.1: [Specific test description]**
- **Setup**: [Required data/mocks]
- **Input**: `function_name(param1 = value1, param2 = value2)`
- **Expected**: [Specific expected outcome]
- **Verify**: [What to check - return value, structure, error message, etc.]

**Test 1.2: [Specific test description]**
- **Setup**: [Required data/mocks]
- **Input**: [Specific function call]
- **Expected**: [Specific expected outcome]
- **Verify**: [What to check]
```

---

## test-inputs-configs.R

### Function: `create_configs()`

#### Test Group 1: Input Validation

**Test 1.1: Required parameters must be provided**
- **Setup**: None
- **Input**: `create_configs()` (no arguments)
- **Expected**: Error about missing required arguments
- **Verify**: `expect_error(..., "missing")`

**Test 1.2: Validates soil_covariates type**
- **Setup**: None
- **Input**: `create_configs(models = "plsr", transformations = "none", preprocessing = "raw", feature_selection = "none", soil_covariates = 123)`
- **Expected**: Error with message "soil_covariates must be a character vector or NULL"
- **Verify**: `expect_error(..., "character vector or NULL")`

**Test 1.3: Empty character vectors not allowed for covariates**
- **Setup**: None
- **Input**: `create_configs(..., soil_covariates = character(0))`
- **Expected**: Error about empty character vector
- **Verify**: `expect_error(..., "character vector or NULL")`

#### Test Group 2: Covariate Combination Generation

**Test 2.1: No covariates produces single 'none' set**
- **Setup**: Basic required params only
- **Input**: `create_configs(models = "plsr", transformations = "none", preprocessing = "raw", feature_selection = "none")`
- **Expected**: config_grid with single covariate_set = "none"
- **Verify**:
  - `expect_equal(unique(result$covariate_set), "none")`
  - `expect_true(is.null(result$covariates[[1]]))`

**Test 2.2: Power set generation for covariates**
- **Setup**: None
- **Input**: `create_configs(..., soil_covariates = c("clay", "sand"))`
- **Expected**: 4 covariate sets: none, clay, sand, clay_sand
- **Verify**:
  - `expect_equal(length(unique(result$covariate_set)), 4)`
  - Check each expected combination exists

**Test 2.3: Multiple covariate types combine correctly**
- **Setup**: None
- **Input**:
  ```r
  create_configs(...,
    soil_covariates = c("clay"),
    climate_covariates = c("MAT"))
  ```
- **Expected**: 4 sets: none, clay, MAT, clay_MAT
- **Verify**: Check all combinations present

#### Test Group 3: Grid Creation

**Test 3.1: Cartesian product calculation**
- **Setup**: None
- **Input**:
  ```r
  create_configs(
    models = c("plsr", "rf"),
    transformations = c("none", "log"),
    preprocessing = c("raw", "snv"),
    feature_selection = c("none"))
  ```
- **Expected**: 2×2×2×1 = 8 base combinations
- **Verify**: `expect_equal(nrow(result), 8)`

**Test 3.2: Config IDs are unique and sequential**
- **Setup**: Create any config grid
- **Input**: Standard function call
- **Expected**: IDs like config_0001, config_0002, etc.
- **Verify**:
  - All IDs unique
  - Properly formatted with 4 digits

#### Test Group 4: Output Structure

**Test 4.1: Column order is correct**
- **Setup**: None
- **Input**: Standard function call
- **Expected**: First column is config_id
- **Verify**: `expect_equal(names(result)[1], "config_id")`

**Test 4.2: Covariates column is list type**
- **Setup**: Create configs with covariates
- **Input**: Standard function call
- **Expected**: covariates column contains lists
- **Verify**: `expect_true(is.list(result$covariates))`

#### Test Group 5: Verbose Output

**Test 5.1: Verbose messages appear correctly**
- **Setup**: Capture cli output
- **Input**: `create_configs(..., verbose = TRUE)`
- **Expected**: Bold headers and tree structure
- **Verify**: Check for "Model Configuration Grid Generation" and tree characters

**Test 5.2: Silent mode produces no output**
- **Setup**: Capture output
- **Input**: `create_configs(..., verbose = FALSE)`
- **Expected**: No cli messages
- **Verify**: `expect_silent(...)`

#### Test Group 6: Edge Cases

**Test 6.1: Single value for each parameter**
- **Setup**: None
- **Input**: All parameters with single values
- **Expected**: Single configuration row
- **Verify**: `expect_equal(nrow(result), 1)`

**Test 6.2: Large covariate sets**
- **Setup**: None
- **Input**: 5 covariates (32 combinations)
- **Expected**: Handles 32 combinations without error
- **Verify**: Check performance and memory usage

---

## test-inputs-create.R

### Function: `create_dataset()`

#### Test Group 1: Input Validation

**Test 1.1: Validates spectra_data type**
- **Setup**: None
- **Input**: `create_dataset(spectra_data = "not_a_dataframe", response_data = valid_df)`
- **Expected**: Error with message "spectra_data must be a data frame or tibble"
- **Verify**: `expect_error(..., "data frame or tibble")`

**Test 1.2: Requires id_column in spectra**
- **Setup**: Create spectra without Sample_ID
- **Input**: `create_dataset(spectra_data = missing_id_df, response_data = valid_df)`
- **Expected**: Error about missing Sample_ID column
- **Verify**: `expect_error(..., "not found in spectra_data")`

**Test 1.3: Response file must exist**
- **Setup**: None
- **Input**: `create_dataset(spectra_data = valid_df, response_data = "nonexistent.csv")`
- **Expected**: Error about file not found
- **Verify**: `expect_error(..., "Response file not found")`

**Test 1.4: Parse IDs requires format**
- **Setup**: Valid data
- **Input**: `create_dataset(..., parse_ids = TRUE, id_format = NULL)`
- **Expected**: Error "id_format must be provided when parse_ids = TRUE"
- **Verify**: `expect_error(..., "id_format must be provided")`

#### Test Group 2: ID Parsing

**Test 2.1: Successful ID parsing**
- **Setup**:
  ```r
  spectra_data <- data.frame(
    Sample_ID = c("P1_S001_bulk_1", "P1_S001_bulk_2"),
    `600` = c(0.1, 0.2)
  )
  ```
- **Input**: `create_dataset(..., parse_ids = TRUE, id_format = "project_sampleid_fraction_scan")`
- **Expected**: Parsed columns added (project, sampleid, fraction, scan)
- **Verify**: Check for presence of parsed columns

**Test 2.2: Aggregation by parsed columns**
- **Setup**: Multiple scans of same sample
- **Input**: `create_dataset(..., parse_ids = TRUE, aggregate_by = c("project", "sampleid"))`
- **Expected**: Scans averaged into single row per sample
- **Verify**: `expect_equal(nrow(result), n_unique_samples)`

#### Test Group 3: Replicate Aggregation

**Test 3.1: Averaging spectral replicates**
- **Setup**:
  ```r
  spectra_data <- data.frame(
    Sample_ID = c("S1", "S1", "S2"),
    `600` = c(0.1, 0.3, 0.5),
    `650` = c(0.2, 0.4, 0.6)
  )
  ```
- **Input**: `create_dataset(spectra_data, response_data)`
- **Expected**: S1 values averaged: 600=0.2, 650=0.3
- **Verify**: Check averaged values and n_replicates column

**Test 3.2: Preserves non-spectral columns during aggregation**
- **Setup**: Spectra with metadata columns
- **Input**: Standard aggregation
- **Expected**: First value of metadata preserved
- **Verify**: Check metadata columns retained

#### Test Group 4: Joining Data

**Test 4.1: Inner join behavior**
- **Setup**: Spectra with 5 samples, response with 3 (2 overlap)
- **Input**: `create_dataset(..., join_type = "inner")`
- **Expected**: Result has 2 rows (overlapping only)
- **Verify**: `expect_equal(nrow(result), 2)`

**Test 4.2: Left join preserves all spectra**
- **Setup**: More spectra than response data
- **Input**: `create_dataset(..., join_type = "left")`
- **Expected**: All spectra retained, NAs for missing response
- **Verify**: Check row count matches spectra

**Test 4.3: Response variable selection**
- **Setup**: Response with many columns
- **Input**: `create_dataset(..., response_variables = c("SOC", "pH"))`
- **Expected**: Only requested variables in output
- **Verify**: Check column names

#### Test Group 5: Coordinate Handling

**Test 5.1: Auto-detect coordinate columns**
- **Setup**: Response with Latitude, Longitude columns
- **Input**: `create_dataset(..., include_coords = TRUE)`
- **Expected**: Coordinates included in output
- **Verify**: `expect_true(all(c("Latitude", "Longitude") %in% names(result)))`

**Test 5.2: Explicit coordinate specification**
- **Setup**: Response with custom coord names
- **Input**: `create_dataset(..., coord_columns = c("x_coord", "y_coord"))`
- **Expected**: Specified coords included
- **Verify**: Check for custom column names

#### Test Group 6: Missing Data Handling

**Test 6.1: Drop NA in response variables**
- **Setup**: Response with some NA values
- **Input**: `create_dataset(..., drop_na = TRUE)`
- **Expected**: Rows with NA in response removed
- **Verify**: `expect_false(any(is.na(result[response_vars])))`

**Test 6.2: Keep NA when requested**
- **Setup**: Response with NA values
- **Input**: `create_dataset(..., drop_na = FALSE)`
- **Expected**: NA values retained
- **Verify**: Check NA values present

#### Test Group 7: Verbose Output

**Test 7.1: Verbose messages appear**
- **Setup**: Capture cli output
- **Input**: `create_dataset(..., verbose = TRUE)`
- **Expected**: Bold headers and tree structure
- **Verify**: Check for "Dataset Creation Pipeline" header

**Test 7.2: Silent mode**
- **Setup**: Capture output
- **Input**: `create_dataset(..., verbose = FALSE)`
- **Expected**: No cli messages
- **Verify**: `expect_silent(...)`

---

## test-covariates-orchestrator.R

### Function: `fetch_covariates()`

#### Test Group 1: Input Validation

**Test 1.1: Rejects non-data.frame input**
- **Setup**: Create a matrix with spectral data
- **Input**: `fetch_covariates(input_data = matrix(1:100, ncol = 10))`
- **Expected**: Error with message "input_data must be a data.frame or tibble"
- **Verify**: `expect_error(..., "must be a data.frame")`

**Test 1.2: Requires Sample_ID column**
- **Setup**: Create data.frame without Sample_ID column
  ```r
  test_data <- data.frame(
    `600` = rnorm(10),
    `650` = rnorm(10),
    `700` = rnorm(10)
  )
  ```
- **Input**: `fetch_covariates(input_data = test_data)`
- **Expected**: Error with message "input_data must contain a Sample_ID column"
- **Verify**: `expect_error(..., "Sample_ID column")`

**Test 1.3: Validates coordinates when climate requested**
- **Setup**: Data with Sample_ID and spectra but no coordinates
  ```r
  test_data <- data.frame(
    Sample_ID = paste0("S", 1:10),
    `600` = rnorm(10),
    `650` = rnorm(10)
  )
  ```
- **Input**: `fetch_covariates(input_data = test_data, climate_covariates = c("MAT", "MAP"))`
- **Expected**: Error with message about missing Longitude/Latitude
- **Verify**: `expect_error(..., "Longitude and Latitude")`

**Test 1.4: Validates numeric parameters**
- **Setup**: Valid input data
- **Input**: `fetch_covariates(input_data = valid_data, prop_train = 1.5)`
- **Expected**: Error with message "prop_train must be between 0 and 1"
- **Verify**: `expect_error(..., "between 0 and 1")`

#### Test Group 2: Configuration Handling

**Test 2.1: Configurations override individual parameters**
- **Setup**:
  ```r
  configs <- create_configs(soil_covariates = c("clay", "sand"))
  test_data <- create_test_spectral_data(n = 20)
  ```
- **Input**: `fetch_covariates(test_data, configurations = configs, soil_covariates = c("ph"))`
- **Expected**: Warning about ignoring individual parameters, uses "clay" and "sand" from configs
- **Verify**:
  - `expect_warning(..., "Ignoring individual")`
  - Check that returned metadata shows soil_covariates = c("clay", "sand")

**Test 2.2: Unknown covariates trigger abort**
- **Setup**:
  ```r
  configs <- data.frame(
    covariates = list(c("clay", "bogus_covariate"))
  )
  ```
- **Input**: `fetch_covariates(test_data, configurations = configs)`
- **Expected**: Error via stop_quietly() about unknown covariates
- **Verify**: Check for specific error pattern

#### Test Group 3: Soil Prediction Workflow

**Test 3.1: Cache key generation is deterministic**
- **Setup**: Same input data, run twice
  ```r
  test_data <- create_test_spectral_data(n = 50)
  with_mocked_bindings(
    predict_soil_covariates = function(...) list(predictions = data.frame(Sample_ID = test_data$Sample_ID, clay = rnorm(50))),
    {
      result1 <- fetch_covariates(test_data, soil_covariates = "clay")
      result2 <- fetch_covariates(test_data, soil_covariates = "clay")
    }
  )
  ```
- **Expected**: Both calls use same cache (second should be faster)
- **Verify**:
  - Check execution times
  - Verify cache file exists in cache_dir

**Test 3.2: Handles predict_soil_covariates failure**
- **Setup**: Mock predict_soil_covariates to return NULL
  ```r
  with_mocked_bindings(
    predict_soil_covariates = function(...) NULL,
    {
      result <- fetch_covariates(test_data, soil_covariates = "clay")
    }
  )
  ```
- **Expected**: Function continues, returns result without soil data
- **Verify**:
  - No error thrown
  - result$soil_predictions is NULL
  - result$covariate_data has only Sample_ID

#### Test Group 4: Climate Data Workflow

**Test 4.1: Climate data filtered to requested variables**
- **Setup**: Mock fetch_climate_covariates to return all variables
  ```r
  with_mocked_bindings(
    fetch_climate_covariates = function(...) {
      data.frame(
        Sample_ID = test_data$Sample_ID,
        MAT = rnorm(10),
        MAP = rnorm(10),
        PET = rnorm(10),  # Not requested
        AI = rnorm(10)    # Not requested
      )
    },
    {
      result <- fetch_covariates(test_data, climate_covariates = c("MAT", "MAP"))
    }
  )
  ```
- **Expected**: Only MAT and MAP in final covariate_data
- **Verify**:
  - `expect_true(all(c("MAT", "MAP") %in% names(result$covariate_data)))`
  - `expect_false("PET" %in% names(result$covariate_data))`

#### Test Group 5: Integration Tests

**Test 5.1: Full workflow with soil + climate**
- **Setup**: Complete test dataset with coordinates
  ```r
  test_data <- data.frame(
    Sample_ID = paste0("S", 1:20),
    `600` = rnorm(20),
    `650` = rnorm(20),
    `700` = rnorm(20),
    Longitude = runif(20, -100, -80),
    Latitude = runif(20, 35, 45)
  )
  ```
- **Input**:
  ```r
  with_mocked_bindings(
    predict_soil_covariates = mock_soil_predictor,
    fetch_climate_covariates = mock_climate_fetcher,
    {
      result <- fetch_covariates(
        test_data,
        soil_covariates = c("clay", "sand"),
        climate_covariates = c("MAT", "MAP"),
        verbose = FALSE
      )
    }
  )
  ```
- **Expected**: Complete result with all components
- **Verify**:
  - result$covariate_data has Sample_ID, clay, sand, MAT, MAP
  - result$soil_predictions is not NULL
  - result$metadata$n_covariates_returned == 4

**Test 5.2: Spectra-only workflow (no covariates)**
- **Setup**: Valid spectral data
- **Input**: `fetch_covariates(test_data, verbose = FALSE)`
- **Expected**: Returns valid result with just Sample_ID
- **Verify**:
  - result$covariate_data has only Sample_ID column
  - No errors or warnings

#### Test Group 6: Edge Cases

**Test 6.1: Single sample input**
- **Setup**: Data frame with one row
  ```r
  single_sample <- data.frame(
    Sample_ID = "S1",
    `600` = 0.5,
    `650` = 0.6,
    `700` = 0.7
  )
  ```
- **Input**: `fetch_covariates(single_sample, soil_covariates = "clay")`
- **Expected**: Works without error
- **Verify**: Result structure is valid

**Test 6.2: All covariates fail**
- **Setup**: Mock all prediction functions to fail
  ```r
  with_mocked_bindings(
    predict_soil_covariates = function(...) NULL,
    fetch_climate_covariates = function(...) NULL,
    {
      result <- fetch_covariates(test_data,
                                 soil_covariates = "clay",
                                 climate_covariates = "MAT")
    }
  )
  ```
- **Expected**: Returns result with empty covariate_data
- **Verify**:
  - No error
  - result$metadata$n_covariates_returned == 0

---

## test-covariates-similarity.R

### Function: `select_global_training_set()`

#### Test Group 1: Input Validation

**Test 1.1: Validates proportion parameter**
- **Setup**: Valid PCA scores data
- **Input**: `select_global_training_set(..., prop = 1.5)`
- **Expected**: Error with message about prop being between 0 and 1
- **Verify**: `expect_error(..., "between 0 and 1")`

**Test 1.2: Requires PCA columns in input**
- **Setup**: Data frames without Dim.* columns
- **Input**: `select_global_training_set(unknown_pca_scores = no_pca_df, ...)`
- **Expected**: Error or empty result
- **Verify**: Check for appropriate error handling

#### Test Group 2: Pre-filtering Logic

**Test 2.1: Relevance threshold filters correctly**
- **Setup**:
  ```r
  unknown_scores <- create_test_pca_scores(100)
  ossl_scores <- create_test_pca_scores(1000)
  ```
- **Input**: `select_global_training_set(..., relevance_threshold = 0.5)`
- **Expected**: Pre-filters to ~500 OSSL samples before selection
- **Verify**: Check intermediate sizes in verbose output

**Test 2.2: Mahalanobis distance calculation**
- **Setup**: Known PCA scores with calculable distances
- **Input**: Standard function call
- **Expected**: Correct ordering by distance to unknown centroid
- **Verify**: Manually calculate distances and compare ordering

#### Test Group 3: Clustering Integration

**Test 3.1: Clustering is called correctly**
- **Setup**: Mock cluster_unknown_samples to return known output
- **Input**: Standard function call
- **Expected**: Function uses cluster results for stratification
- **Verify**: Check that cluster proportions affect selection

#### Test Group 4: Train/Validation Split

**Test 4.1: Split respects proportion**
- **Setup**: Standard test data
- **Input**: `select_global_training_set(..., prop = 0.7)`
- **Expected**: 70% in train_data, 30% in val_data
- **Verify**:
  ```r
  expect_equal(nrow(result$train_data) / nrow(selected_ossl), 0.7, tolerance = 0.05)
  ```

#### Test Group 5: Edge Cases

**Test 5.1: Small dataset handling**
- **Setup**: Only 10 unknown and 50 OSSL samples
- **Input**: `select_global_training_set(..., n_select = 20)`
- **Expected**: Handles gracefully, selects available samples
- **Verify**: No errors, reasonable output

### Function: `cluster_unknown_samples()`

#### Test Group 1: Parameter Validation

**Test 1.1: Handles max_clusters larger than samples**
- **Setup**: 5 samples, max_clusters = 10
- **Input**: `cluster_unknown_samples(small_data, max_clusters = 10)`
- **Expected**: Adjusts max_k to floor(5/3) = 1, returns single cluster
- **Verify**: Result has n_clusters = 1

#### Test Group 2: Clustering Logic

**Test 2.1: Too few samples returns single cluster**
- **Setup**: Create PCA scores with 3 rows
- **Input**: `cluster_unknown_samples(tiny_data)`
- **Expected**: Single cluster with appropriate message
- **Verify**:
  - `expect_equal(result$n_clusters, 1)`
  - `expect_true(all(result$cluster_assignments == 1))`

**Test 2.2: Silhouette optimization selects best k**
- **Setup**: Data with clear 3-cluster structure
  ```r
  # Create three separated groups in PCA space
  group1 <- matrix(rnorm(50*10, mean = 0), ncol = 10)
  group2 <- matrix(rnorm(50*10, mean = 5), ncol = 10)
  group3 <- matrix(rnorm(50*10, mean = 10), ncol = 10)
  pca_scores <- rbind(group1, group2, group3)
  ```
- **Input**: `cluster_unknown_samples(pca_scores, max_clusters = 5)`
- **Expected**: Selects k = 3 as optimal
- **Verify**: `expect_equal(result$n_clusters, 3)`

**Test 2.3: Reproducibility with seed**
- **Setup**: Same data, run twice
- **Input**: `cluster_unknown_samples(..., seed = 123)`
- **Expected**: Identical results both times
- **Verify**: `expect_identical(result1, result2)`

#### Test Group 3: Error Handling

**Test 3.1: K-means failure handling**
- **Setup**: Mock kmeans to fail for k=3
  ```r
  with_mocked_bindings(
    kmeans = function(...) if (k == 3) stop("convergence") else real_kmeans(...),
    {
      result <- cluster_unknown_samples(data, max_clusters = 5)
    }
  )
  ```
- **Expected**: Skips k=3, continues with other values
- **Verify**: Check silhouette_scores[2] == -1 (for k=3)

#### Test Group 4: Progress Reporting

**Test 4.1: Verbose output structure**
- **Setup**: Capture cli output
- **Input**: `cluster_unknown_samples(..., verbose = TRUE)`
- **Expected**: Tree-structured output with status updates
- **Verify**: Check for expected cli messages

### Function: `stratified_kennard_stone()`

#### Test Group 1: Allocation Logic

**Test 1.1: Proportional allocation sums correctly**
- **Setup**:
  ```r
  clusters <- list(
    n_clusters = 3,
    cluster_proportions = c(0.5, 0.3, 0.2),
    cluster_assignments = c(rep(1,50), rep(2,30), rep(3,20))
  )
  ```
- **Input**: `stratified_kennard_stone(..., n_select = 100)`
- **Expected**: Allocates ~50, 30, 20 samples per cluster
- **Verify**: Sum of allocations equals exactly 100

**Test 1.2: Allocation adjustment for rounding**
- **Setup**: Proportions that don't round evenly
  ```r
  clusters <- list(
    n_clusters = 3,
    cluster_proportions = c(0.333, 0.333, 0.334)
  )
  ```
- **Input**: `stratified_kennard_stone(..., n_select = 100)`
- **Expected**: Adjusts to get exactly 100
- **Verify**: Check adjustment logic adds/removes correctly

#### Test Group 2: OSSL Assignment

**Test 2.1: Each OSSL assigned to nearest cluster**
- **Setup**: Known cluster centers and OSSL positions
- **Input**: Standard function call
- **Expected**: OSSL samples assigned to geometrically nearest cluster
- **Verify**: Manually calculate distances and verify assignments

**Test 2.2: All clusters get OSSL samples**
- **Setup**: Well-distributed OSSL and unknown samples
- **Input**: Standard function call
- **Expected**: Each cluster has some OSSL samples assigned
- **Verify**: No cluster has zero OSSL samples

#### Test Group 3: Kennard-Stone Selection

**Test 3.1: Handles clusters with insufficient OSSL**
- **Setup**: One cluster with only 5 OSSL but needs 10
- **Input**: Standard function call
- **Expected**: Takes all 5 available, warns user
- **Verify**:
  - Warning message appears
  - Returns all available samples for that cluster

**Test 3.2: ProspectR integration**
- **Setup**: Mock prospectr::kenStone
- **Input**: Standard function call
- **Expected**: Calls kenStone with correct parameters
- **Verify**: Check mock was called with expected arguments

#### Test Group 4: Edge Cases

**Test 4.1: Single cluster scenario**
- **Setup**: cluster_result with n_clusters = 1
- **Input**: `stratified_kennard_stone(...)`
- **Expected**: Performs standard Kennard-Stone on all samples
- **Verify**: Returns expected number of samples

**Test 4.2: No duplicates in output**
- **Setup**: Standard test data
- **Input**: Standard function call
- **Expected**: All returned indices are unique
- **Verify**: `expect_equal(length(result), length(unique(result)))`

---

## Helper Functions for Tests

```r
# Create consistent test spectral data
create_test_spectral_data <- function(n = 100,
                                     with_coords = FALSE,
                                     seed = 123) {
  set.seed(seed)

  wavenumbers <- seq(600, 4000, by = 2)
  spectral_cols <- matrix(
    rnorm(n * length(wavenumbers)),
    nrow = n
  )
  colnames(spectral_cols) <- as.character(wavenumbers)

  base_data <- data.frame(
    Sample_ID = paste0("S", seq_len(n)),
    spectral_cols
  )

  if (with_coords) {
    base_data$Longitude <- runif(n, -100, -80)
    base_data$Latitude <- runif(n, 35, 45)
  }

  return(base_data)
}

# Create test PCA scores for similarity functions
create_test_pca_scores <- function(n = 100,
                                  n_dims = 10,
                                  seed = 123) {
  set.seed(seed)

  pca_cols <- matrix(
    rnorm(n * n_dims),
    nrow = n
  )
  colnames(pca_cols) <- paste0("Dim.", seq_len(n_dims))

  data.frame(
    Sample_ID = paste0("S", seq_len(n)),
    pca_cols
  )
}

# Mock soil predictor for consistent testing
mock_soil_predictor <- function(input_data, covariates, ...) {
  n <- nrow(input_data)
  predictions <- data.frame(Sample_ID = input_data$Sample_ID)

  for (cov in covariates) {
    predictions[[cov]] <- rnorm(n, mean = 30, sd = 10)
  }

  return(list(
    predictions = predictions,
    global_models = list()
  ))
}

# Mock climate fetcher for consistent testing
mock_climate_fetcher <- function(input_data, ...) {
  data.frame(
    Sample_ID = input_data$Sample_ID,
    MAT = rnorm(nrow(input_data), 15, 5),
    MAP = rnorm(nrow(input_data), 800, 200),
    PET = rnorm(nrow(input_data), 1000, 100),
    AI = rnorm(nrow(input_data), 0.5, 0.2)
  )
}
```