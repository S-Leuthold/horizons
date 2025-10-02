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

## test-inputs-finalize.R

### Function: `finalize_dataset()`

#### Test Group 1: Input Validation

**Test 1.1: Validates dataset is data frame**
- **Setup**: None
- **Input**: `finalize_dataset(dataset = "not_a_dataframe", response_variable = "SOC")`
- **Expected**: Error with message "dataset must be a data frame or tibble"
- **Verify**: `expect_error(..., "data frame or tibble")`

**Test 1.2: Response variable must exist in dataset**
- **Setup**: Create dataset without requested response variable
- **Input**: `finalize_dataset(dataset = test_data, response_variable = "missing_var")`
- **Expected**: Error with message "Response variable 'missing_var' not found in dataset"
- **Verify**: `expect_error(..., "not found in dataset")`

**Test 1.3: Response variable must be numeric**
- **Setup**:
  ```r
  test_data <- data.frame(
    Sample_ID = c("S1", "S2"),
    `600` = c(0.1, 0.2),
    SOC = c("high", "low")  # Character instead of numeric
  )
  ```
- **Input**: `finalize_dataset(dataset = test_data, response_variable = "SOC")`
- **Expected**: Error with message "Response variable must be numeric for outlier detection"
- **Verify**: `expect_error(..., "must be numeric")`

**Test 1.4: Dataset must contain spectral columns**
- **Setup**: Create dataset with no spectral columns (no numeric column names)
  ```r
  test_data <- data.frame(
    Sample_ID = c("S1", "S2"),
    SOC = c(1.5, 2.3)
  )
  ```
- **Input**: `finalize_dataset(dataset = test_data, response_variable = "SOC")`
- **Expected**: Error with message "No spectral columns detected in dataset"
- **Verify**: `expect_error(..., "No spectral columns")`

**Test 1.5: Spectral cutoff must be in (0, 1) range**
- **Setup**: Valid dataset
- **Input**: `finalize_dataset(..., spectral_cutoff = 1.5)`
- **Expected**: Error with message "spectral_cutoff must be between 0 and 1 (you provided 1.5)"
- **Verify**: `expect_error(..., "between 0 and 1")`

**Test 1.6: Response cutoff must be positive**
- **Setup**: Valid dataset
- **Input**: `finalize_dataset(..., response_cutoff = -1)`
- **Expected**: Error with message "response_cutoff must be positive (you provided -1)"
- **Verify**: `expect_error(..., "must be positive")`

**Test 1.7: Minimum sample size requirement**
- **Setup**: Dataset with only 5 samples
- **Input**: `finalize_dataset(dataset = small_data, response_variable = "SOC")`
- **Expected**: Error with message "Need at least 10 samples for reliable outlier detection (dataset has 5)"
- **Verify**: `expect_error(..., "at least 10 samples")`

#### Test Group 2: Spectral Outlier Detection (Mahalanobis)

**Test 2.1: PCA retains 99% variance**
- **Setup**: Create dataset with known variance structure
- **Input**: `finalize_dataset(..., spectral_outlier_method = "mahalanobis", verbose = FALSE)`
- **Expected**: PCA component selection reaches ~99% variance
- **Verify**: Check that n_components is reasonable for dataset dimensionality

**Test 2.2: Identifies extreme spectral outliers**
- **Setup**:
  ```r
  # Create dataset with one obvious outlier (all zeros)
  test_data <- data.frame(
    Sample_ID = paste0("S", 1:50),
    matrix(rnorm(50 * 100, mean = 0.5, sd = 0.1), ncol = 100)
  )
  colnames(test_data)[-1] <- paste0("", 600:699)
  # Add outlier
  test_data[1, -1] <- 0  # All-zero spectrum
  test_data$SOC <- rnorm(50, mean = 2, sd = 0.5)
  ```
- **Input**: `finalize_dataset(test_data, response_variable = "SOC", spectral_cutoff = 0.975)`
- **Expected**: Sample S1 flagged as outlier
- **Verify**:
  - `expect_equal(result$outlier_flag[1], "outlier")`
  - Check Mahalanobis distance for S1 is extreme

**Test 2.3: Graceful degradation when PCA fails**
- **Setup**: Mock prcomp to fail
  ```r
  with_mocked_bindings(
    prcomp = function(...) stop("Singular matrix"),
    {
      result <- finalize_dataset(test_data, response_variable = "SOC")
    }
  )
  ```
- **Expected**: Function continues without spectral outlier detection
- **Verify**:
  - No error thrown
  - Warning about PCA failure
  - Error hints displayed
  - result has outlier_flag column

**Test 2.4: Handles singular covariance matrix**
- **Setup**: Create data where PC scores have perfect collinearity
- **Input**: `finalize_dataset(..., spectral_outlier_method = "mahalanobis")`
- **Expected**: Graceful degradation with warning
- **Verify**:
  - Warning about Mahalanobis calculation failure
  - Function continues
  - No crash

**Test 2.5: Skip spectral detection when method = "none"**
- **Setup**: Valid dataset
- **Input**: `finalize_dataset(..., spectral_outlier_method = "none")`
- **Expected**: Only response outliers detected
- **Verify**: Check that spectral outlier code path not executed

#### Test Group 3: Response Outlier Detection (IQR)

**Test 3.1: IQR method identifies extreme values**
- **Setup**:
  ```r
  test_data <- data.frame(
    Sample_ID = paste0("S", 1:50),
    `600` = rnorm(50),
    SOC = c(rnorm(49, mean = 2, sd = 0.5), 20)  # Last value is extreme
  )
  ```
- **Input**: `finalize_dataset(test_data, response_variable = "SOC", response_cutoff = 1.5)`
- **Expected**: Sample S50 flagged as outlier
- **Verify**:
  - `expect_equal(result$outlier_flag[50], "outlier")`
  - Check calculated bounds

**Test 3.2: Warns for heavily skewed distributions**
- **Setup**:
  ```r
  # Create heavily right-skewed data
  test_data <- data.frame(
    Sample_ID = paste0("S", 1:50),
    `600` = rnorm(50),
    SOC = rexp(50, rate = 0.5)  # Exponential distribution (skewed)
  )
  ```
- **Input**: `finalize_dataset(test_data, response_variable = "SOC", verbose = TRUE)`
- **Expected**: Warning about heavy skewness with skewness value
- **Verify**:
  - `expect_warning(..., "heavily skewed")`
  - Suggestion to consider log-transformation appears

**Test 3.3: Warns for small sample size**
- **Setup**: Dataset with 25 samples (< 30)
- **Input**: `finalize_dataset(small_data, response_variable = "SOC", verbose = TRUE)`
- **Expected**: Warning "Small sample size (n = 25) may reduce outlier detection reliability"
- **Verify**: `expect_warning(..., "Small sample size")`

**Test 3.4: Reports outlier bounds in verbose mode**
- **Setup**: Valid dataset
- **Input**: `finalize_dataset(..., verbose = TRUE)`, capture output
- **Expected**: Output contains "Outlier bounds: [lower, upper]" message
- **Verify**: Check cli output contains bounds

**Test 3.5: Skip response detection when disabled**
- **Setup**: Valid dataset
- **Input**: `finalize_dataset(..., detect_response_outliers = FALSE)`
- **Expected**: Only spectral outliers detected
- **Verify**: Response outlier code path not executed

#### Test Group 4: Combined Outlier Handling

**Test 4.1: Combines spectral and response outliers**
- **Setup**: Dataset with one spectral outlier and one response outlier (different samples)
- **Input**: `finalize_dataset(test_data, response_variable = "SOC")`
- **Expected**: Both samples flagged as outliers
- **Verify**: Check outlier_flag for both samples = "outlier"

**Test 4.2: Sample can be flagged by both methods**
- **Setup**: Create sample that is outlier in both spectral and response
- **Input**: `finalize_dataset(...)`
- **Expected**: Sample flagged once (unique combination)
- **Verify**: outlier_flag = "outlier", count verified

**Test 4.3: Outlier removal when requested**
- **Setup**: Dataset with 50 samples, 3 outliers
- **Input**: `finalize_dataset(..., remove_outliers = TRUE)`
- **Expected**: Returned dataset has 47 rows, outliers removed
- **Verify**:
  - `expect_equal(nrow(result), 47)`
  - Removed samples not present

**Test 4.4: Outlier flagging when removal disabled**
- **Setup**: Dataset with outliers
- **Input**: `finalize_dataset(..., remove_outliers = FALSE)`
- **Expected**: All samples retained, outlier_flag column added
- **Verify**:
  - `expect_equal(nrow(result), nrow(test_data))`
  - `expect_true("outlier_flag" %in% names(result))`
  - Check specific flag values

#### Test Group 5: Output Structure

**Test 5.1: outlier_flag column added**
- **Setup**: Any valid dataset
- **Input**: `finalize_dataset(...)`
- **Expected**: Result has outlier_flag column with values "good" or "outlier"
- **Verify**:
  - `expect_true("outlier_flag" %in% names(result))`
  - `expect_true(all(result$outlier_flag %in% c("good", "outlier")))`

**Test 5.2: All original columns preserved (when not removing)**
- **Setup**: Dataset with specific columns
- **Input**: `finalize_dataset(..., remove_outliers = FALSE)`
- **Expected**: All original columns plus outlier_flag
- **Verify**:
  - `expect_true(all(names(test_data) %in% names(result)))`
  - Only new column is outlier_flag

**Test 5.3: Returns tibble**
- **Setup**: Any valid dataset
- **Input**: `finalize_dataset(...)`
- **Expected**: Result is a tibble
- **Verify**: `expect_s3_class(result, "tbl_df")`

#### Test Group 6: Verbose Output

**Test 6.1: Configuration summary displays correctly**
- **Setup**: Capture cli output
- **Input**: `finalize_dataset(..., verbose = TRUE)`
- **Expected**: Shows input samples, spectral features, response variable, method settings
- **Verify**: Check for "Dataset Finalization Configuration" header and all config lines

**Test 6.2: Outlier detection pipeline header**
- **Setup**: Capture cli output
- **Input**: `finalize_dataset(..., verbose = TRUE)`
- **Expected**: Shows "Outlier Detection Pipeline" with tree structure
- **Verify**: Check for pipeline header and ├─ └─ characters

**Test 6.3: Final summary shows all metrics**
- **Setup**: Dataset with known outliers
- **Input**: `finalize_dataset(..., verbose = TRUE)`, capture output
- **Expected**: Summary shows total outliers, spectral count, response count, final samples, action, time
- **Verify**: Check all summary lines present

**Test 6.4: Silent mode produces no output**
- **Setup**: Capture output
- **Input**: `finalize_dataset(..., verbose = FALSE)`
- **Expected**: No cli messages
- **Verify**: `expect_silent(...)` or output length = 0

#### Test Group 7: Edge Cases

**Test 7.1: No outliers detected (clean data)**
- **Setup**: Well-behaved dataset with no extreme values
- **Input**: `finalize_dataset(clean_data, response_variable = "SOC")`
- **Expected**: All samples have outlier_flag = "good"
- **Verify**:
  - `expect_true(all(result$outlier_flag == "good"))`
  - Summary shows 0 outliers

**Test 7.2: All samples are outliers (edge case)**
- **Setup**: Extreme data where all might be flagged
- **Input**: `finalize_dataset(...)`
- **Expected**: Handles gracefully
- **Verify**: Function doesn't crash, reasonable output

**Test 7.3: NA values in response variable**
- **Setup**:
  ```r
  test_data <- data.frame(
    Sample_ID = paste0("S", 1:50),
    `600` = rnorm(50),
    SOC = c(rnorm(45), rep(NA, 5))
  )
  ```
- **Input**: `finalize_dataset(test_data, response_variable = "SOC")`
- **Expected**: NAs handled in IQR calculation (removed for stats)
- **Verify**: Function completes, only non-NA values used for bounds

**Test 7.4: Minimum viable dataset (n = 10)**
- **Setup**: Exactly 10 samples
- **Input**: `finalize_dataset(minimal_data, response_variable = "SOC")`
- **Expected**: Passes validation, completes outlier detection
- **Verify**: No errors, reasonable output

#### Test Group 8: Error Hint Display

**Test 8.1: PCA failure shows actionable hints**
- **Setup**: Mock PCA to fail
- **Input**: `finalize_dataset(..., verbose = TRUE)`
- **Expected**: Error hints start with "Check if..." or "Check for..."
- **Verify**: Check cli output contains diagnostic suggestions

**Test 8.2: Mahalanobis failure shows correct hints**
- **Setup**: Mock Mahalanobis to fail
- **Input**: `finalize_dataset(..., verbose = TRUE)`
- **Expected**: Hints about singular covariance, insufficient variance
- **Verify**: Check specific hint text

---

## test-inputs-helpers.R

### Function: `read_opus_internal()`

#### Test Group 1: File Input Handling

**Test 1.1: Reads single OPUS file successfully**
- **Setup**: Create test OPUS file with known spectral data
- **Input**: `read_opus_internal(path = "test.0", spectra_type = "MIR", verbose = FALSE)`
- **Expected**: Tibble with Sample_ID and spectral columns
- **Verify**: Check column names include Sample_ID and numeric wavenumbers

**Test 1.2: Reads directory of OPUS files**
- **Setup**: Create directory with multiple OPUS files
- **Input**: `read_opus_internal(path = "test_dir/", spectra_type = "MIR", verbose = FALSE)`
- **Expected**: Tibble with all samples combined
- **Verify**: `nrow(result) == number_of_files`

**Test 1.3: Aborts when directory has no OPUS files**
- **Setup**: Empty directory
- **Input**: `read_opus_internal(path = "empty_dir/", spectra_type = "MIR", verbose = FALSE)`
- **Expected**: Error "No OPUS files found in directory"
- **Verify**: `expect_error(..., "No OPUS files")`

#### Test Group 2: Channel Selection Logic

**Test 2.1: Selects preferred channel (ab_no_atm_comp)**
- **Setup**: OPUS file with multiple channels including ab_no_atm_comp
- **Input**: `read_opus_internal(...)`
- **Expected**: Uses ab_no_atm_comp channel
- **Verify**: `attr(result, "channel_used") == "ab_no_atm_comp"`

**Test 2.2: Falls back to next available channel**
- **Setup**: OPUS file with only sc_sample channel
- **Input**: `read_opus_internal(...)`
- **Expected**: Uses sc_sample channel
- **Verify**: `attr(result, "channel_used") == "sc_sample"`

**Test 2.3: Warns when multiple channels used across files**
- **Setup**: Directory where different files have different channels
- **Input**: `read_opus_internal(..., verbose = TRUE)`
- **Expected**: Verbose output shows warning about multiple channels
- **Verify**: Check cli output contains "Multiple channels used"

#### Test Group 3: Error Handling

**Test 3.1: Handles opusreader2 failure gracefully**
- **Setup**: Mock opusreader2::read_opus to fail
- **Input**: `read_opus_internal(...)`
- **Expected**: Returns NULL with error hints
- **Verify**: Check error hints mention file path, format, package installation

**Test 3.2: Returns NULL when no valid spectral data found**
- **Setup**: OPUS files with no parseable channels
- **Input**: `read_opus_internal(...)`
- **Expected**: Error "No valid spectral data found in any files"
- **Verify**: `expect_error(..., "No valid spectral data")`

---

### Function: `read_csv_internal()`

#### Test Group 1: CSV Reading and Validation

**Test 1.1: Reads valid CSV with numeric wavenumbers**
- **Setup**: CSV with Sample_ID column and numeric wavenumber columns
- **Input**: `read_csv_internal(path = "test.csv", spectra_type = "MIR", verbose = FALSE)`
- **Expected**: Tibble with standardized column names
- **Verify**: `expect_true("Sample_ID" %in% names(result))`

**Test 1.2: Converts first column to Sample_ID**
- **Setup**: CSV where first column is named "sample_name"
- **Input**: `read_csv_internal(...)`
- **Expected**: First column renamed to Sample_ID
- **Verify**: `names(result)[1] == "Sample_ID"`

**Test 1.3: Converts spectral values to numeric**
- **Setup**: CSV with character-encoded numeric values
- **Input**: `read_csv_internal(...)`
- **Expected**: All spectral columns are numeric
- **Verify**: `all(sapply(result[,-1], is.numeric))`

**Test 1.4: Aborts when CSV has fewer than 2 columns**
- **Setup**: CSV with only one column
- **Input**: `read_csv_internal(...)`
- **Expected**: Error "CSV must have at least 2 columns"
- **Verify**: `expect_error(..., "at least 2 columns")`

#### Test Group 2: Column Name Validation

**Test 2.1: Warns when column names aren't numeric**
- **Setup**: CSV with text column names like "peak1", "peak2"
- **Input**: `read_csv_internal(...)`
- **Expected**: Yellow warning about non-numeric column names
- **Verify**: Check cli output contains warning, but function still returns data

**Test 2.2: Accepts numeric column names**
- **Setup**: CSV with column names like "600.5", "601.0"
- **Input**: `read_csv_internal(...)`
- **Expected**: No warnings, data returned successfully
- **Verify**: `expect_silent(...)`

#### Test Group 3: Error Handling

**Test 3.1: Handles readr::read_csv failure gracefully**
- **Setup**: Mock readr::read_csv to fail
- **Input**: `read_csv_internal(...)`
- **Expected**: Returns NULL with error hints about file path, format, encoding
- **Verify**: Check error hints are informative

**Test 3.2: Returns data despite column name warning**
- **Setup**: CSV with non-numeric but valid column names
- **Input**: `read_csv_internal(...)`
- **Expected**: Data returned successfully despite warning
- **Verify**: `expect_s3_class(result, "data.frame")`

---

### Function: `parse_filename_metadata()`

#### Test Group 1: Basic Filename Parsing

**Test 1.1: Parses simple format correctly**
- **Setup**: None
- **Input**: `parse_filename_metadata("PROJ_001_Bulk.0", "project_sampleid_fraction", "_")`
- **Expected**: Tibble with Project, Sample_ID, Fraction columns
- **Verify**:
  - `result$Project == "PROJ"`
  - `result$Sample_ID == "001"`
  - `result$Fraction == "Bulk"`

**Test 1.2: Handles complex format with all tokens**
- **Setup**: None
- **Input**: `parse_filename_metadata("P1_S01_F2_W3_SC4.0", "project_sampleid_fraction_wellid_scanid", "_")`
- **Expected**: Tibble with all parsed metadata
- **Verify**: All token values correctly mapped

**Test 1.3: Removes file extension correctly**
- **Setup**: None
- **Input**: `parse_filename_metadata("sample.0", "sampleid", "_")`
- **Expected**: Sample_ID is "sample" not "sample.0"
- **Verify**: `result$Sample_ID == "sample"`

#### Test Group 2: Delimiter Handling

**Test 2.1: Works with hyphen delimiter**
- **Setup**: None
- **Input**: `parse_filename_metadata("PROJ-001-Bulk", "project_sampleid_fraction", "-")`
- **Expected**: Correct parsing with hyphen
- **Verify**: All fields parsed correctly

**Test 2.2: Works with dot delimiter**
- **Setup**: None
- **Input**: `parse_filename_metadata("PROJ.001.Bulk", "project_sampleid_fraction", ".")`
- **Expected**: Correct parsing with dot
- **Verify**: All fields parsed correctly

#### Test Group 3: Missing and Default Values

**Test 3.1: Uses default fraction when not in filename**
- **Setup**: None
- **Input**: `parse_filename_metadata("PROJ_001", "project_sampleid", "_")`
- **Expected**: Fraction defaults to "GroundBulk"
- **Verify**: `result$Fraction == "GroundBulk"`

**Test 3.2: Allows custom default fraction**
- **Setup**: None
- **Input**: `parse_filename_metadata("PROJ_001", "project_sampleid", "_", default_fraction = "Custom")`
- **Expected**: Fraction is "Custom"
- **Verify**: `result$Fraction == "Custom"`

**Test 3.3: Returns UNKNOWN for missing Sample_ID**
- **Setup**: None
- **Input**: `parse_filename_metadata("PROJ_F1", "project_fraction", "_")`
- **Expected**: Warning and Sample_ID = "UNKNOWN"
- **Verify**:
  - `expect_warning(...)`
  - `result$Sample_ID == "UNKNOWN"`

**Test 3.4: Handles insufficient filename parts**
- **Setup**: None
- **Input**: `parse_filename_metadata("PROJ", "project_sampleid_fraction", "_")`
- **Expected**: Warning about fewer parts than expected, returns defaults
- **Verify**:
  - `expect_warning(..., "fewer parts")`
  - `result$Sample_ID == "UNKNOWN"`

#### Test Group 4: Token Mapping

**Test 4.1: Maps all standard tokens correctly**
- **Setup**: None
- **Input**: `parse_filename_metadata("p_s_f_w_sc", "project_sampleid_fraction_wellid_scanid", "_")`
- **Expected**: Column names are Project, Sample_ID, Fraction, Well_ID, Scan
- **Verify**: Check all standardized column names present

**Test 4.2: Preserves unknown tokens as-is**
- **Setup**: None
- **Input**: `parse_filename_metadata("p_s_custom", "project_sampleid_customtoken", "_")`
- **Expected**: "customtoken" kept as column name
- **Verify**: `"customtoken" %in% names(result)`

**Test 4.3: Handles 'ignore' token**
- **Setup**: None
- **Input**: `parse_filename_metadata("p_skip_s", "project_ignore_sampleid", "_")`
- **Expected**: Ignore token still parsed but can be dropped later
- **Verify**: Check that Sample_ID is correctly identified

#### Test Group 5: Edge Cases

**Test 5.1: Handles empty filename**
- **Setup**: None
- **Input**: `parse_filename_metadata("", "project_sampleid", "_")`
- **Expected**: Warning and default values
- **Verify**: Returns tibble with UNKNOWN Sample_ID

**Test 5.2: Handles filename with extra parts**
- **Setup**: None
- **Input**: `parse_filename_metadata("p_s_f_extra_parts", "project_sampleid_fraction", "_")`
- **Expected**: Parses first 3 parts, ignores rest
- **Verify**: Only requested tokens are parsed

**Test 5.3: Case-insensitive token matching**
- **Setup**: None
- **Input**: `parse_filename_metadata("P_S", "PROJECT_SAMPLEID", "_")`
- **Expected**: Tokens recognized despite case
- **Verify**: Correct parsing with uppercase format string

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
---

## test-inputs-preprocess.R

### Function: `preprocess_spectra()`

#### Test Group 1: Input Validation

**Test 1.1: spectra_data must be a data frame**
- **Setup**: None
- **Input**: `preprocess_spectra(spectra_data = "not a dataframe")`
- **Expected**: Error with message "spectra_data must be a data frame or tibble"
- **Verify**: `expect_error(..., "data frame or tibble")`

**Test 1.2: spectra_data must have Sample_ID column**
- **Setup**: Create tibble without Sample_ID
  ```r
  bad_data <- tibble(`600` = c(0.1, 0.2), `602` = c(0.15, 0.25))
  ```
- **Input**: `preprocess_spectra(spectra_data = bad_data)`
- **Expected**: Error with message "spectra_data must have a Sample_ID column"
- **Verify**: `expect_error(..., "Sample_ID column")`

**Test 1.3: spectra_data must have at least one spectral column**
- **Setup**: Create tibble with only Sample_ID
  ```r
  bad_data <- tibble(Sample_ID = c("S1", "S2"))
  ```
- **Input**: `preprocess_spectra(spectra_data = bad_data)`
- **Expected**: Error with message "spectra_data must have at least one spectral column"
- **Verify**: `expect_error(..., "at least one spectral column")`

**Test 1.4: resample_interval must be positive numeric**
- **Setup**: Valid spectra data
- **Input**: `preprocess_spectra(spectra_data = valid_data, resample_interval = -2)`
- **Expected**: Error with message "resample_interval must be a positive number"
- **Verify**: `expect_error(..., "positive number")`

**Test 1.5: baseline_method must be valid choice**
- **Setup**: Valid spectra data
- **Input**: `preprocess_spectra(spectra_data = valid_data, baseline_method = "invalid")`
- **Expected**: Error about baseline_method argument matching
- **Verify**: `expect_error(..., "should be one of")`

#### Test Group 2: Wavenumber Extraction and Validation

**Test 2.1: Numeric column names are extracted correctly**
- **Setup**: Create tibble with numeric column names
  ```r
  test_data <- tibble(
    Sample_ID = c("S1", "S2"),
    `600` = c(0.1, 0.2),
    `602` = c(0.15, 0.25),
    `604` = c(0.12, 0.22)
  )
  ```
- **Input**: `preprocess_spectra(test_data, baseline_method = "none", verbose = FALSE)`
- **Expected**: Function processes without error
- **Verify**: Successful execution and correct wavenumber range in output

**Test 2.2: Non-numeric column names cause error**
- **Setup**: Create tibble with non-numeric spectral columns
  ```r
  bad_data <- tibble(
    Sample_ID = c("S1", "S2"),
    wave_A = c(0.1, 0.2),
    wave_B = c(0.15, 0.25)
  )
  ```
- **Input**: `preprocess_spectra(bad_data, verbose = FALSE)`
- **Expected**: Error with message "Non-numeric column names found in spectral data"
- **Verify**: `expect_error(..., "Non-numeric column names")`

#### Test Group 3: Baseline Correction

**Test 3.1: baseline_method = "none" skips baseline correction**
- **Setup**: Valid MIR spectra (600-4000 cm⁻¹ range)
- **Input**: `preprocess_spectra(spectra, baseline_method = "none", verbose = FALSE)`
- **Expected**: Data is resampled but not baseline-corrected
- **Verify**: Output dimensions match expected resampled grid

**Test 3.2: baseline_method = "rubberband" applies convex hull baseline**
- **Setup**: Valid MIR spectra with baseline drift
- **Input**: `preprocess_spectra(spectra, baseline_method = "rubberband", verbose = FALSE)`
- **Expected**: Baseline correction applied, then resampling
- **Verify**: 
  - Output has expected dimensions
  - Spectral values differ from input (baseline removed)

**Test 3.3: baseline_method = "polynomial" applies SNV + detrending**
- **Setup**: Valid MIR spectra
- **Input**: `preprocess_spectra(spectra, baseline_method = "polynomial", verbose = FALSE)`
- **Expected**: Polynomial detrending applied, then resampling
- **Verify**: 
  - Output has expected dimensions
  - Mean of each spectrum approximately zero (SNV effect)

**Test 3.4: Rubberband handles errors gracefully**
- **Setup**: Mock `prospectr::baseline()` to fail
- **Input**: `preprocess_spectra(spectra, baseline_method = "rubberband")`
- **Expected**: Informative error with diagnostic hints
- **Verify**: Error message includes hints about irregular data, zero spectra, wavelength range

**Test 3.5: Polynomial handles errors gracefully**
- **Setup**: Mock `prospectr::detrend()` to fail
- **Input**: `preprocess_spectra(spectra, baseline_method = "polynomial")`
- **Expected**: Informative error with diagnostic hints
- **Verify**: Error message includes hints about wavelength points, collinear data

#### Test Group 4: Spectral Data Validation

**Test 4.1: Warns about infinite values in spectral matrix**
- **Setup**: Create spectra with Inf values
  ```r
  bad_spectra <- tibble(
    Sample_ID = c("S1", "S2"),
    `600` = c(Inf, 0.2),
    `602` = c(0.15, -Inf)
  )
  ```
- **Input**: `preprocess_spectra(bad_spectra, baseline_method = "none", verbose = FALSE)`
- **Expected**: Warning about non-finite values
- **Verify**: `expect_warning(..., "Non-finite values detected")`

**Test 4.2: Warns about NaN values in spectral matrix**
- **Setup**: Create spectra with NaN values
  ```r
  bad_spectra <- tibble(
    Sample_ID = c("S1", "S2"),
    `600` = c(NaN, 0.2),
    `602` = c(0.15, 0.25)
  )
  ```
- **Input**: `preprocess_spectra(bad_spectra, baseline_method = "none", verbose = FALSE)`
- **Expected**: Warning about non-finite values
- **Verify**: `expect_warning(..., "NaN")`

**Test 4.3: Info message for NA values when verbose = TRUE**
- **Setup**: Create spectra with NA values
  ```r
  spectra_with_na <- tibble(
    Sample_ID = c("S1", "S2"),
    `600` = c(NA, 0.2),
    `602` = c(0.15, 0.25)
  )
  ```
- **Input**: `preprocess_spectra(spectra_with_na, baseline_method = "none", verbose = TRUE)`
- **Expected**: Info message about NA values (not error or warning)
- **Verify**: `expect_message(..., "NA values")`

**Test 4.4: Warns about all-zero spectra**
- **Setup**: Create spectra with one all-zero row
  ```r
  zero_spectra <- tibble(
    Sample_ID = c("S1", "S2", "S3"),
    `600` = c(0.1, 0, 0.2),
    `602` = c(0.15, 0, 0.25),
    `604` = c(0.12, 0, 0.22)
  )
  ```
- **Input**: `preprocess_spectra(zero_spectra, baseline_method = "none", verbose = FALSE)`
- **Expected**: Warning identifying zero spectra with Sample_IDs
- **Verify**: 
  - `expect_warning(..., "all zero values")`
  - Warning message includes "S2"

#### Test Group 5: Resampling to Regular Grid

**Test 5.1: MIR spectra resampled to 600-4000 cm⁻¹ grid**
- **Setup**: Create MIR spectra (650-3900 cm⁻¹, irregular spacing)
  ```r
  attr(mir_spectra, "spectra_type") <- "MIR"
  ```
- **Input**: `preprocess_spectra(mir_spectra, resample_interval = 2, verbose = FALSE)`
- **Expected**: Output has columns from 600 to 4000 in 2 cm⁻¹ steps
- **Verify**: 
  - `expect_equal(min(as.numeric(names(result)[-1])), 600)`
  - `expect_equal(max(as.numeric(names(result)[-1])), 4000)`
  - Column spacing = 2 cm⁻¹

**Test 5.2: NIR spectra resampled to actual data range**
- **Setup**: Create NIR spectra (4500-9500 cm⁻¹)
  ```r
  attr(nir_spectra, "spectra_type") <- "NIR"
  ```
- **Input**: `preprocess_spectra(nir_spectra, resample_interval = 4, verbose = FALSE)`
- **Expected**: Output grid covers 4500-9500 range with 4 cm⁻¹ spacing
- **Verify**: 
  - Min/max wavenumbers approximately match input range (ceiling/floor)
  - Column spacing = 4 cm⁻¹

**Test 5.3: Missing spectra_type attribute defaults to MIR with warning**
- **Setup**: Create spectra without spectra_type attribute
- **Input**: `preprocess_spectra(no_attr_spectra, verbose = FALSE)`
- **Expected**: Warning about missing attribute, assumes MIR
- **Verify**: 
  - `expect_warning(..., "No spectra_type attribute")`
  - Output uses MIR range (600-4000)

**Test 5.4: Unknown spectra_type causes error**
- **Setup**: Create spectra with invalid type
  ```r
  attr(bad_spectra, "spectra_type") <- "INVALID"
  ```
- **Input**: `preprocess_spectra(bad_spectra, verbose = FALSE)`
- **Expected**: Error about unknown spectra_type
- **Verify**: `expect_error(..., "Unknown spectra_type")`

**Test 5.5: Resampling handles errors with diagnostic hints**
- **Setup**: Mock `prospectr::resample()` to fail
- **Input**: `preprocess_spectra(spectra, verbose = FALSE)`
- **Expected**: Error with hints about wavelength mismatch, range issues
- **Verify**: 
  - Error message includes original and target wavelength ranges
  - Hints mention grid mismatch, non-monotonic wavelengths

**Test 5.6: resample_interval affects output grid density**
- **Setup**: Valid MIR spectra
- **Input**: 
  - `result_2 <- preprocess_spectra(spectra, resample_interval = 2, verbose = FALSE)`
  - `result_4 <- preprocess_spectra(spectra, resample_interval = 4, verbose = FALSE)`
- **Expected**: result_2 has approximately 2× more columns than result_4
- **Verify**: 
  - `expect_equal(ncol(result_2), 2 * ncol(result_4), tolerance = 0.1)`

#### Test Group 6: Attribute Preservation

**Test 6.1: Input attributes preserved in output**
- **Setup**: Create spectra with custom attributes
  ```r
  test_spectra <- tibble(Sample_ID = c("S1", "S2"), `600` = c(0.1, 0.2), `602` = c(0.15, 0.25))
  attr(test_spectra, "spectra_type") <- "MIR"
  attr(test_spectra, "source") <- "opus"
  attr(test_spectra, "source_path") <- "/path/to/data"
  attr(test_spectra, "custom_metadata") <- list(instrument = "Bruker")
  ```
- **Input**: `result <- preprocess_spectra(test_spectra, verbose = FALSE)`
- **Expected**: All attributes preserved except class, names, row.names
- **Verify**: 
  - `expect_equal(attr(result, "spectra_type"), "MIR")`
  - `expect_equal(attr(result, "source"), "opus")`
  - `expect_equal(attr(result, "source_path"), "/path/to/data")`
  - `expect_equal(attr(result, "custom_metadata"), list(instrument = "Bruker"))`

**Test 6.2: Tibble structure attributes not overwritten**
- **Setup**: Valid spectra
- **Input**: `result <- preprocess_spectra(spectra, verbose = FALSE)`
- **Expected**: Result is still a tibble with proper class
- **Verify**: 
  - `expect_s3_class(result, "tbl_df")`
  - `expect_true("data.frame" %in% class(result))`

#### Test Group 7: Output Structure and Integrity

**Test 7.1: Output has correct structure**
- **Setup**: Create 3 samples with MIR spectra
- **Input**: `result <- preprocess_spectra(spectra, resample_interval = 2, verbose = FALSE)`
- **Expected**: Tibble with Sample_ID + numeric wavenumber columns
- **Verify**: 
  - `expect_s3_class(result, "tbl_df")`
  - `expect_equal(nrow(result), 3)`
  - `expect_true("Sample_ID" %in% names(result))`
  - All other columns are numeric wavenumbers

**Test 7.2: Sample_ID preserved and in same order**
- **Setup**: Create spectra with specific Sample_IDs
  ```r
  test_spectra <- tibble(
    Sample_ID = c("SAMPLE_C", "SAMPLE_A", "SAMPLE_B"),
    `600` = c(0.1, 0.2, 0.3),
    `602` = c(0.15, 0.25, 0.35)
  )
  ```
- **Input**: `result <- preprocess_spectra(test_spectra, verbose = FALSE)`
- **Expected**: Sample_IDs in same order as input
- **Verify**: 
  - `expect_equal(result$Sample_ID, c("SAMPLE_C", "SAMPLE_A", "SAMPLE_B"))`

**Test 7.3: No missing values introduced by resampling (unless in input)**
- **Setup**: Valid spectra without NAs
- **Input**: `result <- preprocess_spectra(clean_spectra, verbose = FALSE)`
- **Expected**: Output has no NAs in spectral data
- **Verify**: 
  - `spectral_cols <- setdiff(names(result), "Sample_ID")`
  - `expect_false(any(is.na(result[, spectral_cols])))`

**Test 7.4: Memory efficiency with large datasets**
- **Setup**: Create large spectra matrix (e.g., 5000 samples × 3500 wavenumbers)
- **Input**: `result <- preprocess_spectra(large_spectra, verbose = FALSE)`
- **Expected**: Function completes without memory errors
- **Verify**: 
  - Successful completion
  - Output dimensions correct
  - (Optional: monitor memory usage if >2GB warning shown)

#### Test Group 8: Verbose Output

**Test 8.1: verbose = TRUE shows configuration summary**
- **Setup**: Valid spectra
- **Input**: `preprocess_spectra(spectra, verbose = TRUE)`
- **Expected**: Output shows pipeline configuration
- **Verify**: `expect_output(..., "Spectral Preprocessing Pipeline")`

**Test 8.2: verbose = TRUE shows processing steps**
- **Setup**: Valid spectra
- **Input**: `preprocess_spectra(spectra, baseline_method = "rubberband", verbose = TRUE)`
- **Expected**: Output shows "Processing Steps", "Baseline correction", "Resampling"
- **Verify**: 
  - `expect_output(..., "Processing Steps")`
  - `expect_output(..., "Baseline correction")`
  - `expect_output(..., "Resampling")`

**Test 8.3: verbose = TRUE shows summary with timing**
- **Setup**: Valid spectra
- **Input**: `preprocess_spectra(spectra, verbose = TRUE)`
- **Expected**: Output shows summary with samples processed and total time
- **Verify**: 
  - `expect_output(..., "Summary")`
  - `expect_output(..., "Samples processed")`
  - `expect_output(..., "Total time")`

**Test 8.4: verbose = FALSE suppresses all output**
- **Setup**: Valid spectra
- **Input**: `result <- preprocess_spectra(spectra, verbose = FALSE)`
- **Expected**: No output to console
- **Verify**: `expect_silent(preprocess_spectra(spectra, verbose = FALSE))`

**Test 8.5: High memory warning shows when appropriate**
- **Setup**: Create large spectra triggering >2GB threshold
- **Input**: `preprocess_spectra(large_spectra, verbose = TRUE)`
- **Expected**: Warning about high memory usage
- **Verify**: `expect_output(..., "High memory usage")`

#### Test Group 9: Integration Tests

**Test 9.1: Complete workflow with all preprocessing options**
- **Setup**: Valid MIR spectra from read_spectra()
- **Input**: 
  ```r
  raw_spectra <- read_spectra("opus", test_path, "MIR", verbose = FALSE)
  result <- preprocess_spectra(raw_spectra, 
                                resample_interval = 2, 
                                baseline_method = "rubberband",
                                verbose = FALSE)
  ```
- **Expected**: Successfully preprocessed spectra ready for modeling
- **Verify**: 
  - Output is tibble with correct structure
  - Attributes preserved from read_spectra()
  - Wavenumbers on regular 2 cm⁻¹ grid

**Test 9.2: Preprocessing maintains compatibility with create_dataset()**
- **Setup**: Preprocessed spectra + response data
- **Input**: 
  ```r
  preprocessed <- preprocess_spectra(spectra, verbose = FALSE)
  dataset <- create_dataset(preprocessed, response_data, "SOC")
  ```
- **Expected**: create_dataset() accepts preprocessed output without errors
- **Verify**: Successful dataset creation

**Test 9.3: Multiple baseline methods give different results**
- **Setup**: Same input spectra
- **Input**: 
  - `result_none <- preprocess_spectra(spectra, baseline_method = "none", verbose = FALSE)`
  - `result_rubber <- preprocess_spectra(spectra, baseline_method = "rubberband", verbose = FALSE)`
  - `result_poly <- preprocess_spectra(spectra, baseline_method = "polynomial", verbose = FALSE)`
- **Expected**: All three produce different spectral values
- **Verify**: 
  - Same output dimensions
  - Different spectral values (correlation < 1.0)
  - All preserve Sample_ID order

---

### Helper Functions for test-inputs-preprocess.R

```r
# Create minimal valid MIR spectra for testing
create_test_mir_spectra <- function(n_samples = 3, wn_range = c(650, 3900), n_points = 100) {
  wavenumbers <- seq(wn_range[1], wn_range[2], length.out = n_points)
  spectra_matrix <- matrix(runif(n_samples * n_points, 0.1, 0.5), 
                          nrow = n_samples, 
                          ncol = n_points)
  
  spectra <- tibble::as_tibble(spectra_matrix) %>%
    stats::setNames(as.character(round(wavenumbers))) %>%
    tibble::add_column(Sample_ID = paste0("S", seq_len(n_samples)), .before = 1)
  
  attr(spectra, "spectra_type") <- "MIR"
  attr(spectra, "source") <- "opus"
  return(spectra)
}

# Create NIR spectra for testing
create_test_nir_spectra <- function(n_samples = 3, wn_range = c(4500, 9500), n_points = 100) {
  wavenumbers <- seq(wn_range[1], wn_range[2], length.out = n_points)
  spectra_matrix <- matrix(runif(n_samples * n_points, 0.1, 0.5), 
                          nrow = n_samples, 
                          ncol = n_points)
  
  spectra <- tibble::as_tibble(spectra_matrix) %>%
    stats::setNames(as.character(round(wavenumbers))) %>%
    tibble::add_column(Sample_ID = paste0("S", seq_len(n_samples)), .before = 1)
  
  attr(spectra, "spectra_type") <- "NIR"
  attr(spectra, "source") <- "csv"
  return(spectra)
}

# Create spectra with baseline drift for testing baseline correction
create_spectra_with_baseline <- function(n_samples = 3) {
  wavenumbers <- seq(650, 3900, length.out = 100)
  
  # Create spectra with linear baseline drift
  spectra_matrix <- matrix(0, nrow = n_samples, ncol = length(wavenumbers))
  for (i in seq_len(n_samples)) {
    baseline <- seq(0.1, 0.3, length.out = length(wavenumbers))
    signal <- 0.1 * sin(wavenumbers / 200) + runif(length(wavenumbers), 0, 0.05)
    spectra_matrix[i, ] <- baseline + signal
  }
  
  spectra <- tibble::as_tibble(spectra_matrix) %>%
    stats::setNames(as.character(round(wavenumbers))) %>%
    tibble::add_column(Sample_ID = paste0("S", seq_len(n_samples)), .before = 1)
  
  attr(spectra, "spectra_type") <- "MIR"
  return(spectra)
}
```

---

## test-join-covariates.R

### Function: `step_add_covariates()`

#### Test Group 1: Input Validation

**Test 1.1: sample_id_column NULL handling (default case)**
- **Setup**: Create recipe with Sample_ID role = "id"
  ```r
  rec <- recipe(~., data = test_data) %>%
    update_role(Sample_ID, new_role = "id")
  covs <- tibble(Sample_ID = c("S1", "S2"), clay = c(20, 30))
  ```
- **Input**: `step_add_covariates(rec, covariate_data = covs)`
- **Expected**: Step created without error, sample_id_column remains NULL
- **Verify**:
  - `expect_no_error()`
  - `expect_null(step$sample_id_column)`

**Test 1.2: sample_id_column specified as bare name**
- **Setup**: Same as above
- **Input**: `step_add_covariates(rec, covs, sample_id_column = Sample_ID)`
- **Expected**: Bare name converted to string "Sample_ID"
- **Verify**: `expect_equal(step$sample_id_column, "Sample_ID")`

**Test 1.3: sample_id_column specified as string**
- **Setup**: Same as above
- **Input**: `step_add_covariates(rec, covs, sample_id_column = "Sample_ID")`
- **Expected**: String accepted and stored
- **Verify**: `expect_equal(step$sample_id_column, "Sample_ID")`

#### Test Group 2: prep() Method - ID Column Detection

**Test 2.1: Auto-detects ID column from role = "id"**
- **Setup**: Recipe with one variable having role = "id"
  ```r
  rec <- recipe(~., data = test_data) %>%
    update_role(Sample_ID, new_role = "id") %>%
    step_add_covariates(covariate_data = covs)
  ```
- **Input**: `prep(rec)`
- **Expected**: Automatically uses Sample_ID as join column
- **Verify**: Prepped step has `sample_id_column = "Sample_ID"`

**Test 2.2: Multiple ID variables causes error**
- **Setup**: Recipe with two variables having role = "id"
  ```r
  rec <- recipe(~., data = test_data) %>%
    update_role(Sample_ID, new_role = "id") %>%
    update_role(Plot_ID, new_role = "id") %>%
    step_add_covariates(covariate_data = covs)
  ```
- **Input**: `prep(rec)`
- **Expected**: Error about specifying sample_id_column
- **Verify**: `expect_error(..., "exactly one variable has role = 'id'")`

**Test 2.3: No ID variables and no sample_id_column causes error**
- **Setup**: Recipe with no ID roles and NULL sample_id_column
- **Input**: `prep(rec)`
- **Expected**: Error about specifying sample_id_column
- **Verify**: `expect_error(..., "exactly one variable")`

**Test 2.4: ID column not in training data causes error**
- **Setup**: Specify sample_id_column that doesn't exist
  ```r
  rec <- recipe(~., data = test_data) %>%
    step_add_covariates(covs, sample_id_column = "NonExistent")
  ```
- **Input**: `prep(rec)`
- **Expected**: Error with message "ID column `NonExistent` not found in training data"
- **Verify**: `expect_error(..., "not found in training data")`

**Test 2.5: ID column not in covariate_data causes error**
- **Setup**: Covariate data missing the ID column
  ```r
  bad_covs <- tibble(clay = c(20, 30))  # No Sample_ID
  rec <- recipe(~., data = test_data) %>%
    update_role(Sample_ID, new_role = "id") %>%
    step_add_covariates(bad_covs)
  ```
- **Input**: `prep(rec)`
- **Expected**: Error with message "ID column `Sample_ID` not found in covariate data"
- **Verify**: `expect_error(..., "not found in covariate data")`

#### Test Group 3: Covariate Scaling

**Test 3.1: Covariates are standardized during prep()**
- **Setup**: Covariate data with known mean and SD
  ```r
  covs <- tibble(
    Sample_ID = c("S1", "S2", "S3"),
    clay = c(10, 20, 30),  # mean=20, sd≈10
    sand = c(30, 40, 50)   # mean=40, sd≈10
  )
  rec <- recipe(~., data = test_data) %>%
    update_role(Sample_ID, new_role = "id") %>%
    step_add_covariates(covs)
  ```
- **Input**: `prepped <- prep(rec)`
- **Expected**: Scaled covariates have mean≈0, sd≈1
- **Verify**:
  - `expect_equal(mean(prepped$steps[[1]]$covariate_data$clay), 0, tolerance = 1e-10)`
  - `expect_equal(sd(prepped$steps[[1]]$covariate_data$clay), 1, tolerance = 1e-10)`

**Test 3.2: ID column excluded from scaling**
- **Setup**: Covariate data with Sample_ID
- **Input**: `prepped <- prep(rec)`
- **Expected**: Sample_ID values unchanged after prep
- **Verify**: `expect_equal(prepped$steps[[1]]$covariate_data$Sample_ID, covs$Sample_ID)`

**Test 3.3: ID column moved to first position after scaling**
- **Setup**: Covariates with ID not in first position
  ```r
  covs <- tibble(clay = c(20, 30), Sample_ID = c("S1", "S2"))
  ```
- **Input**: `prepped <- prep(rec)`
- **Expected**: Sample_ID is first column in scaled data
- **Verify**: `expect_equal(names(prepped$steps[[1]]$covariate_data)[1], "Sample_ID")`

**Test 3.4: All numeric covariates are converted and scaled**
- **Setup**: Mixed covariate types
  ```r
  covs <- tibble(
    Sample_ID = c("S1", "S2"),
    clay = c(20, 30),
    sand = c(40, 50)
  )
  ```
- **Input**: `prepped <- prep(rec)`
- **Expected**: Both clay and sand are numeric and scaled
- **Verify**:
  - All non-ID columns are numeric
  - Each has mean≈0, sd≈1

#### Test Group 4: bake() Method - Joining Covariates

**Test 4.1: Left join adds covariates to new_data**
- **Setup**:
  ```r
  prepped <- prep(rec)
  new_data <- tibble(Sample_ID = c("S1", "S2"), x = c(1, 2))
  ```
- **Input**: `bake(prepped, new_data)`
- **Expected**: new_data with added covariate columns
- **Verify**:
  - Output has columns: Sample_ID, x, clay, sand
  - Sample_ID values match input

**Test 4.2: Handles samples present in new_data but not covariates (NA)**
- **Setup**: new_data has Sample_ID not in covariate_data
  ```r
  new_data <- tibble(Sample_ID = c("S1", "S3"), x = c(1, 3))  # S3 not in covs
  ```
- **Input**: `bake(prepped, new_data)`
- **Expected**: S3 gets NA values for covariates (left join behavior)
- **Verify**:
  - `expect_true(is.na(result$clay[2]))`
  - `expect_equal(nrow(result), 2)`

**Test 4.3: ID column missing in new_data causes error**
- **Setup**: new_data without Sample_ID
  ```r
  bad_new_data <- tibble(x = c(1, 2))
  ```
- **Input**: `bake(prepped, bad_new_data)`
- **Expected**: Error with message about ID column not found
- **Verify**: `expect_error(..., "ID column.*not found")`

**Test 4.4: Uses scaled covariate values from prep()**
- **Setup**: Prepare recipe, bake with known data
- **Input**: `bake(prepped, new_data)`
- **Expected**: Covariate values are scaled (from prep), not raw
- **Verify**: Covariate values are standardized (≈0 mean, 1 SD)

#### Test Group 5: print() Method

**Test 5.1: Displays join column name**
- **Setup**: Step with known sample_id_column
  ```r
  step <- step_add_covariates_new(
    covariate_data = covs,
    role = "predictor",
    trained = FALSE,
    skip = FALSE,
    id = "test",
    sample_id_column = "Sample_ID"
  )
  ```
- **Input**: `print(step)`
- **Expected**: Output shows "Covariate join via `Sample_ID`"
- **Verify**: Output contains "Sample_ID"

**Test 5.2: Displays number of covariate columns**
- **Setup**: Covariates with 3 columns: Sample_ID, clay, sand
- **Input**: `print(step)`
- **Expected**: Output shows "2 columns" (excludes Sample_ID)
- **Verify**: Output contains "2 columns"

**Test 5.3: Handles unknown ID column gracefully**
- **Setup**: Step with NULL sample_id_column
  ```r
  step <- step_add_covariates_new(
    covariate_data = covs,
    sample_id_column = NULL,
    ...
  )
  ```
- **Input**: `print(step)`
- **Expected**: Output shows "unknown" for ID column
- **Verify**: Output contains "unknown"

#### Test Group 6: Integration Tests

**Test 6.1: Full workflow with auto-detected ID**
- **Setup**: Complete recipe pipeline
  ```r
  train_data <- tibble(Sample_ID = c("S1", "S2"), y = c(10, 20), x = c(1, 2))
  covs <- tibble(Sample_ID = c("S1", "S2"), clay = c(20, 30))

  rec <- recipe(y ~ ., data = train_data) %>%
    update_role(Sample_ID, new_role = "id") %>%
    step_add_covariates(covariate_data = covs)
  ```
- **Input**:
  ```r
  prepped <- prep(rec)
  result <- bake(prepped, new_data = NULL)
  ```
- **Expected**: Training data with scaled clay covariate added
- **Verify**:
  - Result has columns: Sample_ID, y, x, clay
  - clay is scaled
  - No errors throughout

**Test 6.2: Full workflow with explicit sample_id_column**
- **Setup**: Recipe with explicit ID specification
  ```r
  rec <- recipe(y ~ ., data = train_data) %>%
    step_add_covariates(covs, sample_id_column = Sample_ID)
  ```
- **Input**: `prep(rec) %>% bake(new_data = NULL)`
- **Expected**: Same as Test 6.1
- **Verify**: Successful execution with correct output

**Test 6.3: Multiple covariates join correctly**
- **Setup**: Covariates with multiple columns
  ```r
  covs <- tibble(
    Sample_ID = c("S1", "S2"),
    clay = c(20, 30),
    sand = c(40, 50),
    pH = c(6.5, 7.0)
  )
  ```
- **Input**: Full prep/bake workflow
- **Expected**: All covariates added and scaled
- **Verify**:
  - Output has all covariate columns
  - Each is scaled independently

**Test 6.4: Predictor role assigned correctly**
- **Setup**: Recipe with step_add_covariates
- **Input**: `prepped <- prep(rec)`
- **Expected**: Covariate columns have role = "predictor"
- **Verify**:
  ```r
  info <- summary(prepped)
  cov_roles <- info$role[info$variable %in% c("clay", "sand")]
  expect_true(all(cov_roles == "predictor"))
  ```

### Helper Functions for test-join-covariates.R

```r
# Create simple test dataset with ID column
create_test_data_with_id <- function() {
  tibble(
    Sample_ID = paste0("S", 1:5),
    y = rnorm(5),
    x1 = rnorm(5),
    x2 = rnorm(5)
  )
}

# Create covariate data for joining
create_test_covariates <- function(n_samples = 5, n_covariates = 2) {
  cov_data <- tibble(Sample_ID = paste0("S", seq_len(n_samples)))

  for (i in seq_len(n_covariates)) {
    cov_data[[paste0("cov", i)]] <- rnorm(n_samples, mean = 50, sd = 10)
  }

  return(cov_data)
}

# Create recipe with ID role set
create_recipe_with_id_role <- function(data) {
  recipe(y ~ ., data = data) %>%
    update_role(Sample_ID, new_role = "id")
}
```
