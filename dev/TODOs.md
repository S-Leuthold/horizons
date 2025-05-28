# TODOs for horizons Package

## 1️⃣ Cross-Cutting Refactors – horizons (Improved Granularity)

### [1] Modularize Coupled I/O and Logic (Data Ingestion Pipeline)

These functions currently do file loading and data processing in one step, which makes unit testing and reuse harder.

#### Detailed Plan

**Define New Modular Functions:**

- **`read_opus_files(projects, data_root_path)`:**
  - **Purpose:** Reads all OPUS files for specified projects, using `locate_opus_files` (renamed `get_file_location`) and `read_opus_spectrum` (renamed `read_spectral_data`).
  - **Returns:** A list of data frames (one per file), or a single combined data frame in long format. Handles errors for unreadable files gracefully (e.g., returning NULL for problematic files and aggregating warnings).
  - **TODOs:**
    - Implement.
    - Integrate global `data_root_path` option.
    - Add unit tests for error handling (unreadable files, missing files).

- **`format_spectral_matrix(long_spectral_data)`:**
  - **Purpose:** Takes long-format spectral data, averages replicate scans, and pivots to wide format.
  - **Returns:** Wide-format spectral matrix.
  - **TODOs:**
    - Implement.
    - Add unit tests for averaging and pivoting logic.

- **`join_soil_data(wide_spectral_data, projects, variables, data_root_path)`:**
  - **Purpose:** Wraps `join_physicochemical_data` to merge physicochemical covariate data with wide spectral data.
  - **Returns:** Joined tibble.
  - **TODOs:**
    - Implement.
    - Integrate global `data_root_path` option.
    - Ensure robust error handling for missing/malformed physicochemical files.

**Refactor `create_input_data()` as Orchestration Wrapper:**

- **Purpose:** `create_input_data()` becomes the top-level orchestrator calling the new modular functions.
- **TODOs:**
  - Update `create_input_data` to call `read_opus_files()`, then `format_spectral_matrix()`, then `join_soil_data()`.
  - Ensure `create_input_data` correctly passes the `data_root_path` (obtained from `getOption("horizons.data_path")`) to its sub-functions.
  - Update `create_input_data`'s Roxygen to reflect its new role as an orchestrator.

**Define New Modular Functions for OSSL Data:**

- **`fetch_ossl_metadata(data_root_path, bounding_box)`:**
  - **Purpose:** Downloads topsoil metadata from OSSL, filters for U.S. sites, and applies spatial filtering if `bounding_box` is provided.
  - **TODOs:**
    - Implement.
    - Implement bounding_box filtering.
    - Integrate `data_root_path` for local caching.
    - Add unit tests for bounding_box and metadata filtering.

- **`fetch_ossl_lab_data(data_root_path, ossl_layer_ids, ossl_data_dictionary)`:**
  - **Purpose:** Downloads OSSL laboratory measurements, filters by Layer_ID, reshapes, and joins with the data dictionary.
  - **TODOs:**
    - Implement.
    - Integrate `data_root_path` for caching.
    - Add unit tests for data filtering and reshaping.

- **`fetch_ossl_spectra(data_root_path, ossl_layer_ids)`:**
  - **Purpose:** Loads raw OSSL MIR spectra from local cache/specified path.
  - **TODOs:**
    - Implement.
    - Integrate `data_root_path`.
    - Add unit tests for caching logic.

- **`process_ossl_spectra(raw_mir_data, window_size)`:**
  - **Purpose:** Applies smoothing (Savitzky-Golay) and SNV to OSSL spectra.
  - **TODOs:**
    - Implement.
    - Reuse `apply_spectral_transformation` helper (from Cross-Cutting TODO 5).
    - Add unit tests for preprocessing.

**Refactor `download_ossl_data()` as Orchestration Wrapper & Caching Layer:**

- **Purpose:** `download_ossl_data()` becomes the orchestrator, calling the new OSSL modular functions and managing the caching layer.
- **TODOs:**
  - Update `download_ossl_data` to call `fetch_ossl_metadata()`, `fetch_ossl_lab_data()`, `fetch_ossl_spectra()`, and `process_ossl_spectra()`.
  - Ensure `download_ossl_data` correctly manages the caching of processed OSSL data (`.qs` file) based on covariates.
  - Add comprehensive unit tests for the caching mechanism (correct loading, reprocessing when covariates don't match).

### [2] Normalize Function Naming

Current naming mixes verb_noun and noun_verb forms inconsistently.

#### Detailed Plan

**Systematic Renaming (Verb-First):**

- Rename `cubist_model_function_covar` → `fit_cubist_model`.
- Rename `get_file_location` → `locate_opus_files`.
- Rename `download_ossl_data` → `get_ossl_data`.
- Rename `evaluate_predictions` → `evaluate_covariate_predictions`.
- Rename `reduce_dimensions_pca_covpred` → `reduce_dimensions_pca`.
- Rename `cluster_input_data` → `cluster_spectral_data`.
- Rename `create_training_subsets` → `create_clustered_subsets`.
- Rename `read_spectral_data` → `read_opus_spectrum` (singular).

**Audit All Internals:**

- Go through every unexported internal function and ensure names follow verb_noun convention and match purpose (e.g., `process_spectra` could become `apply_spectral_transformation`).

**Update All References:**

- Update all calls to renamed functions throughout the codebase.
- Update all Roxygen documentation, internal comments, and examples.

### [3] Add Top-Level Config Option for Data Paths

**Detailed Plan:**

- Define a new package option in `zzz.R`:
  - `options(horizons.data_path = path)`
  - Create `get_horizons_data_path()` to retrieve this and throw errors if not set.
- Create `horizons_set_data_path(path)`:
  - Sets and validates path.
  - Provides informative feedback.
- Refactor data access functions:
  - Use dynamic paths built with `file.path(get_horizons_data_path(), ...)`.
  - Provide clear error messages if not set.
- Update documentation (README and vignettes).
- Add unit tests for presence/absence of `horizons.data_path` setting.

### [4] Improve Error Handling / Graceful Failure

**Detailed Plan:**

- Create `safely_execute()` helper in `R/utils_error_handling.R`:
  - Wraps `purrr::safely()`.
  - Optionally logs errors.
  - Returns `default_value` on error.
- Apply `safely_execute()`:
  - In `read_opus_files` (wrap `read_spectral_data`).
  - In `join_soil_data` (wrap reading individual `.csv` files).
- Refactor `join_physicochemical_data`:
  - Replace `suppressMessages` with explicit logging.
  - Return NAs with warnings instead of stopping.
- Use `safely_execute()` in model training/tuning as well.
- Standardize error messages (using `cli::cli_abort`, `cli::cli_warn`).

### [5] Improve Reusability of Spectral Processing

**Detailed Plan:**

- Create `apply_spectral_transformation(spectrum_vector, method, window_size)` in `R/utils_spectral_processing.R`:
  - Encapsulate Savitzky-Golay and SNV.
  - Handles matrix/vector conversion.
- Add unit tests for all method options.
- Create `calculate_trimming_indices(spectrum_length, window_size)`.
- Create `resample_spectrum(spectrum_vector, wavenumbers, new_wavenumbers, interpolation_method)`.
- Refactor:
  - `process_spectra` (in `step_transform_spectra.R`).
  - `process_ossl_spectra`.
- Consider a high-level `spectral_pipeline()` for user-defined chaining.

---

## 2️⃣ Function-Specific TODOs (Prioritized)

### P0: Critical (Must Do Immediately)

- `predict_covariates`: Modularization & orchestration.
- `create_input_data`: Modularization & orchestration.
- `get_file_location`: Add filter for OPUS files.
- `step_add_covariates_new`: Remove duplicate definitions.

### P1: High (Next Sprint)

- Implement `horizons.data_path` configuration (`Cross-Cutting TODO 3`).
- Create `safely_execute()` (`Cross-Cutting TODO 4`).
- Create `apply_spectral_transformation()` (`Cross-Cutting TODO 5`).
- Improve robustness of `predict_covariates`, `input_read_opus_spectra.R`, `model_backtransform_tune_results.R`, and `model_evaluate_holdout.R`.

### P2: Medium (Later Sprint)

- Finalize modular functions (`Cross-Cutting TODO 1`).
- Orchestration wrapper for OSSL (`download_ossl_data`).
- Normalize function naming (`Cross-Cutting TODO 2`).
- Refactor data access and error handling (`Cross-Cutting TODOs 3 & 4`).
- Centralize index and resampling logic (`Cross-Cutting TODO 5`).
- Additional function-specific robustness and testing.

### P3: Low (Backlog/Future)

- Comprehensive unit tests for all modules (e.g., `create_training_subsets`, `evaluate_predictions`, `predict_covariates`, etc.).
- Additional configuration and logging improvements.
- Consistency and flexibility checks (e.g., `clean_workflow_id`, `step_add_covariates`, `process_spectra`).
- Implement future enhancements (e.g., `spectral_pipeline()`).

---
