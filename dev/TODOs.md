## ---------------------------------------------------------------------------
## TODO: Cross-Cutting Refactors – horizons
## ---------------------------------------------------------------------------

# -- [1] Modularize Coupled I/O and Logic ------------------------------------
# These functions currently do file loading and data processing in one step,
# which makes unit testing and reuse harder.

# TODO: Split `create_input_data()` into:
#   - `read_opus_files()` (wraps get_file_location + read_spectral_data)
#   - `format_spectral_matrix()` (averaging + pivot)
#   - `join_soil_data()` (wraps join_physicochemical_data)
#   - `create_input_data()` becomes orchestration wrapper

# TODO: Split `download_ossl_data()` into:
#   - `fetch_ossl_metadata()`
#   - `fetch_ossl_lab_data()`
#   - `fetch_ossl_spectra()`
#   - `process_ossl_spectra()`
#   - consider: add a caching wrapper layer (could be generic)


# -- [2] Normalize Function Naming -------------------------------------------
# Current naming mixes verb_noun and noun_verb forms inconsistently.

# TODO: Rename `cubist_model_function_covar` → `fit_cubist_model()` or `train_cubist_model()`
# TODO: Rename `get_file_location()` → `locate_opus_files()`
# TODO: Rename `download_ossl_data()` → `load_ossl_data()` (or break up per above)
# TODO: Audit all unexported internals and align naming conventions


# -- [3] Add Top-Level Config Option for Data Paths --------------------------
# Multiple functions rely on hardcoded relative paths (`../../2_Data/...`)

# TODO: Add `options(horizons.data_path = "path/to/root")`
# TODO: Refactor `get_file_location()`, `join_physicochemical_data()`, and others
#       to build paths relative to `getOption("horizons.data_path")`
# TODO: Document this in the package README for contributor clarity


# -- [4] Improve Error Handling / Graceful Failure ---------------------------
# Some internal functions `stop()` on partial failures (e.g. missing files)

# TODO: Refactor `join_physicochemical_data()` to:
#   - Warn (not stop) on partial joins
#   - Optionally return rows with `NA` if covariates are missing
# TODO: Add `tryCatch()` around `read_spectral_data()` inside `create_input_data()`


# -- [5] Improve Reusability of Spectral Processing -------------------------
# Resampling, smoothing, and SNV logic currently duplicated

# TODO: Move SNV + Savitzky-Golay + resample logic into:
#   - `preprocess_spectra()` (shared by OSSL + OPUS pipelines)
#   - Possibly expose `spectral_pipeline()` for user-defined preprocessing

