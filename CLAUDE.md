# CLAUDE.md - Horizons Package Development Guide

## R CODING STYLE GUIDE - FOLLOW EXACTLY

### Core Principles
- **Clarity over cleverness**: Code should be immediately readable
- **Aggressive alignment**: Align everything that can be aligned
- **Explicit namespacing**: Always use `package::function()`
- **Minimal comments**: Code structure and naming should be self-documenting

### Assignment Operators
```r
# One-line assignments: use <-
x <- 5
df <- my_data

# Multi-line operations: use -> at the end
# IMPORTANT: First argument stays on same line as function/operation
data %>%
  filter(wavelength > 500) %>%
  mutate(absorbance = log10(1/reflectance)) %>%
  group_by(sample_id) %>%
  summarise(mean_abs = mean(absorbance)) ->
processed_data

# Multi-line function calls with assignment
list(model             = as.character(config_row$model),
     transformation    = as.character(config_row$transformation),
     preprocessing     = as.character(config_row$preprocessing),
     feature_selection = as.character(config_row$feature_selection)) ->
config_clean

# Multiple consecutive one-line assignments: align <- with no blank lines
short      <- 1
longer_var <- 2
x          <- 3
```

### Function Formatting
```r
# Function DEFINITIONS: First argument on new line if multi-line
build_recipe <- function(project_data,
                        preprocessing      = "raw",
                        transformation     = "No Transformation",
                        feature_selection  = "None",
                        response_variable  = NULL,
                        id_variable       = "sample_id") {
  # Function body
}

# Function CALLS: Different rules!
# Single-line calls: use <-
result <- my_function(arg1, arg2, arg3)

# Multi-line calls: first argument on SAME line, use ->
my_function(short_arg        = 1,
           longer_argument   = "value",
           another_argument  = TRUE) ->
result
```

### Section Headers and Organization
```r
## ---------------------------------------------------------------------------
## Step 1: Major Section Title
## ---------------------------------------------------------------------------

code_here()

## ---------------------------------------------------------------------------

# Blank line + separator between major logical blocks

## -------------------------------------------------------------------------
## Step 1.1: Sub-section Title
## -------------------------------------------------------------------------

# For complex sections, add narrative description after header
# This section handles the complex logic for processing spectral data
# including baseline correction, normalization, and derivative transforms.
# The output is a matrix ready for modeling.

more_code()
```

### Conditional Formatting
```r
# Always include blank lines inside braces for readability
if (condition) {

  single_action()

}

# Even for simple assignments
if (x > 5) {

  result <- "large"

} else {

  result <- "small"

}

# Single line only for very simple cases
if (is.null(x)) return(NULL)
```

### Pipeline Formatting
```r
# Each pipe operation on new line, arrow at end
data %>%
  filter(quality == "good") %>%
  mutate(normalized = value / max(value),
         squared    = value^2) %>%
  group_by(sample_id) %>%
  summarise(mean_val = mean(normalized)) ->
final_result
```

### Comments
```r
# Section headers are primary documentation method
# Inline comments only when algorithm is non-obvious
# Comments should explain WHY, not WHAT

## ---------------------------------------------------------------------------
## Step 2.3.1: Apply Savitzky-Golay Filter
## ---------------------------------------------------------------------------

# Using 2nd derivative to identify peak positions
# Window size of 11 chosen based on Beleites et al. 2013
sg_result <- savitzkyGolay(spectra, m = 2, p = 2, w = 11)
```

### Error Messages and CLI Output
```r
# Always use cli functions for user communication
cli::cli_abort("Variable '{var_name}' not found in data")
cli::cli_alert_warning("{n_missing} samples have missing values")
cli::cli_alert_success("Model evaluation complete")

# Use glue-style interpolation
cli::cli_alert_info("Processing {nrow(data)} samples")
```

### Naming Conventions
```r
# Variables and functions: snake_case
process_spectra()
spectral_data
n_models

# Constants: UPPER_SNAKE_CASE
MAX_ITERATIONS <- 1000
DEFAULT_WAVELENGTHS <- c(400, 450, 500)

# Boolean variables: use is_ or has_ prefix
is_valid
has_missing_values
```

### Namespace and Dependencies
```r
# Always use explicit namespacing in functions
dplyr::filter()
ggplot2::ggplot()

# Combine with @importFrom in roxygen for common functions
#' @importFrom dplyr filter mutate select
#' @importFrom ggplot2 ggplot aes geom_line

# Never use library() in package functions
# Never use require() in package functions
```

### CLI Tree-Style Output (Verbose Mode)
```r
# Use box-drawing characters for hierarchical progress
cli::cli_text("│")                                    # Vertical continuation
cli::cli_text("├─ {cli::style_bold('Step Name')}...") # Branch point
cli::cli_text("│  └─ Detail: {value}")               # Completion/sub-item
cli::cli_text("└─ {cli::style_bold('Final Step')}")  # Last step (no continuation)

# Example workflow output:
│
├─ Loading OSSL library...
│  └─ Loaded 12,453 samples
│
├─ Training PCA...
│  └─ Complete: 15 components retained
│
├─ Clustering library...
│  ├─ Testing K ∈ {5, 7, 9, 11}
│  └─ Complete: 7 clusters (BIC-optimal)
│
├─ Training models...
│  ├─ Cluster 1/7: SUCCESS (R² = 0.89)
│  ├─ Cluster 2/7: SUCCESS (R² = 0.91)
│  └─ Memory freed: 250MB
│
└─ Prediction complete: 342 samples

# For errors within tree:
│
├─ Training models...
│  ├─ Cluster 1/7: SUCCESS
│  └─ Cluster 2/7: FAILED - memory exhausted
│     └─ Memory: 450MB → 980MB
└─ ABORT: Model training failed
   Run rlang::last_error() for backtrace
```

### Error Handling with safely_execute()
```r
# Pattern: safely_execute() → handle_results()

# Orchestrator (user-facing function):
safely_execute(
  risky_operation(data),
  error_message = "Failed to process {property}"
) %>%
  handle_results(
    error_title = "Processing failed",
    error_hints = c("Check data format", "Verify property exists"),
    abort_on_null = TRUE  # Orchestrators abort on failure
  ) -> result

# Workflow function (internal):
safe_result <- safely_execute(
  internal_operation(data),
  error_message = "Operation failed"
)
if (!is.null(safe_result$error)) return(NULL)  # Let caller decide
return(safe_result$result)

# Helper (lowest level):
tryCatch({
  pure_processing(data)
}, error = function(e) {
  cli::cli_warn("Processing failed: {e$message}")
  return(NULL)
})
```

---

## CURRENT OBJECTIVES (2025-10-31)

**Active Work**: Phase 3 - Uncertainty Quantification (CV+ Conformal Calibration)
**Branch**: `uncertainty-quantification` (feature branch from main)
**Roadmap**: `LIBRARY_PREDICTION_ROADMAP.md` (comprehensive - **UPDATE THIS AS WE BUILD**)

### Session 7 Progress (2025-10-31):
- 🎉 **M3.3 CV+ CONFORMAL COMPLETE**: Statistically valid coverage guarantee achieved!
- ✅ **Shared CV Folds Architecture**:
  - Modified `train_and_score_config()` to accept optional `resamples` parameter
  - Modified `train_quantile_model()` to accept optional `resamples` parameter
  - Updated `train_cluster_models_with_uq()` to create folds ONCE and share between models
- ✅ **OOF Quantile Extraction**:
  - Implemented fold loop to extract q05/q95 predictions from assessment sets
  - Uses `rsample::complement()` to track `.row` indices for alignment
  - Returns `list(workflow, cv_quantiles)` instead of just workflow (breaking change)
- ✅ **Proper Conformal Calibration**:
  - Rewrote `calculate_conformal_margin()` to use matched OOF predictions from BOTH models
  - Joins by `.row` to ensure point and quantile predictions aligned by sample
  - Fixes optimistic bias from in-sample quantile predictions (same issue as M3.1 residuals!)
- 🐛 **Critical Bug Fixed**: OOF loop was predicting on wrong data
  - **Issue**: Used `rsample::assessment(split)` which has original Response values (pH)
  - **Impact**: Quantile predictions were pH values (3-10 range) not residuals (-3 to +3)
  - **Fix**: Extract indices and subset `train_data` with residuals as Response
  - **Result**: Quantiles now correctly predict residuals!
- ✅ **Validation Results** (n=1000, 80/20 split):
  - **Base coverage**: 89.4% (OOF intervals before conformal)
  - **c_alpha**: 0.0177 pH (tiny margin - base model well-calibrated!)
  - **Test coverage**: 92% on n=200 independent samples ✓ (target 88-92%)
  - **Mean width**: 2.17 pH (adaptive heteroscedasticity)
  - **Width range**: 1.15-6.05 pH (varies by sample uncertainty)
- 📊 **Coverage Guarantee Achieved**: CV+ conformal delivers statistical validity
- 💾 **Files Modified**:
  - `R/library-train.R`: Added `resamples` parameter to `train_and_score_config()`
  - `R/library-uq.R`: Added shared folds + OOF extraction + proper conformal
  - `DESCRIPTION`: Updated Collate field for new library-*.R files
- 📋 **Test Scripts Created**:
  - `tests/debug_m3.3_cv_plus_conformal.R`: Debug validation (n=500)
  - `tests/manual_test_cv_plus_conformal.R`: Manual REPL test (n=1000, 80/20 split)
- ⏭️ **Next Steps**:
  1. Add pinball loss tuning (optimize quantile accuracy)
  2. Test edge cases (small clusters, other properties)
  3. Remove debug output (production-ready)
  4. Update documentation
  5. Mark M3.3 complete in roadmap

### Session 6 Progress (2025-10-31):
- 🔬 **Phase 3 Implementation Started**: UQ infrastructure with TDD approach
- ✅ **M3.1 Initial Implementation Complete**:
  - `define_quantile_specification()`: Creates ranger quantile spec
  - `predict_quantiles()`: Extracts q05/q95 from single model (24/24 tests ✓)
  - `repair_quantile_crossings()`: Monotonicity enforcement
  - `train_quantile_model()`: Standalone quantile training
  - `train_cluster_models_with_uq()`: Orchestrator for 2-model training
- ✅ **Real Data Validation**: pH test successful (R²=0.78, coverage=94%)
- 🔄 **CRITICAL ARCHITECTURE DECISION**: Switching from library-based to **residual-based intervals**
  - Library-based: Wide intervals (2.6 pH units) - reflects OSSL population variability
  - Residual-based: Narrow intervals (est. 0.8 pH units) - reflects MODEL confidence
  - **Consensus (Gemini 9/10, GPT-5 7/10)**: Residual-based more actionable for clients
  - Client needs: "90% confident YOUR soil is 5.7-6.5" not "library ranges 4.6-7.2"
  - Hybrid approach planned: Residual-based primary (Phase 3) + AD-gated library fallback (Phase 4)
- 📋 **Architecture**: Train ranger on model RESIDUALS (not Response values)
- 📋 **Conformal Still Essential**: Ensures 90% coverage even if residual quantiles miscalibrate
- ✅ **Residual-Based Implementation**: Core functions modified
  - Modified: `train_cluster_models_with_uq()` computes residuals from point model
  - Created: `predict_with_uq()` adds residual quantiles to point predictions
  - Added: Residual diagnostics (mean, SD) for quality monitoring
- 🐛 **CRITICAL BUG FOUND & FIXED**: In-sample overfitting bias
  - **Issue**: Used in-sample predictions for residuals (SD=0.022 → intervals 50× too narrow!)
  - **Impact**: 7% coverage (should be ~90%), dangerously overconfident
  - **Fix**: Use out-of-fold CV predictions for unbiased residuals
  - **Result**: OOF residual SD=0.584 (26× larger, realistic!), interval width=1.75 pH
  - **Validation**: Debug script confirms OOF SD matches test error SD (0.584 vs 0.63)
- ✅ **FINAL VALIDATION**: Full OSSL test successful!
  - Coverage: 90.2% (perfect, no conformal needed yet!)
  - Mean width: 1.43 pH (37% narrower than library-based 2.29 pH)
  - OOF residual SD: 0.487 (matches test error SD 0.63)
  - Interval range: 0.85-3.65 pH (adaptive heteroscedasticity)
- 💾 **Committed**: M3.1 complete + OOF residual fix (commit d2cd8e2)
- 🚧 **M3.3 Conformal Started (Incomplete)**:
  - Infrastructure in place (c_alpha integration, prediction adjustment)
  - **Issue Identified**: Need OOF quantile predictions (not in-sample)
  - Current uses in-sample quantiles (optimistic bias - same issue as residuals!)
  - **Decision**: Implement proper CV+ with matched OOF predictions (Session 7)
  - See `M3.3_CONFORMAL_IMPLEMENTATION_SPEC.md` for detailed plan
- 🧹 **Cleanup**: Removed 7 redundant test scripts, organized codebase
- 💾 **Session 6 Summary**:
  - **Lines Added**: 2,763 (R/library-uq.R + tests + docs)
  - **Functions Created**: 8 core UQ functions
  - **Tests Passing**: 24 prediction tests
  - **Critical Bug Fixed**: OOF residuals (26× impact)
  - **Coverage Achieved**: 90.2% (no conformal needed yet!)
  - **Interval Reduction**: 37% vs library-based
- ⏭️ **Next Session (M3.3 Completion)**:
  1. Implement shared CV folds for point + quantile models
  2. Extract OOF quantile predictions from CV
  3. Match OOF point + quantile by fold
  4. Compute proper conformal margin
  5. Validate 90% coverage guarantee
  6. Optional: Add pinball loss tuning
  7. Test texture ILR residuals

### Session 5 Progress (2025-10-30):
- 🔬 **Starting Phase 3**: Uncertainty Quantification
- ✅ **UQ Strategy Finalized**: Decoupled approach (any model for point, ranger for UQ)
- ✅ **Spike Script Created**: `tests/spike_heteroscedasticity.R` (validates assumptions)
- ✅ **M3.2 COMPLETE**: Heteroscedasticity confirmed across pH, OC, total_carbon
  - All 3 properties show significant heteroscedasticity (p < 0.05)
  - Quantile models justified - will provide adaptive interval widths
  - Belt-and-suspenders approach validated
- 📋 **Key Decision**: Belt-and-suspenders (quantile models + conformal calibration)
- 📋 **Architecture**: Point model (best config) + separate ranger quantile models + CV+ conformal
- 📋 **Texture Handling**: 6 models per cluster (ilr_1/ilr_2 × point/q05/q95)
- ⏭️ **Next**: Implement M3.1 (quantile infrastructure with ILR support)

### Session 4 Progress (2025-10-30):
- ✅ **PLSR Bug Fixed** - Investigated and fixed upstream plsmod bug (tidymodels/plsmod#47)
  - Forked plsmod, applied drop=FALSE fix to R/predict.R lines 67-68
  - Tested thoroughly (10 test scenarios, all passed)
  - Added to DESCRIPTION Remotes for automatic installation
  - PLSR now works with tune_grid() in horizons

### Session 3 Progress (2025-10-27):
- ✅ **Test Suite Stabilization** - Fixed all failing tests (4 → 0 failures, 118 → 189 passing)
- ✅ **Bug Fixes**:
  - Fixed duplicate Response column issue in `prepare_cluster_splits()`
  - Fixed water band removal regex to handle numeric column names
  - Standardized column naming to `Sample_ID` (not `sample_id`)
  - Fixed test data to use integer wavenumbers (like real OSSL)
- ✅ **E2E Validation** - Full pipeline tested and working (load → cluster → train prep)
- ✅ **M1.5 COMPLETE** - ILR transformation system implemented:
  - Created `library-targets.R` with ILR transforms and bounds enforcement
  - Modified `load_ossl_raw()` to fetch all 3 texture columns when texture requested
  - Integrated ILR into `prepare_cluster_splits()` with `ilr_coordinate` parameter
  - 62 new tests for transformations, 189 total library tests passing
  - Trains 2 models (ilr_1, ilr_2) instead of 3 (33% reduction)

### Session 3 Final Status:
- ✅ **M1.6 COMPLETE** - Full prediction pipeline operational!
- ✅ End-to-end test passing (5 pH samples, MAE = 0.18 units)
- ✅ Auto-optimization integrated (config selection per cluster)
- ✅ Texture and non-texture paths both implemented

### Phase 1 Progress (6/6 milestones COMPLETE):
- ✅ M1.1: Library data loading (KSSL filtering, numeric columns, SNV preprocessing)
- ✅ M1.2: GMM clustering (BIC selection, Ledoit-Wolf shrinkage)
- ✅ M1.3: OPTIMAL_CONFIGS_V1 (59 configs for 15 properties)
- ✅ M1.4: Model training infrastructure (two-stage optimization)
- ✅ M1.5: Target handling (ILR, bounds, constraints)
- ✅ M1.6: Prediction API ← **COMPLETE!**

**PHASE 1 COMPLETE! 🎉**

### Next: Phase 2 - Refinements & Polish
1. Fix PLSR computational singularity with small samples
2. Implement water band removal (default ON per expert review)
3. Update composite scoring (add RPIQ metric)
4. Comprehensive integration tests with all properties

### Remember:
- **Update LIBRARY_PREDICTION_ROADMAP.md** as decisions are made
- **Track progress** in Phase/Milestone sections
- **Build tests in parallel** with implementation (TDD approach):
  1. Write test skeleton FIRST
  2. Implement to make tests pass
  3. Expand both iteratively
  4. Maintain >80% coverage on new code
- **No breaking changes** to existing Custom mode (evaluate_models_local/hpc)
- **Memory discipline**: rm/gc after every stage, butcher workflows

### Known Issue: PLSR + tune_grid() Incompatibility (2025-10-28)

**Status**: Upstream bug in plsmod package (tidymodels/plsmod#47)

**Problem**: PLSR models with mixOmics engine fail during `tune_grid()` with error:
```
Error in `tmp_pred[.x, ]`: incorrect number of dimensions
```

**Root Cause**: Missing `drop = FALSE` in `multi_numeric_preds()` at line 67 of plsmod/R/predict.R
- When p=1 (single outcome) and subsetting to specific components, array collapses to vector
- Subsequent indexing `tmp_pred[.x, ]` expects 2D but gets 1D → fails

**Proposed Fix** (from issue #47):
```r
# Line 67-68 should be:
tmp_pred <- tmp_pred[, 1, , drop = FALSE]
tmp_pred <- purrr::map(1:n, \(.x) data.frame(.pred = as.numeric(tmp_pred[.x, , , drop = FALSE])))
```

**Workarounds**:
1. Use `fit_resamples()` with fixed `num_comp` (no tuning) - works fine
2. Use other models (cubist, ranger) for spectroscopy - actually perform better
3. Wait for plsmod maintainers to merge fix

**Solution**: ✅ Fixed via custom fork
- Using `remotes::install_github("S-Leuthold/plsmod-fork", ref="fix/plsr-tune-grid-dimension")`
- PLSR now works with tune_grid in horizons

*Fixed: 2025-10-28*

---

### Collaboration Pattern (Claude + Sam):

**Build Section-by-Section** (`## Step ##` is the unit of work):
1. **Discuss approach**: Claude proposes implementation strategy with reasoning
2. **Review & approve**: Sam approves or adjusts approach
3. **Implement**: Claude writes that specific section
4. **Verify**: Quick check, then move to next section
5. **For complex sections**: Break into Step N.1, N.2, etc. with same pattern

**Decision-Making Protocol**:
- **Design choices** (e.g., caching vs recompute): Propose with reasoning → wait for input
- **Potential issues** (e.g., memory concerns): Flag immediately, discuss alternatives
- **Test coverage**: Aim for ~80% on new files (match package average), trend higher where feasible

**Roles**:
- **Sam**: PM + Senior Dev (architectural decisions, approvals, direction)
- **Claude**: Code Executor (implementation, questions, issue flagging)

**Pacing**: Quality over speed - we're not in a hurry, let's do this right

---

## PACKAGE STATUS - HORIZONS v0.9.0

**Main Branch**: Merged from `refactor-for-joss` (2025-10-24), 80.22% coverage, JOSS-ready
**Feature Branch**: `uncertainty-quantification` - Library Prediction Service with UQ (planning complete)
**Architecture**: Dual-mode prediction system (Custom training + Library-based prediction)
**Dependencies**: 57 total (needs reduction to <30 for JOSS submission)

### Main Branch Status (2025-10-24) - Merged & Stable

**Merged from refactor-for-joss** (2025-10-24):
- ✅ 171 refactor commits + 1 merge commit
- ✅ Pushed to origin/main successfully
- ✅ Repository cleanup (archive/ for development docs)
- ✅ Git history cleaned (no co-authorship tags)

**Quality Metrics**:
- ✅ **80.22% test coverage** - JOSS requirement met!
- ✅ **4,560 tests passing**, 0 failures, 48 intentional skips
- ✅ **37 R source files**, 15,254 LOC
- ✅ **56 exported functions** with roxygen documentation
- ⚠️ **R CMD check**: 1 ERROR (examples), 6 WARNINGS (docs), 7 NOTES (needs cleanup for JOSS)

**Core Architecture**:
- **Custom Training Mode**: evaluate_models_local/hpc() for novel properties
- **Evaluation System**: Local + HPC backends with nested parallelization
- **Model Support**: 9 algorithms (RF, cubist, xgboost, PLSR, etc.)
- **Feature Selection**: PCA, correlation, CARS, Boruta
- **Ensemble Methods**: Stacked ensembles, weighted averaging
- **Memory Optimized**: butcher::butcher(), aggressive rm/gc, streaming options
- **OS-Aware Parallel**: doMC for Linux, future for macOS

### Feature Branch: uncertainty-quantification (2025-10-24) - In Development

**Purpose**: Library Prediction Service with Uncertainty Quantification
**Roadmap**: See `LIBRARY_PREDICTION_ROADMAP.md` (1,212 lines, comprehensive planning)
**Status**: Planning complete, ready for Phase 1 implementation

**Feature Overview:**
- **Dual-Mode Prediction System**:
  - **Library Mode**: Leverage OSSL/KSSL reference libraries for standard properties (clay, pH, SOC, etc.)
  - **Custom Mode**: Existing evaluate_models_local/hpc() for novel properties (unchanged)
- **15 Supported Properties**: Texture, carbon forms, nitrogen, pH, CEC, base cations, major elements
- **Auto-Optimization**: Property-specific optimal configs, test top N per cluster, select winner
- **Uncertainty Quantification**: Quantile models (q05/q95) + conformal calibration → per-sample prediction intervals
- **Production-Ready**: Memory-optimized, applicability domain awareness, abstention policy

**Key Technical Decisions** (see roadmap ADRs):
- GMM clustering with BIC selection, Ledoit-Wolf shrinkage
- CV+ conformal for stable calibration
- ILR transformation for texture (compositional data)
- Ranger quantiles with monotonicity enforcement
- Method harmonization (single lab method per property)

**Implementation Timeline**: 10-12 weeks (4 phases)
- **Phase 1** (Weeks 1-2): Core infrastructure, clustering, basic API
- **Phase 2** (Weeks 3-4): Auto-optimization
- **Phase 3** (Weeks 5-7): Quantile models + conformal UQ
- **Phase 4** (Weeks 8-10): AD-aware calibration, policies, monitoring

**Current Work** (Week 0):
- ✅ Roadmap drafted (1,212 lines)
- ✅ OPTIMAL_CONFIGS_V1 created in constants.R (59 configs for 15 properties)
- ✅ Ng et al. 2022 benchmarking analyzed
- ⏭️ Next: Begin Phase 1, Milestone 1.1 (refactor library-data.R)

---

## DEVELOPMENT COMMANDS

### Daily Development Workflow
```bash
# Load and test changes
Rscript -e "devtools::load_all(); devtools::test()"

# Check package health
Rscript -e "devtools::check()"

# Update documentation
Rscript -e "devtools::document()"

# Check test coverage
Rscript -e "covr::package_coverage()"
```

### Pre-submission Checklist
```bash
# Final validation
R CMD check --as-cran horizons_0.9.0.tar.gz

# Ensure clean build
R CMD build --no-build-vignettes .

# Dependency check
Rscript -e "tools::package_dependencies('horizons', recursive=TRUE)"
```

---

## KEY DESIGN DECISIONS

### Why Two Evaluation Backends?
- **Local**: Simple desktop use, sequential models with parallel CV
- **HPC**: Sophisticated nested parallelization for cluster environments
- **Rationale**: Different users, different hardware, different needs

### Why Custom Metrics?
- **RPD**: Standard in spectroscopy (ratio performance to deviation)
- **CCC**: Better than R² for agreement assessment
- **RRMSE**: Relative error metric for fair comparison across variables

### Why Ensemble Focus?
- **Spectroscopy**: Multiple algorithms often perform similarly
- **Ensemble**: Combines strengths, reduces overfitting
- **Production**: More robust predictions than single models

---

## CONTACT INFORMATION

**Developer**: Sam Leuthold
**Email**: sam.leuthold@colostate.edu
**Goal**: JOSS publication
**Timeline**: 3 weeks to submission
**Version**: 0.9.0 (ready for JOSS)

---

## USEFUL PACKAGE COMMANDS

### Find Functions by Pattern
```bash
# Find all exported functions
grep -r "^#' @export" R/ | wc -l

# Find long functions (>200 lines)
awk '/^[a-zA-Z_].*<- function/ {name=$0; start=NR} /^}$/ {if(NR-start>200) print name, NR-start}' R/*.R

# Check dependency usage
grep -r "package::" R/ | cut -d: -f3 | cut -d: -f1 | sort | uniq -c | sort -nr
```

### Test Specific Areas
```r
# Test evaluation functions
testthat::test_file("tests/testthat/test-evaluation-*.R")

# Test input handling
testthat::test_file("tests/testthat/test-inputs-*.R")

# Quick integration test
devtools::run_examples()
```

---

## ERROR HANDLING PATTERNS

### Current State (Mixed - needs gradual refactoring)

The package currently has mixed error handling patterns that we're gradually standardizing. Here's the intended pattern:

### Three-Layer Error Handling Architecture

```
Layer 1: Orchestrators (User-facing)
├─ Validate ALL inputs with cli::cli_abort()
├─ Coordinate workflow
└─ Decide what failures are fatal

Layer 2: Workflow Functions (Major operations)
├─ Assume validated inputs from orchestrators
├─ Use cli::cli_abort() for critical failures
└─ Return structured results

Layer 3: Helper Functions (Processing/utilities)
├─ Assume valid inputs (no validation)
├─ Return NULL on processing failure
├─ Let caller decide if failure is fatal
└─ Include cli::cli_warn() for debugging
```

### The Pattern (To Be Applied Gradually)

**Orchestrators validate everything:**
```r
fetch_covariates <- function(input_data, ...) {
  # ALL validation here
  if (!"Sample_ID" %in% names(input_data)) cli::cli_abort("...")
  if (!is.numeric(input_data$latitude)) cli::cli_abort("...")

  # Call helpers with validated data
  result <- helper_function(input_data)
  if (is.null(result)) cli::cli_abort("Processing failed")
}
```

**Helpers just process:**
```r
helper_function <- function(data, ...) {
  # NO validation - assume data is good
  result <- tryCatch({
    # Do the work
  }, error = function(e) {
    cli::cli_warn("Processing failed: {e$message}")
    return(NULL)
  })
}
```

### Current Issues (Don't fix all at once!)

- Some helpers validate inputs (should move to orchestrators)
- Some functions mix patterns (return NULL sometimes, abort others)
- Validation is duplicated in multiple layers

### Refactoring Strategy

1. **New code**: Follow the pattern strictly
2. **Existing code**: Add TODO comments, fix gradually
3. **Critical fixes**: Only fix error suppression bugs now
4. **Full refactor**: After adding comprehensive tests

### TODO Comments for Future Work

When you see validation in the wrong place, add:
```r
# TODO: Move this validation to orchestrator level
if (length(spectral_cols) == 0) cli::cli_abort("...")
```

---

## NOTES

**Main Branch (Production)**:
- ✅ Merged to main 2025-10-24
- ✅ 80.22% test coverage - JOSS ready
- ✅ Architecture refactoring complete and stable
- ⏳ JOSS submission pending (R CMD check cleanup + vignettes + paper)

**Feature Branch (uncertainty-quantification)**:
- 🚧 Library Prediction Service with UQ - planning complete
- 📋 Roadmap: LIBRARY_PREDICTION_ROADMAP.md (1,212 lines)
- 📋 Initial configs: OPTIMAL_CONFIGS_V1 in constants.R (59 configs, 15 properties)
- ⏭️ Ready to start Phase 1 implementation

**Development Priorities**:
1. **Immediate**: Begin Phase 1 of Library Prediction feature
2. **Parallel**: JOSS submission prep (can work on both)
3. **No breaking changes** to existing Custom mode

*Last Updated: 2025-10-24*
*Package Version: 0.9.0*
*Branch: uncertainty-quantification*
*Status: Main stable + JOSS-ready | Feature branch planning complete*
