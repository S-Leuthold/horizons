# CodeRabbit Fixes - PR #9

**Generated:** 2026-01-15
**Repository:** S-Leuthold/horizons
**PR Title:** feat: Library prediction system with uncertainty quantification
**PR URL:** https://github.com/S-Leuthold/horizons/pull/9
**Total Issues:** 39 (2 Critical, 6 Major, 14 Minor, 18 Trivial)

---

## Summary

| # | Severity | File | Comment ID | Status |
|---|----------|------|------------|--------|
| 1 | 🔴 Critical | R/library-clustering.R:213 | 2695601196 | [x] Fixed |
| 2 | 🔴 Critical | tests/testthat/test-step_select_boruta.R:159 | 2695601234 | [x] Fixed |
| 3 | 🟠 Major | man/predict_library.Rd:29 | 2695601113 | [x] Fixed |
| 4 | 🟠 Major | man/train_cluster_models_with_uq.Rd:51 | 2695601154 | [x] Fixed |
| 5 | 🟠 Major | R/library-clustering.R:201 | 2695601193 | [x] Fixed |
| 6 | 🟠 Major | R/library-clustering.R:495 | 2695601216 | [x] Fixed |
| 7 | 🟠 Major | tests/testthat/test-step_select_boruta.R:145 | 2695601242 | [x] Fixed |
| 8 | 🟠 Major | tests/testthat/test-step_select_boruta.R:170 | 2695601243 | [x] Fixed |

---

## Critical Issues

### Issue 1: mclust covariance structure incompatibility

**Severity:** 🔴 Critical
**File:** `R/library-clustering.R`
**Lines:** 207-213
**Comment ID:** 2695601196

**Problem:**
When `covariance_regularization = FALSE`, the code directly assigns `gmm_model$parameters$variance$Sigma` without handling the varying covariance structures returned by mclust depending on model type. mclust returns non-3D structures (scalars or vectors) for spherical models (EII, VII, etc.), causing subscript errors when downstream code expects 3D array shape `(n_components × n_components × n_clusters)`.

**Status:** [x] Fixed
**Resolution:** Added comprehensive handling for all mclust Sigma structures (VVV, EEE, EII, VII, diagonal models) to ensure consistent 3D array output.

---

### Issue 2: Undefined variables in test

**Severity:** 🔴 Critical
**File:** `tests/testthat/test-step_select_boruta.R`
**Lines:** 139-159
**Comment ID:** 2695601234

**Problem:**
Several variables used in the mocking block are never defined:
- `data` (lines 143, 166, 170) — the test creates `test_data` on line 127, not `data`
- `boruta_object` (line 144) — referenced but never created
- `cluster_stub` (line 151) — referenced but never created

**Status:** [x] Fixed
**Resolution:** Defined missing boruta_object and cluster_stub variables, changed data to test_data throughout, fixed step index to [[2]], and aligned mock returns with assertions.

---

## Major Issues

### Issue 3: Missing parameter documentation

**Severity:** 🟠 Major
**File:** `man/predict_library.Rd`
**Lines:** 17-29
**Comment ID:** 2695601113

**Problem:**
The function signature includes `remove_water_bands`, `debug_mode`, `allow_par`, and `n_workers` parameters, but only `spectra`, `property`, and `verbose` are documented in the `\arguments` section.

**Status:** [x] Fixed
**Resolution:** Added documentation for remove_water_bands, debug_mode, allow_par, and n_workers parameters in R/library-orchestrator.R. Regenerated .Rd files with devtools::document().

---

### Issue 4: Missing return value documentation

**Severity:** 🟠 Major
**File:** `man/train_cluster_models_with_uq.Rd`
**Lines:** 42-51
**Comment ID:** 2695601154

**Problem:**
The `\value{}` section is incomplete. The function returns 10 components, but only 6 are documented. Missing:
- `is_residual_based`
- `residual_stats`
- `c_alpha`
- `ad_metadata`

**Status:** [x] Fixed
**Resolution:** R source already had complete return value docs. Regenerated .Rd files with devtools::document() to sync.

---

### Issue 5: Fallback to cov() may produce singular matrix

**Severity:** 🟠 Major
**File:** `R/library-clustering.R`
**Lines:** 195-201
**Comment ID:** 2695601193

**Problem:**
When `cov.shrink` fails on a small cluster, falling back to `cov(cluster_data)` can yield a singular or near-singular covariance matrix—the same issue shrinkage was meant to prevent.

**Status:** [x] Fixed
**Resolution:** Added ridge regularization (ε = 1e-6 × max(diag)) to cov() fallback to ensure positive definiteness.

---

### Issue 6: percentiles parameter ignored

**Severity:** 🟠 Major
**File:** `R/library-clustering.R`
**Lines:** 479-495
**Comment ID:** 2695601216

**Problem:**
The function accepts a `percentiles` parameter (default `c(0.95, 0.995)`) but the implementation hardcodes `0.95` and `0.995`, ignoring the parameter entirely.

**Status:** [x] Fixed
**Resolution:** Now uses percentiles[1] and percentiles[2] instead of hardcoded values.

---

### Issue 7: Mock return value doesn't match assertion

**Severity:** 🟠 Major
**File:** `tests/testthat/test-step_select_boruta.R`
**Line:** 145
**Comment ID:** 2695601242

**Problem:**
Line 145 mocks `getSelectedAttributes` to return `c("cluster_A")`, but line 164 asserts that `selected_vars` equals `c("600", "602")`. These values don't align.

**Status:** [x] Fixed
**Resolution:** getSelectedAttributes now returns actual wavelength names (c("600", "605")) that match the assertion.

---

### Issue 8: Step index mismatch in assertions

**Severity:** 🟠 Major
**File:** `tests/testthat/test-step_select_boruta.R`
**Line:** 170
**Comment ID:** 2695601243

**Problem:**
Line 161 accesses `prepped$steps[[1]]`, but the recipe has two steps: `step_transform_spectra` (first) and `step_select_boruta` (second). Should access `prepped$steps[[2]]`.

**Status:** [x] Fixed
**Resolution:** Changed from prepped$steps[[1]] to prepped$steps[[2]] since step_select_boruta is the second step after step_transform_spectra.

---

## Minor Issues (14 total)

Not addressed in this session. Can be tackled in a follow-up if desired.
