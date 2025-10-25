# Library Prediction Service with Uncertainty Quantification
**Feature Branch:** `uncertainty-quantification`
**Started:** 2025-10-24
**Status:** Planning & Design

---

## 1. Overview & Strategic Goals

### 1.1. Problem Statement

**Current Limitation:**
The horizons package currently requires users to provide their own training data for ALL soil properties, even well-characterized ones like texture, pH, and organic carbon. This approach:
- Ignores the 12,000+ labeled MIR spectra in public reference libraries (OSSL, KSSL)
- Forces expensive and time-consuming local data collection
- Limits accessibility for service labs and researchers without large datasets
- Provides no quantification of prediction uncertainty

**User Impact:**
- Routine soil analyses (texture, pH, OC) require months of sample collection before predictions
- Predictions lack uncertainty intervals, limiting operational use and decision-making
- Novel properties (POM, MAOM) and standard properties treated identically despite different data availability

### 1.2. High-Level Goals

This feature must achieve:

**PRIMARY GOALS:**
1. **Dual-Mode Prediction System**
   - **Library Mode**: Leverage reference libraries (OSSL/KSSL) for training-data-free predictions of standard properties
   - **Custom Mode**: Maintain existing local training workflow for novel properties
   - Both modes equally important and well-supported

2. **Flexible Model Configurations**
   - Support ANY model type (not hardcoded to cubist)
   - User-specified preprocessing pipelines
   - Auto-optimization: test property-specific optimal configs, select best for each cluster

3. **Uncertainty Quantification**
   - Per-sample prediction intervals (not just point predictions)
   - Quantile models (q05/q95) + conformal calibration
   - Statistically valid coverage guarantees (90% intervals achieve ~90% empirical coverage)
   - Modular design: UQ can eventually extend to Custom mode

4. **Production-Ready Robustness**
   - Memory-optimized for service lab deployment
   - Applicability domain awareness (detect out-of-distribution samples)
   - Comprehensive logging and monitoring

**SECONDARY GOALS:**
5. Streamlined UX compared to current 4-function pipeline
6. Clear documentation and examples for both prediction modes
7. Foundation for future research (property-specific benchmarking paper)

### 1.3. Non-Goals (Explicit Scope Limitations)

**Out of scope for v1.0:**
- Real-time model retraining in production
- Support for non-MIR spectroscopy (Vis-NIR, LIBS, etc.)
- Integration with external databases beyond OSSL/KSSL
- Covariate integration improvements (deferred - current <5% accuracy gains don't justify complexity)
- UQ for Custom mode (deferred to Phase 4 - Library mode first)
- Pre-trained models shipped with package (clusters vary at prediction time)
- Graphical user interface or web service

**Explicitly Deferred:**
- Advanced covariate encoding strategies (weighted loss, residual modeling)
- Real-time coverage monitoring dashboards
- Multi-property joint predictions
- Calibration transfer across instruments

---

## 2. Architecture Overview

### 2.1. Dual-Mode Prediction System

horizons provides two complementary prediction workflows:

```
┌─────────────────────────────────────────────────────────────────┐
│                   HORIZONS PREDICTION MODES                      │
└─────────────────────────────────────────────────────────────────┘

MODE 1: Custom Training (EXISTING)
  Use Case: Novel properties, site-specific calibrations
  Input: User's spectra + user's training data
  Process: evaluate_models_local/hpc()
  Output: Custom-trained models
  Timeline: ~1 hour compute (after weeks/months of data collection)

MODE 2: Library-Based Prediction (NEW)
  Use Case: Standard properties (texture, pH, OC, N)
  Input: User's spectra only (no training data required)
  Process: predict_library() with OSSL/KSSL reference
  Output: Predictions with uncertainty intervals
  Timeline: ~1-2 hours compute (no data collection needed)
```

**Key Distinction:**
- **Custom mode** optimizes models for YOUR specific samples/conditions
- **Library mode** optimizes for generalizability across diverse soil types
- Users choose based on property type and data availability

### 2.2. Component Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│              LIBRARY PREDICTION WORKFLOW                         │
└─────────────────────────────────────────────────────────────────┘

1. LIBRARY DATA LAYER
   ├─ Load OSSL/KSSL spectral library
   ├─ Preprocess library (SNV, derivatives, etc.)
   └─ Build clustering space (PCA → kmeans)

2. UNKNOWN PROCESSING
   ├─ Load user spectra
   ├─ Preprocess (same pipeline as library)
   └─ Project into library clustering space

3. CLUSTER ASSIGNMENT
   ├─ Assign each unknown to library cluster
   ├─ Identify relevant library training samples per cluster
   └─ Calculate applicability domain metrics

4. MODEL OPTIMIZATION (per cluster)
   ├─ Lookup property-specific optimal configs (from sysdata)
   ├─ Quick-test top N configs on cluster subset
   ├─ Select best performer
   └─ Return winning config

5. MODEL TRAINING (per cluster)
   ├─ Train point model (winning config, full cluster data)
   ├─ Train quantile models (q05/q95, same config)
   └─ Apply memory optimization (butcher, streaming)

6. CONFORMAL CALIBRATION (per cluster)
   ├─ Hold out calibration set (20% of cluster)
   ├─ Compute nonconformity scores
   ├─ Calculate calibration margins (global or AD-stratified)
   └─ Validate coverage on calibration set

7. PREDICTION WITH UQ
   ├─ Generate point prediction
   ├─ Generate quantile predictions (q05/q95)
   ├─ Apply conformal adjustment
   ├─ Compute AD distance and apply policy (accept/inflate/abstain)
   └─ Return: {pred, pi_low, pi_high, cluster_id, ad_distance, flags}
```

### 2.3. Key Components & Responsibilities

**LIBRARY INFRASTRUCTURE:**
- `library-data.R` - Load/cache reference library data
- `library-clustering.R` - Build clustering space, assign unknowns
- `library-configs.R` - Manage optimal config registry

**MODEL TRAINING:**
- `library-optimize.R` - Auto-select best config per cluster
- `library-train.R` - Train point + quantile models
- `library-models.R` - Model specifications and memory management

**UNCERTAINTY QUANTIFICATION (Modular):**
- `uq-quantile.R` - Quantile model training logic
- `uq-conformal.R` - Conformal calibration
- `uq-ad.R` - Applicability domain metrics
- `uq-wrapper.R` - Generic UQ wrapper (works for Library OR Custom mode)

**USER API:**
- `predict-library.R` - Main entry point: `predict_library()`
- `predict-uq.R` - UQ-enabled prediction wrapper

### 2.4. Key Interfaces & Data Contracts

**Library Clustering Object:**
```r
LibraryClustering <- list(
  pca_transformation = recipes::step_pca object,
  kmeans_model = stats::kmeans object,
  cluster_centroids = tibble,
  cluster_stats = tibble(cluster_id, n_samples, properties),
  version = "v1.0",
  created_date = date
)
```

**Optimal Config Registry (sysdata.rda):**
```r
OPTIMAL_CONFIGS <- tibble(
  property = character,     # "clay", "pH", "SOC", etc.
  rank = integer,           # 1 = best, 2 = second best, etc.
  model = character,        # "random_forest", "cubist", etc.
  preprocessing = character, # "snv_deriv1", "sg", etc.
  transformation = character, # "none", "log", "sqrt"
  feature_selection = character, # "pca", "correlation", etc.
  expected_r2 = numeric,    # From benchmarking research
  typical_time_sec = numeric, # Expected training time
  notes = character
)
# Populated from future KSSL benchmarking paper
```

**UQ System Object:**
```r
UQSystem <- list(
  point_model = workflow,
  quantile_model = workflow,  # Emits q05 and q95
  calibration = list(
    alpha = 0.10,
    c_global = numeric,  # Global calibration margin
    c_bins = tibble,     # AD-stratified margins (optional)
    width_floor = numeric
  ),
  ad_config = list(
    metric = "mahalanobis",
    thresholds = list(soft = p95, hard = p99.5)
  ),
  cluster_id = integer,
  property = character,
  version = "v1.0"
)
```

**Prediction Output Schema:**
```r
predict_library() returns tibble with:
  sample_id = character,
  property = character,
  pred = numeric,              # Point prediction
  pi_low = numeric,            # Lower bound (1-alpha)
  pi_high = numeric,           # Upper bound (1-alpha)
  pi_level = numeric,          # Nominal level (e.g., 0.90)
  cluster_id = integer,
  assignment_prob = numeric,   # GMM probability for assigned cluster
  assignment_entropy = numeric, # Entropy of cluster distribution
  ad_distance = numeric,       # Mahalanobis distance in model space
  ad_bin = integer,            # Quartile bin
  flag = character,            # "ok", "inflated", "abstained", "low_confidence"
  model_config = character,    # Winning config ID
  lab_method = character,      # Laboratory method used (e.g., "pH_H2O", "NH4OAc")
  version = character          # System version for reproducibility
```

### 2.5. Integration with Existing horizons Components

**REUSE from Current Package:**
- ✅ `create_configs()` - Extend to support quantile model variants
- ✅ `build_recipe()` - All preprocessing/feature selection logic
- ✅ `define_model_specifications()` - Model specs (add quantile support)
- ✅ Evaluation metrics (RPD, CCC, RRMSE)
- ✅ Memory optimization patterns (butcher, rm/gc, streaming)
- ✅ Parallel processing infrastructure (future, furrr)

**REFACTOR from Covariates System:**
- 📦 `covariates-data.R` → `library-data.R` (generalize OSSL loading)
- 📦 `covariates-soil.R` → `library-clustering.R` + `library-train.R` (split concerns)
- 📦 V2 clustering approach (PCA projection) → core of Library mode

**NEW Components:**
- 🆕 Auto-optimization logic
- 🆕 Quantile model training
- 🆕 Conformal calibration
- 🆕 Applicability domain metrics
- 🆕 Unified prediction API

**KEEP Separate (No Changes):**
- ✅ `evaluate_models_local/hpc()` - Custom mode stays as-is
- ✅ `finalize_top_workflows()` - Used by both modes eventually
- ✅ `build_ensemble()` - May integrate with Library mode later

### 2.6. Target Variable Handling Strategy

**Challenge:** Different soil properties require different handling for valid predictions and uncertainty intervals.

**Approach:** Property-specific transformations and constraints

**Texture Properties (Sand, Silt, Clay) - Compositional Data:**
```r
# Problem: Must sum to 100%, but independent models don't guarantee this
# Solution: Isometric Log-Ratio (ILR) transformation
1. Transform: [sand, silt, clay] → [ilr_1, ilr_2] (2 unconstrained coordinates)
2. Model ilr_1 and ilr_2 independently (2 models instead of 3)
3. Back-transform: [ilr_1, ilr_2] → [sand, silt, clay] (guaranteed sum = 100%)

# R package: compositions::ilr() and compositions::ilrInv()
```
**Implementation:**
- v1.0: Point predictions always project to simplex (sum = 100%)
- v1.0: Intervals are MARGINAL (per-component, may sum >100% - document this limitation)
- v1.1+: Joint interval sampling in ILR space for coherent intervals

**Bounded Properties (pH):**
```r
# pH theoretically bounded [0, 14]
# Clip predictions post-hoc:
ph_pred <- pmax(0, pmin(14, ph_pred))
ph_pi_low <- pmax(0, ph_pi_low)
ph_pi_high <- pmin(14, ph_pi_high)
```

**Strictly Positive Properties (SOC, Carbonate, Elements):**
```r
# Enforce non-negativity:
pred <- pmax(0, pred)
pi_low <- pmax(0, pi_low)

# Transformation testing:
# Let configs test: "none", "log", "sqrt"
# Auto-optimization will find best per property per cluster
```

**Skewed Properties:**
- Test log/sqrt transformations as part of config optimization
- Not prescribed - let data decide via auto-optimization
- Note: Quantiles on transformed scale must be back-transformed correctly

**Method Harmonization:**
- Filter OSSL to single standardized method per property:
  - pH: 1:1 H2O suspension only
  - Extractable cations: NH4OAc only
  - Texture: Pipette or hydrometer (consistent method)
- Document method used in output schema
- Flag in README: "Validate predictions if using different lab methods"

---

## 3. Critical Design Decisions

### 3.1. Clustering Strategy (THE HINGE POINT)

**Approach: Library-Anchored GMM Clustering**

**Training Phase (One-time on OSSL):**
```r
1. Load OSSL library (~12K labeled spectra)
2. Preprocess:
   - Optional: Remove water bands (3600-3000, 1650-1600 cm⁻¹) [EXPERIMENTAL FLAG]
   - Baseline correction (ALS or rubberband)
   - SNV or EMSC scatter correction
   - Optional derivative (SG filter)
3. PCA: Retain 99% variance (~15-25 components)
4. GMM clustering: Fit Gaussian Mixture Model (optimal K selected by BIC)
   - Test K ∈ {5, 7, 9, 11}, select via BIC
   - Full covariance with Ledoit-Wolf shrinkage (prevents singularity)
   - k-means++ initialization for stability
   - Enforce minimum cluster size: n > 300 samples
   - EM algorithm for fitting
5. Calculate per-cluster Mahalanobis thresholds (p95, p99.5)
   - Use regularized covariances for distance calculation
6. Save: PCA loadings, GMM parameters, centroids, regularized covariances, posterior entropy
```

**Prediction Phase (Runtime per user batch):**
```r
1. Preprocess unknowns (identical pipeline to library)
2. Project into library PCA space (using saved loadings)
3. Assign to clusters via GMM:
   - Get probability distribution over clusters
   - Assign to highest-probability cluster
   - Flag low-confidence assignments (max probability < 0.70)
4. Calculate Mahalanobis distance to assigned cluster centroid
5. Optional: If >1000 unknowns and >30% in one cluster → refine sub-clustering
```

**Key Decisions:**

**Decision 3.1.1: GMM over k-means**
- **Rationale**:
  - Captures within-cluster covariance (needed for Mahalanobis AD metrics)
  - Provides probability scores (useful for UQ - inflate intervals for low-confidence assignments)
  - Soft boundaries make sense for continuous soil properties
- **Implementation Details**:
  - Model selection: BIC across K ∈ {5, 7, 9, 11}
  - Covariance regularization: Ledoit-Wolf shrinkage to prevent singularity
  - Initialization: k-means++ for stability
  - Minimum cluster size: n > 300 (merge or refit if violated)
  - Track posterior entropy: high entropy → low assignment confidence
- **Cost**: ~2x slower than k-means (acceptable - one-time training)
- **Alternative rejected**: k-means (hard boundaries, assumes spherical clusters, no probabilities)

**Decision 3.1.2: Water band removal (Experimental)**
- **Default**: OFF (remove_water_bands = FALSE)
- **Rationale**: Literature is mixed, OSSL may already handle differently
- **Implementation**: Optional flag, document as experimental
- **Validation needed**: Test cluster quality with/without removal
- **Regions**: 3600-3000 cm⁻¹, 1650-1600 cm⁻¹ (H2O interference)

**Decision 3.1.3: Library clusters are stable anchors**
- Unknowns assigned TO library clusters (not re-clustered jointly)
- Ensures reproducibility and computational efficiency
- Clusters represent stable soil spectral patterns from diverse global library

**Decision 3.1.4: Clustering space = Preprocessing for assignment**
- Not necessarily same as model feature space
- Clustering uses robust, generalizable preprocessing (SNV/deriv → PCA)
- Models per cluster can use cluster-specific feature selection (Boruta, CARS, etc.)

### 3.2. Known Limitations & Future Work

**Instrument Standardization:**
- **Limitation**: v1.0 optimized for Bruker DRIFT spectrometers (KSSL/OSSL standard)
- **Impact**: Other instruments (Agilent, PerkinElmer, Thermo) may require calibration transfer
- **Mitigation**: Document in README, recommend validation with known samples
- **Future (v1.2+)**: Implement Piecewise Direct Standardization (PDS) for instrument transfer
- **Rationale**: KSSL + most modern labs use Bruker → reasonable v1 scope

**Covariate Integration:**
- **Current status**: <5% accuracy improvement, not worth complexity
- **Deferred**: Advanced encoding strategies (weighted loss, residual models, covariate-stratified ensembles)
- **Future research**: Two-step architectures, covariate-based sample weighting

### 3.3. Memory Management Strategy

**Philosophy: Strip aggressively at EVERY stage**

**Stage 1: After Library Loading**
```r
ossl_raw <- load_ossl()
ossl_processed <- preprocess(ossl_raw)
rm(ossl_raw); gc()
```

**Stage 2: After PCA**
```r
pca_result <- perform_pca(ossl_processed)
pca_loadings <- extract_loadings(pca_result)
pca_scores <- pca_result$x
rm(ossl_processed, pca_result); gc()
```

**Stage 3: After Clustering**
```r
gmm_model <- fit_gmm(pca_scores)
cluster_assignments <- predict(gmm_model, pca_scores)
centroids <- gmm_model$means
covariances <- gmm_model$covariances
rm(pca_scores, gmm_model$other_attributes); gc()
```

**Stage 4: After Model Training (CRITICAL)**
```r
train_and_strip <- function(workflow, cluster_data) {

  fitted <- parsnip::fit(workflow, cluster_data)
  stripped <- butcher::butcher(fitted)

  # Verify stripping worked
  size_before <- object.size(fitted)
  size_after <- object.size(stripped)
  reduction_pct <- 100 * (1 - size_after/size_before)

  cli::cli_alert_info("Model stripped: {reduction_pct}% reduction")

  rm(fitted, cluster_data)
  gc()

  return(stripped)
}
```

**Stage 5: Process Clusters Sequentially**
```r
# DO NOT parallelize across clusters if memory-constrained
# Process one cluster at a time, strip, move to next
for (cluster_id in unique(assignments)) {

  cluster_data <- filter_cluster(ossl_data, cluster_id)
  model <- train_and_strip(workflow, cluster_data)
  predictions <- predict(model, unknowns_in_cluster)

  results <- bind_rows(results, predictions)

  rm(cluster_data, model, predictions)
  gc()
}
```

**Future Consideration: Smart Caching**
- Use `qs::qsave()` for intermediate artifacts (faster than .rds)
- Cache expensive computations (PCA, clustering) to disk
- Investigate copy-on-write optimization for parallel contexts
- **Flag for Phase 2+**: Research memory-mapped arrays for large OSSL data

---

## 4. Implementation Phases

### PHASE 1: Core Infrastructure & Baseline (No UQ)
**Duration**: 2-3 weeks
**Goal**: Establish working library prediction WITHOUT uncertainty quantification

**Milestone 1.1: Library Data Layer** ✅ COMPLETE (2025-10-24)
- ✅ Created `library-data.R` (1,026 lines, 7 functions)
- ✅ Created `test-library-data.R` (488 lines, 62 tests passing)
- ✅ Generalized OSSL loading for all 15 LIBRARY_PROPERTIES
- ✅ Memory-efficient loading with global caching (lab + MIR files)
- ✅ Column renaming (scan_mir.600_abs → X600 format)
- ✅ Water band removal flag (experimental)
- ✅ SNV preprocessing for clustering space
- ✅ PCA training (99% variance threshold) and projection
- ✅ Comprehensive error handling (safely_execute + handle_results)
- ✅ Tree-style verbose output
- **Acceptance**: ✅ All 15 properties load successfully (79K-132K samples each)
- **Commit**: e9363f2

**Milestone 1.2: Clustering System** ✅ COMPLETE (2025-10-24/25)
- ✅ Created `library-clustering.R` (501 lines, 3 functions)
- ✅ Created `test-library-clustering.R` (382 lines, 38 tests passing)
- ✅ GMM clustering with BIC model selection (K ∈ {5, 7, 9, 11})
- ✅ Ledoit-Wolf covariance shrinkage for stability
- ✅ Unknown assignment with probability scores and entropy
- ✅ Mahalanobis distance calculation for AD metrics (corrected sqrt)
- ✅ Per-cluster percentile thresholds (p95, p99.5)
- ✅ Confidence flagging (high/moderate/low/ambiguous)
- ✅ Centroids transposed to n_clusters × n_dims format
- ✅ **Smart OSSL Filtering Added**:
  - Dataset: KSSL only (Kellogg Soil Survey Lab)
  - Instrument: Bruker Vertex 70 with HTS-XT accessory
  - Depth: Surface samples (< 30 cm)
  - Completeness: Removed samples with missing spectra
  - Result: 135K → ~26K high-quality samples (zero NAs!)
- **Acceptance**: ✅ Unknowns assigned to clusters with valid probabilities
- **Validation**: ✅ Tested on real clay data - K=7, sensible cluster sizes
- **Commits**: 67a7cf5, 287f76f, 37265a6

**Milestone 1.3: Initial Optimal Configs**
- Create `OPTIMAL_CONFIGS_V1` in `constants.R` based on:
  - Current covariates-soil.R performance
  - Literature best practices
  - Expert intuition (5-10 configs per property for: clay, sand, pH, SOC, N)
- **Acceptance**: Config registry exists and is queryable

**Milestone 1.4: Model Training Infrastructure**
- Refactor model training from `covariates-soil.R`
- Support flexible configs (not hardcoded to cubist)
- Implement memory stripping pattern
- Train point models only (no quantiles yet)
- **Acceptance**: Can train ANY config on ANY property, memory-optimized

**Milestone 1.5: Target Handling Implementation**
- Implement ILR transformation for texture properties
  - Add `compositions` dependency
  - Create wrapper functions for ilr ↔ texture conversion
  - Test: texture predictions always sum to 100%
- Implement bounds enforcement:
  - pH: clip to [0, 14]
  - All properties: enforce non-negativity where applicable
- **Acceptance**:
  - Texture predictions respect mass balance
  - No impossible values (negative SOC, pH > 14, etc.)

**Milestone 1.6: Basic Prediction API**
- Create `predict_library()` stub
- Implement: load library → cluster → train → predict (point only)
- Apply target transformations and bounds
- No UQ, no optimization (uses single config for now)
- **Acceptance**:
  - End-to-end workflow works for clay prediction
  - Texture (sand + silt + clay) sums to 100%

**Phase 1 Deliverable**:
Working library prediction for 5 properties (clay, sand, silt, pH, SOC) with point predictions, proper target handling, no UQ

---

### PHASE 2: Auto-Optimization
**Duration**: 1-2 weeks
**Goal**: Automatically select best model config per property per cluster

**Milestone 2.1: Quick Evaluation Framework**
- Implement subset-based config testing
  - Sample 10-20% of cluster data (or max 500 samples)
  - **Stratified sampling** by property value quantiles (avoid selection bias)
  - Run 5-fold CV (not 10-fold for speed)
  - Test 5-10 grid points (not 15)
- Compute performance metrics (R², RMSE, RPD)
- **Acceptance**:
  - Can test 10 configs in <15 minutes
  - Subset sample distribution matches full cluster (KS-test p > 0.05)

**Milestone 2.2: Config Selection Logic**
- Rank tested configs by performance
- Select winner
- Log selection decision (which configs tested, why winner chosen)
- **Acceptance**: Auto-select returns best config with justification

**Milestone 2.3: Full Training on Winner**
- Train winning config on full cluster data
- Apply memory optimization
- **Acceptance**: Full pipeline: test configs → pick winner → train → predict

**Phase 2 Deliverable**:
Auto-optimized predictions that test multiple configs and select best performer per cluster

---

### PHASE 3: Uncertainty Quantification
**Duration**: 3-4 weeks
**Goal**: Add calibrated prediction intervals

**Milestone 3.1: Quantile Model Support**
- Extend `define_model_specifications()` to support quantile variants
  - `ranger` with `quantreg = TRUE` (PRIMARY - fits both quantiles jointly)
  - `lightgbm` with `objective = "quantile"` (alternative)
  - `xgboost` with quantile loss (alternative)
- Modify `create_configs()` to generate quantile variants
- Train both point and quantile models per cluster
- Implement monotonicity enforcement:
  ```r
  # Post-prediction safety check
  q05 <- pmin(q05, q95)
  q95 <- pmax(q05, q95)
  ```
- **Acceptance**:
  - Can train q05/q95 models
  - <5% of predictions have crossings before repair
  - Monotonicity enforced post-prediction

**Milestone 3.2: Validate Heteroscedasticity**
- **Research Spike** (2 days time-boxed):
  - Plot residuals vs predicted values for 5 properties
  - Test for heteroscedasticity (Breusch-Pagan test)
  - Document findings: Do we NEED quantile models?
- **Acceptance**: Evidence documented that quantile models capture conditional uncertainty

**Milestone 3.3: CV+ Conformal Calibration**
- Implement cross-conformal prediction (CV+ approach):
  ```r
  # Workflow:
  # 1. Split OSSL: 80% training pool + 20% external test (never touched)
  # 2. Within 80% training pool:
  #    - Run 5-fold CV to get out-of-fold (OOF) quantile predictions
  #    - Compute nonconformity on OOF: s_i = max(q05_oof - y, y - q95_oof)
  #    - Calculate c_alpha from ALL OOF samples (uses 100% of training pool)
  # 3. Train final models on full 80% pool
  # 4. Validate on 20% external test (measure coverage)
  ```
- **Benefit**: More stable c_alpha (9.6K samples vs 1.6K with simple holdout)
- Adjusted intervals: [q05 - c_alpha, q95 + c_alpha]
- Validate coverage on external test: should be 88-92% for alpha=0.10
- **Acceptance**:
  - Coverage ≈ nominal on external test (86-94% acceptable, target 88-92%)
  - c_alpha estimates stable across CV folds (CV < 20%)

**Milestone 3.4: Width Floor from Replicates**
- Estimate measurement noise from replicate scans (if available in OSSL)
- Set minimum interval width = 2 × noise_sd
- Enforce: width = max(conformal_width, width_floor)
- **Acceptance**: No intervals narrower than measurement precision

**Phase 3 Deliverable**:
Library predictions with calibrated 90% prediction intervals (global conformal, no AD stratification yet)

---

### PHASE 4: Applicability Domain & Advanced UQ
**Duration**: 2-3 weeks
**Goal**: Distance-aware calibration and inference policies

**Milestone 4.1: AD Metrics in Model Space**
- Compute Mahalanobis distance in MODEL feature space (not clustering space)
- For each cluster: calculate distances for all training samples
- Create AD bins (quartiles initially, not deciles)
- **Acceptance**: Every prediction has AD distance and bin assignment

**Milestone 4.2: Distance-Aware Conformal**
- **Research Spike** (3 days time-boxed):
  - Compute calibration margins per AD bin: c_b
  - Test: Does coverage vary by distance without stratification?
  - If YES → stratification justified
  - If NO → global conformal sufficient
- Implement per-bin calibration
- **Acceptance**: Coverage ~90% within each AD bin (not just overall)

**Milestone 4.3: Inference Policy**
- Implement soft/hard thresholds:
  - Soft zone (p95-p99.5): Inflate intervals by factor
  - Hard zone (>p99.5): Abstain (return NA with warning)
- Inflation function: linear ramp in v1, can learn g(distance) later
- **Acceptance**: Flags work correctly, abstention prevents bad predictions

**Milestone 4.4: Logging & Monitoring**
- JSONL prediction logs (sample hash, versions, flags, confidence)
- Basic QC report: coverage overall + by cluster + by AD bin
- **Acceptance**: Can diagnose coverage issues by querying logs

**Phase 4 Deliverable**:
Production-ready library predictions with distance-aware UQ, abstention policy, and monitoring

---

### PHASE 5: Polish & Integration (Future)
**Duration**: TBD
**Goal**: Extend UQ to Custom mode, improve UX

**Deferred to Post-v1.0:**
- Apply UQ wrapper to Custom mode predictions
- Unified `horizons::predict()` interface for both modes
- Streamline Custom mode UX (reduce 4-function pipeline)
- Pre-trained model bundles for common properties (if feasible)
- Advanced features: multi-property predictions, ensemble UQ, etc.

---

## 5. Architecture Decision Records (ADRs)

### ADR-001: Clustering Algorithm Choice
**Context**: Need to cluster 12K OSSL samples and assign unknowns
**Decision**: Use Gaussian Mixture Models (GMM) instead of k-means
**Rationale**:
- Captures within-cluster covariance (essential for Mahalanobis AD metrics)
- Provides probability scores for soft assignments (useful for UQ)
- Handles elliptical clusters (realistic for spectral data)
- Only ~2x computational cost vs k-means (acceptable for one-time training)
**Alternatives Considered**:
- k-means: Rejected (hard boundaries, assumes spherical clusters)
- Hierarchical (Ward's): Rejected (no probability scores, computationally expensive)
**Status**: Approved

### ADR-002: Quantile Models + Conformal (Belt-and-Suspenders)
**Context**: How to generate prediction intervals?
**Decision**: Train separate quantile models (q05/q95) THEN apply conformal calibration
**Rationale**:
- **Quantile models**: Learn heteroscedastic uncertainty from data (errors vary with magnitude)
- **Conformal**: Provides coverage guarantee even if quantile models miscalibrate
- Production service labs need robustness - double layer of protection
- Validated heteroscedasticity in soil data (residuals increase with property magnitude)
**Implementation**:
- Use `ranger` with `quantreg = TRUE` (fits BOTH quantiles jointly, reduces crossings)
- Also support: `lightgbm` (objective = "quantile"), `xgboost` (quantile loss)
- Post-prediction monotonicity enforcement:
  ```r
  # Safety check - repair crossings:
  if (any(q05 > q95)) {
    q05 <- pmin(q05, q95)
    q95 <- pmax(q05, q95)
    cli_warn("Quantile crossings detected and repaired")
  }
  ```
**Alternatives Considered**:
- Conformal on point predictions only: Rejected (misses conditional uncertainty)
- Bayesian models: Rejected (too slow, harder to integrate with tidymodels)
- Bootstrap: Rejected (computationally expensive for large library)
**Cost**: Doubles model training time and storage
**Status**: Approved (production robustness justifies cost)

### ADR-003: Property-Level Optimal Configs (Not Cluster-Specific)
**Context**: How to determine which model config to use per cluster?
**Decision**: Store optimal configs at PROPERTY level, auto-select best PER CLUSTER at runtime
**Rationale**:
- Clusters may vary between runs (based on unknowns)
- One benchmarking study per property (not per cluster)
- Quick-test top 10 property configs on each cluster, pick best
- Flexible: clusters find their best config dynamically
**Alternatives Considered**:
- Pre-compute optimal config per property × cluster: Rejected (clusters not stable)
- Single global config per property: Rejected (ignores cluster heterogeneity)
**Status**: Approved

### ADR-004: Two-Space Design (Clustering vs Model)
**Context**: Should clustering and modeling use the same feature space?
**Decision**: Separate spaces
- **Clustering space**: SNV/deriv → PCA (stable, assignment-focused)
- **Model space**: Per-cluster feature selection (Boruta, CARS, PCA - prediction-focused)
**Rationale**:
- Clustering provides stable IDENTITY (sample X always → cluster 3)
- Models can EVOLVE (cluster 3 might switch from Boruta to CARS in v2)
- AD metrics in model space reflect "weird for THIS model" not "weird in general"
**Cost**: Added complexity (two preprocessing pipelines, two distance metrics)
**Benefit**: Model iteration without re-clustering
**Status**: Approved

### ADR-005: No Pre-Trained Models in Package
**Context**: Should we ship pre-trained models with the package?
**Decision**: NO - models trained at runtime
**Rationale**:
- Clusters vary based on user's unknowns (can't pre-train for all scenarios)
- Package size would explode (5 clusters × 10 properties × 2 models × configs = 100+ objects)
- Training is fast enough (~1-2 hours acceptable for service labs)
**What CAN be pre-computed**: PCA loadings, GMM parameters, optimal config registry
**Status**: Approved

### ADR-006: Memory Stripping at Every Stage
**Context**: Kernel panic issues with large tidymodels workflows (v0.9.0 experience)
**Decision**: Aggressive memory management after EVERY processing stage
**Implementation**:
- `rm()` + `gc()` after: loading, PCA, clustering, training
- `butcher::butcher()` on all fitted workflows (60-80% reduction)
- Sequential cluster processing (not parallel) if memory-constrained
- `return_models = FALSE` option for streaming
**Rationale**: Previous kernel panic from workflow memory bloat (600 unknowns × 2 covariates = 1GB → crash)
**Validation**: Monitor memory usage, ensure <500MB overhead
**Status**: Approved (critical for production deployment)

### ADR-007: Hard Cluster Boundaries (v1.0 Simplification)
**Context**: Should clusters have hard boundaries (GMM assignment only) or soft boundaries (distance-weighted borrowing)?
**Decision**: Use hard GMM cluster assignments for v1.0
**Rationale**:
- Cluster sizes are sufficient (min = 988, median = 2,412 samples)
- Simpler to implement and test
- Each sample used in exactly one model (clean provenance)
- GMM probabilities already capture assignment uncertainty
**Performance Lever for Future**:
- If cluster-specific models underperform, implement soft boundaries:
  - Core samples: Assigned to cluster (weight = 1.0)
  - Neighbor samples: Within distance threshold from adjacent clusters
  - Sample weighting: weight = exp(-mahalanobis_distance)
  - Smooth transitions between cluster models
**Implementation Note**:
```r
# v1.0: Hard assignment
train_data_cluster_k <- library_data[gmm_assignments == k, ]

# v1.1: Soft boundaries (if needed)
train_data_cluster_k <- {
  core = library_data[gmm_assignments == k, ]
  neighbors = library_data[distance_to_centroid_k < threshold, ]
  bind_rows(
    core %>% mutate(weight = 1.0),
    neighbors %>% mutate(weight = exp(-distance))
  )
}
```
**Status**: Hard boundaries approved for v1.0, soft boundaries deferred to v1.1+ if performance indicates need

### ADR-008: Optional Spiking of User Data into Library Training
**Context**: What if user has SOME labeled data for a standard property but not enough for full Custom mode?
**Decision**: Allow optional spiking of user's labeled samples into library training data
**Use Case**:
- User has 50 labeled clay samples (not enough for Custom mode)
- Library has 12K OSSL samples
- Combine: Train on OSSL + user's 50 samples → better local adaptation
**Implementation**:
```r
predict_library(
  spectra = unknowns,
  property = "clay",
  spike_training_data = my_labeled_samples,  # Optional tibble
  spike_weight = 2.0  # Upweight user samples (optional)
)
```
**Benefits**:
- Bridges Library and Custom modes (hybrid approach)
- Leverages library baseline + local refinement
- Domain adaptation for site-specific conditions
**Challenges**:
- Ensure user data preprocessed identically to library
- Handle potential label quality issues (user vs OSSL lab standards)
- Re-cluster or just add to existing clusters?
**Phasing**:
- Phase 1: No spiking (pure library)
- Phase 2 or 3: Add spiking capability
- Validate: Does 50 spiked samples improve predictions for that site?
**Status**: Approved for Phase 2+ (deferred from initial implementation)

### ADR-009: Composite Metric for Config Ranking
**Context**: How to rank model configs during auto-optimization?
**Decision**: Use weighted composite score combining spectroscopy-appropriate metrics
**Formula**:
```r
composite_score = 0.35 * RPD_normalized +
                  0.25 * CCC_normalized +
                  0.25 * R²_normalized +
                  0.15 * (1 - RMSE_normalized)
```
**Rationale**:
- **RPD (35%)**: Primary spectroscopy metric (>2.5 excellent, 2.0-2.5 good, 1.5-2.0 fair)
- **CCC (25%)**: Lin's concordance - measures agreement with 1:1 line (critical for calibration)
- **R² (25%)**: Variance explained (standard metric, still informative)
- **RMSE (15%)**: Absolute error (less weight - scale-dependent across properties)
**Normalization**: Scale each metric to [0, 1] within tested configs per cluster
**Tie-Breaking**: If scores within 0.01, prefer simpler model (fewer features, faster)
**Alternatives Considered**:
- RPD only: Rejected (ignores concordance quality)
- R² only: Rejected (not spectroscopy-specific)
- Equal weighting: Rejected (RPD should dominate for spectroscopy)
**Status**: Approved

### ADR-010: Texture as Compositional Data (ILR Transformation)
**Context**: Sand + silt + clay must sum to 100%, but independent models don't guarantee this
**Decision**: Use Isometric Log-Ratio (ILR) transformation for texture properties
**Problem**:
- Modeling sand, silt, clay independently can produce:
  - Predictions that sum to 95% or 105% (mass balance violation)
  - Incoherent intervals (ranges that don't respect compositional constraint)
- Texture is compositional data (parts of a whole), not 3 independent variables
**Solution**:
```r
# Transform 3 constrained values → 2 unconstrained coordinates
ilr_coords <- compositions::ilr(cbind(sand, silt, clay))
# ilr_1, ilr_2 can range -∞ to +∞

# Model the 2 ILR coordinates:
ilr_1_model <- train(ilr_1 ~ spectra)
ilr_2_model <- train(ilr_2 ~ spectra)

# Back-transform to get sand, silt, clay:
texture <- compositions::ilrInv(cbind(ilr_1_pred, ilr_2_pred))
# Guaranteed to sum to 100%!
```
**Implementation Phasing**:
- **v1.0**: Point predictions always project to simplex (sum = 100%)
- **v1.0**: Intervals are MARGINAL (each component gets its own interval, may sum >100%)
  - Document this limitation clearly
  - Acceptable for initial release
- **v1.1+**: Joint interval sampling in ILR space for compositional coherence
**Benefit**: Prevents embarrassing mass-balance violations
**Cost**: Requires `compositions` package dependency, slightly more complex workflow
**Status**: Approved (scientifically necessary for texture)

**Future Investigation: Smart Caching**
- `qs::qsave()` for intermediate artifacts (faster than .rds)
- Memory-mapped arrays for OSSL data (revisit in Phase 2+)
- Copy-on-write optimization for parallel contexts
- **Research Spike**: Profile cache vs re-compute tradeoffs (time-box 2 days)

---

## 6. Research Spikes & Open Questions

### Spike-001: Heteroscedasticity Validation (Phase 3, 2 days)
**Question**: Do soil property prediction errors actually increase with magnitude?
**Method**:
- Train baseline models on OSSL for clay, pH, SOC, sand, N
- Plot residuals vs predicted values
- Run Breusch-Pagan test for heteroscedasticity
- Compare quantile model coverage vs conformal-only
**Success Criteria**: p < 0.05 for heteroscedasticity, quantile intervals narrower at low end
**If Fails**: Reconsider quantile models (but unlikely given soil science theory)

### Spike-002: Water Band Removal Impact (Phase 1, 1 day)
**Question**: Does removing water interference bands improve clustering quality?
**Method**:
- Cluster OSSL with and without water band removal
- Compare: silhouette scores, within-cluster variance, cluster interpretability
- Test predictions on holdout with both approaches
**Success Criteria**: >5% improvement in cluster quality metrics OR prediction accuracy
**If Fails**: Keep water band removal as experimental flag, default OFF

### Spike-003: AD Stratification Value (Phase 4, 3 days)
**Question**: Does coverage vary significantly by applicability domain distance?
**Method**:
- Implement global conformal first
- Stratify validation set by AD distance (quartiles)
- Measure coverage in each quartile
- Test: chi-square for coverage heterogeneity
**Success Criteria**: Coverage varies >10% across quartiles (e.g., 95% in Q1, 80% in Q4)
**If Fails**: Global conformal sufficient, skip AD-aware calibration

### Spike-004: Optimal Cluster Count (Phase 1, 1 day)
**Question**: How many clusters optimize the bias-variance tradeoff?
**Method**:
- Test k = 3, 5, 7, 10, 15 clusters on OSSL
- Compute: gap statistic, silhouette scores, prediction accuracy
- Consider cluster size (min n > 500 samples/cluster desirable)
**Success Criteria**: Clear elbow in metrics, interpretable clusters
**Default**: 5-7 clusters if no clear optimum

### Spike-005: GMM vs k-means Empirical Comparison (Phase 1, 1 day)
**Question**: Does GMM provide meaningful benefits over k-means for our data?
**Method**:
- Cluster OSSL with both algorithms
- Compare: cluster quality, prediction accuracy, probability scores' informativeness
- Check if GMM probabilities correlate with prediction error
**Success Criteria**: GMM probability < 0.7 → higher residuals (justifies UQ integration)
**If Fails**: Fall back to k-means (simpler)

### Spike-006: Cache vs Recompute Tradeoffs (Phase 2+, 2 days)
**Question**: Should we cache intermediate results (PCA, clustering) or recompute?
**Method**:
- Profile time: loading from cache vs recomputing
- Profile memory: cached objects vs on-the-fly
- Test with varying unknown batch sizes (10, 100, 1000, 10K samples)
**Success Criteria**: Cache saves >30 seconds and <200MB overhead
**Decision**: Implement smart caching if criteria met

---

## 7. Open Questions & Future Considerations

### Open Question 1: Covariate Encoding Strategies
**Status**: Deferred (current <5% gains don't justify complexity)
**Future Research Directions**:
1. **Residual modeling**: Train model on spectra, then model residuals with covariates
   - **Challenge**: Breaks tidymodels single-workflow paradigm
2. **Covariate-weighted loss**: Penalize errors more when covariate uncertainty is high
   - **Challenge**: Custom loss functions in tidymodels recipes
3. **Sample weighting**: Upweight training samples with similar covariate profiles
   - **Challenge**: Defining similarity metric, potential overfitting
4. **Stratified ensembles**: Blend models based on covariate-space distance
   - **Challenge**: Adds another layer of model complexity

**Revisit When**: Post-v1.0, if user feedback indicates need for >5% improvement

### Open Question 2: Multi-Property Joint Predictions
**Scenario**: User wants clay, sand, pH predictions simultaneously
**Current**: Run predict_library() 3 times (inefficient - clusters 3x, trains 3x)
**Future Optimization**:
- Cluster once, reuse for all properties
- Cache cluster assignments and training data
- Train all properties in parallel
**Implementation**: Phase 5 or v1.1

### Open Question 3: Calibration Set Size per Cluster
**Current Plan**: 20% holdout per cluster
**Concern**: Small clusters (n=500) → 100 calibration samples → 10 per AD bin (if deciles)
**Monitoring**: Track calibration set sizes, flag if n < 50 per cluster
**Adaptive Strategy**: Merge small clusters or use global calibration for clusters with n < 200

### Open Question 4: Model Space Distance Metric
**Current**: Mahalanobis distance in model feature space
**Alternatives**:
- Spectral Angle Mapper (shape similarity, ignores magnitude)
- Cosine similarity (common in spectroscopy)
- Leverage score (influence on model)
**Decision**: Start with Mahalanobis (well-understood), add alternatives if needed

### Open Question 5: Optimal Config Initialization Strategy
**Bootstrap Approach** (Before Benchmarking Paper):
```r
# Phase 1: Expert intuition + literature
OPTIMAL_CONFIGS_V0 <- tribble(
  ~property, ~rank, ~config_id, ~notes,
  "clay",    1,     "rf_snv_deriv1_pca", "Common in literature",
  "clay",    2,     "cubist_snv_corr",   "Current covariate default",
  ...
)

# Phase 2: Empirical testing as we build
# Test these configs, re-rank based on actual performance

# Phase 3: Comprehensive benchmarking (HPC paper)
# Replace with research-validated optimal configs
```

**Question**: Should we run mini-benchmark NOW on a few properties to seed better initial configs?
**Time Investment**: 1-2 days of HPC time, could improve Phase 1 results
**Decision**: TBD based on Phase 1 progress

---

## 8. Testing Strategy

### Test-Driven Development Approach

**Philosophy**: Build tests IN PARALLEL with implementation, not after

**Pattern for Each Milestone**:
```
1. Create test file skeleton FIRST
   - Define test structure and acceptance criteria
   - Tests initially fail (function doesn't exist)

2. Implement minimum code to pass tests
   - Write just enough to make tests green

3. Expand tests and code iteratively
   - Add edge cases to tests
   - Handle edge cases in implementation
   - Refactor with test safety net

4. Verify component coverage >80% before moving to next milestone
```

**Benefits**:
- Maintains 80.22% package coverage (don't regress!)
- Catches design issues early (ILR bugs, memory leaks, etc.)
- Tests document intended behavior
- Prevents "test debt" accumulation

**Practical Timeline** (Per Milestone):
- Morning: Write test skeleton (30 min)
- Morning: Implement core logic (2 hours)
- Afternoon: Expand tests + handle edge cases (2 hours)
- End of day: Verify tests pass, check coverage

---

### Unit Tests (Per Component)

**test-library-data.R**
- OSSL loading and caching
- Preprocessing pipeline reproducibility
- Water band removal (when enabled)
- Memory cleanup after loading

**test-library-clustering.R**
- GMM training deterministic (fixed seed)
- BIC selects optimal K correctly
- Covariance regularization prevents singularity
- Minimum cluster size enforced (n > 300)
- Assignment probability scores sum to 1
- Posterior entropy calculation
- Mahalanobis threshold calculation (using regularized covariance)
- Unknown assignment correctness
- Low-confidence flagging (prob < 0.70)

**test-library-targets.R**
- ILR transformation for texture:
  - ilr() and ilrInv() are inverses
  - Back-transformed predictions always sum to 100%
  - Edge case: handle zeros in texture components
- pH bounds enforcement (clipped to [0, 14])
- Non-negativity enforcement for all properties
- Transformation handling (log/sqrt back-transformation correct)

**test-library-configs.R**
- OPTIMAL_CONFIGS schema validation
- Config lookup by property
- Ranking logic

**test-library-optimize.R**
- Subset sampling stratification
- Config testing returns metrics
- Winner selection logic
- Edge case: all configs fail

**test-library-train.R**
- Memory stripping (verify size reduction >50%)
- Model training success for all model types
- Workflow bundling and unbundling

**test-uq-quantile.R**
- Quantile model specs correct (emits q05, q95)
- Monotonicity: q05 ≤ pred ≤ q95
- Quantile crossing detection

**test-uq-conformal.R**
- Calibration set holdout (no data leakage)
- Nonconformity score calculation
- Coverage on validation ~90% for alpha=0.10
- Per-bin calibration (if AD-stratified)

**test-uq-ad.R**
- Mahalanobis distance calculation
- AD bin assignment
- Threshold enforcement (soft/hard zones)

**test-predict-library.R**
- End-to-end: spectra → predictions with intervals
- Schema validation (all required fields present)
- Edge cases: OOD samples, small batches, missing data

### Integration Tests

**test-integration-library-workflow.R**
- Complete workflow for 3 properties: clay, pH, SOC
- Test with 100 synthetic unknowns
- Validate: predictions reasonable, intervals contain truth ~90%, no memory leaks

**test-integration-optimization.R**
- Auto-optimization finds best config
- Winner performs better than random config on holdout
- Time budget respected (<30 min for 10 configs)

**test-integration-uq.R**
- Quantile + conformal produces valid intervals
- Coverage measured on external validation set (spectral distance approach):
  ```r
  # External validation strategy:
  # 1. Hold out spectrally-distant cluster(s) as external test
  # 2. Cluster remaining OSSL, train models
  # 3. Predict held-out cluster (spectrally dissimilar samples)
  # 4. Measure coverage - should be robust to spectral shift
  ```
- Flags work correctly (inflate, abstain)
- Coverage reported by:
  - Overall
  - Property value quartiles (low vs high SOC)
  - AD distance bins

### Performance & Memory Tests

**test-memory-management.R**
- Measure memory before/after each stage
- Verify <500MB overhead for 1000 unknowns
- No memory leaks over 10 sequential runs

**test-computational-time.R**
- Profile end-to-end time for varying batch sizes
- Clustering: <5 min
- Training (per cluster): <20 min
- Prediction: <1 min per 100 samples

---

## 9. Success Metrics

### Phase 1 Success:
- ✅ Library predictions work for 5 properties
- ✅ Accuracy ≈ current covariates-soil.R performance (R² > 0.85 for clay/pH)
- ✅ Memory usage <500MB per cluster
- ✅ No code duplication with Custom mode

### Phase 2 Success:
- ✅ Auto-optimization selects winning config in <30 min
- ✅ Winner outperforms baseline by >3% R²
- ✅ Config selection logged and reproducible

### Phase 3 Success:
- ✅ Empirical coverage 86-94% on external validation (90% intervals)
- ✅ Quantile models show evidence of heteroscedasticity
- ✅ Intervals narrower than global ±2SD for 70%+ of samples

### Phase 4 Success:
- ✅ Coverage stable across AD bins (no bin <85% or >95%)
- ✅ Abstention policy prevents >99% of extreme outliers
- ✅ QC reports identify coverage drift

### Overall v1.0 Success:
- ✅ Production-ready for 5+ standard properties
- ✅ <5% of predictions flagged as abstained (good coverage)
- ✅ User documentation complete with examples
- ✅ Test coverage >75% for new components
- ✅ Service lab validation: 100 samples processed without issues

---

## 10. Timeline & Milestones

**Week 1-2**: Phase 1 (Core Infrastructure)
**Week 3-4**: Phase 2 (Auto-Optimization)
**Week 5-7**: Phase 3 (UQ - Quantile Models + Conformal)
**Week 8-10**: Phase 4 (AD-Aware UQ + Policies)
**Week 11+**: Polish, documentation, validation

**Total Estimated Duration**: 10-12 weeks for production-ready v1.0

**Checkpoint Reviews**:
- End of Phase 1: Validate baseline predictions work
- End of Phase 3: Validate UQ coverage guarantees
- End of Phase 4: Service lab beta testing

---

## 11. Next Actions

**Immediate (This Session):**
1. ✅ Roadmap document drafted
2. ⏭️ Review and approve roadmap
3. ⏭️ Create initial `OPTIMAL_CONFIGS_V0` in constants.R
4. ⏭️ Begin Phase 1, Milestone 1.1 (refactor library-data.R)

**Week 1 Goals:**
- Refactor data loading from covariates system
- Implement GMM clustering
- Create basic prediction scaffold

**Communication:**
- Update CLAUDE.md with new feature status
- Document decisions as we make them
- Flag blocking issues immediately

---

## 12. Technical Improvements Integrated (Post-Review)

**Date**: 2025-10-24
**Source**: Expert feedback on initial roadmap

### Critical Fixes Integrated:

**✅ ILR for Texture (ADR-008)**
- Compositional data handled correctly
- Sand + silt + clay guaranteed to sum to 100%
- v1.0: Marginal intervals (documented limitation)

**✅ CV+ Conformal (Phase 3.3)**
- Uses all training data for calibration (not just 20% holdout)
- More stable c_alpha estimates (9.6K vs 1.6K samples)
- Still maintains external test set for validation

**✅ GMM Regularization (ADR-001 Update)**
- BIC-based K selection
- Ledoit-Wolf shrinkage for covariance stability
- Minimum cluster size enforcement (n > 300)
- k-means++ initialization

**✅ Ranger Quantiles (ADR-002 Update)**
- Joint quantile fitting reduces crossings
- Monotonicity enforcement as safety net
- Crossings logged when detected

**✅ Target Handling (Section 2.6)**
- Property-specific transformations (log/sqrt tested in configs)
- Bounds enforcement (pH, non-negativity)
- Method harmonization (single lab method per property)

**✅ External Validation**
- Spectral distance approach (hold out distant clusters)
- Coverage reported by property quartiles and AD bins

**✅ Stratified Subset Sampling (Phase 2.1)**
- Prevents selection bias in auto-optimization
- KS-test validation of representativeness

**✅ Enhanced Output Schema**
- Added: assignment_prob, assignment_entropy, lab_method
- Flags include: "low_confidence" for uncertain assignments

### Deferred to Future Versions:

**⏭️ v1.1+:**
- Joint interval sampling for texture (coherent compositional intervals)
- Method Mondrianing (calibration by instrument/method)
- Nested CV for auto-optimization
- Covariate encoding improvements

**⏭️ v1.2+:**
- Instrument standardization (PDS)
- Advanced AD metrics (leverage scores, spectral angle)
- Multi-property joint predictions

---

*Last Updated: 2025-10-24*
*Status: Phase 1 In Progress - Milestones 1.1 ✅ 1.2 ✅ Complete*
*Next: Milestone 1.3 - OPTIMAL_CONFIGS (already done!) → M1.4 Model Training*
*Progress: Week 1, Day 1 complete - Library data + clustering infrastructure working*

