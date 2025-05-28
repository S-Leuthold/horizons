# Product Requirements Document — *horizons*

_Last updated: 2025-05-27_
_Written by: Sam Leuthold (sam.leuthold@colostate.edu)_

---

## 1. Purpose & Scope

Horizons is an R package that facilitates the analysis of mid-infrared (MIR) spectral data for soil characterization and property prediction. It provides a modular framework for spectral preprocessing, model tuning (including Bayesian optimization and parallel processing), ensemble stacking, and evaluation—grounded in reproducible workflows.

Designed for soil biogeochemists and data scientists, Horizons is not a standalone application but a curated set of R functions and structured workflows. It prioritizes clarity and modularity, supporting robust predictions and characterization across varied soil datasets. By offering a transparent and reproducible foundation, Horizons aims to serve as a shared reference point for future work in soil spectroscopy.

---

## 2. User Stories

- **Soil Scientist**: Integrate spectral and covariate data within a documented framework to produce reproducible predictions and robust characterizations of soils.
- **Environmental Data Scientist**: Systematically compare model configurations and tuning strategies to identify the most accurate and reproducible workflows for soil spectral data.
- **Lab Technician**: Read in and preprocess raw spectral and covariate data to ensure data quality and provide well-structured inputs for predictive modeling workflows.

---

## 3. Technical Requirements

**Data Ingestion & Preprocessing**
- Support input of MIR spectra in OPUS format and associated covariate CSV data.
- Trim spectra to 600–4000 cm⁻¹ and resample to a consistent wavenumber interval.
- Provide standard preprocessing tools, including standard normal variate (SNV), Savitzky-Golay smoothing (0th and 1st derivatives), and user-configurable transformations.
- Handle missing scans or covariate data robustly, with clear logging of dropped samples.

**Model Grid & Workflow Construction**
- Support workflows that combine:
  - Response transformations: none, log, square root, Box-Cox.
  - Preprocessing pipelines: e.g., SNV, Savitzky-Golay.
  - Model types: PLSR, Cubist, Random Forest, SVM, Bagged Neural Networks (BNN).
  - Optional covariate subsets.
- Store workflow configurations in structured tibbles with unique workflow IDs.

**Covariate Prediction & Integration**
- Support prediction of soil covariates (e.g., clay, pH, organic carbon) from external datasets (e.g., OSSL).
- Treat predicted covariates consistently with measured covariates in downstream modeling.
- Allow dynamic inclusion or exclusion of predicted covariates in model tuning and stacking.

**Non-Soil Covariate Integration**
- Support inclusion of non-soil covariates (e.g., weather, topography, land management) in predictive workflows.
- Ensure these covariates are joinable by sample ID and documented in output summaries.

**Model Tuning & Stacking**
- Implement grid search tuning (`tune_grid`) and Bayesian optimization (`tune_bayes`), with user-configurable grid sizes and iterations.
- Support workflow filtering based on performance metrics (e.g., RMSE thresholds).
- Support final model stacking using `stacks::stacks()`, with explicit outputs.

**Evaluation & Outputs**
- Compute evaluation metrics: RMSE, R², RRMSE, Concordance Correlation Coefficient (CCC), and RPIQ.
- Structure outputs as tibbles with explicit columns for metrics, workflow configuration, and sample IDs.
- Provide access to final fitted workflows for new data prediction.

**Spectral Characterization & Interpretation**
- Provide functions for exploratory analysis and characterization of MIR spectra:
  - PCA to reveal dominant patterns.
  - Raw spectral overlays by grouping factors.
  - Loadings plots for key wavenumbers.
  - Correlation analyses of wavenumbers with PCA scores.
- Complement predictive workflows with robust spectral interpretation tools.

**Batch Model Evaluation & Summaries**
- Include a batch evaluation system to iterate over model configurations, run evaluations, and save key metrics and stacking data.
- Log errors with reproducible outputs and generate a master summary table.
- Functionalize this workflow in future revisions for consistency and modularity.

**Reproducibility & Transparency**
- Consistently set random seeds and log modeling decisions and configurations.
- Align function names and argument structures with `tidymodels` idioms for clarity and reuse.

**Boundaries / Out of Scope**
- Horizons does not provide a graphical user interface or deployment-ready tools.
- It is intended for research workflows in R.
- Horizons is currently experimental and not yet a production-ready solution; users should expect to adapt or customize workflows to their specific data structures and research needs.

---

## 4. Roadmap & Milestones

| Version | Focus / Features                                                                                               |
|---------|---------------------------------------------------------------------------------------------------------------|
| MVP     | Core ingestion (soil spectra + soil covariates), preprocessing, model grid construction, tuning (grid + Bayesian), stacking, reproducible logs.        |
| v0.2    | Spectral characterization module (PCA, loadings, correlation), basic spectral and model visualizations.                                              |
| v0.5    | Expanded model library (XGBoost, GBM, SVM), inclusion of non-soil covariates (weather, topography).                                                  |
| v0.8    | Model interpretability tools (variable importance, SHAP), YAML/JSON config file support.                                                             |
| v1.0    | Full documentation (function-level, vignettes), test suite, community feedback integration, soil property-specific modules (if needed).              |

---

## 5. Refactoring & Technical Debt

Horizons is robust but evolving. Key areas for improvement include:

**Code Modularization & Function Boundaries**
- Data ingestion and preprocessing functions (e.g., `create_input_data`, `download_ossl_data`) currently combine I/O with transformation logic.  
  - **Action:** Refactor into clear read/process layers (e.g., `read_and_preprocess_spectra`, `average_replicate_scans`, `pivot_spectra_to_wide`) to improve testability and reuse.  
- Several functions rely on hardcoded file paths (e.g., `../../2_Data/1_Input_Data/`), limiting portability.  
  - **Action:** Externalize file paths to configuration files or function arguments.  
- `predict_covariates` and `full_model_evaluation` are large orchestrator functions.  
  - **Action:** Decompose into modular sub-functions.  
- `transform_outcome` currently operates outside the `recipes` system.  
  - **Action:** Consider replacing with `recipes::step_log` or a custom `step_transform_outcome`.  
- Duplicate definitions (e.g., `step_add_covariates_new`) should be consolidated.

**Workflow Orchestration & Naming Consistency**
- Inconsistent naming conventions (e.g., verb-noun vs noun-verb, `n_componenets` typo).  
  - **Action:** Adopt consistent verb-first naming and fix typos.  
  - **Benefit:** Better readability and maintainability.

**Redundant or Repeated Logic**
- Consolidate duplicated preprocessing logic (SNV, Savitzky-Golay) into shared helpers.  
- Replace scattered `tryCatch` usage with a generic safe execution wrapper.  
- Centralize seed and config management to avoid inconsistencies.

**Batch Evaluation & Safe Execution**
- Refactor the current batch evaluation script into integrated functions (`run_batch_evaluation()`, `safe_run()`, `prune_results()`) with robust argument validation.

**Dynamic Covariate Handling**
- Centralize logic for dynamic covariate inclusion/exclusion to ensure consistency across modules.

**Reproducibility & Logging**
- Develop structured YAML/JSON logs for reproducibility and transparent reporting.
- Decouple `cli` messaging from core logic for more flexible logging.

**Missing Tests & Edge Case Handling**
- Expand tests for edge cases (e.g., missing covariates, empty rows, extreme `coverage` values).  
- Provide explicit user overrides for hardcoded logic (e.g., 3-cluster cap).

**Known Data & External Handling Gaps**
- Strengthen file name validation and CSV parsing.
- Harden error handling in `download_ossl_data` and related scripts.
- Address potential division-by-zero or `NaN` issues in metrics calculations.

**Visualization & Spectral Characterization (Planned)**
- Integrate PCA overlays, loadings plots, and wavenumber correlations as dedicated functions.

**README & Documentation**
- Example code in `README.md` uses placeholders (`mydata`, `my_covs`) that should be replaced with minimal, reproducible examples.  
- Outdated copyright.

**Dependencies & Extensibility**
- Document external dependencies (e.g., `wavelets`, `plotly`) in the `DESCRIPTION` file to ensure reproducibility.

---