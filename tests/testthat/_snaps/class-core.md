# print.horizons_data shows the empty state and the spectra() hint, and an object with only provenance as not empty

    Code
      print(obj)
    Output
      ── horizons_data ──
      
      (empty)
      
      Use spectra() to load data.
    Code
      print(new_horizons_data(spectra_source = "/path/to/spectra", spectra_type = "opus"))
    Output
      ── horizons_data ──
      
      Provenance
         ├─ Source: /path/to/spectra
         └─ Type: opus
      
      Use summary() for details.

# print.horizons_data shows the counts, the outcome and the provenance

    Code
      print(obj)
    Output
      ── horizons_data ──
      
      Data
         ├─ Samples: 4
         ├─ Predictors: 3 (4000–3996 cm⁻¹)
         └─ Outcome: SOC
      
      Provenance
         ├─ Source: /path/to/spectra
         └─ Type: opus
      
      Use summary() for details.

# summary.horizons_data of an empty object shows the tuning defaults, the version and spectra() as the next step

    Code
      summary(obj)
    Output
      ── horizons_data summary ───────────────────────────────────────────────────────
      
      Data
         └─ (no data loaded)
      
      Provenance
         ├─ Created: <time>
         └─ Horizons version: <version>
      
      Configuration
         ├─ Configs defined: 0
         └─ Tuning defaults:
               ├─ Grid size: 10
               ├─ Bayesian iterations: 15
               └─ CV folds: 5
      
      Validation
         └─ Status: not run
      
      Pipeline Status
         └─ Next step: spectra()
      
      ──────────────────────────────────────────────────────────────────────────────── 

# summary.horizons_data shows the data, provenance, configuration and validation blocks, and configure() as the next step

    Code
      summary(obj)
    Output
      ── horizons_data summary ───────────────────────────────────────────────────────
      
      Data
         ├─ Samples: 4
         │     └─ First: SAMPLE_001, SAMPLE_002, SAMPLE_003, ...
         ├─ Predictors: 3
         │     ├─ Range: 4000–3996 cm⁻¹
         │     └─ Step: 2 cm⁻¹
         ├─ Outcome: SOC
         └─ Memory: <size>
      
      Provenance
         ├─ Spectra source: /path/to/spectra
         ├─ Spectra type: opus
         ├─ Created: <time>
         └─ Horizons version: <version>
      
      Configuration
         ├─ Configs defined: 0
         └─ Tuning defaults:
               ├─ Grid size: 10
               ├─ Bayesian iterations: 15
               └─ CV folds: 5
      
      Validation
         └─ Status: not run
      
      Pipeline Status
         └─ Next step: configure()
      
      ──────────────────────────────────────────────────────────────────────────────── 

# each class validator prints its findings as a tree before it aborts

    Code
      validate_horizons_eval(eval_obj)
    Output
      ! The horizons_eval object failed validation:
         ├─ rank_metric must be one of: rmse, rrmse, rsq, ccc, rpd, mae
         └─ n_train must be a single non-negative whole number
      
    Condition
      Error in `validate_horizons_eval()`:
      ! Validation failed:
      rank_metric must be one of: rmse, rrmse, rsq, ccc, rpd, mae
      n_train must be a single non-negative whole number

---

    Code
      validate_horizons_fit(fit_obj)
    Output
      ! The horizons_fit object failed validation:
         ├─ n_models must equal `length(workflows)` (2)
         └─ predictor_schema must be a non-empty character vector of training-axis columns
      
    Condition
      Error in `validate_horizons_fit()`:
      ! Validation failed:
      n_models must equal `length(workflows)` (2)
      predictor_schema must be a non-empty character vector of training-axis columns

---

    Code
      validate_horizons_ensemble(ens_obj)
    Output
      ! The horizons_ensemble object failed validation:
         ├─ improvement must be a single non-NA numeric
         └─ runtime_secs must be a single non-negative numeric
      
    Condition
      Error in `validate_horizons_ensemble()`:
      ! Validation failed:
      improvement must be a single non-NA numeric
      runtime_secs must be a single non-negative numeric

# print.horizons_data shows the selection section

    Code
      print(obj)
    Output
      ── horizons_data ──
      
      Data
         ├─ Samples: 12
         ├─ Predictors: 851 (4000–600 cm⁻¹)
         └─ Responses: clay, oc
      
      Provenance
         ├─ Source: tibble
         └─ Type: tibble
      
      Selection
         ├─ Scope: batch (2 groups)
         ├─ k: 3
         ├─ Targets: 4 rows
         ├─ Pool: 12 rows; drawn per property: clay 12/12, oc 12/12
         └─ Space: PCA 12 components, mahalanobis; twins excluded: 0 flagged, 0 removed
      
      Use summary() for details.

# summary.horizons_data mirrors the selection block and names it in the pipeline status

    Code
      summary(obj)
    Output
      ── horizons_data summary ───────────────────────────────────────────────────────
      
      Data
         ├─ Samples: 12
         │     └─ First: P001, P002, P003, ...
         ├─ Predictors: 851
         │     ├─ Range: 4000–600 cm⁻¹
         │     └─ Step: 4 cm⁻¹
         ├─ Responses: clay, oc
         └─ Memory: <size>
      
      Provenance
         ├─ Spectra source: tibble
         ├─ Spectra type: tibble
         ├─ Created: <time>
         └─ Horizons version: <version>
      
      Selection
         ├─ Scope: batch (2 groups)
         ├─ Properties: clay, oc
         ├─ k: 3
         ├─ Targets: 4 rows (source: tibble)
         ├─ Pool: 12 rows
         │     ├─ Drawn per property: clay 12/12, oc 12/12
         │     └─ Membership rows: 24
         ├─ Space: PCA 12 components
         │     └─ Metric: mahalanobis
         └─ Twins excluded: 0 flagged, 0 removed
      
      Configuration
         ├─ Configs defined: 0
         └─ Tuning defaults:
               ├─ Grid size: 10
               ├─ Bayesian iterations: 15
               └─ CV folds: 5
      
      Validation
         └─ Status: not run
      
      Pipeline Status
         ├─ Rows: drawn from a pool by select_training() (scope: batch)
         └─ Next step: configure()
      
      ──────────────────────────────────────────────────────────────────────────────── 

