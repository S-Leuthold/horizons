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

# each class validator puts its findings in the condition, with the remedy

    Code
      validate_horizons_eval(eval_obj)
    Condition
      Error:
      ! The <horizons_eval> object failed validation.
      x rank_metric must be one of: rmse, rrmse, rsq, ccc, rpd, mae
      x n_train must be a single non-negative whole number
      i Re-run evaluate() on the object, which rewrites its evaluation record.

---

    Code
      validate_horizons_fit(fit_obj)
    Condition
      Error:
      ! The <horizons_fit> object failed validation.
      x n_models must equal `length(workflows)` (2)
      x predictor_schema must be a non-empty character vector of training-axis columns
      i Re-run fit() on the object, which rewrites its fitted models.

---

    Code
      validate_horizons_ensemble(ens_obj)
    Condition
      Error:
      ! The <horizons_ensemble> object failed validation.
      x improvement must be a single numeric
      x runtime_secs must be a single non-negative numeric
      i Re-run ensemble() on the fitted object, which rebuilds the ensemble.

# a refusal shows five findings, counts the rest, and keeps every one verbatim in the field

    Code
      abort_validation(findings)
    Condition
      Error:
      ! The <horizons_data> object failed validation.
      x Columns missing from role_map: {x}
      x finding 2
      x finding 3
      x finding 4
      x finding 5
      i 1 more finding not shown; all are in the error's `findings` field.
      i If the object was edited by hand or saved by an older version of horizons, rebuild it with the verbs that wrote it: it cannot be repaired in place.

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

