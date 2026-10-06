# print.horizons_data shows the empty state and the spectra() hint

    Code
      print(obj)
    Output
      ── horizons_data ──
      
      (empty)
      
      Use spectra() to load data.

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
         └─ Responses: clay, oc
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

