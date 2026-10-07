# a d13C-like outcome with outcome_range = c(-Inf, Inf) / summary() shows the range when it is not the default

    Code
      summary(cfg)
    Output
      ── horizons_data summary ───────────────────────────────────────────────────────
      
      Data
         ├─ Samples: 250
         │     └─ First: S001, S002, S003, ...
         ├─ Predictors: 10
         │     ├─ Range: 4000–3982 cm⁻¹
         │     └─ Step: 2 cm⁻¹
         ├─ Outcome: SOC
         └─ Memory: <size>
      
      Provenance
         ├─ Spectra source: test
         ├─ Spectra type: mir
         ├─ Created: NULL
         └─ Horizons version: 
      
      Configuration
         ├─ Configs defined: 1
         ├─ Outcome: SOC
         ├─ Outcome range: c(-Inf, Inf)
         ├─ Models: rf
         ├─ Preprocessing: raw
         ├─ Transformations: none
         ├─ Feature selection: none
         └─ Tuning:
               ├─ Grid size: 2
               ├─ Bayesian iterations: 0
               └─ CV folds: 3
      
      Validation
         └─ Status: passed
      
      Pipeline Status
         └─ Next step: evaluate()
      
      ──────────────────────────────────────────────────────────────────────────────── 
    Code
      summary(plain)
    Output
      ── horizons_data summary ───────────────────────────────────────────────────────
      
      Data
         ├─ Samples: 60
         │     └─ First: S001, S002, S003, ...
         ├─ Predictors: 10
         │     ├─ Range: 4000–3982 cm⁻¹
         │     └─ Step: 2 cm⁻¹
         ├─ Outcome: SOC
         └─ Memory: <size>
      
      Provenance
         ├─ Spectra source: test
         ├─ Spectra type: mir
         ├─ Created: NULL
         └─ Horizons version: 
      
      Configuration
         ├─ Configs defined: 1
         ├─ Outcome: SOC
         ├─ Models: rf
         ├─ Preprocessing: raw
         ├─ Transformations: none
         ├─ Feature selection: none
         └─ Tuning:
               ├─ Grid size: 2
               ├─ Bayesian iterations: 0
               └─ CV folds: 3
      
      Validation
         └─ Status: passed
      
      Pipeline Status
         └─ Next step: evaluate()
      
      ──────────────────────────────────────────────────────────────────────────────── 

# the remedy text / points at the property's physical bounds, not the data's

    Code
      check_outcome_range(obj, verb = "evaluate")
    Condition
      Error:
      ! SOC lies outside `outcome_range`, c(0, Inf), so `evaluate()` would score it against predictions clamped to that range.
      x 60 of 60 values below the lower bound 0 (minimum -27.64)
      i `outcome_range` is the outcome's physical range, set by `configure()`. The default, `c(0, Inf)`, is for non-negative properties.
      i Re-run `configure()` with the property's physical bounds as `outcome_range`: `c(-Inf, Inf)` for a signed property.

