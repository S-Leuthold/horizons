# configure() validation / error for multiple responses lists available variables

    Code
      configure(hd)
    Condition <horizons_error_input>
      Error in `configure()`:
      ! `x` has several responses, so `outcome` must name one.
      i Available: SOC and pH.

# configure() tune_ranges / refuses ranges it cannot search, naming each, as an input error

    Code
      writeLines(said, sep = "\n\n")
    Output
      `tune_ranges` must be a named list of ranges of parameters the configured models tune.
      x `tune_ranges` names a parameter no configured model tunes: commitees. The configured models tune committees, neighbors, max_rules (cubist).
      i Each range is `c(lower, upper)` in dials' units, e.g. `tune_ranges = list(committees = c(1, 50))`.
      
      `tune_ranges` must be a named list of ranges of parameters the configured models tune.
      x `tune_ranges` names a parameter no configured model tunes: mtry. The configured models tune committees, neighbors, max_rules (cubist).
      i Each range is `c(lower, upper)` in dials' units, e.g. `tune_ranges = list(committees = c(1, 50))`.
      
      `tune_ranges` must be a named list of ranges of parameters the configured models tune.
      x `tune_ranges` must be a named list of ranges, not a number.
      i Each range is `c(lower, upper)` in dials' units, e.g. `tune_ranges = list(committees = c(1, 50))`.
      
      `tune_ranges` must be a named list of ranges of parameters the configured models tune.
      x Every range in `tune_ranges` must be named by the parameter it sets.
      i Each range is `c(lower, upper)` in dials' units, e.g. `tune_ranges = list(committees = c(1, 50))`.
      
      `tune_ranges` must be a named list of ranges of parameters the configured models tune.
      x `tune_ranges` names committees more than once.
      i Each range is `c(lower, upper)` in dials' units, e.g. `tune_ranges = list(committees = c(1, 50))`.
      
      `tune_ranges` must be a named list of ranges of parameters the configured models tune.
      x `tune_ranges`'s committees must be two finite numbers, the lower below the upper, not `c(1, 8, 9)`.
      i Each range is `c(lower, upper)` in dials' units, e.g. `tune_ranges = list(committees = c(1, 50))`.
      
      `tune_ranges` must be a named list of ranges of parameters the configured models tune.
      x `tune_ranges`'s committees must be two finite numbers, the lower below the upper, not `c(NA, 8)`.
      i Each range is `c(lower, upper)` in dials' units, e.g. `tune_ranges = list(committees = c(1, 50))`.
      
      `tune_ranges` must be a named list of ranges of parameters the configured models tune.
      x `tune_ranges`'s num_comp must be two finite numbers, the lower below the upper, not `c(1, Inf)`.
      i Each range is `c(lower, upper)` in dials' units, e.g. `tune_ranges = list(committees = c(1, 50))`.
      
      `tune_ranges` must be a named list of ranges of parameters the configured models tune.
      x `tune_ranges`'s committees must be two finite numbers, the lower below the upper, not `c(8, 8)`.
      i Each range is `c(lower, upper)` in dials' units, e.g. `tune_ranges = list(committees = c(1, 50))`.
      

# configure() CLI output / prints the outcome, models, tuning, recipe settings and config count, with the PCA threshold only when PCA runs

    Code
      result <- configure(hd, models = c("rf", "cubist"), verbose = TRUE)
    Output
      ├─ Configuring pipelines...
      │  ├─ Outcome: SOC
      │  ├─ Models: rf, cubist
      │  ├─ Tuning: 5-fold CV, grid = 10
      │  ├─ Ranges: committees 1 to 25 (cubist)
      │  ├─ Recipe: SG window 9 (36 cm⁻¹)
      │  └─ Configs: 2 total
      │
    Code
      result <- configure(hd, sg_window = 11L, verbose = TRUE)
    Output
      ├─ Configuring pipelines...
      │  ├─ Outcome: SOC
      │  ├─ Models: rf, cubist, plsr
      │  ├─ Tuning: 5-fold CV, grid = 10
      │  ├─ Ranges: committees 1 to 25 (cubist); num_comp 1 to 30 (plsr)
      │  ├─ Recipe: SG window 11 (44 cm⁻¹)
      │  └─ Configs: 3 total
      │
    Code
      result <- configure(hd, feature_selection = "pca", pca_threshold = 0.9,
        verbose = TRUE)
    Output
      ├─ Configuring pipelines...
      │  ├─ Outcome: SOC
      │  ├─ Models: rf, cubist, plsr
      │  ├─ Tuning: 5-fold CV, grid = 10
      │  ├─ Ranges: committees 1 to 25 (cubist); num_comp 1 to 30 (plsr)
      │  ├─ Recipe: SG window 9 (36 cm⁻¹), PCA threshold 0.9
      │  └─ Configs: 3 total
      │

