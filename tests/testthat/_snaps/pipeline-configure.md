# configure() validation / error for multiple responses lists available variables

    Code
      configure(hd)
    Condition <horizons_error_input>
      Error in `configure()`:
      ! `x` has several responses, so `outcome` must name one.
      i Available: SOC and pH.

# configure() CLI output / prints the outcome, models, tuning, recipe settings and config count, with the PCA threshold only when PCA runs

    Code
      result <- configure(hd, models = c("rf", "cubist"), verbose = TRUE)
    Output
      ├─ Configuring pipelines...
      │  ├─ Outcome: SOC
      │  ├─ Models: rf, cubist
      │  ├─ Tuning: 5-fold CV, grid = 10
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
      │  ├─ Recipe: SG window 9 (36 cm⁻¹), PCA threshold 0.9
      │  └─ Configs: 3 total
      │

