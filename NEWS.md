# horizons (development version)

## New Features

* `ensemble()` now calibrates CV+ conformal prediction intervals by default
  (`compute_uq = TRUE`). `predict()` on a `horizons_ensemble` returns
  `.pred_lower` / `.pred_upper` / `.interval_width` alongside `.pred`;
  previously `interval = TRUE` degraded to point predictions with a note.
  Pass `compute_uq = FALSE` to skip calibration. Bounds are aggregated from
  retained per-fold meta-learner refits on a calibration partition disjoint
  from the tuning folds; see `?fit_ensemble_uq` for the methodology and its
  honesty caveats. The `compute_uq` argument is placed after `seed`, so
  existing positional calls (`method`, `optimize`, `seed`) are unaffected.

* Deploy-time predictions are winsorized to a training-derived response upper
  bound (`max(training outcome) * 1.5`, stored as `models$response_bound` by
  `fit()`), with a visible warning when the clamp fires — guarding against
  physically impossible back-transform blow-ups (e.g. an unconstrained
  log-scale prediction inflating through `exp()`). Single-model predictions
  clamp per config; ensemble predictions clamp once at the combined output
  (member predictions are meta-learner features and stay raw). Fit-time
  ranking, evaluation, and UQ-calibration paths are never clamped; interval
  bounds are never clamped. Objects fitted before this version predict
  without a clamp. `back_transform_predictions()` gains an `upper_bound`
  argument (default `NULL` = previous behavior).

* `ensemble()` contracts now record `optimize` and `seed`, and every
  `ensemble()` return is structurally validated (condition class
  `horizons_validation_error` on a malformed contract).

* All warnings from `back_transform_predictions()` now signal via `cli`
  (condition class `rlang_warning` rather than `simpleWarning`); message text
  is unchanged.

* `ensemble()` — combine the models from `fit()` into a stacked predictor via a
  meta-learner over the members' out-of-fold predictions. Three engines:
  `penalized` (glmnet, the default), `weighted` (inverse-RMSE average), and
  `xgb` (xgboost). Ensemble performance is reported on the held-out test set and
  is directly comparable to the single-model metrics from `fit()`, including an
  honest improvement-over-best-member comparison.

# horizons 0.9.0

## Major Changes

* **Soil covariate prediction now uses OSSL-centric PCA clustering**
  - Trains PCA on global OSSL reference library (12K+ samples)
  - Projects unknown samples into OSSL spectral space
  - **Performance**: 8-10% R² improvement over previous approach
  - Default configuration: 1st derivative + k-means clustering
  - Achieves R² > 0.92 for most soil properties

## Breaking Changes

* None - API remains fully backward compatible
* `n_similar` parameter deprecated (ignored, uses all available OSSL)

## New Features

* Advanced clustering options via experimental parameters:
  - `derivative_order`: 0 (smoothing), 1 (1st derivative), 2 (2nd derivative)
  - `clustering_method`: "kmeans" or "ward"
  - `use_mahalanobis`: TRUE/FALSE distance metric selection
  - `distance_percentile`: Threshold for OSSL cluster assignment

## Bug Fixes

* Increased `future.globals.maxSize` to 8GB to handle large OSSL datasets
* Fixed CLI tree structure for nested verbose output
* Added strategic `gc()` calls to reduce memory pressure

## Performance

* Default config: R² = 0.925 (clay) in ~22 minutes
* Maximum accuracy config (Ward + 2nd derivative): R² = 0.953 in ~68 minutes
* Memory usage: ~4GB per parallel worker

---

# horizons 0.8.2

Previous version - see Git history for details.
