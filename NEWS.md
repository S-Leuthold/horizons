# horizons (development version)

## Performance

* `evaluate(workers > 1)` now works at library scale. It previously aborted
  with `future.globals.maxSize` or R's `long vectors not supported yet` on
  datasets above a few thousand rows, because R's serializer does not
  deduplicate data frames: every *reference* to the training table became a
  full *copy* when an object crossed to a parallel worker. Neither
  `object.size()` nor `lobstr::obj_size()` reports this, since the duplication
  exists only at serialization time.

  Three sources are fixed. Recipe step selectors no longer retain the frame
  they were built in (~5x smaller recipes). `evaluate()` sends the analysis
  table once with integer indices and rebuilds the resamples worker-side,
  rather than sending an `rsplit` and a `vfold_cv` that shared one table in
  memory and serialized as one plus `v` copies. And the parallel worker body
  moved to a top-level function, so it no longer carries `evaluate()`'s entire
  frame to every worker.

  Measured on a 17,788 x 1,701 spectral matrix, the per-worker payload went
  from 2,086.6 MB to 417.6 MB. The old figure sat above R's 2,048 MB
  long-vector limit, which is what the failures were.

* `evaluate_single_config()` and `fit_single_config()` now pin the RNG kind as
  well as the seed. A worker started under `furrr_options(seed = TRUE)` runs
  L'Ecuyer-CMRG, and `set.seed()` alone does not reset it, so sequential and
  parallel runs previously drew different streams from the same seed.

## Breaking / behavioural

* Parallel `evaluate()` now refuses to run under `devtools::load_all()`.
  Workers resolve the package namespace by name and therefore load the
  *installed* `horizons`, not the source tree — so a stale install would have
  run old code behind a new worker body without saying so. Install the package
  (`R CMD INSTALL`) before using parallelism in development.

* `magrittr` is no longer a dependency; the package uses the base pipe (`|>`)
  throughout. `%>%` was imported but never exported, so nothing user-facing
  changes.

## Internal

* `build_recipe()` returns a recipe whose step selector quosures point at a
  minimal environment rather than the calling frame. Code reaching into
  `rec$steps[[i]]$terms` environments will see this; the prepped and baked
  output is unchanged.

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

* All warnings from `back_transform_predictions()` now signal via
  `cli::cli_warn()` rather than base `warning()` (callers matching on the
  `simpleWarning` condition class should match on `warning` instead); message
  text is unchanged.

* `ensemble()` — combine the models from `fit()` into a stacked predictor via a
  meta-learner over the members' out-of-fold predictions. Three engines:
  `penalized` (glmnet, the default), `weighted` (inverse-RMSE average), and
  `xgb` (xgboost). Ensemble performance is reported on the held-out test set and
  is directly comparable to the single-model metrics from `fit()`, including an
  honest improvement-over-best-member comparison.

## Documentation

* `?configure` now documents that `cubist` is not bit-reproducible under a
  fixed seed when `committees > 1` (#51). This is inside the Cubist C
  implementation: it persists with an explicit `cubistControl(seed = )`,
  and `committees = 1` is stable. The other engines are deterministic given
  `seed`. Equality tests in the package use `rf` for that reason.

## Bug Fixes

* `fit()` now honours `configure(final_bayesian_iter = )` (#46). It passed
  the screening budget `bayesian_iter` to the final re-tune instead, so the
  user-facing knob did nothing; `configure(bayesian_iter = 0,
  final_bayesian_iter = 25)` re-tuned with zero iterations. Objects
  configured before the field existed fall back to the package default.

* `evaluate()` and `fit()` now rank configurations on the cross-validated
  metric, not the test-set metric (#50). `evaluation$results` gains six
  columns, `cv_rmse`, `cv_rrmse`, `cv_rsq`, `cv_ccc`, `cv_rpd` and `cv_mae`:
  the CV means at each config's selected hyperparameters, on the original
  scale. `best_config` and `fit()`'s member set are chosen on
  `cv_<rank_metric>`; `metric` / `rank_metric` keep the bare name. The
  test-set columns are still reported for every config, and are honest
  held-out estimates precisely because selection no longer touches them.
  Previously the winner's reported test metric was a maximum over all
  configs on the same rows. `best_config` and the members `fit()` picks can
  change for an existing object re-run through `evaluate()`; objects
  evaluated before this version have no `cv_*` columns and `fit()` asks for
  a re-run. `monitor_evaluate()`'s "best so far" reads the same column.

* `fit()`'s train/test partition (Split F) is now seeded with
  `fit_split_seed(seed)` (`seed + 1L`) rather than `seed` (#50). It was built
  with the same `initial_split()` call as `evaluate()`'s on the same frame,
  so at the shared default seed the two partitions were bit-identical and
  `fit()`'s test metrics, and its degradation check, were measured on the rows
  the configs had been selected on. `fit()` now warns if the two partitions
  coincide anyway. Every `fit()` result changes test rows as a consequence,
  by design.

* Tuning metrics are now scored on the original response scale (#49). The
  response transform is a `skip = TRUE` recipe step, so tune never applied it
  to an assessment set: every CV metric for a `log`, `log10` or `sqrt` config
  compared original-scale truth to transformed-scale predictions. The number
  `select_best()` and `tune_bayes()` optimised was dominated by the scale
  offset, and the prune gate discarded healthy transformed models (the second
  half of #38). `evaluate_single_config()` and `fit_single_config()` now build
  their tuning metrics with `tuning_metric_set()`, which back-transforms the
  estimate inside each metric while preserving metric names and directions.
  Hyperparameter selection and pruning for transformed configs change as a
  result; `transformation = "none"` results are bit-identical.

* Back-transformed predictions are now floored at zero on every path, not
  only in `predict()` (#53). `back_transform_predictions()` applies the floor
  for every transformation, including `"none"`, and the evaluation, OOF and
  UQ-calibration paths call it unconditionally, so leaderboard metrics are
  scored on the same predictions a deployed model serves. Previously only the
  `sqrt` branch clamped, so a `log`/`log10`/`none` model that extrapolated
  below zero was scored on values it would never emit, and the ensemble's
  meta-learner trained on unfloored member OOF predictions while receiving
  floored member predictions at serve time. Metrics move only for configs
  that produced negative original-scale predictions. The deploy-time
  `upper_bound` guardrail is unchanged and still opt-in.

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
