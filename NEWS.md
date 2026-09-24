# horizons (development version)

## New features

* **`select_training()`**, the training-set selection verb for library
  mode. Targets and a reference pool in, a `horizons_data` drawn from the
  pool out, on the targets' wavenumber grid, and the rest of the pipeline
  runs on it unchanged. The rule is each target's `k` nearest pool rows
  that have the property measured, drawn per property, rows entering once.
  Four scopes over one return shape (`batch`, `cluster`, `sample`,
  `global`): the return is always the union, and the grouping into training
  sets lives in `x$selection$groups` alongside the full membership table.
  Every lever of the similarity space is an argument (`snv`, `derivative`,
  `window`, `poly`, `mask`, `space = "pca"|"pls"`, `ncomp`, `metric`), with
  the design the 2026-09 experiments ran as the defaults, so the open
  questions (k on a pool, the metric, tail batches) run as loops over the
  verb. The verb reconciles the pool onto the targets' axis itself (#64 is
  why), excludes and reports twins by the gap between first and second
  nearest, and warns about targets beyond the pool's own nearest-neighbour
  spread. Design: `dev/specs/v1-refactor/select-training-design.md`.

* **`subset_rows()` and `set_analysis()`** (internal) give `horizons_data`
  a row-subset operation (#43). `validate()`'s outlier removal and
  `average()`'s collapse now go through them, so the derived counts have
  one source of truth.

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

* **`average(by = <column>)` no longer returns the `by` column.** The
  grouping values now become the averaged rows' `sample_id`, and the
  original column name is kept in `provenance$aggregation_by`. Previously
  the result had no `sample_id` column at all and carried a dangling id
  role, which nothing downstream could use. Code that read the `by` column
  off the averaged object should read `sample_id` instead.

* **`evaluate()` no longer manages a parallel backend.** The caller
  registers a `future::plan()`; `evaluate(allow_par = TRUE)` dispatches
  onto it along the axis chosen by the new `parallelize_over` argument
  (`"auto"`, `"configs"`, `"resamples"`), and if the registered plan offers
  fewer than two workers it warns, naming the plan, and runs sequentially.
  `evaluate()` never registers, alters or restores a plan. `workers` is
  deprecated outright: it warns (condition class
  `horizons_deprecated_workers`) and is ignored. The old design took a core
  count, auto-split it into outer and inner levels, and built a nested
  multisession plan inside `evaluate()`; that construction is what #35 (the
  nested plan tripping parallelly's localhost limit) and #37 (the silent
  no-plan path) lived in, and both close with its removal. `"auto"` picks
  `"configs"` when there are at least `cv_folds` configs and `"resamples"`
  otherwise, from the measured cost model. A nested plan is not built or
  endorsed in this version; register one yourself and use `"configs"`.

* `ensemble()` gains `allow_par = FALSE`. Its meta-learner tuning and
  out-of-fold pass previously took tune's own default of `allow_par = TRUE`
  and dispatched onto any registered plan silently, with no argument to say
  otherwise. `fit(allow_par = TRUE)` and `ensemble(allow_par = TRUE)` now
  check for a usable backend the same way `evaluate()` does. `fit()`
  parallelises CV folds only; a configs axis across members is gated on the
  fitted-object memory contract.

* The evaluation manifest (`eval_manifest.rds`) is schema 2: it records the
  axis used, the requested axis, the registered plan's label and the worker
  count it offered, in place of `workers`/`outer`/`inner`.
  `monitor_evaluate()` reads both schemas. `evaluation$parallelize_over`
  records the axis actually used and is required by the validator;
  `evaluation$workers` is now the count the plan offered (1 when sequential),
  observed rather than requested.

* **tune's inner setting is now `parallel_over = "resamples"`** in
  `evaluate_single_config()`'s grid and Bayesian stages (it was
  `"everything"`), and the cross-validated numbers change as a result. Under
  `"everything"` each grid candidate re-prepped the recipe per fold, so
  `step_select_cars()` and `step_select_boruta()` drew a fresh, unseeded
  feature selection for every candidate and `select_best()` was partly
  choosing the luckiest draw; under `"resamples"` one prep per fold is
  shared by all candidates, so between-candidate differences are
  hyperparameters plus engine noise. `"everything"` also shipped the whole
  rset to every task. Every result row now carries `scoring_schema` (2), and
  resuming an `output_dir` drops checkpoint rows from schema 1 with a
  message so the two regimes are never ranked together.

* **Results no longer depend on which axis ran.** tune's future path
  advances the parent RNG stream past where the sequential loop leaves it,
  so the Bayesian stage and `last_fit()` drew from a different position on
  the resamples axis and could return different hyperparameters and test
  metrics for the same seed. The seed is now re-pinned before every
  stochastic stage in `evaluate_single_config()` and `fit_single_config()`,
  and `fit_uq()`'s quantile forest takes an explicit seed, so `allow_par` is
  a pure performance choice. Ties in the ranking are broken on `config_id`,
  and `monitor_evaluate()` names the best config by the same rule
  `evaluate()` uses (successes only, `cv_<metric>`, same tie-break).

* Objects evaluated before this version have no `evaluation$parallelize_over`
  slot; that is tolerated (they still `fit()`), and the slot is validated
  when present. The manifest is written on every run with an `output_dir`,
  whichever axis, so the monitor can watch any run.

* Thread pinning moved to the parent: before dispatch `evaluate()`,
  `fit()` and `ensemble()` pin BLAS/OpenMP (via `RhpcBLASctl` when
  installed), data.table and ranger threads in the calling process and
  restore them on exit. The worker-side `Sys.setenv()` block was dead code:
  OpenBLAS reads its thread count when it loads. `fit_uq()`'s quantile
  forest is pinned at the call site; it previously ran on every core.
  `future.apply` is declared (tune's future path calls it and never checks
  it); `mirai` is suggested so a live daemon pool can be detected and
  warned about, since tune prefers it silently.

* Parallel `evaluate()` now refuses to run under `devtools::load_all()`.
  Workers resolve the package namespace by name and therefore load the
  *installed* `horizons`, not the source tree — so a stale install would have
  run old code behind a new worker body without saying so. Install the package
  (`R CMD INSTALL`) before using parallelism in development.

* `magrittr` is no longer a dependency; the package uses the base pipe (`|>`)
  throughout. `%>%` was imported but never exported, so nothing user-facing
  changes.

* **`configure(cov_fusion = "late")` now aborts**, whether or not the object has covariates (#69). The condition carries both `horizons_configure_error`, like every other `configure()` argument check, and `horizons_input_error`. Late fusion is designed but was never built: `build_recipe()` only fuses early, so `"late"` was validated, stored and printed, and then ran early fusion bit for bit. Use `"early"`. `cov_fusion` must also be `NULL` or a single string; a vector was not rejected cleanly before.

* **`configure()` now warns about rows it keeps but did not choose** (#70). Re-configuring keeps the rows `validate()` removed and any `select_training()` record, because both describe rows rather than an outcome. It warns, naming the earlier outcome and the count, when rows were removed only as response outliers (`reason == "response"`) of a different outcome than the one being configured; a row removed as both a spectral and a response outlier would have gone anyway and is not counted. It also warns when a `select_training()` record (other than `scope = "global"`) was drawn for properties that do not include the outcome, since each target's `k` nearest rows were not drawn for it.

## Review fixes (2026-09-21)

A six-reviewer pass over `select_training()` and the code it touches, with
the findings fixed the same day. The entries below are the user-visible
consequences; the review itself is in
`dev/reviews/2026-09-21-horizons-review.md`.

### The selection verb

* **The self-leakage rule is now neighbourhood-relative.** Every pool row
  closer to a target than `twin_ratio` times a reference distance for that
  target is flagged, not just the nearest one. The reference is the 75th
  percentile of a fixed-width reference set — the target's 50 nearest pool
  rows, capped at a quarter of the measured rows on a small pool — so the
  threshold does not move when `k` moves, and a `k` sweep varies one thing
  at a time. The old rule compared the nearest distance to the
  second-nearest, which cannot fire when a pool holds replicate scans of a
  target, both distances being tiny and their ratio about one; the case the
  check was written for was the case it missed.

* **`twin_ratio` means something different, and is still uncalibrated.** It
  is unchanged at 0.05, but 0.05 is now a fraction of that reference
  distance rather than of the second-nearest distance, so the old and new
  values are not comparable. The constant has always been a placeholder
  pending a calibration on the KSSL replicate scans; that calibration is
  more pressing under the new rule, not less.

* **Flagged twins are subtracted from the returned training set** under
  every scope but `"global"`, `"sample"` included, with the count in
  `x$selection$n_excluded_union`. The exclusion was previously
  per-neighbourhood only, so a twin dropped from its own target's
  neighbourhood walked back in through any other target that drew it — the
  normal case at `k = 400` over a coherent batch, not a corner one. Under
  `"global"` the check runs and is reported but the rows stay, since global
  returns the pool. `x$selection$membership` keeps every flagged row with
  `retained = FALSE`, so the neighbourhoods stay inspectable after the
  subtraction.

* **`nearest_neighbours()` returns the `k` in recomputed-distance order.**
  The bulk screen uses the squared-norm identity, which loses precision
  exactly where this code makes its decisions, so the retained neighbours
  are now re-differenced directly. Rank and nearest distance are exact in
  the near-duplicate regime, which is the regime the twin rule reads.

* **`select_training()` gains `sdev_floor`, default 0.1.** Components whose
  standard deviation falls below that fraction of PC1's leave the distance.
  Retaining components on cumulative variance alone and then whitening by
  their own standard deviation gave the noise tail the same weight as the
  dominant chemical axes, which made neighbour rankings unstable to small
  changes in the pool. `sdev_floor = 0` restores the old behaviour.

* **The raw absorbance scale of pool and targets is compared before SNV**,
  and a gross mismatch warns. The comparison is on the interquartile
  ranges, with the threshold at twofold: a natural-log against base-ten
  convention differs by 2.303 and is caught, while an ordinary instrument
  gain difference of about 1.5 stays silent. A pure additive offset is
  deliberately not flagged, since baseline offset is what the pipeline's
  own preprocessing exists to remove. SNV removes per-spectrum offset and
  scale, so the resemblance check, which measures after it, was
  structurally blind to a unit mismatch.

* **`select_training()` refuses a pool that has been validated, evaluated
  or fitted, or that is itself a prior selection.** The return is built by
  subsetting the pool, so a promoted pool used to come back out still
  claiming its class, carrying an evaluation split and a row index keyed to
  positions the subset had just destroyed.

* **The resemblance check is seeded from `seed`** and restores the caller's
  RNG stream. It previously sampled the pool unseeded, so the "targets
  beyond the pool's spread" warning was not reproducible on any real pool
  and the call silently advanced the caller's stream.

* Several additions to the record. `settings$window_cm` gives the
  Savitzky-Golay window's width in cm⁻¹ on the reconciled grid, since
  `window` is in points and the grid is the user's. The distance tables
  carry a `space` column naming the space each distance came from. Draws
  that cannot reach `k` are recorded in `x$selection$short_draws`, with a
  `reason` column, and warned, rather than silently returning a short
  neighbourhood. A target with no defined distance at all — `cosine` on a
  target sitting at the pool centroid gives `NaN` — is a failed draw,
  recorded and warned, where it previously fell through `order()` and drew
  pool rows 1 to `k` in storage order. Under `"global"` scope `mean_k` is a
  real mean over `k` neighbours rather than a placeholder.
  `.min_distance` is `NA` under `space_rows = "measured"` with several
  properties, where the per-property distances have no common scale, and
  `.group` is `NA` in that case under `scope = "cluster"` only. A PLS space
  records `settings$space_note`: the selection was made on the pool's own
  responses, so the pool CV is optimistic.

### The similarity space and reconciliation

* **`reconcile_axes()` refuses an axis with a gap**, on either side. A gap
  is spacing that is both over three times the grid's median *and* over
  30 cm⁻¹, so an axis that is merely non-uniform — nm-sampled NIR
  converted to wavenumbers, or two instrument resolutions merged — passes,
  while a deleted band does not. A pool standardized
  with `remove_water = TRUE` used to pass the endpoint-only coverage check
  and then be spline-filled across the deleted bands and differentiated
  across them, which put invented absorbance and a large spurious feature
  into the returned predictors. Water bands belong in `mask`, which is
  applied after the derivative.

* **Coverage is tolerant to half the pool's median spacing.** A target grid
  overshooting the pool by up to that much is clamped with a warning and
  recorded in `record$clamp`; beyond it the verb aborts and states the
  overshoot. #64 makes a fractional overshoot the expected case rather
  than a corner one, and the old exact test made it a hard stop.

* The Savitzky-Golay arguments are validated at the call site — `window`
  odd, `poly < window`, `poly >= derivative` — rather than relying on
  `prospectr` to raise. Two separate guards replace the old blanket one: a
  `mask` that removes over 90 % of the columns aborts as a mask error, and
  the column floor below which a space cannot be built is now
  `max(2, ncomp)` rather than twice the component cap, so a legitimate
  24 cm⁻¹ pool builds a space instead of being refused.

* The space records `ncomp`, `ncomp_variance`, `sdev_ratio` and
  `variance_retained`, so a run's component stability can be audited after
  the fact. `sdev_ratio` is measured before the floor, over the set the
  variance rule retained, so the record shows what `sdev_floor` cut rather
  than only what survived it.

### Recipes and prediction

* **`build_recipe()` gives every non-spectral column an explicit role.** A
  lab-measured response that `configure()` did not promote used to fall
  through to the default predictor role, so a second measured property
  entered the model matrix and inflated the cross-validated metrics.
  Non-outcome responses now get a `response_hold` role.

* **Meta, held-covariate and held-response columns are no longer required
  at bake time**, so `predict()` accepts new data carrying only an
  identifier and spectra. Unused covariates are held by role rather than
  removed with `step_rm()`.

* **The feature-selection steps select spectral columns by name.** PCA,
  correlation filtering, Boruta and CARS previously resolved
  `all_predictors()` at prep time, after `update_role()` had promoted a
  covariate, so a covariate could be folded into the spectral rotation or
  silently dropped by a spectral selection rule.

* **`predict()` keeps the covariates a fitted config actually uses** and
  names a missing one clearly rather than failing inside `shrink()`. The
  presence check also runs on objects fitted before the predictor schema
  existed. Ensemble prediction follows the same rule, and a malformed
  ensemble now reports which members are missing rather than emitting a
  covariate message that has nothing to do with the failure.

* **`predict()` warns once when conformal intervals are requested on a fit
  built from a selected training set**, and only when a UQ bundle exists.
  Coverage assumes exchangeability between the calibration rows and the
  prediction rows, and selection deliberately breaks it. `fit()` records
  `models$selection_present` so the warning has something to check, and the
  single-model and ensemble paths share one helper so the two cannot drift.

* **Applicability-domain distances are computed row by row.** A row that
  bakes to `NA` gets `NA` in `.ad_distance` and `.ad_flag` while every other
  row is scored normally, and abstention still applies to the scored rows;
  previously one bad row could take the whole bundle down to `NULL`. A bake
  that aborts outright now warns, since that is a bug or a schema mismatch
  rather than a degradation, and `abstain_ood = TRUE` with no applicability
  information available warns instead of quietly returning unabstained
  predictions.

* **The recipe steps fail at `prep()` rather than producing an empty
  matrix.** `step_select_correlation()`, `step_select_cars()` and
  `step_select_boruta()` abort when the selector resolves to zero columns,
  and the PCA branch does the same through a selector-level check.
  `step_transform_spectra()` aborts naming the column when a generated name
  (`spec01` and its siblings) collides with a pass-through column, and when
  `window_size` would trim every spectral column away.

* **The `mtry` upper bound comes from the prepped recipe's own predictor
  roles.** It was taken from the analysis table's column count, so held
  responses and unused covariates inflated the range a tuning grid searched
  over.

### `evaluate()` checkpoints

* **`evaluate()` fingerprints the training rows** — a hash of the sorted
  sample ids, the row count and the outcome column — into every checkpoint,
  every per-config file and `eval_manifest.rds`, now schema 3, and
  **refuses to resume on a mismatch**. The outcome is in the hash because
  config ids do not encode it, so two properties evaluated on the same rows
  would otherwise resume each other's checkpoints. Resuming an `output_dir` against a different table used to
  mix results from two datasets silently. Checkpoints written before this
  version carry no fingerprint; they warn once and resume.
  `monitor_evaluate()` shows the fingerprint.

### The object contract

* **`$selection` is a ninth documented section** of `horizons_data`, `NULL`
  unless the object came from `select_training()`, and shape-checked rather
  than dereferenced on faith by `print()`.

* **`validate_horizons_data()` enforces four more invariants:** wavelength
  order (the existing check never fired, because it matched bare numeric
  names and every predictor carries the `wn_` prefix), the closed role
  vocabulary, at most one `outcome` column, and agreement between the
  stored counts and the data.

* **Breaking: the row operations refuse a promoted object.**
  `subset_rows()` and `set_analysis()` replace the analysis table and
  recompute counts while leaving splits, row indices and evaluation state
  untouched, which on a promoted object leaves indices pointing at rows
  that have moved. They now abort instead. The guard tests the slots the
  verbs actually write — `evaluation$split`, `models$split`,
  `models$row_index`, the fitted workflows, the results table and
  `ensemble` — and refuses a promoted class vector on its own, so an object
  carrying the class but no state is caught too.

* **`configure()` recounts the stored role counts** through
  `set_analysis()`. They were assigned by hand and went stale on every
  configured object, so every one of them violated the count invariant the
  validator now enforces, and `configure() |> standardize()` aborted.
  Reconfiguring an object also clears what the old configuration produced:
  `evaluation$split`, `models$split`, `models$row_index`,
  `models$cv_predictions`, `models$predictor_schema` and `ensemble`, with
  the class reset to `horizons_data`.

* **The selection record is recomputed wherever rows leave**, in
  `set_analysis()` rather than only in `subset_rows()`, so no path can drop
  rows and leave the record describing the old set.
  `validate(remove_outliers = TRUE)` refilters membership and groups,
  recounts the draws, and records `selection$rows_removed`, which
  `summary()` shows. `print()` and `summary()` report twins as "N flagged,
  M removed", since after the union subtraction those are different
  numbers. `average()` refuses a custom `by` on a selected object, because
  collapsing groups of pool rows would make the record unrecoverable.

* **Validation reaches into the selection record**: each table is checked
  for its required columns, and invariant I4b — every retained row's
  `pool_id` is present in the data — is enforced rather than assumed.

### `average()`

* **`average()` carries response and outcome columns through the
  collapse** as within-group means; a conflicting non-numeric column is an
  error rather than a silent drop. It previously dropped them, which since
  the `set_analysis()` refactor aborted with a message blaming the role
  map.

* A promoted `by` column is coerced to a character `sample_id` and asserted
  unique and non-missing, so a numeric or factor grouping column can no
  longer produce an identifier that fails downstream joins.
  `provenance$average$source_ids` keeps the mapping back to the scans each
  averaged row came from.

## Known limitations

* **No pool-internal de-duplication, and the cross-validation downstream of
  a selection is ungrouped.** Replicate scans of one library sample are each
  other's nearest neighbours, so a target that draws one draws all of them,
  and the selected training set is enriched in sibling pairs relative to the
  pool it came from. Plain `vfold_cv()` then puts siblings on both sides of
  a fold, which optimistically biases the pool-internal estimate, and the
  effect scales inversely with `k`, so it also biases a `k` sweep. The twin
  rule addresses target self-leakage, which is a different problem, and does
  not touch this. Tracked as
  [#66](https://github.com/S-Leuthold/horizons/issues/66).

## Internal

* The `horizons_eval`, `horizons_fit` and `horizons_ensemble` validators take their required keys from `new_horizons_data()`, less a commented list of keys older objects may lack (`workers`, `parallelize_over`, `response_bound`, `selection_present`), instead of keeping lists of their own. `response_bound` may now be absent as well as `NULL`, which is what the validator's documentation already said.

* `build_recipe()` returns a recipe whose step selector quosures point at a
  minimal environment rather than the calling frame. Code reaching into
  `rec$steps[[i]]$terms` environments will see this; the prepped and baked
  output is unchanged.

* `resample_spectra()` accepts an explicit `new_wav` grid and refuses to
  extrapolate, so the package has one resampling routine for both
  `standardize()` and `select_training()`.

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

* **Re-running `configure()` on an object that has been through `ensemble()` no longer aborts** (#70). Reconfiguring cleared a hand-kept list of keys that missed `ensemble$model`, and `set_analysis()` counts a non-NULL `ensemble$model` as promotion, so it refused the object. The same list let `models$uq`, `models$ad`, `models$results` and most of `evaluation` survive the demotion, which left `has_uq()` answering `TRUE` on a plain `horizons_data` whenever the fit had calibrated UQ. Reconfiguring now resets the `evaluation`, `models` and `ensemble` slots whole to the constructor's shape, through an internal `reset_promotion()` that sits beside the promotion check so the two stay in step. The list also erased the record of outliers `validate(remove_outliers = TRUE)` had removed, although those rows stayed removed; that record is now kept, and only the verdict and the flagged ids are cleared. A `select_training()` record is kept, as before.

* **`spectra()` now builds its object with the class constructor, `new_horizons_data()`** (#71), rather than a second constructor of its own that had drifted from the contract. A raw object had no `data$n_responses` or `selection` key, carried `models` and `ensemble` stubs that predated the current contract, and its `models$uq` stub was a non-empty list, so `has_uq()` answered `TRUE` before anything was fitted. The constructor itself now declares the `models` keys `fit()` writes (`ad` and `selection_present` were missing) and the `evaluation` keys `evaluate()` writes (it declared `backend` and `runtime`, which nothing writes). `summary()` read `evaluation$runtime`, so its evaluation runtime line never printed; it reads `runtime_secs` now. The tuning defaults on a raw object are integers (`10L`, `15L`, `5L`) rather than doubles. An object saved by an earlier version keeps its old shape until its downstream slots are rewritten: by `configure()` when it runs on an object that already has a grid (not by the first `configure()`), or by `evaluate()` and `fit()`, which now reset the slots after their own.

* **`validate()` no longer erases the record of rows an earlier call removed** (#70). Each call replaced `removed_ids`, `removal_detail` and `removed` with its own, so `validate(remove_outliers = TRUE) |> configure(outcome = ...) |> validate()` reported nothing removed (and dropped the `removal_detail` key) while the rows stayed gone. The verdict and the flagged ids are still replaced; the removal record now accumulates. `removal_detail` gains three columns: `outcome`, the outcome whose fences flagged a response removal (`NA` otherwise), and the `spectral_threshold` and `response_threshold` in force. Its `reason` now counts only the detectors the call removed by, so `remove_outliers = "spectral"` labels a row `"spectral"` even when it also sits outside the response fences (it was `"both"`), and likewise for `"response"`.

* **`evaluate()` on a fitted or ensembled object, and `fit()` on an ensembled one, now empty the slots downstream of their own** (#70). Both demoted the class but left the later slots filled, so re-evaluating a `horizons_fit` returned a `horizons_eval` whose `has_uq()` was still `TRUE`, and re-fitting a `horizons_ensemble` kept a meta-learner built on the members it replaced. They now reset `models` and `ensemble` (`evaluate()`) or `ensemble` (`fit()`) to the constructor's shape before writing their own.

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
