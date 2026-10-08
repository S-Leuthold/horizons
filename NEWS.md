# horizons (development version)

## Changes

* The covariate API is removed (#134). No verb ever gave a column the `covariate` role, so it could be reached only by editing the role map by hand. `configure()` loses `expand_covariates` and `cov_fusion`, `config$configs` its `covariates` column and `config$expansion` its covariate entries, and `data$n_covariates` goes; `build_recipe()`, `predict()`, `print()`, `summary()` and `evaluate()`'s checkpoint fingerprint no longer handle covariates. Config ids change, so a checkpoint directory written by 0.10.0 is evaluated again rather than resumed. A `covariate` role, and an object whose `config$configs` still has a `covariates` column, are refused with class `horizons_validation_error`: give the column the `meta` role, or re-run `configure()` and the verbs after it.
* Checkpoint stores, run manifests and records written before 0.10.0 are evaluated again, not resumed (#201). `evaluate()` no longer reads the single-file `eval_checkpoint.rds`; a checkpoint row that does not record the training-data fingerprint and every setting the run records is re-evaluated instead of resumed with a warning; `monitor_evaluate()` refuses a manifest written by another version; and `validate()`'s `removal_detail` drops its always-`NA` `outcome` and `response_threshold` columns.
* `validate_horizons_fit()` checks the type of every column `fit()` writes to `models$results`, where the column is present, and names each column of the wrong type ("degraded is integer, not logical") (#203).
* `parse_ids()` refuses an evaluated, fitted or ensembled object. It rewrites `sample_id`, and the stored splits and models are keyed to it (#135).
* Each class is checked against everything it promises (#129). An evaluated object is checked against the base data contract as well as its evaluation, and an ensemble against its fitted models, evaluation and data. `ensemble()` checks the whole fitted object before it fits anything, as `evaluate()` and `fit()` already did. Two rules are new: the column with the `id` role must be `sample_id`, and `evaluation$results` must have exactly one row per configuration in `config$configs`. Every validation failure now aborts with class `horizons_validation_error`, including the checks that run first, which used to abort without it.
* `select_training()` validates the targets and the library before drawing. Targets need one row per sample with finite spectra, so replicate scans are averaged first. A library that has been through `configure()` or `validate()` is refused, and the selection record now identifies the targets in `x$selection$targets`: their source, row count and an id hash (#133).
* `evaluate()` and `fit()`, including the cold start, abort with class `horizons_input_error` when the outcome has zero variance, on the modelled rows or on the rows the models are fitted on; `validate()` is documented as an advisory report, and `summary()` suggests it only before `evaluate()` (#132).
* The object no longer carries the unused `artifacts` slot, `models$row_index` or `provenance$schema_version`, and `models$cv_predictions` gains `sample_id`. The validators no longer accept objects missing keys that current versions always write, so objects saved by earlier versions need a re-run.
* Attaching a development build says so and names the command that installs the latest release (`remotes::install_github("S-Leuthold/horizons@main")`); development builds carry a `.9000` version suffix.
* `needs_back_transformation()` no longer treats `"notrans"`, `"na"` or `""` as no transformation; `"none"` is the only such value, and every other value, `""` and `NA` included, returns `TRUE`. `back_transform_predictions()` treats `""` and `NA` as unknown transformations, as it already did `"notrans"` and `"na"`: the predictions are not back-transformed (they are still clamped to `outcome_range`), with a warning when `warn = TRUE`.

## Bug fixes

* `evaluate()` and `fit()` record when a Bayesian search fails or returns without running an iteration past the grid, and the grid's choice is kept, instead of reporting an ordinary success. `configure()` refuses `grid_size = 1` when `bayesian_iter` or `final_bayesian_iter` is above 0, where the search could never start (#209).
* `evaluate()` no longer refuses to resume a checkpoint written on the same data under another locale. The check compared a hash of the sample ids sorted in the session's collation; it now compares the recorded data fields (#213).
* `summary()` no longer closes the Data tree twice for an object with no outcome (#205).
* `step_select_correlation()` errors at `bake()` when new data lacks a wavenumber it selected, as `step_select_cars()` and `step_select_boruta()` do. It returned the data without its selected columns (#200).
* `print()` and `summary()` of a fitted object report the member `fit()` selected on cross-validation (`models$best_config`) and its test metrics, as `fit()`'s console does. They reported the member with the lowest test RMSE, a best-of-N on the held-out rows that could name a different configuration (#136).
* `add_response()` refuses an `NA` join key on either side and reports how many there are. Before, an `NA` key on one side was matched to an `NA` key on the other (#139).
* `predict()` no longer drops arguments it does not take without saying so. A `level` warns, with class `horizons_input_warning`, that it is ignored: intervals are at the level they were calibrated at, 0.90 by default. Any other unknown argument, such as a misspelled one, is an error (#141).
* `fit()` refuses an evaluated object whose response-trim request changed after `evaluate()`, instead of silently fitting with the trim `evaluate()` applied. `validate()` can record a new request on an evaluated object, but `fit()` reuses the rows `evaluate()` trimmed, so the new request never took effect. The error says what differs; re-run `evaluate()` to apply the current request (#137).
* `predict()`, `ensemble()` and `select_training()` now check that values attached to samples by position line up with their rows, and stop with a `horizons_internal_error` instead of mislabelling samples if a step upstream dropped or reordered rows. In `fit()` the same check keeps misaligned uncertainty scores from being stored; the member is then fitted without intervals (#140).

# horizons 0.10.0

First tagged release. It brings `main` up to date with the development branch, where horizons was rebuilt around an S3 approach: a single `horizons_data` object that each verb in the pipeline takes and returns, carrying the spectra, configuration, results and a record of how each step was run, from raw spectra to predictions. It is still pre-1.0, so the API may change between minor versions.

Earlier version numbers (0.8 and 0.9) were untagged development snapshots of a previous API, which this release replaces.

## Highlights

* `spectra()` reads Bruker OPUS files or a table of spectra. `parse_ids()` extracts sample identifiers from OPUS file names, and `average()` averages replicate scans with a correlation check that flags outlying scans.
* `standardize()` resamples spectra onto a common wavenumber grid anchored at fixed points, so spectra from different instruments or sources line up column for column. It can also trim the range, remove water bands and correct the baseline.
* `add_response()` joins laboratory measurements, and `configure()` builds the factorial grid of models, spectral preprocessing, feature selection and response transformations to compare. `configure(outcome_range =)` declares the physical range of the outcome, which every prediction respects.
* `validate()` checks the data and the grid before any modeling runs.
* `evaluate()` tunes and cross-validates every configuration. It checkpoints each configuration as it finishes, so an interrupted run resumes, and it runs in parallel on whatever `future` backend you register.
* `fit()` retunes the best configurations and fits the final models, with conformal prediction intervals (conformalized quantile regression) and an applicability-domain check that flags samples unlike the training data. It reports interval coverage measured on held-out test rows.
* `ensemble()` stacks fitted models with a penalized, weighted or XGBoost meta-learner, with CV+ prediction intervals.
* `predict()` returns point predictions, prediction intervals and applicability flags for new samples, from a single model or an ensemble.
* `select_training()` draws a training set for a batch of samples from a reference spectral library. The first registered library is the USDA NRCS Kellogg Soil Survey Laboratory MIR library from the Open Soil Spectral Library, built on your machine from the public files on first use.
* The recipe steps `step_transform_spectra()`, `step_select_boruta()`, `step_select_cars()` and `step_select_correlation()`, and the metrics `rpd()`, `rrmse()` and `ccc()`, are exported for use in your own tidymodels workflows.

## Breaking changes since 0.9.0

* The previous API (`create_project_data()`, `evaluate_models_local()`, `build_ensemble_stack()` and related functions) is removed, along with OSSL covariate prediction.
* `evaluate()` no longer creates its own parallel backend. Register one with `future::plan()` and use `allow_par` and `parallelize_over`; the `workers` argument is gone.
* Outcomes outside `outcome_range` are refused before tuning. The default range is non-negative, `c(0, Inf)` (#76).
* `validate()` no longer removes response outliers. `evaluate()` instead trims them from its training partition only, and the test rows are left as they are (#77).
* `fit()` scores on `evaluate()`'s train/test split rather than drawing its own.
* `step_select_boruta()` fails when Boruta fails, rather than falling back to a correlation filter (#75).

## Bug fixes

* The interval coverage `fit()` reported was an in-sample diagnostic; it is now measured on held-out test rows (#118).
* The DEGRADED flag in `fit()` fired on healthy fits. It now bootstraps the test RPD and flags a configuration only when the whole interval lies below the cross-validated RPD (#119).
* `ensemble()` failed on a fitted object read into a fresh R session.
* Failed and warned configurations now record their cause (#96).
* `average()`'s quality-control report no longer counts a sample whose every scan failed as clean (#89).
* `standardize(baseline = TRUE)` works on a single sample (#78).
* Tuning metrics are scored on the original response scale (#49).
