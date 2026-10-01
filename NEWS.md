# horizons (development version)

## Bug fixes

* `predict()` no longer drops arguments it does not take without saying so. A `level` warns, with class `horizons_input_warning`, that it is ignored: intervals are at the level they were calibrated at, 0.90 by default. Any other unknown argument, such as a misspelled one, is an error (#141).

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
