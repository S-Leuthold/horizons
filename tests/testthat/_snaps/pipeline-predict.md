# predict.horizons_fit() - input validation / refuses new_data that is neither a horizons_data nor a data frame, as predict()

    Code
      predict(fitted_fixture, list(a = 1))
    Condition
      Error in `predict()`:
      ! `new_data` must be a <horizons_data> object or a data frame, not a list.

# predict.horizons_fit() - input validation / refuses a horizons_data new_data with no predictor columns

    Code
      predict(fitted_fixture, hd)
    Condition
      Error in `predict()`:
      ! `new_data` has no predictor columns in its role map.
      i Give the spectral columns the "predictor" role, as `spectra()` does.

# predict.horizons_fit() - applicability domain / refuses abstention under config = 'all' when any configuration has no AD, naming it and the ones that do

    Code
      predict(two, new_df, config = "all", interval = FALSE, abstain_ood = TRUE)
    Condition
      Error in `predict()`:
      ! `abstain_ood` needs an applicability domain for every configuration predicted.
      x 1 configuration has none: "cfg_002".
      i Predict the ones that have one ("cfg_001") with `config`, refit with `fit(compute_ad = TRUE)`, or drop `abstain_ood`.

# predict.horizons_fit() - each configuration is one unit / aborts with the failure in results when the configuration asked for cannot predict

    Code
      predict(fitted_fixture, new_df)
    Condition
      Error in `predict()`:
      ! Nothing could be predicted: the configuration asked for failed.
      x Prediction failed: the engine broke (cfg_001)
      i The failures are on this condition as results: `rlang::last_error()$results`.

