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
      predict(broken, new_df)
    Condition
      Error in `predict()`:
      ! Nothing could be predicted: the configuration asked for failed.
      x Prediction failed: no applicable method for 'predict' applied to an object of class "character" (cfg_001)
      i The failures are on this condition as results: `rlang::last_error()$results`.

# warn_interval_failure() - its reason / words each kind of reason: none, an error, a plain string, with and without a config

    Code
      warn_interval_failure("baking new data through the UQ recipe", NULL)
    Condition
      Warning:
      ! Prediction intervals could not be computed (baking new data through the UQ recipe); returning point predictions only.
      x unknown error
    Code
      warn_interval_failure("baking new data through the UQ recipe", simpleError(
        "the bake broke"))
    Condition
      Warning:
      ! Prediction intervals could not be computed (baking new data through the UQ recipe); returning point predictions only.
      x the bake broke
    Code
      warn_interval_failure("predicting quantiles from the UQ model",
        "a plain reason")
    Condition
      Warning:
      ! Prediction intervals could not be computed (predicting quantiles from the UQ model); returning point predictions only.
      x a plain reason
    Code
      warn_interval_failure("predicting quantiles from the UQ model",
        "a plain reason", config_id = "cfg_001")
    Condition
      Warning:
      ! Prediction intervals could not be computed for config "cfg_001" (predicting quantiles from the UQ model); returning point predictions only.
      x a plain reason

