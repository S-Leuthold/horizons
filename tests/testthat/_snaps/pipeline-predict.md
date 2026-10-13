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

