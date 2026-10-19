# every predict_ad warning names the config when it is given

    Code
      one_na <- predict_ad(fx$workflow, bundle, one_bad, config_id = "cfg_ad")
    Condition
      Warning:
      step_transform_spectra: 1 of 5 spectra produced no usable output and were returned as NA (rows: 3). No error was raised, so the input was likely non-finite. Downstream predictions for these rows are NA.
      Warning:
      ! Applicability domain for config "cfg_ad" is NA for 1 of 5 samples whose spectra baked to NA.
      i The remaining samples are scored normally; the NA rows are neither binned nor abstained on.
    Code
      all_na <- predict_ad(fx$workflow, bundle, all_bad, config_id = "cfg_ad")
    Condition
      Warning:
      step_transform_spectra: 5 of 5 spectra produced no usable output and were returned as NA (rows: 1, 2, 3, 4, 5). No error was raised, so the input was likely non-finite. Downstream predictions for these rows are NA.
      Warning:
      ! Applicability domain for config "cfg_ad" is unavailable for all 5 samples: every spectrum baked to NA.
      i See the `step_transform_spectra()` warning above for the cause.

