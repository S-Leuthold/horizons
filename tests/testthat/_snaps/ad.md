# every predict_ad warning names the config when it is given

    Code
      bake <- predict_ad(list(), bundle, new_df, config_id = "cfg_ad")
    Condition
      Warning:
      ! Applicability domain for config "cfg_ad" could not be computed: baking `new_data` through the fitted recipe failed.
      x no applicable method for 'extract_recipe' applied to an object of class "list"
      i This is a bug or a schema mismatch between `new_data` and the training axis, not a property of one sample.
      i No AD columns are returned, so out-of-domain abstention cannot be applied to this batch.
    Code
      one_na <- predict_ad(fx$workflow, bundle, one_bad, config_id = "cfg_ad")
    Condition
      Warning:
      ! Applicability domain for config "cfg_ad" is NA for 1 of 5 samples whose spectra baked to NA.
      i The remaining samples are scored normally; the NA rows are neither binned nor abstained on.
    Code
      all_na <- predict_ad(fx$workflow, bundle, all_bad, config_id = "cfg_ad")
    Condition
      Warning:
      ! Applicability domain for config "cfg_ad" is unavailable for all 5 samples: every spectrum baked to NA.
      i See the `step_transform_spectra()` warning above for the cause.
    Code
      distance <- predict_ad(fx$workflow, bad_bundle, new_df, config_id = "cfg_ad")
    Condition
      Warning:
      ! Applicability domain for config "cfg_ad" could not be computed: the distance to the training centroid failed.
      x Number of features must match: new has 4, training has 3
      i Point predictions are returned without .ad_distance and .ad_flag, so out-of-domain abstention cannot be applied to this batch.

