# average() errors when all samples dropped

    Code
      average(hd, quality_check = TRUE, on_all_outliers = "drop", verbose = FALSE)
    Condition <horizons_error_qc>
      Error in `average()`:
      ! Every sample failed quality control on every replicate, and `on_all_outliers = "drop"` dropped them all.
      i Keep them with `on_all_outliers = "warn"` or `"keep_best"`, or lower `correlation_threshold`.
      i The samples, one row each, are on this condition as results: `rlang::last_error()$results`.

