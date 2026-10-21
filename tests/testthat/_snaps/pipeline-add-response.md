# add_response() detects case mismatch pattern

    Code
      add_response(hd, lab, variable = "SOC")
    Condition <horizons_error_input>
      Error in `add_response()`:
      ! No sample in the horizons data matches the source.
      x 0 of 3 samples matched on sample_id.
      i Horizons IDs: "S001", "S002", "S003".
      i Source IDs: "s001", "s002", "s003".
      i This looks like a case mismatch: try `mutate(source, sample_id = tolower(sample_id))`.

