# parse_ids() records and reports the unmatched count

    Code
      result <- parse_ids(hd, format = "{first}_{second}", verbose = TRUE)
    Output
      ├─ Parsing sample IDs...
      │  └─ 2 IDs extracted (1 unmatched)
      │
    Condition
      Warning:
      Pattern did not match 1 filename.
      i Its sample_id is unchanged, and the parsed columns are `NA` for it.
      i Non-matching: "no-match".
    Code
      invisible(parse_ids(hd, format = "{first}_{second}", too_few = "na", verbose = TRUE))
    Output
      ├─ Parsing sample IDs...
      │  ├─ 2 IDs extracted (1 unmatched)
      │  └─ 1 sample id set to `NA` (`too_few = "na"`)
      │

