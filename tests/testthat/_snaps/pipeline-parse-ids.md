# parse_ids() records and reports the unmatched count

    Code
      result <- parse_ids(hd, format = "{first}_{second}", verbose = TRUE)
    Condition
      Warning:
      Pattern did not match 1 filename
      i Keeping original filename as sample_id
      i Non-matching: "no-match"
    Output
      ├─ Parsing sample IDs...
      │  └─ 2 IDs extracted (1 unmatched)
      │

