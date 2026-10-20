# spectra() prints its banner, with the sorting line only when it sorts

    Code
      invisible(spectra(test_data, verbose = TRUE))
    Output
      ── horizons pipeline ──────────────────────────────────────────────
      ├─ Loading spectra...
      │  └─ 3 samples × 3 predictors
      │
    Code
      invisible(spectra(test_data[, c("Sample_ID", "2000", "4000", "3000")], verbose = TRUE))
    Output
      ── horizons pipeline ──────────────────────────────────────────────
      ├─ Loading spectra...
      │  ├─ 3 samples × 3 predictors
      │  └─ Sorting: wavenumber columns put in decreasing order
      │

# spectra() aborts with every file's failure when none can be read

    Code
      spectra(files)
    Condition
      Error in `spectra()`:
      ! None of the 2 OPUS files in `source` could be read.
      x File read failed: corrupt header (a.0)
      x Block extraction failed: The file holds no absorbance block ("ab" or "ab_no_atm_comp"). Its blocks: "sc_sample". (b.0)
      i The failures are on this condition as results: `rlang::last_error()$results`.

