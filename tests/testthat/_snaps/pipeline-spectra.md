# spectra() prints its banner, with the sorting line only when it sorts

    Code
      invisible(spectra(test_data))
    Output
      ── horizons pipeline ──────────────────────────────────────────────
      ├─ Loading spectra...
      │  └─ 3 samples × 3 predictors
      │
    Code
      invisible(spectra(test_data[, c("Sample_ID", "2000", "4000", "3000")]))
    Output
      ── horizons pipeline ──────────────────────────────────────────────
      ├─ Loading spectra...
      │  ├─ 3 samples × 3 predictors
      │  └─ Sorting: wavenumber columns put in decreasing order
      │

