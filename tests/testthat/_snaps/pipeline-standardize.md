# standardize()'s tree names each operation that ran, and says when none did

    Code
      x <- standardize(kssl, resample = NULL, trim = NULL, verbose = TRUE)
    Output
      ├─ Standardizing...
      │  ├─ Sorting: wavenumber columns put in decreasing order
      │  └─ 3 samples × 1701 predictors
      │
    Code
      x <- standardize(dec, resample = NULL, trim = NULL, verbose = TRUE)
    Output
      ├─ Standardizing...
      │  └─ No operations applied
      │
    Code
      x <- standardize(dec, resample = 4, remove_water = TRUE, baseline = TRUE,
        verbose = TRUE)
    Output
      ├─ Standardizing...
      │  ├─ Trimming: 600-4000 cm⁻¹ (1701 → 1701)
      │  ├─ Resampling: 2 → 4 cm⁻¹ onto 600-4000 (1701 → 851)
      │  ├─ Baseline correction (convex hull)
      │  ├─ Water bands: 187 removed (851 → 664)
      │  └─ 3 samples × 664 predictors
      │
    Code
      x <- standardize(offgrid, resample = 4, trim = c(600, 4000), verbose = TRUE)
    Output
      ├─ Standardizing...
      │  ├─ Sorting: wavenumber columns put in decreasing order
      │  ├─ Trimming: 600-4000 cm⁻¹ (3575 → 1761)
      │  ├─ Resampling: 1.93 → 4 cm⁻¹ onto 600-4000 (1761 → 851)
      │  └─ 3 samples × 851 predictors
      │
    Code
      x <- standardize(dec, resample = 2, trim = c(600, 4000), verbose = TRUE)
    Output
      ├─ Standardizing...
      │  ├─ Trimming: 600-4000 cm⁻¹ (1701 → 1701)
      │  ├─ Resampling: already on the 2 cm⁻¹ grid, 600-4000 (1701), not re-interpolated
      │  └─ 3 samples × 1701 predictors
      │
    Code
      x <- standardize(on_4, resample = NULL, trim = NULL, force = TRUE, verbose = TRUE)
    Output
      ├─ Standardizing...
      │  ├─ Re-standardizing (force = TRUE): a new record, keeping any hull an earlier call fitted
      │  └─ 3 samples × 851 predictors
      │

