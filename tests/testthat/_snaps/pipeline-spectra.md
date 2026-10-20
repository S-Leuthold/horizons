# spectra() prints its banner, with the sorting line only when it sorts, and scans under ids when they repeat

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
    Code
      invisible(spectra(scans, verbose = TRUE))
    Output
      ── horizons pipeline ──────────────────────────────────────────────
      ├─ Loading spectra...
      │  ├─ 5 scans under 2 sample ids × 2 predictors
      │  └─ 1 scan with no sample id (`NA`)
      │

# spectra() aborts with every file's failure when none can be read, naming the blocks a file holds

    Code
      spectra(files)
    Condition
      Error in `spectra()`:
      ! None of the 2 OPUS files in `source` could be read.
      x File read failed: corrupt header (a.0)
      x Block extraction failed: The file holds no absorbance block ("ab" or "ab_no_atm_comp"). Its blocks: "sc_sample". (b.0)
      i The failures are on this condition as results: `rlang::last_error()$results`.

# a CSV file whose columns cannot be read fails as a file, each cause keeping its remedy

    Code
      spectra(noid, verbose = FALSE)
    Condition
      Error in `spectra()`:
      ! None of the 1 CSV file in `source` could be read.
      x Column check failed: Could not identify sample ID column. Checked aliases: "sample_id", "Sample_ID", "SampleID", "sampleid", "id", "ID", "Id", "sample", "Sample", "filename", "Filename", "name", and "Name". Available columns: "label" and "4000". Specify it with `id_col`. (<dir>/noid.csv)
      i The failures are on this condition as results: `rlang::last_error()$results`.

