# Test Fixtures

This directory contains test data fixtures used throughout the horizons package test suite.

## Files

- `small_spectra_fixture.qs`: Small dataset with 10 samples and full spectral range (600-4000 cm⁻¹)
  - Based on real MAOM_C data from the package's testdata
  - Includes Project, Sample_ID, Response, and spectral columns
  - Use for testing spectral preprocessing and recipe building

- `ensemble_fit.rds`: A small fitted `horizons_fit` used by `test-pipeline-ensemble.R`.
  - Built from **real (anonymized) AONR MIR spectra predicting Bulk_C** — sample
    IDs are generic (`sample_001…`), provenance/coordinates stripped. Real signal
    matters: members predict (R² ≈ 0.25–0.40) so the ensemble's "did the
    meta-learner learn anything" assertions are not vacuous.
  - 3 members (cubist/rf) spanning **log and none** transforms, so the
    double-back-transform regression test is exercisable (identity-only would
    make it vacuous).
  - Rebuild with `dev/build-ensemble-fixture.R` if the object model changes
    (requires local ai-leaf AONR OPUS data; see the paths at the top of that
    script). Tracked despite the repo-wide `*.rds` ignore via a `.gitignore`
    negation for `tests/testthat/fixtures/*.rds`.

## Usage

Load fixtures in tests using:

```r
# Load spectral fixture
test_data <- qs::qread(test_path("fixtures", "small_spectra_fixture.qs"))

# Or use helper functions
test_data <- make_test_spectra(n_samples = 10)
```

## Guidelines

- Keep fixtures small (<1MB each) for fast test execution
- Use real data structure but synthetic/subset values for privacy
- Document the purpose and structure of each fixture
- Use qs format for efficient storage of R objects