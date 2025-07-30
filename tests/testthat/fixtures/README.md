# Test Fixtures

This directory contains test data fixtures used throughout the horizons package test suite.

## Files

- `small_spectra_fixture.qs`: Small dataset with 10 samples and full spectral range (600-4000 cm⁻¹)
  - Based on real MAOM_C data from the package's testdata
  - Includes Project, Sample_ID, Response, and spectral columns
  - Use for testing spectral preprocessing and recipe building

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