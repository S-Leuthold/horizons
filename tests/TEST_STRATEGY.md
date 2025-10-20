# Comprehensive Testing Strategy for Soil Covariate Prediction System

## Overview

> **Execution note**: The evaluation-hpc suite is currently gated behind the `HORIZONS_RUN_HPC_TESTS` environment variable. Leave it unset (default) for fast local runs; set to `true` when validating the full HPC orchestration tests.

This document outlines the testing strategy for the refactored soil covariate prediction system using stratified Kennard-Stone selection and Cubist models.

## Test Suite Structure

### 1. Unit Tests

#### `test-covariates-data.R`
- **Purpose**: Test OSSL data integration and preprocessing functions
- **Coverage**: 
  - Property mapping functions
  - Spectral preprocessing (smoothing, SNV)
  - PCA computation and application
  - Data transformation consistency
- **Key Functions**:
  - `get_ossl_property_mapping()`
  - `preprocess_mir_spectra()`
  - `perform_pca_on_ossl()`
  - `apply_pca_to_unknown()`
  - `get_processed_ossl_training_data()`

#### `test-covariates-similarity.R`
- **Purpose**: Test stratified Kennard-Stone selection algorithm
- **Coverage**:
  - Unknown sample clustering
  - Mahalanobis distance calculations
  - Kennard-Stone algorithm
  - Stratified selection
  - Global training set selection
- **Key Functions**:
  - `cluster_unknown_samples()`
  - `calculate_mahalanobis_distances()`
  - `kennard_stone_cluster()`
  - `stratified_kennard_stone()`
  - `select_global_training_set()`

#### `test-covariates-soil-fit.R`
- **Purpose**: Test Cubist model fitting and optimization
- **Coverage**:
  - Model training with different covariates
  - Bayesian hyperparameter optimization
  - Cross-validation
  - Prediction accuracy
  - Model persistence
- **Key Functions**:
  - `fit_cubist_model()`
  - Bayesian optimization internals
  - Cross-validation procedures

### 2. Integration Tests

#### `test-covariates-integration.R`
- **Purpose**: Test complete prediction pipeline
- **Coverage**:
  - End-to-end workflow
  - Data flow through all components
  - Multiple batch processing
  - Error propagation
  - Statistical validity
- **Key Scenarios**:
  - Single sample prediction
  - Batch prediction
  - Missing wavelengths
  - Extreme values
  - Cross-validation

### 3. Performance Tests

#### `test-covariates-performance.R`
- **Purpose**: Benchmark computational performance
- **Coverage**:
  - Linear scaling with sample size
  - Memory usage profiling
  - Parallelization efficiency
  - Caching performance
  - Optimization convergence
- **Metrics**:
  - Execution time vs. sample size
  - Memory footprint
  - Parallel speedup
  - Cache hit rates

## Mock Data Strategy

### Helper Functions (`helper-mocks.R`)

#### Mock OSSL Database
- Generates realistic spectral data with absorption features
- Creates correlated soil properties
- Supports variable dataset sizes
- Includes proper OSSL column naming

#### Mock Cubist Models
- Fast model fitting for testing
- Configurable performance metrics
- Reproducible predictions
- Optimization history simulation

#### Mock Infrastructure
- File system mocking
- Cache directory setup
- Parallel processing control
- Error injection

## Test Data Generation

### Spectral Data
```r
# Realistic absorption patterns
- OH stretch: 1400-1500 nm
- CH stretch: 2800-3000 nm
- Amide bands: 1600-1700 nm
```

### Property Correlations
- Clay content correlated with OH absorption
- pH affects overall baseline
- Organic carbon linked to CH absorption

### Dataset Sizes
- Small: 10-50 samples (edge cases)
- Medium: 100-500 samples (typical use)
- Large: 1000-5000 samples (performance testing)
- Stress: 10000+ samples (scalability limits)

## Performance Benchmarks

### Target Metrics

| Operation | Small (n=100) | Medium (n=1000) | Large (n=5000) |
|-----------|---------------|-----------------|----------------|
| Preprocessing | < 0.5s | < 2s | < 10s |
| PCA | < 1s | < 5s | < 30s |
| KS Selection | < 2s | < 10s | < 60s |
| Model Fitting | < 5s | < 30s | < 3min |
| Prediction | < 0.1s | < 0.5s | < 2s |

### Memory Limits
- Preprocessing: < 3x input size
- PCA: < 5x data matrix size
- Selection: < 2x combined dataset size
- Model: < 500MB per covariate

## Statistical Validation

### Accuracy Requirements
- Clay: RMSE < 100 g/kg, R² > 0.70
- pH: RMSE < 0.5, R² > 0.75
- OC: RMSE < 10 g/kg, R² > 0.70
- CEC: RMSE < 5 cmol/kg, R² > 0.65

### Cross-Validation
- 5-fold CV for model assessment
- Stratified sampling by property ranges
- Bias < 5% of property range
- Consistent performance across folds

## Edge Cases and Error Handling

### Data Issues
1. **Empty datasets**: Graceful failure with informative errors
2. **Single sample**: Special handling, no clustering
3. **Missing values**: Interpolation or removal
4. **Extreme values**: Clipping or transformation
5. **Constant values**: Detection and warning

### Computational Issues
1. **Singular covariance**: Regularization (ε = 1e-6)
2. **Memory overflow**: Chunked processing
3. **Convergence failure**: Early stopping with warning
4. **Parallel failures**: Fallback to sequential

### Model Issues
1. **Overfitting**: Detected via validation metrics
2. **Extrapolation**: Bounded predictions
3. **Poor convergence**: Restart with different seeds
4. **Insufficient data**: Minimum sample requirements

## CI/CD Integration

### GitHub Actions Workflow
```yaml
name: Test Covariates
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: r-lib/actions/setup-r@v2
      - name: Install dependencies
        run: |
          install.packages(c("testthat", "bench", "profmem"))
          remotes::install_deps()
      - name: Run unit tests
        run: |
          testthat::test_file("tests/testthat/test-covariates-data.R")
          testthat::test_file("tests/testthat/test-covariates-similarity.R")
          testthat::test_file("tests/testthat/test-covariates-soil-fit.R")
      - name: Run integration tests
        run: |
          testthat::test_file("tests/testthat/test-covariates-integration.R")
      - name: Check performance
        run: |
          testthat::test_file("tests/testthat/test-covariates-performance.R")
```

### Test Coverage Requirements
- Line coverage: > 80%
- Function coverage: > 90%
- Branch coverage: > 70%

## Testing Best Practices

### Test Organization
1. Group related tests in `describe()` blocks
2. Use descriptive test names
3. One assertion per test when possible
4. Test both success and failure paths

### Mock Usage
1. Use mocks for external dependencies
2. Keep mocks simple and focused
3. Document mock behavior
4. Verify mock calls when appropriate

### Performance Testing
1. Skip expensive tests on CRAN
2. Use sampling for large dataset tests
3. Set reasonable timeouts
4. Profile before optimizing

### Reproducibility
1. Always set seeds for random operations
2. Use fixed test data where possible
3. Document environment requirements
4. Version lock critical dependencies

## Manual Testing Checklist

### Pre-Release Testing
- [ ] Full pipeline with real OSSL data
- [ ] Stress test with 10,000+ samples
- [ ] Memory profiling under load
- [ ] Parallel processing on multi-core systems
- [ ] Cache effectiveness measurement
- [ ] Model comparison with baseline methods
- [ ] Cross-validation stability
- [ ] Documentation completeness

### Regression Testing
- [ ] Compare results with previous version
- [ ] Verify backward compatibility
- [ ] Check API stability
- [ ] Validate configuration options

## Monitoring and Maintenance

### Key Metrics to Track
1. Test execution time trends
2. Memory usage patterns
3. Prediction accuracy over time
4. Optimization convergence rates
5. Cache hit rates

### Regular Updates
1. Update mock data quarterly
2. Review performance benchmarks
3. Adjust accuracy thresholds based on real-world usage
4. Add tests for reported issues

## Future Enhancements

### Planned Tests
1. GPU acceleration testing
2. Distributed computing scenarios
3. Real-time prediction latency
4. Uncertainty quantification validation
5. Transfer learning effectiveness

### Testing Infrastructure
1. Automated performance regression detection
2. Visual regression testing for plots
3. Integration with cloud testing services
4. Continuous benchmarking dashboard

---

## Quick Start for Developers

### Running All Tests
```r
# Run complete test suite
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-covariates-data.R")

# Run with coverage
covr::package_coverage()
```

### Running Performance Tests
```r
# Skip CRAN tests
Sys.setenv(NOT_CRAN = "true")
testthat::test_file("tests/testthat/test-covariates-performance.R")
```

### Using Mock Data
```r
# Generate mock OSSL database
source("tests/testthat/helper-mocks.R")
with_mocked_ossl_covariates({
  # Your test code here
  result <- predict_soil_covariates(...)
})
```