# Spectroscopy and Machine Learning Analysis: Horizons Package Input Files

## Executive Summary

The horizons package's input files show a functional but incomplete implementation of spectroscopic data handling and preprocessing. While basic infrastructure exists, several critical spectroscopy-specific features are missing or inadequately implemented, particularly regarding spectral quality control, baseline correction, and domain-specific preprocessing requirements.

## 1. SPECTROSCOPY-SPECIFIC ISSUES

### 1.1 Critical Missing Features

#### **Water and CO2 Band Removal**
**Current State**: No implementation found for removing atmospheric interference bands.
**Issue**: MIR spectra contain strong atmospheric water (3600-3000, 1850-1800 cm⁻¹) and CO2 (2350-2280 cm⁻¹) absorption bands that should be removed or masked.
**Scientific Justification**: These regions contain no soil-specific information and introduce noise/artifacts that degrade model performance.

**Recommendation**:
```r
# Add to inputs-create_project_data.R after line 167
remove_atmospheric_bands <- function(spectra_df) {
  co2_bands <- c(2350:2280)
  water_bands <- c(3600:3000, 1850:1800)
  
  spectra_df %>%
    select(-any_of(as.character(c(co2_bands, water_bands))))
}
```

#### **Baseline Correction**
**Current State**: Not implemented in preprocessing pipeline.
**Issue**: DRIFT and ATR-FTIR spectra often exhibit baseline drift due to scattering effects.
**Scientific Justification**: Baseline drift masks true absorption features and introduces systematic bias.

**Recommendation**: Add baseline correction options to `step_transform_spectra.R`:
- Polynomial baseline correction
- Asymmetric least squares (ALS)
- Rubberband baseline correction

#### **Spectral Quality Control**
**Current State**: No quality metrics or outlier detection implemented.
**Issue**: Poor quality spectra (low SNR, instrumental artifacts) can severely impact model performance.

**Recommendation**: Implement quality checks in `read_spectral_data()`:
```r
assess_spectral_quality <- function(absorbance, wavenumber) {
  # Signal-to-noise ratio
  snr <- calculate_snr(absorbance, region = c(2000, 2100))
  
  # Check for saturation
  saturated <- any(absorbance > 3.5)
  
  # Check for negative absorbance
  negative <- any(absorbance < -0.1)
  
  # Mahalanobis distance for outlier detection
  md <- mahalanobis_distance(absorbance, reference_spectra)
  
  list(snr = snr, saturated = saturated, 
       negative = negative, md = md)
}
```

### 1.2 Preprocessing Issues

#### **Spectral Range Selection**
**Current State**: Fixed range 600-4000 cm⁻¹
**Issue**: Includes noisy edge regions; optimal range for soil is typically 4000-400 cm⁻¹ or 3800-650 cm⁻¹
**Recommendation**: Make spectral range configurable and default to 3800-650 cm⁻¹

#### **Resampling Resolution**
**Current State**: 2 cm⁻¹ (line 158)
**Issue**: While adequate, consider that original OPUS files may have 4 cm⁻¹ resolution
**Recommendation**: Match resampling to original resolution or make configurable

#### **Derivative Calculation**
**Current State**: Savitzky-Golay with fixed parameters
**Issue**: Window size (9) may be suboptimal for 2 cm⁻¹ resolution
**Recommendation**: 
- For 2 cm⁻¹: window size 11-21
- For 4 cm⁻¹: window size 5-11
- Make window size data-driven based on spectral resolution

### 1.3 OPUS File Handling

#### **Channel Selection Logic**
**Current State**: Prioritizes "ab_no_atm_comp" over "ab" (lines 49-59)
**Good Practice**: Correctly handles atmospheric compensation
**Issue**: No documentation of which channel was used in output

**Recommendation**: Store channel metadata:
```r
attr(all_projects, "spectral_channels") <- channel_summary
```

## 2. MACHINE LEARNING BEST PRACTICES

### 2.1 Feature Engineering Gaps

#### **Missing Preprocessing Options**
Not implemented but essential for soil spectroscopy:
- Multiplicative Scatter Correction (MSC)
- Extended MSC (EMSC) 
- Continuum removal
- Detrending
- Wavelet denoising

#### **Spectral Indices**
**Current State**: No spectral indices or band ratios
**Issue**: Domain-specific indices can improve predictions
**Recommendation**: Add soil-specific spectral indices:
```r
calculate_spectral_indices <- function(spectra) {
  # Organic matter index
  omi <- spectra[,"1650"] / spectra[,"1750"]
  
  # Clay index  
  clay_idx <- spectra[,"3620"] / spectra[,"3700"]
  
  # Carbonate index
  carb_idx <- spectra[,"2520"] / spectra[,"2600"]
  
  cbind(spectra, omi, clay_idx, carb_idx)
}
```

### 2.2 Cross-Validation Issues

#### **No Spectral-Specific CV Strategy**
**Issue**: Random CV can lead to overfitting with spectral replicates
**Recommendation**: Implement:
- Leave-one-site-out CV for multi-site studies
- Kennard-Stone splitting for spectral diversity
- Block CV for spatial/temporal dependencies

### 2.3 Model Selection

#### **Current Models**: Good coverage (RF, Cubist, XGBoost, PLSR, etc.)
**Missing**: Spectroscopy-specific models:
- Locally weighted PLSR
- Support Vector Regression with spectral kernels
- Memory-based learning (MBL)
- Deep learning (1D-CNN)

## 3. DOMAIN-SPECIFIC CONCERNS

### 3.1 Soil Fraction Handling

**Current State**: Basic fraction support via metadata parsing
**Issue**: No fraction-specific preprocessing or modeling
**Recommendation**: Different fractions require different preprocessing:
```r
fraction_specific_preprocessing <- function(spectra, fraction) {
  switch(fraction,
    "clay" = snv_deriv1,  # Clay minerals show sharp features
    "sand" = snv,          # Sand has broader features
    "whole" = baseline_snv # Whole soil needs baseline correction
  )
}
```

### 3.2 Moisture Effects

**Not Addressed**: Moisture significantly affects MIR spectra
**Recommendation**: 
- Document sample moisture status
- Consider moisture correction algorithms
- Flag wet samples for special handling

### 3.3 Physical Constraints

**Missing**: Domain knowledge constraints
**Examples**:
- Sand + Silt + Clay = 100%
- pH typically 3-11
- Organic carbon 0-20%

**Recommendation**: Add post-prediction constraints:
```r
apply_physical_constraints <- function(predictions) {
  # Texture constraint
  if (all(c("sand", "silt", "clay") %in% names(predictions))) {
    total <- rowSums(predictions[,c("sand", "silt", "clay")])
    predictions[,c("sand", "silt", "clay")] <- 
      predictions[,c("sand", "silt", "clay")] / total * 100
  }
  
  # Range constraints
  predictions$pH <- pmax(3, pmin(11, predictions$pH))
  predictions$OC <- pmax(0, pmin(20, predictions$OC))
  
  predictions
}
```

## 4. DATA QUALITY ISSUES

### 4.1 Missing Quality Metrics

**Not Implemented**:
- Spectral repeatability checks
- Instrument drift detection  
- Reference material tracking
- Calibration transfer functions

### 4.2 Edge Effect Handling

**Current Implementation**: Simple trimming (lines 52-53, 71)
**Issue**: May lose important information
**Recommendation**: Use gap-segment derivatives or edge-padding

### 4.3 Outlier Detection

**Not Implemented**: No spectral outlier detection
**Recommendation**: Implement multiple approaches:
```r
detect_spectral_outliers <- function(spectra) {
  # PCA-based
  pca_outliers <- detect_pca_outliers(spectra, n_components = 10)
  
  # Mahalanobis distance
  md_outliers <- mahalanobis(spectra, colMeans(spectra), cov(spectra))
  
  # Spectral angle mapper
  sam_outliers <- spectral_angle_mapper(spectra, reference_spectra)
  
  list(pca = pca_outliers, md = md_outliers, sam = sam_outliers)
}
```

## 5. CRITICAL RECOMMENDATIONS

### Immediate Priority (Breaking Issues)

1. **Add atmospheric band removal** - Currently corrupting models
2. **Implement baseline correction** - Essential for DRIFT spectra
3. **Add spectral quality checks** - Prevent bad data from entering models
4. **Fix spectral range** - Current range includes noise

### High Priority (Performance Issues)

5. **Add MSC normalization** - Standard for soil spectroscopy
6. **Implement spectral outlier detection** - Improve model robustness
7. **Add moisture documentation/correction** - Major confounding factor
8. **Implement proper spectral CV strategies** - Prevent overfitting

### Medium Priority (Enhancement)

9. **Add spectral indices** - Domain-specific features
10. **Implement ensemble preprocessing** - Multiple preprocessing paths
11. **Add calibration transfer** - Between-instrument compatibility
12. **Include uncertainty quantification** - Prediction intervals

## 6. CODE QUALITY OBSERVATIONS

### Strengths
- Good error handling with `safely_execute`
- Modular design with separate steps
- Good use of tidyverse patterns
- Comprehensive progress reporting

### Weaknesses  
- No unit tests for spectral processing
- Missing validation of spectral data integrity
- Hardcoded parameters (window sizes, ranges)
- No logging of preprocessing decisions

## 7. IMPLEMENTATION ROADMAP

### Phase 1: Critical Fixes (Week 1)
- Atmospheric band removal
- Baseline correction  
- Quality control metrics
- Spectral range optimization

### Phase 2: ML Enhancements (Week 2)
- MSC implementation
- Outlier detection
- CV strategy improvements
- Physical constraints

### Phase 3: Advanced Features (Week 3-4)
- Spectral indices
- Calibration transfer
- Uncertainty quantification
- Memory-based learning

## CONCLUSION

The horizons package provides a solid foundation for spectroscopic modeling but lacks critical domain-specific features. The most urgent need is implementing atmospheric band removal and baseline correction. Without these, model performance will be severely compromised. The recommended changes will transform this from a generic ML pipeline to a proper chemometric workflow suitable for publication-quality soil spectroscopy research.

The package shows promise but requires significant spectroscopy-specific enhancements to meet scientific standards for MIR soil analysis. Priority should be given to spectral quality control and preprocessing completeness before expanding model complexity.