# Inputs Family Refactor Plan

## Overview
Refactoring the inputs family to create a robust, modular pipeline similar to the evaluation refactor. Supporting both MIR and NIR spectroscopy with proper preprocessing and quality control.

## New File Structure

### Core Files:

1. **`inputs-read.R`**
   - Read OPUS and CSV files
   - Auto-detect format and spectral type (MIR/NIR)
   - Return standardized spectral data structure
   - Track metadata (instrument, channel, date)

2. **`inputs-preprocess.R`**
   - Atmospheric band removal (H2O, CO2)
   - Baseline correction (polynomial, ALS, rubberband)
   - Quality metrics (SNR, saturation, outliers)
   - Spectral resampling

3. **`inputs-create.R`**
   - Replace create_project_data.R
   - No super assignment (<<-)
   - Batch processing for memory efficiency
   - Proper channel consistency checking

4. **`inputs-configs.R`**
   - Replace create_project_configs.R
   - Cleaner parameter validation
   - Baseline model support
   - Flexible covariate handling

5. **`inputs-baseline.R`**
   - Add baseline models (mean, median, linear)
   - Benchmarking support

6. **`inputs-validate.R`**
   - Extract validation logic
   - Check data structure and ranges
   - Verify metadata consistency

## Spectral Type Support

### MIR (Mid-Infrared)
- Range: 4000-400 cm⁻¹ (wavenumbers)
- Atmospheric bands: 
  - H₂O: 3600-3000, 1850-1800 cm⁻¹
  - CO₂: 2350-2280 cm⁻¹
- Default resample: 3800-650 cm⁻¹ at 2 cm⁻¹ steps

### NIR (Near-Infrared)
- Range: 12500-4000 cm⁻¹ or 800-2500 nm
- Atmospheric bands:
  - H₂O: 1350-1450, 1800-1950 nm
- Different preprocessing needs

## Key Improvements

1. **No side effects** - Remove all <<- usage
2. **Modular design** - Functions under 100 lines
3. **Proper validation** - Input checking at every step
4. **Memory efficient** - Batch processing for large datasets
5. **Flexible formats** - Support OPUS and CSV
6. **Quality control** - SNR, saturation, outlier detection
7. **Scientific correctness** - Proper atmospheric band removal and baseline correction

## Migration Strategy

1. Create new functions alongside existing ones
2. Test thoroughly with existing data
3. Update calling code to use new functions
4. Deprecate old functions
5. Remove old functions after full migration