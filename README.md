# horizons

> Spectral Ensemble Modeling for Soil Property Prediction

<img src="man/figures/logo.png" alt="horizons logo" align="right" width="140"/>

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)
[![R-CMD-check](https://github.com/S-Leuthold/horizons/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/S-Leuthold/horizons/actions/workflows/R-CMD-check.yaml)
[![Codecov](https://codecov.io/gh/S-Leuthold/horizons/branch/main/graph/badge.svg)](https://codecov.io/gh/S-Leuthold/horizons)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)

---

## Overview

`horizons` is an R package for predicting soil properties from mid-infrared (MIR) spectra. Built on the `tidymodels` ecosystem, it provides end-to-end workflows for spectral preprocessing, model training, hyperparameter tuning, and ensemble stacking.

The package supports two prediction modes:

- **Custom Training**: Build and evaluate models on your own calibration data with systematic hyperparameter optimization and ensemble stacking
- **Library Prediction** *(in development)*: Predict a batch of unknowns against a reference library without holding lab values of your own. `select_training()` draws a training set out of the library around the batch, and the ordinary fitting pipeline runs on it, with uncertainty quantification. The first registered library is the USDA KSSL soil characterization database

Designed for soil scientists and environmental researchers, `horizons` emphasizes reproducibility, modularity, and scalability from desktop to HPC environments.

---

## Installation

```r
# Install from GitHub
remotes::install_github("S-Leuthold/horizons")
```

### Dependencies

Two dependencies are not on CRAN and are pinned in the `Remotes` field of `DESCRIPTION`, so `remotes::install_github()` picks them up automatically:

- **`spectral-cockpit/opusreader2`** — reads Bruker OPUS binary files, which is how `spectra()` ingests raw spectra.
- **`S-Leuthold/plsmod-fork@fix/plsr-tune-grid-dimension`** — a fork of `plsmod`. The CRAN version drops a dimension when `tune::tune_grid()` passes a single-column matrix, which makes the `plsr` model type fail during tuning; the fork carries the `drop = FALSE` fix.

One further dependency comes from Bioconductor rather than CRAN, and `install.packages()` cannot find it. The PLS similarity space in `select_training(space = "pls")` and the `plsr` model type both route through the `mixOmics` engine, so install it first if you plan to use either:

```r
BiocManager::install("mixOmics")
```

Everything else installs from CRAN as an ordinary dependency.

---

## Key Features

### Data Ingestion

- Read OPUS files directly with automatic metadata extraction
- Flexible filename parsing for sample identification
- Merge spectral data with laboratory measurements

### Spectral Preprocessing

- Standard Normal Variate (SNV) normalization
- Savitzky-Golay smoothing and derivatives
- Multiplicative Scatter Correction (MSC)
- Baseline correction methods

### Training Set Selection

- `select_training()` draws a training set from a reference pool around the batch you actually want to predict, taking each target's nearest pool rows in a spectral similarity space
- The result is an ordinary `horizons_data`, so `configure()`, `validate()`, `evaluate()`, `fit()` and `predict()` run on it unchanged

### Model Training

- Nine supported algorithms: Random Forest, Cubist, XGBoost, PLSR, Elastic Net, SVM, MARS, MLP, LightGBM
- Two-stage hyperparameter tuning (grid search + Bayesian optimization)
- Cross-validation with stratified or grouped folds
- Parallel execution (local multi-core or HPC clusters)

### Ensemble Methods

- Stacked ensemble models using `stacks`
- Weighted model averaging
- Automatic model selection based on performance metrics

### Covariate Integration

- Predict soil covariates (pH, clay, sand) from spectra using OSSL-trained models
- Fetch climate data (MAT, MAP, GDD) from Daymet API
- Incorporate covariates into prediction models

### Performance Metrics

- Standard metrics: RMSE, R², MAE
- Spectroscopy-specific: RPD (Ratio of Performance to Deviation), RPIQ
- Agreement metrics: Concordance Correlation Coefficient (CCC)
- Relative metrics: RRMSE (Relative RMSE)

---

## Quick Start

```r
library(horizons)

# Define project structure
projects <- project_list(
 "my_study" = project_entry(
   spectra_path        = "path/to/opus_files/",
   sample_obs          = "path/to/lab_data.csv",
   file_name_format    = "project_sampleid_replicate",
   file_name_delimiter = "_"
 )
)

# Load and preprocess data
project_data <- create_project_data(
 projects  = projects,
 variables = "total_carbon"
)

# Configure model grid
configs <- create_project_configurations(
 project_data    = project_data,
 models          = c("random_forest", "cubist", "xgboost"),
 transformations = c("None", "Log"),
 preprocessing   = c("snv", "snv_d1")
)

# Run evaluation
evaluate_models_local(
 config     = configs,
 input_data = project_data,
 output_dir = "results/"
)

# Build ensemble
ensemble <- build_ensemble_stack(
 results_dir = "results/",
 input_data  = project_data,
 n_best      = 10
)
```

---

## Roadmap

### Current (v0.9.0)

- Custom training mode with full ensemble pipeline
- Local and HPC execution backends
- 80%+ test coverage

### In Development

- **Library Prediction Mode**: Training-set selection is built (`select_training()`); the packaged library it draws from, resolved by name, path or object through one `library =` argument, is the next piece. The build recipe is internal, not a user verb (`dev/specs/v1-refactor/select-training-design.md`, 2026-09-22)
- **Uncertainty Quantification**: Per-sample prediction intervals with conformal calibration
- **Applicability Domain**: Distance-based reliability metrics for new samples

### Planned

- JOSS publication
- Vignettes with reproducible examples
- Instrument calibration transfer methods

---

## Citation

If you use `horizons` in your research, please cite:

```text
Leuthold, S. (2025). horizons: Spectral Ensemble Modeling for Soil Property
Prediction. R package version 0.9.0. https://github.com/S-Leuthold/horizons
```

---

## Contributing

Contributions are welcome. Please open an issue to discuss proposed changes or submit a pull request.

### Getting help

If something is broken or unclear, open an issue at https://github.com/S-Leuthold/horizons/issues — that is the fastest route and it leaves a record other users can find. For questions that do not fit an issue, email the maintainer, Sam Leuthold, at sam.leuthold@colostate.edu.

---

## License

MIT © 2025 Sam Leuthold
