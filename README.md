
# horizons

> Spectral Ensemble Modeling and Covariate Prediction for Soil Data

<img src="man/figures/logo.png" align="right" width="140"/>

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-failing-red)](https://github.com/S-Leuthold/horizons/actions)
![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

---

`horizons` is a modular framework for analyzing mid-infrared (MIR) soil spectra and predicting soil properties. Built on the `tidymodels` ecosystem, it supports spectral preprocessing, covariate integration (including external sources like OSSL), robust model grid construction, Bayesian and grid-based tuning, ensemble stacking, and reproducible workflow evaluation.

This package is designed for researchers in soil biogeochemistry and environmental data science who prioritize transparent, flexible, and modular workflows. It does not provide a graphical user interface or production-ready deployment but offers a foundation for rigorous modeling and reproducible analyses.

> **Note**: `horizons` is in active development and remains experimental. Expect breaking changes and ongoing refinement.

---

## Installation

```r
# Install from GitHub
remotes::install_github("S-Leuthold/horizons")
```

---

## Key Features

- Modular spectral preprocessing: SNV, Savitzky-Golay smoothing, PCA-based reduction
- Response transformations: log, square root, Box-Cox
- Covariate prediction workflows using external sources (e.g., OSSL)
- Model grid construction supporting multiple models (PLSR, Cubist, RF, SVM, BNN)
- Grid and Bayesian hyperparameter tuning
- Ensemble stacking using `stacks::stacks()`
- Planned support for spectral characterization (PCA overlays, loadings plots)
- Parallel execution with `furrr` + `progressr`
- Batch evaluation of model configurations with structured logging

---

## Usage Example

```r
library(horizons)

results <- full_model_evaluation(
  input_data = mydata,
  models = c("Random Forest", "Partial Least Squares Regression"),
  transformations = "Log Transformation",
  preprocessing = "Standard Normal Variate - Savitzky Golay - 0 Derivative",
  variable = "MAOM_C_g_kg",
  include_covariates = TRUE,
  covariate_data = my_covs,
  expand_covariate_grid = TRUE,
  grid_size = 5,
  bayesian_iter = 15,
  cv_folds = 5
)
```

---

## Current Status and Roadmap

- **Product Requirements Document**: See `dev/01-PRD.md` for a detailed vision, user stories, technical requirements, and planned milestones.
- Core functions are implemented for ingestion, preprocessing, model training, tuning, stacking, and basic evaluation.
- Ongoing efforts include modularizing large orchestrator functions, consolidating covariate logic, expanding preprocessing capabilities, improving error handling, and enhancing reproducibility.
- Future releases will add characterization functions, expand model library support, and improve documentation (vignettes, reproducible examples).

---

## License

MIT © Sam Leuthold
