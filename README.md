# horizons

> Spectral Ensemble Modeling and Covariate Prediction for Soil Data

<img src="man/figures/logo.png" align="right" width="140"/>

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-failing-red)](https://github.com/S-Leuthold/horizons/actions)
![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

---

`horizons` is a modular framework for developing ensemble models that predict soil properties from mid-infrared (MIR) spectral data. Built on the `tidymodels` ecosystem, it streamlines spectral preprocessing, covariate injection, model tuning, and workflow evaluation — with support for parallel execution and external predictors like OSSL-derived covariates.

> **Note**: `horizons` is under active development and not yet stable. Expect breaking changes and incomplete functionality.

---

## Installation

```r
# Currently available via GitHub
devtools::install_github("S-Leuthold/horizons")
```

---

## Features

- Flexible spectral preprocessing (Savitzky-Golay, SNV, PCA)
- Response transformations (log, square root, Box-Cox)
- Covariate prediction using external data (e.g., OSSL outputs)
- Model grid construction and Bayesian tuning
- Workflow set support with ensemble stacking
- Parallel execution with `furrr` + `progressr`

---

```r
library(horizons)

results <- full_model_evaluation(
  input_data              = mydata,
  models                  = c("Random Forest", "Partial Least Squares Regression"),
  transformations         = "Log Transformation",
  preprocessing           = "Standard Normal Variate - Savitzky Golay - 0 Derivative",
  variable                = "MAOM_C_g_kg",
  include_covariates      = TRUE,
  covariate_data          = my_covs,
  expand_covariate_grid   = TRUE,
  grid_size               = 5,
  bayesian_iter           = 15,
  cv_folds                = 5
)
```

---

## License

MIT © Sam Leuthold



