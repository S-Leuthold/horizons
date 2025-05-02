# horizons

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/s-leuthold/horizons/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/s-leuthold/horizons/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)


**Spectral Ensemble Modeling and Covariate Prediction for Soil Data**

`horizons` provides a modular framework for building, tuning, and evaluating ensemble models that predict soil properties using mid-infrared (MIR) spectral data. It supports covariate injection (e.g., OSSL-predicted variables), workflow management via `tidymodels`, and parallel execution on high-performance systems.

---

## Installation

Currently only available via GitHub:

```r
devtools::install_github("samleuthold/horizons")
```

---

## Features

- Flexible spectral preprocessing (Savitzky-Golay, SNV, PCA)  
- Response transformations (log, square root, Box-Cox)  
- Covariate prediction using external data (e.g., OSSL outputs)  
- Model grid construction and Bayesian tuning  
- Workflow set support with ensemble stacking  
- Parallel execution using `furrr` + `progressr`  

---

## Example Usage

```r
library(horizons)

results <- full_model_evaluation(
  input_data              = mydata,
  models                  = c("Random_Forest", "Partial Least Squares Regression"),
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
