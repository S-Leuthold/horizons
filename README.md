
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
- Batch evaluation of model configurations with structured logging

---

## Usage Example

```r
library(horizons)

## -----------------------------------------------------------------------------
## Step 1. Build horizons projects object
## -----------------------------------------------------------------------------

projects <- project_list("project" = project_entry(spectra_path     = "./opus_files/",
                                                sample_obs          = "./fraction_data.csv",
                                                file_name_format    = "project_sampleid_fraction_scanid_wellid",
                                                file_name_delimiter = "_"))

## -----------------------------------------------------------------------------
## Step 2: Load the input data
## -----------------------------------------------------------------------------

create_project_data(projects  = projects,
                    variables = "MAOM_C_g_kg") -> project_data

## -----------------------------------------------------------------------------
## Step 3: Set up project configurations
## -----------------------------------------------------------------------------

create_project_configurations(project_data       = project_data,
                              models             = c("random_forest",
                                                     "cubist",
                                                     "xgboost",
                                                     "elastic_net",
                                                     "svm_rbf",
                                                     "mars",
                                                     "plsr",
                                                     "mlp_nn"),
                              transformations    = c("No Transformation",
                                                     "Log Transformation",
                                                     "Square Root Transformation"),
                              preprocessing      = c("raw",
                                                     "sg",
                                                     "snv",
                                                     "deriv1",
                                                     "deriv2",
                                                     "snv_deriv1",
                                                     "snv_deriv2",
                                                     "msc_deriv1"),
                              soil_covariates    = c("pH",
                                                     "Nitrogen",
                                                     "Clay"),
                              climate_covariates = c("AI",
                                                     "GDD"),
                              spatial_covariates = NULL,
                              refresh            = FALSE,
                              verbose            = TRUE) -> project_configs



## -----------------------------------------------------------------------------
## Step 4: Run the model evaluation
## -----------------------------------------------------------------------------

run_model_evaluation(config         = project_configs$project_configurations,
                     input_data     = project_data,
                     covariate_data = project_configs$covariate_data,
                     variable       = "MAOM_C_g_kg",
                     output_dir     = "./model_results",
                     grid_size      = 25,
                     bayesian_iter  = 25,
                     cv_folds       = 10,
                     return_outputs = FALSE,
                     pruning        = FALSE) -> results
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
