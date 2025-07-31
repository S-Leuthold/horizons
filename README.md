# horizons

> Spectral Ensemble Modeling and Covariate Prediction for Soil Data

<img src="man/figures/logo.png" align="right" width="140"/>

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-green)](https://github.com/S-Leuthold/horizons/actions)
[![Codecov test coverage](https://codecov.io/gh/S-Leuthold/horizons/branch/main/graph/badge.svg)](https://codecov.io/gh/S-Leuthold/horizons?branch=main)
![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

---

`horizons` is a modular R framework for analyzing mid-infrared (MIR) soil spectra. Built on the `tidymodels` ecosystem, it provides a suite of tools for reproducible soil property prediction and spectral analysis. The package supports end-to-end workflows, including data ingestion, spectral preprocessing, automated covariate fetching, robust model tuning, and ensemble stacking.

Designed for researchers in soil biogeochemistry and environmental data science, `horizons` prioritizes transparency, modularity, and reproducibility. It aim to offer a powerful and flexible foundation for rigorous modeling and analysis.

> **Note**: `horizons` is in active development and remains experimental. The whole thing is subject to break, be weird, and not handle edge cases well. If you find a bug, please submit an issue or a pull request, and I'll try to update things to fix it. This is kind of a passion project though, so mileage may vary as to how fast things get done.

---

## Key Features

-   **Modular Data Ingestion**: A structured system (`project_list`, `project_entry`, `create_project_data`) for importing and merging OPUS spectral files with sample-level metadata.
-   **Automated Covariate Integration**:
    -   Predicts soil properties (e.g., pH, clay, sand) using a built-in workflow that leverages the Open Soil Spectral Library (OSSL).
    -   Fetches climate data (e.g., MAT, MAP, GDD) from the Daymet API based on sample coordinates.
-   **Advanced Preprocessing**: Implements `recipes` steps for common spectral transformations, including Savitzky-Golay smoothing, derivatives, Standard Normal Variate (SNV), and Multiplicative Scatter Correction (MSC).
-   **Flexible Model Orchestration**:
    -   Systematically builds large-scale model evaluation grids combining models, preprocessing methods, response transformations, and covariate subsets.
    -   Supports a wide range of model types, including `random_forest`, `cubist`, `xgboost`, and `plsr`.
-   **Robust Tuning and Evaluation**:
    -   Executes batch model evaluations with fault-tolerant execution using `safely_execute()`.
    -   Applies a two-stage hyperparameter tuning process with an initial grid search followed by Bayesian optimization.
-   **Ensemble Stacking**: Builds, blends, and evaluates a stacked ensemble from the top-performing candidate models using the `stacks` package.

---

## Workflow Overview

The `horizons` package is built around a series of orchestrated steps to move from raw data to a final, stacked ensemble model.

1.  **Define and Load Project Data**:
    Use `project_list()` and `project_entry()` to define the locations of your spectral files and sample data. Then, use `create_project_data()` to ingest, resample, and merge everything into a single, wide-format tibble.

2.  **Generate Model Configurations**:
    `create_project_configurations()` builds a comprehensive grid of all modeling approaches to be tested. This includes fetching and predicting all necessary soil and climate covariates and creating combinations of models, transformations, and preprocessing steps.

3.  **Run Batch Model Evaluation**:
    `run_model_evaluation()` iterates through every configuration from the previous step. It trains, tunes, and evaluates each model in isolation, logging results and errors. The best-performing models are saved to disk for the next stage.

4.  **Build the Ensemble Stack**:
    `build_ensemble_stack()` reads the top-performing models from the batch evaluation, retrains them on new cross-validation folds, and blends them into a single, high-performance stacked ensemble model.

---

## Usage Example

```r
library(horizons)

## -----------------------------------------------------------------------------
## Step 1: Define project(s) and load input data
## -----------------------------------------------------------------------------

projects <- project_list(
  "my_project" = project_entry(
    spectra_path        = "./opus_files/",
    sample_obs          = "./sample_data.csv",
    file_name_format    = "project_sampleid_fraction_scanid",
    file_name_delimiter = "_"
  )
)

project_data <- create_project_data(
  projects  = projects,
  variables = "MAOM_C_g_kg"
)

## -----------------------------------------------------------------------------
## Step 2: Set up project configurations and fetch covariates
## -----------------------------------------------------------------------------

project_configs <- create_project_configurations(
  project_data       = project_data,
  models             = c("random_forest", "cubist", "plsr"),
  transformations    = c("No Transformation", "Log Transformation"),
  preprocessing      = c("snv", "deriv1", "snv_deriv1"),
  soil_covariates    = c("pH", "Clay"),
  climate_covariates = "all",
  refresh            = FALSE,
  verbose            = TRUE
)

## -----------------------------------------------------------------------------
## Step 3: Run the model evaluation
## -----------------------------------------------------------------------------

run_model_evaluation(
  config         = project_configs$project_configurations,
  input_data     = project_data,
  covariate_data = project_configs$covariate_data,
  variable       = "MAOM_C_g_kg",
  output_dir     = "./model_results",
  grid_size_eval = 10,
  bayesian_iter_eval = 15,
  number_models_retained = 10,
  pruning        = TRUE
)

## -----------------------------------------------------------------------------
## Step 4: Build the final ensemble stack
## -----------------------------------------------------------------------------

final_ensemble <- build_ensemble_stack(
  results_dir   = "./model_results",
  input_data    = project_data,
  variable      = "MAOM_C_g_kg",
  filter_metric = "rrmse",
  n_best        = 10
)

## -----------------------------------------------------------------------------
## Step 5: Plot the results
## -----------------------------------------------------------------------------

plot_ensemble_results(final_ensemble)
```

---

## Current Status and Roadmap

`horizons` is an actively developed but experimental package. The core framework for ingestion, preprocessing, modeling, and evaluation is in place.

Future development will focus on:
-   **Code Modularization**: Refactoring large orchestrator functions (`predict_covariates`, `run_model_evaluation`) into smaller, more testable units.
-   **Spectral Characterization**: Adding dedicated functions for exploratory analysis, including PCA overlays and loadings plots.
-   **Model Interpretability**: Integrating tools for variable importance and model-agnostic explanations.
-   **Documentation**: Expanding vignettes with end-to-end reproducible examples.

---

## License

MIT © 2025 Sam Leuthold
