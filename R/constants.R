#' Package Constants
#'
#' @description
#' Central location for package-wide constants used in validation and configuration.
#'
#' @keywords internal
#' @name constants

## Model Types ----------------------------------------------------------------

# Valid model types supported by the package
VALID_MODELS <- c(
  "random_forest",
  "plsr",
  "cubist",
  "xgboost",
  "lightgbm",
  "elastic_net",
  "svm_rbf",
  "mars",
  "mlp_nn"
)

## Transformations ------------------------------------------------------------

# Valid response transformations
VALID_TRANSFORMATIONS <- c(
  "none",
  "log",
  "sqrt"
)

## Preprocessing Methods ------------------------------------------------------

# Valid spectral preprocessing methods
VALID_PREPROCESSING <- c(
  "raw",
  "sg",
  "snv",
  "deriv1",
  "deriv2",
  "snv_deriv1",
  "snv_deriv2")

## Feature Selection Methods --------------------------------------------------

# Valid feature selection methods
VALID_FEATURE_SELECTION <- c(
  "none",
  "pca",
  "pls",
  "correlation",
  "boruta",
  "cars"
)

## Default Parameters ---------------------------------------------------------

# Default tuning parameters
DEFAULT_GRID_SIZE <- 10
DEFAULT_BAYES_ITER <- 15
DEFAULT_CV_FOLDS <- 5
BAYES_NO_IMPROVE_LIMIT <- 10
DEFAULT_CORE_BUFFER <- 2  # Keep 2 cores free for system

## Covariate Types ------------------------------------------------------------

# Known soil covariate types
KNOWN_SOIL_COVARIATES <- c(
  "clay", "sand", "silt", "ph", "phh2o", "oc", "ocd",
  "n", "nitrogen", "cec", "bdod", "ca", "mg", "k", "na",
  "p", "fe", "al", "mn", "zn", "cu"
)

# Known climate covariate types
KNOWN_CLIMATE_COVARIATES <- c(
  "MAT", "MAP", "PET", "AI", "GDD", "Precip_Seasonality",
  "Temperature_Range", "Frost_Days"
)

# Known spatial covariate types
KNOWN_SPATIAL_COVARIATES <- c(
  "elevation", "slope", "aspect", "twi", "curvature",
  "hillshade", "roughness"
)

## DAYMET Constants ------------------------------------------------------------

DAYMET_RESOLUTION_DEG <- 1/24
DAYMET_TIMEOUT        <- 60
