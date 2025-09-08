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
  "snv",
  "snv_sg1",
  "snv_sg2", 
  "sg1",
  "sg2",
  "deriv1",
  "deriv2"
)

## Feature Selection Methods --------------------------------------------------

# Valid feature selection methods
VALID_FEATURE_SELECTION <- c(
  "none",
  "pca",
  "pls",
  "correlation",
  "boruta"
)

## Default Parameters ---------------------------------------------------------

# Default tuning parameters
DEFAULT_GRID_SIZE <- 10
DEFAULT_BAYES_ITER <- 15
DEFAULT_CV_FOLDS <- 5
BAYES_NO_IMPROVE_LIMIT <- 10
DEFAULT_CORE_BUFFER <- 2  # Keep 2 cores free for system