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

## Library Prediction Constants ------------------------------------------------
## Based on Ng et al. 2022 benchmarking of KSSL MIR spectral library

# Properties supported for library-based prediction
# Selected based on MIR prediction accuracy (Category A/B, R² > 0.70)
LIBRARY_PROPERTIES <- c(
  ## Texture (complete triangle)
  "clay",              # R² = 0.84, RPIQ = 4.08
  "sand",              # R² = 0.80, RPIQ = 3.61
  "silt",              # R² = 0.70, RPIQ = 2.75

  ## Carbon forms
  "total_carbon",      # R² = 0.95, RPIQ = 5.64 (BEST!)
  "oc",                # R² = 0.92, RPIQ = 3.84
  "carbonate",         # R² = 0.97, RPIQ = 8.25 (HIGHEST!)

  ## Nitrogen
  "total_nitrogen",    # R² = 0.85, RPIQ = 2.50

  ## pH and CEC
  "ph",                # R² = 0.85, RPIQ = 4.55 (H2O)
  "cec",               # R² = 0.91, RPIQ = 5.03

  ## Base Cations (NH4OAc Extractable)
  "calcium",           # R² = 0.91, RPIQ = 3.98
  "magnesium",         # R² = 0.78, RPIQ = 2.78
  "potassium",         # R² = 0.81, RPIQ = 3.47
  "sodium",            # R² = 0.85, RPIQ = 3.50

  ## Major Elements
  "iron_total",        # R² = 0.90, RPIQ = 3.79
  "aluminum_total"     # R² = 0.86, RPIQ = 3.94
)

# Initial optimal configurations for library prediction (v0.1)
# Based on: literature (Ng et al. 2022), current covariates-soil.R, expert intuition
# Will be updated with empirical testing and replaced by benchmarking research
OPTIMAL_CONFIGS_V1 <- tibble::tribble(
  ~property,        ~rank, ~model,          ~preprocessing, ~transformation, ~feature_selection, ~expected_r2, ~notes,

  ## CLAY (mineralogy-driven, strong spectral features)
  "clay",           1,     "random_forest", "snv_deriv1",   "none",         "pca",            0.84,        "Ng2022: Cat A, mineral peaks clear",
  "clay",           2,     "cubist",        "snv",          "none",         "correlation",    0.82,        "Current default, rule-based",
  "clay",           3,     "xgboost",       "snv_deriv1",   "none",         "cars",           0.82,        "Gradient boosting alternative",
  "clay",           4,     "plsr",          "sg",           "none",         "none",           0.78,        "Baseline comparison",
  "clay",           5,     "svm_rbf",       "snv",          "none",         "pca",            0.80,        "Kernel method",

  ## SAND (negatively correlated with clay, similar spectral basis)
  "sand",           1,     "random_forest", "snv_deriv1",   "none",         "pca",            0.80,        "Ng2022: Cat A",
  "sand",           2,     "cubist",        "snv",          "none",         "correlation",    0.78,        "Mirrors clay approach",
  "sand",           3,     "xgboost",       "snv_deriv1",   "none",         "cars",           0.78,        "Tree-based ensemble",
  "sand",           4,     "elastic_net",   "snv",          "none",         "none",           0.75,        "Regularized linear",

  ## SILT (residual of clay+sand, moderate accuracy)
  "silt",           1,     "random_forest", "snv",          "none",         "pca",            0.70,        "Ng2022: Cat B",
  "silt",           2,     "cubist",        "snv_deriv1",   "none",         "correlation",    0.68,        "Rule-based",
  "silt",           3,     "xgboost",       "sg",           "none",         "pca",            0.68,        "Boosting",

  ## TOTAL CARBON (BEST overall predictor, direct chromophore)
  "total_carbon",   1,     "plsr",          "snv_deriv1",   "none",         "none",           0.95,        "Ng2022: BEST! PLSR standard for C",
  "total_carbon",   2,     "random_forest", "snv",          "none",         "pca",            0.94,        "Tree-based alternative",
  "total_carbon",   3,     "cubist",        "snv_deriv1",   "none",         "correlation",    0.93,        "Rule-based, interpretable",
  "total_carbon",   4,     "elastic_net",   "sg",           "log",          "none",           0.92,        "Linear with transform",
  "total_carbon",   5,     "xgboost",       "snv_deriv1",   "none",         "cars",           0.93,        "Gradient boosting",

  ## ORGANIC CARBON (SOC - also excellent, direct chromophore)
  "oc",             1,     "plsr",          "snv_deriv1",   "none",         "none",           0.92,        "Ng2022: Cat A, PLSR gold standard",
  "oc",             2,     "random_forest", "snv",          "none",         "pca",            0.90,        "Non-linear alternative",
  "oc",             3,     "cubist",        "snv_deriv1",   "none",         "correlation",    0.89,        "Current covariate default",
  "oc",             4,     "elastic_net",   "sg",           "log",          "none",           0.88,        "Regularized, log transform",
  "oc",             5,     "xgboost",       "snv_deriv1",   "none",         "pca",            0.90,        "Boosting",

  ## CARBONATE (SIC - HIGHEST accuracy, strong peaks at 1450, 880, 700 cm-1)
  "carbonate",      1,     "plsr",          "sg",           "none",         "none",           0.97,        "Ng2022: HIGHEST! Direct peaks",
  "carbonate",      2,     "random_forest", "snv",          "none",         "pca",            0.96,        "Tree-based",
  "carbonate",      3,     "cubist",        "snv_deriv1",   "none",         "correlation",    0.95,        "Rule-based",
  "carbonate",      4,     "xgboost",       "sg",           "sqrt",         "pca",            0.96,        "Boosting, sqrt for skew",

  ## TOTAL NITROGEN (correlated with OC, moderate-high accuracy)
  "total_nitrogen", 1,     "plsr",          "snv_deriv1",   "none",         "none",           0.85,        "Ng2022: Cat B, amide peaks",
  "total_nitrogen", 2,     "random_forest", "snv",          "none",         "pca",            0.83,        "Tree-based",
  "total_nitrogen", 3,     "cubist",        "snv_deriv1",   "none",         "correlation",    0.82,        "Rule-based",
  "total_nitrogen", 4,     "elastic_net",   "sg",           "log",          "none",           0.80,        "Linear with transform",

  ## pH (indirect correlation with OC, carbonates, exchangeable cations)
  "ph",             1,     "random_forest", "snv",          "none",         "pca",            0.85,        "Ng2022: Cat A (CaCl2 best)",
  "ph",             2,     "cubist",        "snv_deriv1",   "none",         "correlation",    0.83,        "Current default",
  "ph",             3,     "xgboost",       "snv",          "none",         "cars",           0.83,        "Boosting",
  "ph",             4,     "svm_rbf",       "snv_deriv1",   "none",         "pca",            0.82,        "Kernel method",
  "ph",             5,     "plsr",          "sg",           "none",         "none",           0.80,        "Linear baseline",

  ## CEC (correlated with clay mineralogy and OC, excellent accuracy)
  "cec",            1,     "random_forest", "snv_deriv1",   "none",         "pca",            0.91,        "Ng2022: Cat A",
  "cec",            2,     "cubist",        "snv",          "none",         "correlation",    0.89,        "Rule-based",
  "cec",            3,     "xgboost",       "snv_deriv1",   "none",         "cars",           0.90,        "Boosting",
  "cec",            4,     "plsr",          "sg",           "none",         "none",           0.87,        "Linear",

  ## CALCIUM (extractable, high accuracy, correlated with CEC)
  "calcium",        1,     "random_forest", "snv",          "none",         "pca",            0.91,        "Ng2022: Cat A",
  "calcium",        2,     "cubist",        "snv_deriv1",   "none",         "correlation",    0.89,        "Rule-based",
  "calcium",        3,     "xgboost",       "snv",          "none",         "pca",            0.90,        "Boosting",
  "calcium",        4,     "plsr",          "sg",           "sqrt",         "none",           0.87,        "PLSR, sqrt for skew",

  ## MAGNESIUM (extractable, good accuracy)
  "magnesium",      1,     "random_forest", "snv_deriv1",   "none",         "pca",            0.78,        "Ng2022: Cat B",
  "magnesium",      2,     "cubist",        "snv",          "none",         "correlation",    0.76,        "Current approach",
  "magnesium",      3,     "xgboost",       "snv_deriv1",   "none",         "cars",           0.76,        "Boosting",

  ## POTASSIUM (extractable, good accuracy, K peaks at mineral bands)
  "potassium",      1,     "random_forest", "snv",          "none",         "pca",            0.81,        "Ng2022: Cat B",
  "potassium",      2,     "cubist",        "snv_deriv1",   "none",         "correlation",    0.79,        "Rule-based",
  "potassium",      3,     "xgboost",       "snv",          "none",         "cars",           0.80,        "Boosting",

  ## SODIUM (extractable, good accuracy)
  "sodium",         1,     "random_forest", "snv_deriv1",   "none",         "pca",            0.85,        "Ng2022: Cat B",
  "sodium",         2,     "cubist",        "snv",          "none",         "correlation",    0.83,        "Current approach",
  "sodium",         3,     "xgboost",       "snv_deriv1",   "none",         "pca",            0.84,        "Boosting",

  ## IRON TOTAL (major element, excellent accuracy, Fe oxide peaks clear)
  "iron_total",     1,     "random_forest", "snv",          "none",         "pca",            0.90,        "Ng2022: Cat A, Fe peaks 1430, 2520",
  "iron_total",     2,     "cubist",        "snv_deriv1",   "none",         "correlation",    0.88,        "Rule-based",
  "iron_total",     3,     "xgboost",       "snv",          "none",         "cars",           0.89,        "Boosting",
  "iron_total",     4,     "plsr",          "sg",           "none",         "none",           0.86,        "Linear baseline",

  ## ALUMINUM TOTAL (major element, excellent accuracy, mineral component)
  "aluminum_total", 1,     "random_forest", "snv_deriv1",   "none",         "pca",            0.86,        "Ng2022: Cat A",
  "aluminum_total", 2,     "cubist",        "snv",          "none",         "correlation",    0.84,        "Rule-based",
  "aluminum_total", 3,     "xgboost",       "snv_deriv1",   "none",         "cars",           0.85,        "Boosting",
  "aluminum_total", 4,     "plsr",          "sg",           "none",         "none",           0.82,        "Linear"
)
