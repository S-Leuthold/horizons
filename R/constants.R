# R/constants.R
# Central location for package-wide constants used in validation and
# configuration.

## Model Types ----------------------------------------------------------------

# Valid model types — short names used in configure() and config grids
VALID_MODELS <- c(
  "rf",
  "cubist",
  "xgboost",
  "plsr",
  "elastic_net",
  "svm_rbf",
  "mlp",
  "lightgbm",
  "mars"
)

# Model specifications lookup — maps short names to tidymodels constructors
# Used by evaluate() to instantiate parsnip model specs
MODEL_SPECS <- list(
  rf          = list(fn = "rand_forest",  engine = "ranger"),
  cubist      = list(fn = "cubist_rules", engine = "Cubist"),
  xgboost     = list(fn = "boost_tree",   engine = "xgboost"),
  plsr        = list(fn = "pls",          engine = "mixOmics"),
  elastic_net = list(fn = "linear_reg",   engine = "glmnet"),
  svm_rbf     = list(fn = "svm_rbf",      engine = "kernlab"),
  mlp         = list(fn = "mlp",          engine = "nnet"),
  lightgbm    = list(fn = "boost_tree",   engine = "lightgbm"),
  mars        = list(fn = "mars",         engine = "earth")
)

# Human-readable model names for CLI tree output
MODEL_DISPLAY_NAMES <- c(
  rf          = "Random Forest",
  cubist      = "Cubist",
  xgboost     = "XGBoost",
  plsr        = "PLS",
  elastic_net = "Elastic Net",
  svm_rbf     = "SVM-RBF",
  mlp         = "Neural Network",
  lightgbm    = "LightGBM",
  mars        = "MARS"
)

## Transformations ------------------------------------------------------------

# Valid response transformations
VALID_TRANSFORMATIONS <- c(
  "none",
  "log",
  "log10",
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
  "correlation",
  "boruta",
  "cars"
)

## Metrics --------------------------------------------------------------------

# Metrics where a higher value is better (used for ranking configs). All other
# metrics (rmse, rrmse, mae, ...) are lower-is-better.
HIGHER_BETTER_METRICS <- c("rpd", "rsq", "ccc")

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

## fit() Parameters ------------------------------------------------------------

DEFAULT_FINAL_BAYES_ITER <- 25L
N_CALIB_MIN              <- 30L
DEFAULT_UQ_LEVEL         <- 0.90
UQ_QUANTILE_TREES        <- 500L
WARMSTART_GRID_SIZE      <- 25L

## Applicability Domain (AD) ----------------------------------------------------

### Coverage level for the out-of-domain (OOD) cutoff: a sample is flagged OOD
### when its (squared) Mahalanobis distance exceeds the level-th quantile of the
### held-out calibration distances. 0.99 = flag the most extreme ~1% as OOD.
DEFAULT_AD_LEVEL <- 0.99

### Minimum training rows to estimate AD centroid + shrinkage covariance. Below
### this the covariance is dominated by the shrinkage target regardless of data;
### AD is disabled rather than returning a meaningless domain.
N_AD_TRAIN_MIN <- 10L

## Ensemble ----------------------------------------------------------------------

### Default seed for ensemble() — the meta-learner's tuning/OOF folds. Ensemble
### UQ draws its calibration partition at this seed + 1000. Centralized so the
### verb default, the weighted-engine default, and fit_ensemble_uq()'s legacy
### fallback cannot drift independently.
DEFAULT_ENSEMBLE_SEED <- 307L

## Prediction Guardrails --------------------------------------------------------

### Winsorization margin for the deploy-time response upper bound: predictions
### are clamped to max(training outcome) * margin. Permits modest extrapolation
### beyond the observed range while catching physically impossible blow-ups
### (e.g. an unconstrained log-scale prediction exp()-inflating to 257 g/kg).
RESPONSE_BOUND_MARGIN <- 1.5

## DAYMET Constants ------------------------------------------------------------

DAYMET_RESOLUTION_DEG <- 1/24
DAYMET_TIMEOUT        <- 60
