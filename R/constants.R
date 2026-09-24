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

# Predict-time namespace requirements per model (#65), keyed by the same short
# names as MODEL_SPECS and VALID_MODELS. Not derivable from MODEL_SPECS$engine
# alone: cubist_rules() needs `rules` (registers the parsnip spec) alongside
# `Cubist` (the model implementation), and boost_tree(engine = "lightgbm")
# needs `bonsai` (registers the engine) alongside `lightgbm`.
#
# Mostly an AVAILABILITY PREFLIGHT, not a dispatch fix: once workflows and
# parsnip are loaded (ensure_predict_namespaces() always requires both, along
# with recipes), parsnip's own predict.model_fit() dispatch loads each
# model's engine package itself via load_libs() — horizons does not need to
# preload it for dispatch to work. Checking here instead means a genuinely
# missing (Suggested) engine package aborts with an actionable,
# package-naming message up front, rather than surfacing as an obscure
# failure partway through prediction. The one dispatch-critical case is
# `ranger` for the UQ quantile forest: stats::predict() there dispatches on a
# bare ranger::ranger object directly, with no parsnip/workflows layer to
# auto-load it, so a missing ranger namespace genuinely breaks dispatch, not
# just availability (see the ranger branch in ensure_predict_namespaces()).
# Read by ensure_predict_namespaces() (R/pipeline-predict.R).
MODEL_PREDICT_PACKAGES <- list(
  rf          = "ranger",
  cubist      = c("rules", "Cubist"),
  xgboost     = "xgboost",
  plsr        = c("plsmod", "mixOmics"),
  elastic_net = "glmnet",
  svm_rbf     = "kernlab",
  mlp         = "nnet",
  lightgbm    = c("bonsai", "lightgbm"),
  mars        = "earth"
)

# Predict-time packages that are not on CRAN, so an install.packages() hint
# would be actively wrong. mixOmics (the plsr engine) is on Bioconductor; see
# README.md's Dependencies section for the same guidance. Read by
# predict_package_install_hint() (R/pipeline-predict.R).
PREDICT_PACKAGE_INSTALL_HINT <- list(
  mixOmics = 'BiocManager::install("mixOmics")'
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

# Metrics evaluate() and fit() accept as `metric`, the ones configs are ranked
# by (on their cv_<metric> column)
VALID_RANK_METRICS <- c("rpd", "rsq", "rmse", "rrmse", "ccc", "mae")

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

### Training share of the train/test split, for evaluate()'s split and fit()'s
### Split F alike.
SPLIT_PROP <- 0.8

### Share of Split F's training part that fit() keeps as the fit rows when UQ
### or AD is on; the rest is the calibration set the two share.
CALIB_PROP <- 0.8

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

## Parallel Worker Contract ----------------------------------------------------

# The keys evaluate()'s parallel branch sends to evaluate_config_worker().
# Assembled in one place and asserted in the worker, because a mis-keyed entry
# arrives as NULL and two of them fail SILENTLY rather than erroring: a NULL
# `seed` makes set.seed(NULL) reseed from the clock (the run succeeds and is not
# reproducible), and a NULL `grid_size` makes tune_grid() invent its own grid.

SHARED_ARG_NAMES <- c(
  "data",
  "resample_idx",
  "configs",
  "role_map",
  "grid_size",
  "bayesian_iter",
  "prune",
  "prune_threshold",
  "seed",
  "checkpoint_dir",
  "pkg_version"
)

# Vocabulary of evaluate(parallelize_over = ). "both" (a nested plan) is
# deliberately absent for v1; see resolve_parallel_axis().
PARALLELIZE_OVER_VALUES <- c("auto", "configs", "resamples")

# Scoring schema stamped into every evaluate() result row and checked when
# checkpoints are resumed. Bump it whenever anything that moves the cv_*
# panel changes, so rows scored under different regimes are never ranked
# against each other. History:
#   1  pre-2026-09-15: tune parallel_over = "everything" (recipe re-prepped
#      per candidate x fold, so each candidate drew its own feature selection).
#   2  2026-09-15: parallel_over = "resamples" (one prep per fold shared by
#      all candidates); seed re-pinned before every stochastic stage.
# Rows without the column are schema 1.
SCORING_SCHEMA <- 2L

# future.globals.maxSize for the configs-axis dispatch, set by evaluate() for
# the duration of the call. Declared rather than inherited so a payload
# regression fails loudly with future's "size of the globals ... exceeds"
# error instead of R's opaque "long vectors not supported yet". Measured
# 2026-09-14 on 17,788 x 1,701 (KSSL clay at 2 cm-1): 417.6 MB per worker
# after the three serialization fixes. 1 GiB is ~2.4x headroom. The footprint
# tests in test-evaluate-parallel.R are the upstream guard.
EVAL_WORKER_PAYLOAD_LIMIT <- 1 * 1024^3

# select_training(): cap on the components retained when ncomp is given as
# a proportion of variance. 100 was the experiments' cap (EXP_LOCAL in
# dev/experiments/2026-09-local-strategy/00-config.R); the KSSL pool at 99 %
# retains far fewer.
SELECT_PCA_MAX_COMP <- 100L

# select_training(): a pool row is a target's twin when its distance to that
# target is below this fraction of the reference distance, the 75th
# percentile of the target's nearest twin_reference_width() measured rows
# (SELECT_TWIN_REF below, capped at a quarter of the measured rows), the same
# under every scope. An exact match (distance 0) is always a twin. The rule is
# neighbourhood-relative rather than first-to-second-nearest so that a pool
# holding two or three replicate scans of the same sample is caught: with a
# gap rule every replicate distance is tiny, the gap never opens, and nothing
# is flagged (2026-09-21 review, leakage critical 1). PLACEHOLDER: the spec
# says to set this on the KSSL replicate scans. The value was carried over
# from the median-of-k rule this one replaced and has been calibrated against
# neither, so its reach is unmeasured.
SELECT_TWIN_RATIO <- 0.05

# select_training(): columns of the distance matrix that define the twin
# rule's reference distance. The reference has to be wider than any plausible
# replicate cluster, or the replicates set the very number they are being
# measured against: with four self-rows in a neighbourhood of five, the 75th
# percentile of those five distances is itself a replicate distance and one
# row is flagged instead of four. A dozen scans of one sample is the most a
# real library holds, so 50 leaves the reference untouched by them at any k
# a caller would ask for (2026-09-21 re-review, leakage). The reference also
# has to stay local: the twin threshold is a fraction of it, so a reference
# taken over most of the pool is a pool-wide spread and 5 % of that flags
# ordinary nearest neighbours. twin_reference_width() therefore caps it at a
# quarter of the measured rows, which binds only on pools under 200, and every
# scope, global included, takes its width from there.
SELECT_TWIN_REF <- 50L

# select_training(): components whose standard deviation falls below this
# fraction of PC1's leave the similarity space before distances are measured.
# Mahalanobis whitening divides each component by its sd, so a variance-chosen
# tail one to three orders below PC1 carries the same weight in the distance
# as the dominant chemical axes, and its eigenvectors are themselves unstable
# under small changes in the row set (2026-09-21 review, spectroscopy
# critical). Measured on a 20,000-row KSSL sample at 4 cm-1 with the water
# mask: the 0.99 variance rule retained 33 components, sd/PC1 at PC33 = 0.038,
# a 26x amplification; a floor of 0.10 keeps 13 components, 0.05 keeps 27.
# Set to 0 to disable the floor and recover the pre-2026-09-21 behaviour.
SELECT_SDEV_FLOOR <- 0.10

## validate() Heuristics --------------------------------------------------------

# validate()'s P010 check: cubist without dimension reduction on a large
# table. Cubist fits a linear model in every rule, so its cost grows sharply
# with predictor count in a way rf/ranger and most other MODEL_SPECS entries
# do not. Measured 2026-09 (#40): cubist + snv on the KSSL clay library at
# 4 cm-1 (14,228 rows x 851 predictors = ~12.1M cells) did not finish a
# single tune task in 29.5 minutes across 25 tasks (5 folds x grid 5), with
# 8 workers pegged at 99% CPU; the same config with feature_selection = "pca"
# (step_pca(threshold = 0.995)) finished in 368 s (test RPD 3.82), matching
# OSSL's published SNV -> PCA(120) -> Cubist pipeline (Safanelli et al. 2025,
# PLOS ONE 20(1):e0296545). rf/ranger finished the same 851-predictor table
# in minutes. 2e6 is a heuristic floor set well below the ~12.1M-cell
# measured failure point, not a calibrated benchmark of where cubist starts
# to struggle — it exists to warn before the table gets anywhere near the
# point that didn't finish, not to mark the exact boundary of what will.
CUBIST_MAX_CELLS <- 2e6
