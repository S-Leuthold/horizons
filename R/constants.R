# R/constants.R
# Central location for package-wide constants used in validation and
# configuration.

## Model Types ----------------------------------------------------------------

# Everything horizons declares about each model, one row per short name (the
# names configure() takes, VALID_MODELS). A new model is a row here and its
# cells in CONFIG_COST_SECONDS, with registration code only for an engine
# parsnip does not have (the PLS engine, R/pls-engine.R). The fields:
#
# - `fn`, `engine`: parsnip's model, by name, and the engine.
# - `label`: the model's name in the progress trees and reports
#   (model_label()).
# - `packages`: what fitting and predicting with it needs, checked at every
#   verb's door (needed_packages(), R/utils-models.R), so a missing engine is
#   refused once, by name, before any work. Not derivable from the engine
#   alone: cubist_rules() needs `rules` (which registers the parsnip spec)
#   beside `Cubist`, and lightgbm needs `bonsai` (which registers the engine)
#   beside `lightgbm`.
# - `seed`: TRUE when the engine takes a seed, which each stage gives it
#   (seed_engine(), R/utils-rng.R) rather than letting parsnip or bonsai draw
#   one from the stream.
# - `isolate`: the promises the engine breaks when its fits share a process,
#   each named with the upstream report about it. Such a model is fitted
#   through its engine's isolated twin, each fit in a process of its own
#   (model_engine(), register_isolated_engines(), R/isolated-engines.R),
#   unless control_horizons(isolate = FALSE). Cubist's C code keeps its
#   generator's stream behind the neighbour step's distance, so a fit's
#   predictions depend on the fits before it (`repeats`), and leaks two
#   (p + 2)^2 matrices a fit (`memory`).
#
# An engine's traits are read through engine_spec() (R/utils-models.R), which
# finds the row of an isolated twin's engine too.
MODEL_SPECS <- list(

  rf = list(
    fn       = "rand_forest",
    engine   = "ranger",
    label    = "Random Forest",
    packages = "ranger",
    seed     = TRUE
  ),

  cubist = list(
    fn       = "cubist_rules",
    engine   = "Cubist",
    label    = "Cubist",
    packages = c("rules", "Cubist"),
    isolate  = c(repeats = "topepo/Cubist#6", memory = "topepo/Cubist#8")
  ),

  xgboost = list(
    fn       = "boost_tree",
    engine   = "xgboost",
    label    = "XGBoost",
    packages = "xgboost",
    seed     = TRUE
  ),

  plsr = list(
    fn       = "pls",
    engine   = "pls",
    label    = "PLS",
    packages = "pls"
  ),

  elastic_net = list(
    fn       = "linear_reg",
    engine   = "glmnet",
    label    = "Elastic Net",
    packages = "glmnet"
  ),

  svm_rbf = list(
    fn       = "svm_rbf",
    engine   = "kernlab",
    label    = "SVM-RBF",
    packages = "kernlab"
  ),

  mlp = list(
    fn       = "mlp",
    engine   = "nnet",
    label    = "Neural Network",
    packages = "nnet"
  ),

  lightgbm = list(
    fn       = "boost_tree",
    engine   = "lightgbm",
    label    = "LightGBM",
    packages = c("bonsai", "lightgbm"),
    seed     = TRUE
  ),

  mars = list(
    fn       = "mars",
    engine   = "earth",
    label    = "MARS",
    packages = "earth"
  )

)

# The short model names configure() takes
VALID_MODELS <- names(MODEL_SPECS)

# The packages each feature-selection method's recipe step calls when it is
# trained, keyed by VALID_FEATURE_SELECTION: the step's required_pkgs() less
# horizons. Boruta is the one in Suggests. A trained step's bake() calls none
# of them, so predict() does not need them. Read by needed_packages()
# (R/utils-models.R).
SELECTION_PACKAGES <- list(
  none        = character(0),
  pca         = character(0),
  correlation = "zoo",
  boruta      = c("Boruta", "ranger"),
  cars        = "pls"
)

# The most components the plsr model tunes over (#216). The runners cap it
# further at the predictors the recipe keeps and the smallest fold's rows less
# one (cap_pls_components(), R/pls-engine.R). MIR models often want more than
# the handful a default range allows, and 30 keeps a grid's points where
# models land. Not a field of MODEL_SPECS: CARS caps its components with it
# too (R/step-select-cars.R).
PLS_MAX_COMP <- 30L

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

# Recipe settings configure() records and build_recipe() applies to every
# config (#62): the Savitzky-Golay window in grid points, and the share of
# variance step_pca() keeps. These are configure()'s defaults, and what the
# recipe ran before the settings were exposed.
DEFAULT_SG_WINDOW     <- 9L
DEFAULT_PCA_THRESHOLD <- 0.995

# The narrowest Savitzky-Golay window every preprocessing method can run:
# deriv2 and snv_deriv2 fit a cubic, and prospectr::savitzkyGolay() needs the
# window wider than the polynomial order. See is_valid_sg_window().
SG_WINDOW_MIN <- 5L

# The selection steps drop a band before clustering when its variance is at
# most this fraction of the median band's (drop_constant_bands()): a standard
# deviation under a ten-thousandth of the typical band's, so its values differ
# by round-off rather than by signal. The cutoff is relative because
# correlation is scale-free and preprocessing rescales bands by orders of
# magnitude: a second derivative is about a millionth of an absorbance. On
# KSSL spectra under every preprocessing, at Savitzky-Golay windows of 9 and
# 21, the lowest band sits at 7e-4 of the median, four orders of magnitude
# above the cutoff.
NEAR_CONSTANT_RATIO <- 1e-8

# The fewest bands each selection step selects from; with fewer it keeps them
# and warns that it was skipped (selection_warning()). CARS and Boruta need
# two to choose between, and Boruta two to cluster; the correlation filter
# needs five, for the three windows of three bands its threshold (their mean
# plus one standard deviation) needs to select any.
SELECTION_MIN_BANDS <- c(step_select_cars        = 2L,
                         step_select_boruta      = 2L,
                         step_select_correlation = 5L)

## fit() Parameters ------------------------------------------------------------

DEFAULT_FINAL_BAYES_ITER <- 25L
N_CALIB_MIN              <- 30L
DEFAULT_UQ_LEVEL         <- 0.90
UQ_QUANTILE_TREES        <- 500L

### The quantile forest's minimum node size grows with its rows: the rows over
### UQ_QUANTILE_NODE_DIVISOR, rounded up, and never below UQ_QUANTILE_MIN_NODE
### (ranger's default, so the forest is unchanged up to 1,250 rows). See
### uq_min_node_size().
UQ_QUANTILE_MIN_NODE     <- 5L
UQ_QUANTILE_NODE_DIVISOR <- 250L
WARMSTART_GRID_SIZE      <- 25L

### The degradation check bootstraps the test RPD and flags a config only when
### the whole DEGRADATION_LEVEL interval lies below the CV mean RPD. Below
### DEGRADATION_MIN_N test rows a bootstrap interval means little, so nothing
### is flagged.
DEGRADATION_BOOT_N       <- 2000L
DEGRADATION_LEVEL        <- 0.95
DEGRADATION_MIN_N        <- 10L

### Training share of the train/test split, for evaluate()'s split and fit()'s
### Split F alike.
SPLIT_PROP <- 0.8

### Share of Split F's training part that fit() keeps as the fit rows when UQ
### or AD is on; the rest is the calibration set the two share.
CALIB_PROP <- 0.8

### How every stratified draw bins the outcome: rsample's `breaks` and `pool`,
### at rsample's own defaults. Passed to the draws and to the
### outcome_stratifies() check alike, so what the console says about a draw
### cannot drift from the draw if rsample's defaults change (#91). Doubles, as
### rsample's formals are, so the split attributes tune reads are unchanged.
STRATA_BREAKS <- 4
STRATA_POOL   <- 0.1

## Applicability Domain (AD) ----------------------------------------------------

### Coverage level for the out-of-domain (OOD) cutoff: a sample is flagged OOD
### when its (squared) Mahalanobis distance exceeds the level-th quantile of the
### held-out calibration distances. 0.99 = flag the most extreme ~1% as OOD.
DEFAULT_AD_LEVEL <- 0.99

### Minimum training rows to estimate AD centroid + shrinkage covariance. Below
### this the covariance is dominated by the shrinkage target regardless of data;
### AD is disabled rather than returning a meaningless domain.
N_AD_TRAIN_MIN <- 10L

## Seeds -------------------------------------------------------------------------

### The largest magnitude a verb's seed may have: set.seed() takes a whole
### number within R's integer range unchanged. fit()'s n_best, taken as an
### integer too, is held to it. A double, as rlang's checkers take their
### bounds.
SEED_MAX <- as.double(.Machine$integer.max)

### The version of the rule that turns a run's seed into each unit's seed
### (unit_seed(), R/utils-rng.R). Recorded on the object and in the settings a
### checkpoint resumes under. Bump it whenever a unit would get another seed
### from the same root: the hash or its key, a unit's name, or the kinds.
SEED_RULE <- 1L

## Prediction Guardrails --------------------------------------------------------

### Winsorization margin for the deploy-time response upper bound: predictions
### are clamped to max + (margin - 1) * (max - anchor), where the anchor is the
### lower bound of outcome_range, or the training minimum when that bound is
### -Inf (compute_response_bound()). Under the default range the anchor is 0,
### so the bound is max(training outcome) * margin, as it has always been.
### Permits modest extrapolation beyond the observed range while catching
### physically impossible blow-ups (e.g. an unconstrained log-scale prediction
### exp()-inflating to 257 g/kg).
RESPONSE_BOUND_MARGIN <- 1.5

### The physical range of the outcome when configure() is not told otherwise
### (#76). Every prediction the package scores or serves is clamped to
### configure()'s outcome_range; the default is the non-negative range the
### package has always assumed.
DEFAULT_OUTCOME_RANGE <- c(0, Inf)

## Parallel Worker Contract ----------------------------------------------------

# The keys evaluate()'s parallel branch sends to evaluate_config_worker().
# Assembled in one place and asserted in the worker, because a mis-keyed entry
# arrives as NULL and one fails SILENTLY rather than erroring: a NULL
# `grid_size` makes tune_grid() invent its own grid.

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
  "sg_window",
  "pca_threshold",
  "outcome_range",
  "data_fp",
  "settings",
  "checkpoint_dir",
  "keep_traces",
  "isolate",
  "pkg_version"
)

# What one mirai daemon on this machine needs for a run, in bytes, by the
# processes it holds: the daemon itself, and, when the run fits a model whose
# fits are isolated (MODEL_SPECS' `isolate`), that fit's process beside it.
# Each is `base` plus `per_byte` times the run's data, its rows times its
# predictors times 8 bytes (daemon_need(), R/utils-parallel.R). Each line
# goes through two measured points, at 1,047 rows by 1,701 predictors (14.2 MB
# of data) and at 10,341 rows by 1,701 (140.7 MB), and is rounded up (`base`
# to 0.05 GB, `per_byte` to 0.5), so it lies above both:
#
# - The daemon: its peak less its size once connected (0.08 GB), the largest
#   measured at each size: 1.58 GB at 1,047 rows (a grid of every model on
#   eight daemons, through evaluate() and fit()) and 3.08 GB at 10,341 rows
#   (elastic net and xgboost configurations, in fit(); Cubist configurations
#   with their fits isolated peaked at 2.46 GB).
# - The isolated fit's process: its peak, the largest measured, a Cubist fit
#   at 100 committees: 0.64 GB on a 750-row fold of 1,691 predictors and
#   1.25 GB on a 7,444-row fold after correlation selection (about 360
#   predictors).
#
# A run of other models, selections or committees can need more or less, so
# the warning calls the estimate approximate.
DAEMON_MEMORY <- list(
  daemon   = c(base = 1.45e9, per_byte = 12),
  isolated = c(base = 0.6e9, per_byte = 5)
)

# How the message starts when a process horizons started for one call
# (run_isolated(), R/utils-parallel.R) ends without its result. tune keeps an
# error raised inside a fit only as its text, so check_tune_result() finds a
# process's death in tune's notes by this phrase.
PROCESS_DIED <- "The process horizons started for"

# Seconds between the configs axis's polls of the tasks in flight and the
# daemons connected (await_round(), R/utils-parallel.R). mirai's info() costs
# microseconds, so the poll is cheap against configurations of seconds to
# hours.
DISPATCH_POLL_SECONDS <- 0.1

# Seconds one configuration took, by model and feature selection, from which
# the configs axis starts the longest configurations first (dispatch_order(),
# R/utils-parallel.R) and monitor_evaluate() weights its ETA (config_costs(),
# recorded in the manifest). Measured whole, preps and Bayesian candidates
# included, on 0.12.0's coverage grid: 835 training rows, 1,701 predictors,
# snv_deriv1, 10 folds, a grid of 10 and 15 Bayesian iterations, one
# configuration per worker. lightgbm failed every configuration of that run,
# so its cells are xgboost's, which its fits are within 20% of. Every
# configuration in a run shares its rows and predictors, so only the cells
# relative to one another are used (the order, and the ETA's weights), and
# the table is not scaled by them. A new model or selection adds its cells
# (test-utils-parallel.R holds the table to VALID_MODELS x
# VALID_FEATURE_SELECTION). Kept apart from MODEL_SPECS: it is one
# measurement over models and selections, documented and refitted whole.
CONFIG_COST_SECONDS <- c(
  "rf/none"           = 12861, "rf/pca"          = 672, "rf/correlation"          = 1974,
  "rf/boruta"         = 11089, "rf/cars"         = 5379,
  "cubist/none"       = 18772, "cubist/pca"      = 679, "cubist/correlation"      = 4634,
  "cubist/boruta"     = 17954, "cubist/cars"     = 13245,
  "xgboost/none"      = 6273,  "xgboost/pca"     = 357, "xgboost/correlation"     = 1807,
  "xgboost/boruta"    = 11910, "xgboost/cars"    = 6310,
  "plsr/none"         = 93,    "plsr/pca"        = 162, "plsr/correlation"        = 159,
  "plsr/boruta"       = 2820,  "plsr/cars"       = 632,
  "elastic_net/none"  = 72,    "elastic_net/pca" = 114, "elastic_net/correlation" = 157,
  "elastic_net/boruta" = 3854, "elastic_net/cars" = 669,
  "svm_rbf/none"      = 344,   "svm_rbf/pca"     = 175, "svm_rbf/correlation"     = 167,
  "svm_rbf/boruta"    = 3807,  "svm_rbf/cars"    = 731,
  "mlp/none"          = 2377,  "mlp/pca"         = 381, "mlp/correlation"         = 3037,
  "mlp/boruta"        = 5805,  "mlp/cars"        = 7038,
  "lightgbm/none"     = 6273,  "lightgbm/pca"    = 357, "lightgbm/correlation"    = 1807,
  "lightgbm/boruta"   = 11910, "lightgbm/cars"   = 6310,
  "mars/none"         = 364,   "mars/pca"        = 16,  "mars/correlation"        = 52,
  "mars/boruta"       = 604,   "mars/cars"       = 289
)

# Vocabulary of control_horizons(parallelize_over = ). "both" is deliberately
# absent for v1; control_horizons() refuses it with a hint.
PARALLELIZE_OVER_VALUES <- c("auto", "configs", "resamples")

# Scoring schema stamped into every evaluate() result row and checked when
# checkpoints are resumed. Bump it whenever anything that moves the cv_*
# panel changes, so rows scored under different regimes are never ranked
# against each other. History:
#   1  pre-2026-09-15: tune parallel_over = "everything" (recipe re-prepped
#      per candidate x fold, so each candidate drew its own feature selection).
#   2  2026-09-15: parallel_over = "resamples" (one prep per fold shared by
#      all candidates); seed re-pinned before every stochastic stage.
#   3  2026-10-15: every row carries the class of its failure (NA when it
#      succeeded), so rows checkpointed without it are re-evaluated.
#   4  2026-10-16: each configuration runs as one unit: a failed row names
#      its step, its root class and, under keep_traces, its backtrace, and a
#      recipe step's failure inside tune is reproduced with its class. A
#      candidate scored on fewer folds than were drawn is not chosen, a cv_*
#      mean is kept only over every fold (#101), and a failed final fit
#      fails the configuration (#102).
#   5  2026-10-21: an untransformed response's tuning metrics score the
#      estimate clamped to outcome_range, as every other score is (#104).
#   6  2026-11-09: every draw is seeded from the run's seed and its unit's
#      name (SEED_RULE 1), so the split, the folds and each stage move.
SCORING_SCHEMA <- 6L

# Schema of eval_manifest.rds, which evaluate() writes at the start of every
# run with an output_dir and monitor_evaluate() reads. Schema 4 (#42) records
# the axis and its backend (`plan`: "sequential" or "mirai", and `workers`,
# the daemons connected as the run began), the training-data fingerprint and
# its fields,
# and the tuning settings. Schema 5 adds `config_cost`, each configuration's
# cell of CONFIG_COST_SECONDS in `config_ids`' order, which weights the
# monitor's ETA. Bump it when a field the monitor reads changes; the
# monitor refuses a manifest of any other schema.
EVAL_MANIFEST_SCHEMA <- 5L

# evaluate()'s smallest cross-validation assessment fold, in rows, below which
# it warns (quality) that the cross-validated metrics rest on few rows. A
# warning only, so no override: small samples run. Judgement, not a published
# standard; an RMSE over fewer than ten rows is a weak estimate.
SMALL_FOLD_ROWS <- 10L

# The circuit breaker's default (control_horizons(breaker = )): this many of
# the first configurations to finish failing with the same cause stop
# evaluate() or fit(), since every other configuration would fail the same way.
BREAKER_FAILURES <- 3L

# select_training(): cap on the components retained when ncomp is given as
# a proportion of variance. 100 was the cap the defaults were measured
# with; the KSSL pool at 99 % retains far fewer.
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

# select_training(): depth = "topsoil" draws only from library rows whose
# upper depth is below this, in cm. It is the cut every experiment's KSSL
# pool was built on (upper depth < 30 cm, 45,957 rows of OSSL v1.2), so the
# defaults measured there are defaults for this draw. The library keeps
# every depth and draws topsoil by default.
SELECT_TOPSOIL_MAX_CM <- 30

# select_training(): Savitzky-Golay window of the similarity space, as a
# width in cm-1 between the outermost points. It was 11 points, whose width
# followed whatever grid the targets were on; every experiment ran at
# 4 cm-1, where 11 points span 40 cm-1, so 40 keeps that filter and carries
# it to 2 cm-1 (21 points), the grid horizons now runs at.
SELECT_SG_WINDOW_CM <- 40

# select_training(): the fewest library rows the resemblance check will take
# a reference distribution from. Its threshold is the 99th percentile of each
# row's distance to its nearest other row; below a few dozen rows that is
# only the largest of a handful of gaps, and with two rows every target sits
# beyond it. Under depth = "topsoil" on a small library the eligible rows can
# fall that low, and the check is then recorded as skipped.
SELECT_RESEMBLANCE_MIN_ROWS <- 30L

# select_training(): targets per chunk in the neighbour search, the memory
# the distance block takes against the calls it makes
# (control_horizons(chunk_size = )).
SELECT_CHUNK_SIZE <- 500L

# select_training(): released memory at or above this size is collected at
# once. R otherwise collects only when the heap reaches a trigger that an
# earlier peak may have raised, so a large block of garbage (a chunk's
# distances, the library once the draw is done) can lift the call's peak by
# gigabytes. A collection costs about 0.1 s, so smaller releases are left to
# R: every chunk at library scale passes this, and no small call does.
SELECT_COLLECT_BYTES <- 64 * 1024^2

# select_training(): the version of the similarity-space algorithm, part of
# every cached space's key. Cached spaces live in the user's cache directory
# and outlive package upgrades, so a change to how the space is built
# (transform_similarity(), the PCA, the component cap, the sdev floor's rule)
# must bump this, or old spaces are read as if the new code had built them.
# 2: the space is fit without the library's flagged rows (0.12.0).
SELECT_SPACE_CACHE_VERSION <- 2L

## spectra() -------------------------------------------------------------------

# spectra(): how close, as a fraction of a file's own median step, a
# wavenumber must lie to one already read for the two to be the same
# column. Scans from different sessions on one instrument can carry axes
# that differ in the last digits (0.00066 cm-1 at a 1.93 cm-1 step, #224),
# which would otherwise bind into two column sets. A step apart is a
# different point, so 1% leaves a wide margin either way. It only renames,
# never refuses or skips, so it has no override.
WN_SNAP_FRACTION <- 0.01

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
