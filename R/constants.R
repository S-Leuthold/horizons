# R/constants.R
# Central location for package-wide constants used in validation and
# configuration.

## Model Types ----------------------------------------------------------------

# Everything horizons declares about each model, one row per short name (the
# names configure() takes, VALID_MODELS). A new model is a row here, its
# cells in CONFIG_COST_SECONDS and its fixed point in the engine contract
# test (tests/testthat/test-engine-contract.R), with registration code only
# for an engine parsnip does not have (the PLS engine, R/pls-engine.R). The
# contract test holds every row's engine to the promises horizons makes
# about it, through the containment the row declares. The fields:
#
# - `fn`, `engine`: parsnip's model, by name, and the engine.
# - `fixed`: the model's arguments held at one value; `tuned`: the names of
#   those tuned, each `tune::tune()` in the spec; `engine_args`: arguments
#   passed to the engine. define_model_spec() (R/utils-models.R) builds the
#   spec from these alone.
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
# - `threads`: how the engine's threads are set, for an engine that can run
#   more than one: `argument`, its fit argument, which define_model_spec()
#   sets to the run's count (ENGINE_THREADS on the daemons,
#   control_horizons(threads = ) on a run in the session), and `option`, an
#   R option it reads (ranger reads `ranger.num.threads` to predict), which
#   every place that sets threads sets to the same count
#   (engine_thread_options()). An engine with no `threads` runs on one
#   thread, or on what BLAS and OpenMP are given.
# - `isolate`: the promises the engine breaks when its fits share a process,
#   each named with the upstream report about it. Such a model is fitted
#   through its engine's isolated twin, each fit in a process of its own
#   (model_engine(), register_isolated_engines(), R/isolated-engines.R),
#   unless control_horizons(isolate = FALSE). Cubist's C code keeps its
#   generator's stream behind the neighbour step's distance, so a fit's
#   predictions depend on the fits before it (`repeats`), and leaks two
#   (p + 2)^2 matrices a fit (`memory`). The contract test checks that each
#   reason still holds without isolation, so a reason the engine's own fix
#   has answered is dropped rather than kept. tidymodels code that branches
#   on an engine's name (parsnip's tunable() for lightgbm, tune's checks for
#   glmnet) would see a twin as another engine; the contract test compares a
#   twin's tunable() with its engine's.
# - `submodel`: the tuned parameter one fit scores at every value, as the
#   engine's registration marks it (`has_submodel`): tune fits the grid
#   candidates that differ only in it once, at its largest value, and
#   predicts the others from that fit (parsnip::multi_predict()). The
#   contract test holds each row to its engine's registration, both ways;
#   the PLS engine horizons registers takes its own from here
#   (register_pls_engine()), and the whole-space rule counts its values as
#   one fit (exhaustive_grid(), R/fit-warmstart.R).
# - `weights`: for a network, how its weights are limited: `argument`, the
#   engine's argument that refuses a network of more weights, which
#   define_model_spec() sets to configure(max_weights = )'s budget. The
#   runners cap `hidden_units` so the search proposes no network over that
#   budget on the most predictors any of a configuration's preps keeps
#   (cap_by_data(), R/fold-cache.R), so the argument never refuses a
#   candidate the search asked for.
#
# An engine's traits are read through engine_spec() (R/utils-models.R), which
# finds the row of an isolated twin's engine too.
MODEL_SPECS <- list(

  rf = list(
    fn          = "rand_forest",
    engine      = "ranger",
    label       = "Random Forest",
    packages    = "ranger",
    fixed       = list(trees = 500),
    tuned       = c("mtry", "min_n"),
    engine_args = list(importance = "impurity"),
    seed        = TRUE,
    threads     = list(argument = "num.threads", option = "ranger.num.threads")
  ),

  cubist = list(
    fn       = "cubist_rules",
    engine   = "Cubist",
    label    = "Cubist",
    packages = c("rules", "Cubist"),
    tuned    = c("committees", "neighbors", "max_rules"),
    submodel = "neighbors",
    isolate  = c(repeats = "topepo/Cubist#6", memory = "topepo/Cubist#8")
  ),

  xgboost = list(
    fn       = "boost_tree",
    engine   = "xgboost",
    label    = "XGBoost",
    packages = "xgboost",
    fixed    = list(trees = 500),
    tuned    = c("tree_depth", "learn_rate", "mtry", "min_n", "loss_reduction", "sample_size"),
    seed     = TRUE,
    threads  = list(argument = "nthread")
  ),

  plsr = list(
    fn       = "pls",
    engine   = "pls",
    label    = "PLS",
    packages = "pls",
    tuned    = "num_comp",
    submodel = "num_comp"
  ),

  elastic_net = list(
    fn       = "linear_reg",
    engine   = "glmnet",
    label    = "Elastic Net",
    packages = "glmnet",
    tuned    = c("penalty", "mixture"),
    submodel = "penalty"
  ),

  svm_rbf = list(
    fn       = "svm_rbf",
    engine   = "kernlab",
    label    = "SVM-RBF",
    packages = "kernlab",
    tuned    = c("cost", "rbf_sigma")
  ),

  mlp = list(
    fn          = "mlp",
    engine      = "nnet",
    label       = "Neural Network",
    packages    = "nnet",
    tuned       = c("hidden_units", "penalty", "epochs"),
    weights     = list(argument = "MaxNWts")
  ),

  lightgbm = list(
    fn       = "boost_tree",
    engine   = "lightgbm",
    label    = "LightGBM",
    packages = c("bonsai", "lightgbm"),
    fixed    = list(trees = 500),
    tuned    = c("tree_depth", "learn_rate", "mtry", "min_n"),
    seed     = TRUE,
    threads  = list(argument = "num_threads")
  ),

  mars = list(
    fn       = "mars",
    engine   = "earth",
    label    = "MARS",
    packages = "earth",
    tuned    = c("prod_degree", "num_terms"),
    submodel = "num_terms"
  )

)

# The short model names configure() takes
VALID_MODELS <- names(MODEL_SPECS)

# The threads each fit runs on where horizons pins them (the daemons and
# their inner daemons, an isolated fit's process, the session while a
# parallel run fits and as the package loads), the count
# engine_thread_options() (R/utils-models.R) gives the engines' thread
# options there; and control_horizons(threads = )'s default for a run in
# the session
ENGINE_THREADS <- 1L

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

# The most components the plsr model tunes over (#216), its default range's
# upper bound in TUNE_RANGES. The runners cap it further at the fewest
# predictors the recipe keeps in a fold or on the whole training part and at
# the smallest fold's rows less one (cap_by_data(), R/fold-cache.R). MIR models often want more than the handful a default
# range allows, and 30 keeps a grid's points where models land. Not a field
# of MODEL_SPECS: CARS caps its components with it too (R/step-select-cars.R).
PLS_MAX_COMP <- 30L

# The PLS stop's tolerance, relative (pls_informative(), R/pls-engine.R). A
# fit stops before a component once what is left of the outcome is within it
# of the outcome's norm (fitted exactly), once the component's score is
# within it of the scaled predictors' norm (their rank spent: a constant or
# an exactly collinear column), or once that score's cosine with what is left
# of the outcome is within it (nothing left to fit: PCA scores on the rows
# they were computed on, or a fit already at least squares). Measured on 359
# sets of real spectra and 41 rounding cases (#278): rounding sits at least
# 160 times below it; on spectra, real scores sit at least 130 times above it
# and real cosines at least 19 times, 400 times for any component that moves
# held-out predictions by more than a millionth of the outcome's spread.
PLS_TOL <- 1e-9

# horizons' default ranges of tuned parameters, keyed by the dials parameter
# id, in dials' units (a log-scale parameter's in log10): configure()'s
# `tune_ranges` is merged over them and recorded as config$tuning$ranges,
# which evaluate() and fit() apply to every model that tunes the parameter
# (apply_tune_ranges(), R/fit-warmstart.R). Another parameter keeps its
# engine's range. Cubist's fit time grows linearly with its committees, and
# on the profiled grids the best cross-validated RMSE at 25 committees or
# fewer was within about a third of a standard error of the best over 1 to
# 100.
TUNE_RANGES <- list(committees = c(1, 25), num_comp = c(1, PLS_MAX_COMP))

# The most weights a network the search proposes may have when configure()
# is not told otherwise (configure(max_weights = ); MODEL_SPECS' `weights`).
# An mlp's single hidden layer of h units on p predictors, with one linear
# output, holds h * (p + 2) + 1 weights (max_hidden_units(), R/fold-cache.R),
# so its `hidden_units` range is capped at the most units that stay within
# it. 10,000 is the limit nnet was given before the budget was a setting.
# nnet's optimizer keeps a matrix of the weights squared, so a fit's memory
# grows as about 4 * weights^2 bytes and an iteration's time the same way: on
# 800 rows, 1,691 predictors and 5 units (8,466 weights) a fit held 378 MB at
# 0.26 s an iteration, and at 10 units (16,931 weights) 1.2 GB at 1.2 s.
DEFAULT_MAX_WEIGHTS <- 10000

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

### The selections whose prep reads the outcome, so a fold's prep is named by
### the response transformation as well (fold_prep_scope()): two
### configurations that differ only in it select on different outcomes.
OUTCOME_SELECTIONS <- c("correlation", "boruta", "cars")

### The tuned parameters whose upper bound is read from the predictors a
### recipe keeps (dials::finalize(), cap_by_data()): mtry and num_comp from
### the fewest it keeps in any fold or on the whole training part, and
### hidden_units, under the weight budget, from the most. So a model that
### tunes one has its configuration's recipe prepped on the whole training
### part as well as on each fold (bounded_model(), R/fold-cache.R)
BOUNDED_PARAMETERS <- c("mtry", "num_comp", "hidden_units")

### The seconds one fold's prep takes by its feature selection, the recipe
### without the spectral transform on a fold of the coverage grid's training
### part (835 rows, snv_deriv1; measured 2026-11-17). The fold preps start
### longest first by it, as the configurations start by
### CONFIG_COST_SECONDS; no selection has no prep.
FOLD_PREP_COST_SECONDS <- c(
  pca         = 0.57,
  correlation = 0.26,
  boruta      = 15.4,
  cars        = 3.1
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

# The Bayesian iterations without improvement after which a search stops
# (control_horizons(no_improve = )), in evaluate() and fit()'s re-tune alike.
# Each verb says how many of its searches stopped early, and how to change
# it. 10 is tune's own default. A search whose starting grid puts only one
# point in a narrow good region (svm_rbf on PCA scores, for one) can spend
# more than five iterations exploring before it first improves on that point.
BAYES_NO_IMPROVE_DEFAULT <- 10L

# The points a search gives each tuned parameter, the grid and the Bayesian
# iterations together: the iterations asked for are a floor, raised for a
# model tuning d parameters to BAYES_POINTS_PER_PARAM * d less the grid when
# that is more (search_budget(), R/fit-warmstart.R). Tools agree on about ten
# a parameter: SMAC starts from ten configurations a parameter, and Loeppky,
# Sacks and Welch (2009) justify ten points a dimension for a Gaussian
# process design. It is the package's rule, not a checkpoint setting: a
# change to it, or to search_budget(), changes how rows are scored, and bumps
# SCORING_SCHEMA.
BAYES_POINTS_PER_PARAM <- 10L
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

### configure()'s default for final_bayesian_iter: fit()'s re-tune is off, so
### each member keeps evaluate()'s hyperparameters. On the library-scale and
### local benchmarks the re-tune never moved a member past its margin
### (RETUNE_MARGIN_SE) and was most of fit()'s run time.
DEFAULT_FINAL_BAYES_ITER <- 0L
N_CALIB_MIN              <- 30L
DEFAULT_UQ_LEVEL         <- 0.90
UQ_QUANTILE_TREES        <- 500L

### The quantile forest's minimum node size grows with its rows: the rows over
### UQ_QUANTILE_NODE_DIVISOR, rounded up, and never below UQ_QUANTILE_MIN_NODE
### (ranger's default, so the forest is unchanged up to 1,250 rows). See
### uq_min_node_size().
UQ_QUANTILE_MIN_NODE     <- 5L
UQ_QUANTILE_NODE_DIVISOR <- 250L

### fit()'s re-tune keeps evaluate()'s hyperparameters unless its best
### candidate's CV RMSE is lower than theirs by more than this many of the
### best candidate's standard errors: tune::select_by_one_std_err()'s bound,
### with evaluate()'s point in the place of the simplest model. The point was
### chosen on evaluate()'s folds and is scored once on fit()'s, a fresh draw,
### while the best candidate is the best of many tried on fit()'s folds, so it
### beats the point by the selection's optimism alone even where nothing is
### better. Only the draw is fresh: evaluate() chose the point on rows that
### hold fit()'s and, with UQ or AD on, the calibration rows (#44).
RETUNE_MARGIN_SE <- 1

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
SEED_RULE <- 2L

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

# The keys evaluate()'s parallel branch sends to evaluate_config_worker(), and
# to fold_prep_worker() on the configs axis. Assembled in one place and
# asserted in the worker, because a mis-keyed entry arrives as NULL and one
# fails SILENTLY rather than erroring: a NULL `grid_size` makes tune_grid()
# invent its own grid.

SHARED_ARG_NAMES <- c(
  "data",
  "resample_idx",
  "configs",
  "role_map",
  "grid_size",
  "bayesian_iter",
  "tune_ranges",
  "max_weights",
  "no_improve",
  "prune",
  "prune_threshold",
  "seed",
  "sg_window",
  "pca_threshold",
  "outcome_range",
  "data_fp",
  "settings",
  "fold_cache_dir",
  "keep_traces",
  "isolate",
  "pkg_version"
)

# The keys of what a fold preparation's task carries on the resamples axis and
# in fit(), which have no payload file (fold_prep_worker()): the training
# part, the fold's rows, and what its prep is built from. Asserted in the
# worker as SHARED_ARG_NAMES is.

FOLD_PREP_ARG_NAMES <- c(
  "data",
  "rows",
  "role_map",
  "sg_window",
  "pca_threshold",
  "seed",
  "keep_traces",
  "pkg_version"
)

# The keys of what a member's task carries on fit()'s members axis
# (fit_member_worker()), which has no payload file: the modelled rows once
# and the training rows a response trim left out, the positions its split,
# calibration rows and folds are rebuilt from, the role map, the settings
# every member is fitted with (one list, fit()'s `member_settings`, which the
# session's members are fitted with too), and the member's own configuration,
# evaluate()'s parameters for it and its fold preparations. Asserted in the
# worker as SHARED_ARG_NAMES is.

FIT_ARG_NAMES <- c(
  "data",
  "trimmed",
  "resample_idx",
  "role_map",
  "settings",
  "config_row",
  "best_params",
  "fold_entries",
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
# - What a daemon whose configuration runs its folds on inner daemons holds
#   above its size at the configuration's start while they run
#   (split_need()): a fixed part, and a part per byte of the configuration's
#   fold frames (the training rows by the predictors it keeps, one frame a
#   fold), since tune sends a call's folds out at once and the daemon keeps
#   each one's message until an inner daemon takes it. The per-byte part is
#   2, above the 1.2 to 1.8 times the folds' frames measured with a model
#   that fits nothing on 8,272 rows by 360 and 1,701 predictors, at 5 and 10
#   folds and 2 to 10 inner daemons (up to 1,937 MiB, at 1,701 predictors,
#   10 folds and 2 inner daemons). At small frames the fixed part dominates:
#   on local-real at seed 307 (835 training rows, a correlation selection
#   keeping about 242 predictors a fold, 10 folds, 16 MB of folds' frames)
#   the split daemons held up to 478 MiB (cubist on 2 inner daemons; 317
#   MiB xgboost on 5, 91 and 7 MiB svm_rbf and elastic net on 2), so the
#   fixed part is that, less the per-byte part, rounded up to 0.05 GB.
#   The inner daemons themselves are counted at the daemon's line, which is
#   above what one holds (one fold's frame, not the run's data): on that
#   run they peaked at 0.38 to 0.71 GB against its 1.62 GB, which the
#   release benchmark recalibrates at library scale.
#
# A run of other models, selections or committees can need more or less, so
# the warning calls the estimate approximate.
DAEMON_MEMORY <- list(
  daemon   = c(base = 1.45e9, per_byte = 12),
  isolated = c(base = 0.6e9, per_byte = 5),
  queue    = c(base = 0.5e9, per_byte = 2)
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

# Seconds a configuration's inner daemons have to connect once launched on
# its daemon (start_inner_daemons(), R/utils-parallel.R); past it the
# configuration runs its folds on its own daemon, with a note. They connect
# in well under a second.
INNER_START_SECONDS <- 60

# What one configuration is expected to take, from which the configs axis
# starts the longest configurations first (dispatch_order(),
# R/utils-parallel.R), gives the long ones inner daemons (split_plan()) and
# monitor_evaluate() weights its ETA (config_costs(), recorded in the
# manifest):
#
#   seconds = cell(model, selection) x (rows / CONFIG_ROWS_REF)^CONFIG_ROW_EXPONENT[model]
#
# with `rows` the run's training rows. Only each configuration's share of a
# run is read (the order, the split, the ETA's weights), so the seconds'
# scale does not matter. One configuration's estimate can be off by a
# factor of two either way, from when its Bayesian search stops. The
# predictors a configuration's preparation keeps do not enter it: on the
# measured grids a PCA preparation keeping three times the predictors of
# another took about the same time, and the estimate predicted the longest
# configurations' shares best without them.
#
# The cells are seconds on the local grid (835 training rows, 1,701
# predictors, 10 folds, a grid of 10, one configuration per daemon),
# refitted on 2026-11-23:
#
# - cubist, xgboost, svm_rbf and elastic_net with pca or correlation: their
#   mean over the four configurations of local-real at no_improve 10, after
#   the fold stage and the search's changes of 0.13.0;
# - plsr: the coverage grid's five configurations, measured the same way;
# - every other cell: 0.12.0's coverage grid times its model's ratio, the
#   median over its measured cells of the measured cell over 0.12.0's
#   (cubist 0.60, xgboost 0.80, svm_rbf 0.43, elastic_net 0.38), or over
#   every measured cell (0.52) for rf, mlp and mars, none of which was
#   measured since; lightgbm's cells are xgboost's, whose fits its are within
#   20% of.
#
# A new model or selection adds its cells (test-utils-parallel.R holds the
# table to VALID_MODELS x VALID_FEATURE_SELECTION). Kept apart from
# MODEL_SPECS: it is one measurement over models and selections, documented
# and refitted whole.
CONFIG_COST_SECONDS <- c(
  "rf/none"           = 6718,  "rf/pca"          = 351, "rf/correlation"          = 1031,
  "rf/boruta"         = 5792,  "rf/cars"         = 2810,
  "cubist/none"       = 11331, "cubist/pca"      = 545, "cubist/correlation"      = 1870,
  "cubist/boruta"     = 10837, "cubist/cars"     = 7995,
  "xgboost/none"      = 5028,  "xgboost/pca"     = 216, "xgboost/correlation"     = 1805,
  "xgboost/boruta"    = 9547,  "xgboost/cars"    = 5058,
  "plsr/none"         = 11,    "plsr/pca"        = 7,   "plsr/correlation"        = 7,
  "plsr/boruta"       = 42,    "plsr/cars"       = 16,
  "elastic_net/none"  = 27,    "elastic_net/pca" = 29,  "elastic_net/correlation" = 78,
  "elastic_net/boruta" = 1450, "elastic_net/cars" = 252,
  "svm_rbf/none"      = 147,   "svm_rbf/pca"     = 53,  "svm_rbf/correlation"     = 91,
  "svm_rbf/boruta"    = 1622,  "svm_rbf/cars"    = 311,
  "mlp/none"          = 1242,  "mlp/pca"         = 199, "mlp/correlation"         = 1586,
  "mlp/boruta"        = 3032,  "mlp/cars"        = 3677,
  "lightgbm/none"     = 5028,  "lightgbm/pca"    = 216, "lightgbm/correlation"    = 1805,
  "lightgbm/boruta"   = 9547,  "lightgbm/cars"   = 5058,
  "mars/none"         = 190,   "mars/pca"        = 9,   "mars/correlation"        = 27,
  "mars/boruta"       = 316,   "mars/cars"       = 151
)

# The training rows the cells were measured at
CONFIG_ROWS_REF <- 835

# How a model's configuration time grows with the rows: the exponent of
# rows / CONFIG_ROWS_REF. Fitted for the four models measured on both the
# local grid and the library draw (8,272 rows) in 0.12.0: for each, the mean
# over its configurations among the 32 of the log ratio of their seconds
# divided by the log ratio of the rows; lightgbm takes xgboost's, and the
# models with no such pair 1.
CONFIG_ROW_EXPONENT <- c(
  rf = 1, cubist = 1.57, xgboost = 0.86, plsr = 1, elastic_net = 0.98,
  svm_rbf = 1.44, mlp = 1, lightgbm = 0.86, mars = 1
)

# The share of a run's work over its daemons above which a configuration
# runs its folds on inner daemons (split_plan(), R/utils-parallel.R): split
# when its expected time is more than SPLIT_SHARE x total / n. Chosen by
# replaying measured grids' configuration times at 5 and 10 folds and 2 to
# 30 daemons, with estimates off by up to a factor of two: below 1, so a
# configuration near its fair share is split, since leaving the longest one
# whole costs more than splitting one that did not need it.
SPLIT_SHARE <- 0.75

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
#   7  2026-11-18: every engine with a thread argument is given one thread
#      (MODEL_SPECS' `threads`); lightgbm had used every core.
#   8  2026-11-20: each fold's recipe prepped once inside its configuration,
#      so CARS's and Boruta's bands are selected once per fold under their
#      own unit's seed and shared by every candidate (SEED_RULE 2).
#   9  2026-11-23: a PLS fit stops at the components that carry information
#      about the outcome, so plsr on PCA scores no longer scores counts
#      fitted to rounding error (#278).
#  10  2026-11-24: mtry and num_comp are capped at the fewest predictors any
#      fold's prep or the whole training part's keeps, not the training
#      part's alone, so a feature selection that keeps fewer in a fold no
#      longer has candidates truncated, passed over or failed there; and
#      mlp's hidden_units is capped so no network passes the weight budget
#      (configure(max_weights = )), where its larger candidates had failed.
SCORING_SCHEMA <- 10L

# Schema of eval_manifest.rds, which evaluate() writes at the start of every
# run with an output_dir and monitor_evaluate() reads. Schema 4 (#42) records
# the axis and its backend (`plan`: "sequential" or "mirai", and `workers`,
# the daemons connected as the run began), the training-data fingerprint and
# its fields,
# and the tuning settings. Schema 5 adds `config_cost`, each configuration's
# expected cost (config_costs()) in `config_ids`' order, which weights the
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

# The parts of a configuration the breaker reads (new_breaker()), named by
# what its message calls their values. Its failures must span two values of
# each part the configurations it watches vary in, so a failure that belongs
# to one value of one (one selection's fold preparation, a model's engine)
# never stops a run, whatever order the configurations finish in.
BREAKER_FACTORS <- c(model             = "models",
                     transformation    = "transformations",
                     preprocessing     = "preprocessing methods",
                     feature_selection = "feature selections")

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
