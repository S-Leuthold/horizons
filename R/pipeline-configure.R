#' Pipeline: Configure Model Pipelines
#'
#' @description
#' Defines the experimental design space for model evaluation. Takes a
#' `horizons_data` object with response data attached and specifies which
#' combinations of models, transformations, preprocessing, and feature selection
#' to benchmark.
#'
#' @details
#' `configure()` builds a configuration grid — the Cartesian product of all
#' specified modeling axes. Each row represents a distinct pipeline to
#' evaluate. This is the bridge between data preparation (`add_response()`)
#' and model evaluation (`evaluate()`).
#'
#' **Outcome promotion:**
#'
#' `add_response()` assigns `role = "response"` to joined variables.
#' `configure()` promotes exactly one response to `role = "outcome"` for
#' modeling. If only one response exists, it's auto-selected. Multiple
#' responses require explicit `outcome` specification.
#'
#' **Reconfiguration:**
#'
#' Can be called multiple times on the same object. Previous configuration
#' is overwritten, any prior outcome role is reverted to "response", and
#' everything a previous `evaluate()`, `fit()` or `ensemble()` earned is
#' dropped: the `evaluation`, `models` and `ensemble` slots return to their
#' empty state, and the `horizons_eval`, `horizons_fit` or
#' `horizons_ensemble` class goes with them. All of it is keyed to the
#' outcome that is being replaced. The validation verdict is cleared too,
#' but the record of outliers `validate()` already removed is kept, since
#' those rows stay removed, and so is a `select_training()` record, which
#' describes the rows rather than the outcome. Because the record was not
#' drawn for the new outcome, `configure()` warns when a `select_training()`
#' record (other than `scope = "global"`) was drawn for properties that do not
#' include it (`horizons_warning_input`). Re-configuring an evaluated or
#' fitted object warns too (`horizons_warning_input`), naming what it
#' discards; re-configuring one that was only configured is reported in
#' verbose output and a note on the object's log. Either message says when
#' it cleared a validation, and with it a response-trim request, so that
#' `validate()` can be run again.
#' This enables the `purrr::map()` multi-outcome pattern:
#'
#' ```
#' c("SOC", "POM_C", "pH") |>
#'   purrr::map(~base |> configure(outcome = .x) |> evaluate() |> fit())
#' ```
#'
#' **Reproducibility:**
#'
#' Every draw `evaluate()`, `fit()` and `ensemble()` make is seeded from the
#' run's seed and the draw's own name, so a run repeats from the seed it
#' records, whatever the order of the configurations, the workers or the
#' session's random number generator. That holds for `cubist` too because
#' each Cubist fit runs in a process of its own (`control_horizons(isolate =
#' )`): its tuning searches `neighbors` from 0 to 9, and the neighbour
#' distance cutoff comes from a generator inside the Cubist C code that no
#' seed reaches and that carries on from every Cubist fit made before it in
#' the same process, which also keeps memory from each fit. With
#' `control_horizons(isolate = FALSE)` the fits run in the process that runs
#' them (the session, or a daemon on a parallel run), and a `cubist`
#' configuration's scores can then differ slightly between runs, and with
#' them, when candidates are close, the one chosen. See GitHub issue #51.
#'
#' **Choosing models for large spectral libraries:**
#'
#' Cost scales very differently with predictor count (`p`) across
#' `MODEL_SPECS`. `rf` (ranger), `xgboost`, `lightgbm` and `elastic_net`
#' (glmnet) stay fast at full spectral resolution; `plsr` is built for
#' `p >> n` and is not a concern either. `cubist` fits a linear model in
#' every rule, so its cost grows sharply with `p`: on the KSSL clay library
#' at 4 cm-1 (14,228 rows x 851 predictors, ~12.1M cells), not one of 25
#' tune tasks finished in 29.5 minutes, while the same config with
#' `feature_selection = "pca"` finished in 368 s (test RPD 3.82) — OSSL's
#' published pipeline is SNV -> PCA(120) -> Cubist (Safanelli et al. 2025,
#' PLOS ONE 20(1):e0296545). `validate()`'s P010 check reports when a
#' `cubist` config with `feature_selection = "none"` sits on a table whose
#' `n_rows * n_predictors` exceeds `CUBIST_MAX_CELLS` (a conservative floor
#' well under the measured failure point, not a benchmark); the fix is
#' `feature_selection = "pca"`. `svm_rbf` (kernlab) and `mars` (earth) also
#' scale with `p` on well-established grounds — an RBF kernel matrix costs
#' `O(n^2 p)` to build, and MARS's forward pass searches every predictor at
#' every candidate knot — but neither has been measured at library scale,
#' and `validate()` does not report them. A slow run with either on a
#' full-resolution table is a signal to add `feature_selection` there too.
#' See GitHub issue #40.
#'
#' **plsr with `feature_selection = "pca"`:**
#'
#' `plsr` scales its predictors to unit variance, and PCA's scores are
#' uncorrelated, so on them PLS re-projects what PCA has already projected:
#' its first component is already the least-squares fit on the kept
#' components (principal component regression at `pca_threshold`), every
#' count of `num_comp` is that same model, and the choice is one component.
#' The pairing runs, and tunes nothing; `evaluate()` says so once in its
#' tree and on the object's log. See GitHub issue #278.
#'
#' **Recipe settings:**
#'
#' `sg_window` and `pca_threshold` are set once per object, not per
#' configuration: every configuration's recipe uses the same value, and
#' neither is part of the config id. `sg_window` is the Savitzky-Golay window
#' in grid points. It trims `(sg_window - 1) / 2` columns from each end of the
#' spectrum for every preprocessing method, `"raw"` and `"snv"` included, so
#' it matters even where no filter runs. The polynomial order is not a
#' setting: each method fixes its own (`"sg"` order 1; `"deriv1"` first
#' derivative, order 1; `"deriv2"` second derivative, order 3; the `"snv_"`
#' variants the same), and the cubic is why the window must be at least 5.
#' At order 1 the `"sg"` smoother is a moving average, so widening
#' `sg_window` widens a boxcar: it flattens peaks and shoulders narrower than
#' the window rather than preserving them, as a higher-order Savitzky-Golay
#' filter would.
#'
#' Because the window counts grid points, its physical width depends on the
#' axis it runs on, and `standardize()` can still change that axis after
#' `configure()`. So the width is not stored here: `configure()` prints it in
#' cm-1 for the axis it sees, `evaluate()` records the width it actually ran
#' in `evaluation$recipe`, and `evaluate()` refuses a window as wide as the
#' spectrum. `pca_threshold` is the share of variance kept by
#' `feature_selection = "pca"` and is read by nothing else. Both defaults are
#' the values the recipe ran before they were settable. See GitHub issue #62.
#'
#' **Tuning ranges:**
#'
#' `evaluate()` and `fit()`'s re-tune search each tuned parameter over a
#' range: its engine's, or horizons' where it sets one, or one given in
#' `tune_ranges`.
#' horizons sets two: Cubist's `committees` 1 to 25, since a fit's time grows
#' linearly with its committees and more of them seldom lowered the
#' cross-validated error by much, and plsr's `num_comp` 1 to 30. A range is
#' named by the parameter's id and applies to every configured model that
#' tunes it. The ids each model tunes:
#'
#' * `rf`: `mtry`, `min_n`
#' * `cubist`: `committees`, `neighbors`, `max_rules`
#' * `xgboost`: `tree_depth`, `learn_rate`, `mtry`, `min_n`,
#'   `loss_reduction`, `sample_size` (a proportion of the rows)
#' * `plsr`: `num_comp`
#' * `elastic_net`: `penalty`, `mixture`
#' * `svm_rbf`: `cost`, `rbf_sigma`
#' * `mlp`: `hidden_units`, `penalty`, `epochs`
#' * `lightgbm`: `tree_depth`, `learn_rate`, `mtry`, `min_n`
#' * `mars`: `prod_degree`, `num_terms`
#'
#' A range is in dials' units: a parameter searched on a log scale takes its
#' range in log10 (log2 for `cost`), so `learn_rate = c(-3, -1)` searches
#' 0.001 to 0.1. The data still cap `mtry` at the fewest predictors a
#' configuration's recipe keeps in any cross-validation fold or on the whole
#' training part, and `num_comp` at those and the smallest fold's rows less
#' one, narrowing a range given here, so no candidate asks a fold for more
#' predictors than it kept; a range whose lower bound is above that cap
#' fails the configuration, saying so. `configure()` refuses a name no
#' configured model tunes, a range that is not two finite numbers with the
#' lower below the upper, an `mtry`, `num_comp` or `hidden_units` range
#' starting below 1 (each counts the predictors, components or hidden units
#' a fit uses, so 0 is no model), and a fractional bound on a whole-number
#' parameter (typed where the model's engine is
#' installed, and again by `evaluate()` and `fit()` once they have checked
#' the engines of the models they fit). An engine's own limits are not
#' checked: Cubist takes 1 to 100 committees and 0 to 9 neighbors, and a
#' range past them fails those candidates' fits, which the row reports. The
#' ranges are recorded as `config$tuning$ranges`, so `fit()`'s re-tune, when
#' it runs, searches the space `evaluate()` searched, and a checkpoint tuned
#' over other ranges is not resumed. `ensemble()`'s meta-learners keep their
#' own grids.
#'
#' `mtry` has no range of horizons': rf, xgboost and lightgbm search it from
#' 1 to the predictors the data cap it at, which on full spectra is about
#' 1,700. A random forest's fit time grows with it: on one cross-validation
#' fold of full spectra, 500 trees took 148 s at an `mtry` of 41 and 3,051 s
#' at 1,200, on one thread. Where rf's searches landed has varied. With no
#' feature selection, 19 saved searches on about 1,700 bands chose a median
#' of 376: five at or below 41 (the square root of the bands), six above a
#' third of the bands and four above half. On one data set's Boruta and CARS
#' selections, nine of ten chose between 0.06 and 0.22 of the bands, and
#' where one configuration was searched twice the cross-validated error
#' differed by under half a standard error. A narrower range
#' (`tune_ranges = list(mtry = c(1, 200))`, for instance), or a feature
#' selection that keeps fewer bands, searches the faster part of the range
#' at the cost of the values above it, which more than half of the searches
#' without a selection chose. The default searches the whole range (#115).
#'
#' mlp's networks are kept within a weight budget, `max_weights`. A network
#' of h hidden units on p predictors holds h(p + 2) + 1 weights, and nnet's
#' optimizer keeps a matrix of the weights squared, so a fit's memory grows
#' as about four bytes times the weights squared and its time the same way:
#' on 800 rows and 1,691 predictors, a fit of 5 units (8,466 weights) held
#' 378 MB at 0.26 s an iteration, and one of 10 units (16,931 weights)
#' 1.2 GB at 1.2 s. `hidden_units`' range is capped at the most units whose
#' network stays within the budget on the most predictors any of a
#' configuration's preps keeps (a cross-validation fold's or the whole
#' training part's), and when that narrows the range the configuration's
#' row and the verb's log say so with a note; nnet is given the budget as
#' its own limit (`MaxNWts`). At the default of 10,000 weights an mlp on
#' about 1,700 bands searches up to 5 units, and one on 250 selected bands
#' up to 39, where its default range of 1 to 10 is not narrowed. Raise
#' the budget to search larger networks on wide predictors, at that cost,
#' or set it to `Inf` for none; a feature selection and
#' `tune_ranges = list(hidden_units = )` are the other levers. The budget is
#' recorded as `config$tuning$max_weights`, so `fit()` searches under the
#' budget `evaluate()` searched under, and a checkpoint tuned under another
#' is not resumed.
#'
#' **Outcome range:**
#'
#' `outcome_range` is the physical range of the outcome, `c(lower, upper)`,
#' either end possibly infinite. Every prediction the package scores or
#' serves is clamped to it: the test-set and out-of-fold predictions
#' `evaluate()` and `fit()` score, the calibration predictions conformal
#' intervals are built from, `predict()`'s point predictions and interval
#' bounds, and the ensemble's. The default, `c(0, Inf)`, is the non-negative
#' floor the package applied before the range was settable, right for
#' concentrations, contents and most other soil properties. A signed property
#' such as δ13C or δ15N takes `c(-Inf, Inf)`, which clamps nothing, and a
#' bounded one such as a percentage may take `c(0, 100)`.
#'
#' The range comes from what the property can physically be, not from the
#' data in hand. Setting it to the observed minimum and maximum clamps
#' predictions to the training range and, once test rows are in the table,
#' tunes the clamp on them. Prefer `-Inf` to an arbitrarily low finite floor:
#' the range's lower bound anchors the response bound `fit()` stores (see
#' [fit()]), which allows half the span from that anchor above the training
#' maximum, so a distant floor loosens the guardrail. For outcomes between 10
#' and 20 the bound is 530 under `c(-1000, Inf)` and 25 under `c(-Inf, Inf)`.
#'
#' The range is checked against the data: when an observed value of the
#' outcome lies outside it or is infinite, `configure()` aborts with class
#' `horizons_error_input`, and `evaluate()`, `fit()` and `ensemble()` repeat
#' the check before they fit anything. A range whose lower bound is negative
#' cannot be combined with the `"log"`, `"log10"` or `"sqrt"` transformations,
#' which are undefined for negative values and whose back-transforms floor
#' the predictions, and `evaluate()` and `fit()` refuse to rank by `"rrmse"`
#' under it, since its denominator, the mean outcome, can be zero or negative.
#' One range applies to the one outcome an object models; to model several
#' outcomes with different ranges, give each `configure()` call its own. See
#' GitHub issue #76.
#'
#' @param x `horizons_data`. Object with response data attached via
#'   `add_response()`.
#' @param outcome `character(1) or NULL`. Which response variable to model.
#'   Auto-selects if exactly one response exists.
#' @param models `character`. Model algorithms to benchmark. Default
#'   `c("rf", "cubist", "plsr")`. See `VALID_MODELS` for options.
#' @param transformations `character`. Response transformations to test.
#'   Default `"none"`.
#' @param preprocessing `character`. Per-config spectral preprocessing methods.
#'   Default `"raw"`.
#' @param feature_selection `character`. Feature selection methods to test.
#'   Default `"none"`.
#' @param cv_folds `integer`. Number of cross-validation folds. Default 5.
#'   Minimum 2.
#' @param grid_size `integer`. Hyperparameter grid size (Latin hypercube).
#'   Default 10. Minimum 1, and at least 2 when `bayesian_iter` or
#'   `final_bayesian_iter` is above 0, since `evaluate()`'s Bayesian search
#'   starts from the grid, as do `fit()`'s on a cold start and its re-tune of
#'   a member whose hyperparameters from `evaluate()` are unusable. `fit()`'s
#'   re-tune of `evaluate()`'s members starts instead from each member's
#'   hyperparameters and, for d tuned parameters, d points drawn around
#'   them.
#' @param bayesian_iter `integer`. Bayesian optimization iterations of
#'   `evaluate()`'s search, and of `fit()`'s on a cold start, which tunes its
#'   one configuration as `evaluate()` would (see [fit()]), at least: a model
#'   tuning more parameters runs more, so that its grid and its search
#'   together try at least ten points a tuned parameter (`10 * d -
#'   grid_size` iterations for `d` parameters, when that is more). At the
#'   defaults that is 15 for rf, elastic_net and svm_rbf, 20 for cubist and
#'   mlp, 30 for lightgbm and 50 for xgboost. Each search stops once
#'   `control_horizons(no_improve = )` iterations in a row have not improved
#'   on its best. A space that costs no more fits than the grid and the
#'   search together is gridded whole, with no search: a submodel's values
#'   share one fit, so plsr's components are gridded whole, every count
#'   scored from one fit a fold, and at the defaults so is mars's space, two
#'   fits a fold. Default 15. Minimum 0: 0 turns the search off for every
#'   model, and a cold start then keeps the best of its grid.
#' @param final_bayesian_iter `integer`. Bayesian optimization iterations for
#'   `fit()`'s re-tune of each of `evaluate()`'s members, at least. Default 0,
#'   which turns the re-tune off: each member keeps `evaluate()`'s
#'   hyperparameters, clamped into `fit()`'s range and scored by its
#'   out-of-fold predictions alone. A cold start does not read it: it tunes by
#'   `bayesian_iter`, and warns that a `final_bayesian_iter` above 0 is
#'   ignored. The re-tune is off by default because on a library-scale and a
#'   local benchmark it never moved a member past its margin, so the members
#'   kept `evaluate()`'s hyperparameters either way, while it took most of
#'   `fit()`'s run time (see "The re-tune" in [fit()]). Above 0 the re-tune
#'   runs, its budget raised as `bayesian_iter`'s is, counting the points it
#'   starts from: from `evaluate()`'s hyperparameters and d points drawn
#'   around them, for d tuned parameters, 25 iterations run 25 up to two
#'   tuned parameters, 26 for cubist and mlp, 35 for lightgbm and 53 for
#'   xgboost. Each re-tune stops once `control_horizons(no_improve = )`
#'   iterations in a row have not improved on its best. Minimum 0.
#' @param sg_window `integer`. Savitzky-Golay window in grid points, applied
#'   to every configuration. Default 9. Must be odd, at least 5, and (checked
#'   by `evaluate()`) smaller than the number of spectral columns. For
#'   `"sg"` it is the width of a moving average. See Recipe settings.
#' @param pca_threshold `numeric`. Share of variance `feature_selection =
#'   "pca"` keeps, applied to every configuration that uses it. Default
#'   0.995. Must be in (0, 1].
#' @param tune_ranges `list` or `NULL`. Ranges of tuned parameters, by
#'   parameter id, each `c(lower, upper)` in dials' units, applied to every
#'   configured model that tunes the parameter and merged over horizons'
#'   defaults (`committees` 1 to 25, `num_comp` 1 to 30). Default `NULL`, the
#'   defaults alone. See Tuning ranges.
#' @param max_weights `numeric(1)`. The most weights an mlp network the
#'   search proposes may have: a whole number of at least 1, or `Inf` for no
#'   budget. `hidden_units`' range is capped to it. Default 10000. See Tuning
#'   ranges.
#' @param outcome_range `numeric(2)`. The outcome's physical range,
#'   `c(lower, upper)` with lower < upper; either end may be infinite. Every
#'   scored and served prediction is clamped to it. Default `c(0, Inf)`,
#'   non-negative; use `c(-Inf, Inf)` for a signed property such as δ13C.
#'   Set it from the property's physical bounds, not the data's. Every
#'   observed value of the outcome must lie inside it. A negative lower bound
#'   rules out the `"log"`, `"log10"` and `"sqrt"` transformations. See
#'   Outcome range.
#' @param verbose `logical(1)`. Print the configuration's summary. Default:
#'   `getOption("horizons.verbose", TRUE)`, as every verb's.
#'
#' @return A modified `horizons_data` object with:
#'   * Outcome variable promoted to `role = "outcome"` in `data$role_map`
#'   * `config$configs` — tibble of configuration grid (5 columns)
#'   * `config$n_configs` — integer count
#'   * `config$tuning` — list of tuning parameters, with `ranges`, the
#'     ranges searched, and `max_weights`, the weight budget (see Tuning
#'     ranges)
#'   * `config$expansion` — list of original inputs (for reproducibility)
#'   * `config$recipe` — the recipe settings every configuration is built
#'     with: `sg_window` and `pca_threshold`
#'   * `config$outcome_range` — the outcome's range, as a length-2 double
#'
#' @examples
#' \dontrun{
#' # Minimal (single response auto-selected)
#' hd |> configure()
#'
#' # Explicit outcome with multiple responses
#' hd |> configure(outcome = "SOC")
#'
#' # Full experimental design
#' hd |> configure(
#'   outcome         = "SOC",
#'   models          = c("rf", "cubist", "plsr", "xgboost"),
#'   transformations = c("none", "log"),
#'   preprocessing   = c("raw", "snv", "sg"),
#'   cv_folds        = 10L,
#'   grid_size       = 20L
#' )
#'
#' # A wider Savitzky-Golay window and a tighter PCA for every config
#' hd |> configure(
#'   preprocessing     = c("deriv1", "snv_deriv2"),
#'   feature_selection = "pca",
#'   sg_window         = 15L,
#'   pca_threshold     = 0.99
#' )
#'
#' # Cubist searched over up to 50 committees, and rf's mtry over 1 to 200
#' hd |> configure(
#'   models      = c("rf", "cubist"),
#'   tune_ranges = list(committees = c(1, 50), mtry = c(1, 200))
#' )
#'
#' # A signed property: no floor at zero
#' hd |> configure(outcome = "d13C", outcome_range = c(-Inf, Inf))
#'
#' # Multi-outcome pattern
#' c("SOC", "POM_C", "pH") |>
#'   purrr::map(~base |> configure(outcome = .x) |> evaluate() |> fit())
#' }
#'
#' @export

configure <- function(x,
                      outcome            = NULL,
                      models             = c("rf", "cubist", "plsr"),
                      transformations    = "none",
                      preprocessing      = "raw",
                      feature_selection  = "none",
                      cv_folds              = 5L,
                      grid_size             = 10L,
                      bayesian_iter         = 15L,
                      final_bayesian_iter   = 0L,
                      sg_window             = 9L,
                      pca_threshold         = 0.995,
                      tune_ranges           = NULL,
                      max_weights           = 10000,
                      outcome_range         = c(0, Inf),
                      verbose               = getOption("horizons.verbose", TRUE)) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Input validation
  ## ---------------------------------------------------------------------------

  ## 1.1 x must be horizons_data -----------------------------------------------

  if (!inherits(x, "horizons_data")) {

    abort_horizons(
      "{.arg x} must be a horizons_data object, not {.obj_type_friendly {x}}.",
      kind = "input"
    )

  }

  ## 1.1b The object as it arrives (#234) ------------------------------------------
  ## Its configuration is not checked: rewriting it is the remedy for a bad one.

  x <- validate_horizons_data(x, config = FALSE)

  check_arg(rlang::check_bool, verbose)

  ## 1.2 Response data must exist -----------------------------------------------

  responses <- x$data$role_map$variable[x$data$role_map$role == "response"]
  outcomes  <- x$data$role_map$variable[x$data$role_map$role == "outcome"]
  all_response_vars <- c(responses, outcomes)

  if (length(all_response_vars) == 0) {

    abort_horizons(c(
      "{.arg x} has no response data to configure.",
      "i" = "Use {.fn add_response} to join response variables before configuring."
    ), kind = "input")

  }

  ## 1.3 Validate config axis values -------------------------------------------

  axes <- list(
    models            = list(given = models,            valid = VALID_MODELS),
    transformations   = list(given = transformations,   valid = VALID_TRANSFORMATIONS),
    preprocessing     = list(given = preprocessing,     valid = VALID_PREPROCESSING),
    feature_selection = list(given = feature_selection, valid = VALID_FEATURE_SELECTION)
  )

  for (axis in names(axes)) {

    bad <- setdiff(axes[[axis]]$given, axes[[axis]]$valid)

    if (length(bad) > 0) {

      valid <- axes[[axis]]$valid

      abort_horizons(c(
        "{.arg {axis}} must be among the values horizons knows.",
        "x" = "Not known: {.val {bad}}.",
        "i" = "Valid values: {.val {valid}}."
      ), kind = "input")

    }

  }

  ## 1.4 Validate tuning parameters --------------------------------------------

  check_arg(rlang::check_number_whole, cv_folds, min = 2)
  check_arg(rlang::check_number_whole, grid_size, min = 1)
  check_arg(rlang::check_number_whole, bayesian_iter, min = 0)
  check_arg(rlang::check_number_whole, final_bayesian_iter, min = 0)

  ## evaluate()'s Bayesian search starts from a grid of grid_size points, and
  ## so do fit()'s on a cold start (bayesian_iter, as evaluate()'s) and its
  ## re-tune of a member whose parameters from evaluate() are unusable
  ## (final_bayesian_iter). tune_bayes() needs at least two of them to fit its
  ## Gaussian process, so on a grid of one none could run (#209).
  if (grid_size < 2 && (bayesian_iter > 0 || final_bayesian_iter > 0)) {

    searches <- c(
      if (bayesian_iter > 0)       paste0("bayesian_iter = ", bayesian_iter),
      if (final_bayesian_iter > 0) paste0("final_bayesian_iter = ", final_bayesian_iter)
    )

    abort_horizons(c(
      "{.arg grid_size} must be at least 2 when a Bayesian search runs.",
      "x" = "Got {.code grid_size = {grid_size}}, with {.code {searches}}.",
      "i" = "The Bayesian searches that start from the grid, evaluate()'s and fit()'s on a cold start and fit()'s re-tune of a member whose parameters from evaluate() are unusable, need at least two of its points; raise {.arg grid_size}, or set {.arg bayesian_iter} and {.arg final_bayesian_iter} to 0."
    ), kind = "input")

  }

  ## 1.4b Validate tune_ranges ------------------------------------------------
  ## Each name must be a parameter a configured model tunes, so a misspelt
  ## one is refused rather than ignored (tune_range_findings()); each range
  ## is typed on its parameter where the model's engine is installed, and
  ## evaluate() and fit() type it again after their package checks
  ## (range_type_findings()). The findings are rendered text, so they go
  ## into the refusal as values (abort_findings()).

  if (!is.null(tune_ranges)) {

    range_findings <- c(tune_range_findings(tune_ranges, models),
                        range_type_findings(tune_ranges, models))

    if (length(range_findings) > 0) {

      abort_findings(
        range_findings,
        "{.arg tune_ranges} must be a named list of ranges of parameters the configured models tune.",
        kind   = "input",
        remedy = "Each range is {.code c(lower, upper)} in dials' units, e.g. {.code tune_ranges = list(committees = c(1, 50))}."
      )

    }

  }

  ## 1.4c Validate max_weights -----------------------------------------------
  ## The weight budget: Inf sets none. The rule is is_valid_max_weights(),
  ## which the validator applies to the stored budget too.

  if (!is_valid_max_weights(max_weights)) {

    abort_horizons(c(
      "{.arg max_weights} must be a whole number of at least 1, or {.code Inf} for no budget.",
      "x" = "Got {.code {paste(deparse(max_weights), collapse = ' ')}}."
    ), kind = "input")

  }

  ## 1.5 Validate recipe settings ----------------------------------------------

  ## The window is centred on a point, so it is odd. Its floor is 5 because
  ## deriv2 and snv_deriv2 fit a cubic, and prospectr::savitzkyGolay() needs
  ## the window wider than the polynomial order; the setting is object-level,
  ## so it has to suit every method a grid might hold. The rule is the one
  ## step_transform_spectra() enforces (is_valid_sg_window()). Whether the
  ## window fits the spectrum is checked by evaluate(), since standardize()
  ## can still change the axis after this.

  if (!is_valid_sg_window(sg_window)) {

    abort_horizons(c(
      "{.arg sg_window} must be an odd whole number of at least 5.",
      "x" = "Got {.code {paste(deparse(sg_window), collapse = ' ')}}.",
      "i" = "The window is centred on a point, so its width is odd.",
      "i" = "The second-derivative methods fit a cubic, which needs at least 5 points."
    ), kind = "input")

  }

  ### The interval is open at 0, which rlang's checker cannot state.
  if (!is.numeric(pca_threshold) || length(pca_threshold) != 1 ||
      is.na(pca_threshold) || pca_threshold <= 0 || pca_threshold > 1) {

    abort_horizons(c(
      "{.arg pca_threshold} must be a single number in (0, 1].",
      "x" = "Got {.code {paste(deparse(pca_threshold), collapse = ' ')}}.",
      "i" = "It is the share of variance {.code feature_selection = \"pca\"} keeps."
    ), kind = "input")

  }

  ## 1.6 Validate outcome_range ------------------------------------------------

  ## Checked against the outcome's values in Step 2, once the outcome is
  ## known. The rule is is_valid_outcome_range(), which every reader of the
  ## stored range applies too.

  if (!is_valid_outcome_range(outcome_range)) {

    abort_horizons(c(
      "{.arg outcome_range} must be a numeric vector of length 2 with lower < upper.",
      "x" = "Got {.code {paste(deparse(outcome_range), collapse = ' ')}}.",
      "i" = "Either end may be infinite: {.code c(0, Inf)} is non-negative, {.code c(-Inf, Inf)} is unbounded."
    ), kind = "input")

  }

  ## A range that admits negative values rules out the response transforms.
  ## log(x + 1) is NaN below -1 and sqrt(x) below 0, so the recipe hands the
  ## model NaN outcomes, and the back-transforms reimpose a floor the range
  ## says is not there (exp(x) - 1 >= -1, x^2 >= 0). Some engines then fail
  ## after tuning, one crashes R, and some report success on NaN outcomes.
  ## The default range never trips this.

  signed_transforms <- intersect(transformations, c("log", "log10", "sqrt"))

  if (outcome_range[1] < 0 && length(signed_transforms) > 0) {

    range_text <- format_outcome_range(outcome_range)

    abort_horizons(c(
      "{.arg transformations} {.val {signed_transforms}} cannot be used with {.code outcome_range = {range_text}}, which admits negative values.",
      "i" = "log(x + 1) is undefined below -1 and sqrt(x) below 0, and their back-transforms floor the predictions.",
      "i" = "Use {.code transformations = \"none\"} for an outcome that can be negative."
    ), kind = "input")

  }

  collector <- log_open("configure")

  ## ---------------------------------------------------------------------------
  ## Step 2: Resolve and promote outcome
  ## ---------------------------------------------------------------------------

  ## 2.1 Reset any existing outcome to "response" -------------------------------

  reconfigured <- !is.null(x$config$configs)

  if (reconfigured) {

    ## Discarding an evaluation or a fit costs the user its run, so that is
    ## warned; replacing a configuration alone is what they asked for.
    discarded <- intersect(promoted_state(x),
                           c("evaluation results", "fitted models", "an ensemble"))

    ### The validation goes with the configuration, so the message names it
    ### when there was one to clear.
    cleared <- if (!is.null(x$validation$timestamp)) {
      c("i" = paste0("Cleared the validation",
                     if (!is.null(x$validation$outliers$response_trim)) ", with its response-trim request",
                     "; run {.fn validate} again."))
    }

    if (length(discarded) > 0) {

      log_warning(collector, "input", c(
        "Re-configuring discarded {discarded}, keyed to the previous configuration.",
        "i" = "Run {.fn evaluate} again on this object, or keep the earlier one to use them.",
        cleared
      ))

    } else {

      log_note(collector, c("Replaced the previous configuration.", cleared))

    }

    ## Promotion is earned, and reconfiguring un-earns it. The split, the row
    ## index and the cached predictions are all keyed to an outcome that is
    ## about to change, and the class is the claim that they are there, so
    ## the object goes back to being a plain horizons_data. The validation
    ## verdict goes too; the record of rows already removed, and the
    ## select_training() record, describe rows and are kept.

    x <- reset_promotion(x)

  }

  role_map <- x$data$role_map
  role_map$role[role_map$role == "outcome"] <- "response"
  responses <- role_map$variable[role_map$role == "response"]

  ## 2.2 Resolve outcome -------------------------------------------------------

  if (is.null(outcome)) {

    if (length(responses) == 1) {

      outcome_var <- responses

    } else {

      abort_horizons(c(
        "{.arg x} has several responses, so {.arg outcome} must name one.",
        "i" = "Available: {.field {responses}}."
      ), kind = "input")

    }

  } else {

    check_arg(rlang::check_string, outcome)

    if (!outcome %in% responses) {

      abort_horizons(c(
        "{.arg outcome} must name a response, and {.val {outcome}} is not one.",
        "i" = "Available: {.field {responses}}."
      ), kind = "input")

    }

    outcome_var <- outcome

  }

  ## 2.3 The outcome has to lie inside outcome_range ---------------------------

  ## Every prediction is clamped to the range, so an outcome outside it would
  ## be scored against predictions that cannot reach it. Refused here, where
  ## the values are first known, rather than after evaluate() has tuned every
  ## configuration (#76). evaluate() and fit() repeat the check, on the rows
  ## they model, and so does this one: a flagged row is no model's.

  modelled <- modelling_mask(x$data$analysis, outcome_var)
  breach   <- outcome_range_breach(x$data$analysis[[outcome_var]][modelled], outcome_range)

  if (!is.null(breach)) {

    range_text <- format_outcome_range(outcome_range)

    ### The breach lines hold numbers and the package's words, but the
    ### column name is the user's, so they go in as values, not templates.
    abort_horizons(c(
      "Outcome {.field {outcome_var}} lies outside {.code outcome_range = {range_text}}.",
      stats::setNames(sprintf("{breach$lines[%d]}.", seq_along(breach$lines)),
                      rep("x", length(breach$lines))),
      "i" = "{.arg outcome_range} is the outcome's physical range, and every prediction is clamped to it. The default, {.code c(0, Inf)}, is for non-negative properties.",
      "i" = "Set {.arg outcome_range} to the property's physical bounds: {.code c(-Inf, Inf)} for a signed property."
    ), kind = "input")

  }

  ## 2.4 Promote to outcome role -----------------------------------------------

  role_map$role[role_map$variable == outcome_var] <- "outcome"

  ## Demoting the old outcome and promoting the new one changes the response
  ## count, so the roles go back through set_analysis() rather than being
  ## written in place. Otherwise the stored n_responses disagrees with the
  ## role map and validate_horizons_data() aborts on the next verb.

  x <- set_analysis(x, x$data$analysis, role_map)

  ## 2.5 Name what the rows carry from an earlier outcome ----------------------

  ## The selection record describes rows, so it survives a re-configure, but
  ## it was not drawn with this outcome in mind. Warn rather than abort: the
  ## object is usable, and starting again from an earlier object is the
  ## user's call.

  warn_selection_properties(x, outcome_var, collector)

  ## ---------------------------------------------------------------------------
  ## Step 3: Build configuration grid
  ## ---------------------------------------------------------------------------

  config_grid <- tidyr::crossing(
    model             = models,
    transformation    = transformations,
    preprocessing     = preprocessing,
    feature_selection = feature_selection
  )

  ## ---------------------------------------------------------------------------
  ## Step 4: Generate config IDs
  ## ---------------------------------------------------------------------------

  config_grid$config_id <- mapply(
    generate_config_id,
    model             = config_grid$model,
    preprocessing     = config_grid$preprocessing,
    transformation    = config_grid$transformation,
    feature_selection = config_grid$feature_selection,
    USE.NAMES         = FALSE
  )

  ## Reorder: config_id first --------------------------------------------------

  config_grid <- config_grid[, c(
    "config_id", "model", "transformation", "preprocessing",
    "feature_selection"
  )]

  ## Deduplicate (defensive) ---------------------------------------------------

  config_grid <- config_grid[!duplicated(config_grid$config_id), ]

  ## Coerce to tibble ----------------------------------------------------------

  config_grid <- tibble::as_tibble(config_grid)

  ## ---------------------------------------------------------------------------
  ## Step 5: Store configuration
  ## ---------------------------------------------------------------------------

  x$config$configs   <- config_grid
  x$config$n_configs <- nrow(config_grid)

  x$config$tuning <- list(
    cv_folds            = as.integer(cv_folds),
    grid_size           = as.integer(grid_size),
    bayesian_iter       = as.integer(bayesian_iter),
    final_bayesian_iter = as.integer(final_bayesian_iter),
    ## horizons' default ranges with the given ones over them, which
    ## evaluate() and fit() search (apply_tune_ranges())
    ranges              = merge_ranges(tune_ranges),
    ## The weight budget mlp's hidden_units range is capped by
    ## (cap_by_data())
    max_weights         = as.double(max_weights)
  )

  x$config$expansion <- list(
    outcome           = outcome_var,
    models            = models,
    transformations   = transformations,
    preprocessing     = preprocessing,
    feature_selection = feature_selection
  )

  ## One value of each for the whole object, read by build_recipe() for every
  ## config through recipe_settings(). The window's width in cm-1 is not
  ## stored: it depends on the axis, which standardize() can still change, so
  ## it is printed below for the axis as it is now and recorded by evaluate()
  ## for the axis the recipe actually ran on.

  x$config$recipe <- list(
    sg_window     = as.integer(sg_window),
    pca_threshold = as.numeric(pca_threshold)
  )

  ## One range for the object's one outcome, read by every clamp through
  ## outcome_range_setting() (#76). Not a recipe setting, so kept beside
  ## config$recipe rather than in it.

  x$config$outcome_range <- as.numeric(unname(outcome_range))

  ## ---------------------------------------------------------------------------
  ## Step 5b: Re-validate
  ## ---------------------------------------------------------------------------

  x <- validate_horizons_data(x)

  x$log <- log_close(collector, x$log)

  ## ---------------------------------------------------------------------------
  ## Step 6: CLI output
  ## ---------------------------------------------------------------------------

  if (verbose) {

    cat(paste0("\u251C\u2500 ", cli::style_bold("Configuring pipelines"), "...\n"))

    if (reconfigured && length(discarded) == 0) {

      cat(paste0("\u2502  \u251C\u2500 Replaced the previous configuration\n"))

    }

    cat(paste0("\u2502  \u251C\u2500 Outcome: ", outcome_var,
               if (!identical(x$config$outcome_range, DEFAULT_OUTCOME_RANGE)) {
                 paste0(" (range ", format_outcome_range(x$config$outcome_range), ")")
               },
               "\n"))
    cat(paste0("\u2502  \u251C\u2500 Models: ", paste(models, collapse = ", "), "\n"))
    ## A weight budget the user set changes what mlp searches, so it is
    ## shown, as a non-default outcome range is
    budget <- x$config$tuning$max_weights

    cat(paste0("\u2502  \u251C\u2500 Tuning: ", cv_folds, "-fold CV, grid = ",
               grid_size,
               if (!identical(budget, DEFAULT_MAX_WEIGHTS)) {
                 if (is.infinite(budget)) ", no weight budget" else
                   paste0(", at most ", format(budget, scientific = FALSE), " weights a network")
               },
               "\n"))

    ## The ranges the configured models search where horizons or the user
    ## set one, so the committees cap is seen where it is set
    ranges_line <- configured_ranges_line(x$config$tuning$ranges, models)

    if (!is.null(ranges_line)) {

      cat(paste0("\u2502  \u251C\u2500 Ranges: ", ranges_line, "\n"))

    }

    window_cm <- x$config$recipe$sg_window * axis_spacing_cm(x)

    cat(paste0("\u2502  \u251C\u2500 Recipe: SG window ", x$config$recipe$sg_window,
               if (!is.na(window_cm)) paste0(" (", signif(window_cm, 3), " cm\u207B\u00B9)"),
               if ("pca" %in% feature_selection) paste0(", PCA threshold ", pca_threshold),
               "\n"))

    cat(paste0("\u2502  \u2514\u2500 Configs: ", nrow(config_grid), " total\n"))
    cat("\u2502\n")

  }

  signal_log(collector, x)

  ## ---------------------------------------------------------------------------
  ## Step 7: Return
  ## ---------------------------------------------------------------------------

  x

}


## =============================================================================
## Internal helpers
## =============================================================================

#' Generate a deterministic config ID
#'
#' @param model Character. Model short name.
#' @param preprocessing Character. Preprocessing method.
#' @param transformation Character. Response transformation.
#' @param feature_selection Character. Feature selection method.
#'
#' @return Character. Config ID in format `{model}_{preprocessing}_{transformation}_{feature_selection}_{6-char hash}`.
#' @keywords internal

generate_config_id <- function(model, preprocessing, transformation,
                               feature_selection) {

  base <- paste(model, preprocessing, transformation,
                feature_selection, sep = "_")

  hash_input <- list(
    model             = model,
    preprocessing     = preprocessing,
    transformation    = transformation,
    feature_selection = feature_selection
  )

  hash <- substr(digest::digest(hash_input), 1, 6)
  paste(base, hash, sep = "_")

}


#' Spacing of an object's wavenumber axis, in cm-1
#'
#' @description
#' The median absolute spacing of the predictor columns' wavenumbers, read
#' from their names (`wn_<wavenumber>`, or a bare number). That is the axis
#' the recipe's Savitzky-Golay window slides along, so it is what converts the
#' window from grid points to cm-1. The grid step `standardize()` recorded is
#' the fallback, used only when the names do not parse.
#'
#' The axis comes first because a record is only as true as whatever wrote
#' it last. `select_training()` writes its training set's record to describe
#' the columns it returns (#90), so a library standardized at 2 cm-1 and
#' drawn around a batch at 4 cm-1 records a step of 4, and the two agree.
#'
#' Read at the moment it is needed, never stored: `configure()` prints the
#' window's width for the axis it sees, and `evaluate()` records the width for
#' the axis the recipe runs on, which `standardize()` may have changed since.
#'
#' @param x `horizons_data`. The object being configured or evaluated.
#'
#' @return `numeric(1)`. The spacing in cm-1, or `NA` when neither the names
#'   nor the standardization record give one.
#' @noRd

axis_spacing_cm <- function(x) {

  role_map   <- x$data$role_map
  predictors <- role_map$variable[role_map$role == "predictor"]
  wn         <- wn_values(predictors)

  if (length(wn) > 1 && !anyNA(wn)) {

    return(stats::median(abs(diff(wn))))

  }

  step <- x$data$standardization$grid$step

  if (is.numeric(step) && length(step) == 1 && !is.na(step)) {

    return(step)

  }

  NA_real_

}


## -----------------------------------------------------------------------------
## Outcome range (#76)
## -----------------------------------------------------------------------------

#' Is this a usable outcome range?
#'
#' @description
#' A numeric vector of two non-missing values, lower strictly below upper;
#' either end may be infinite. `configure()` refuses anything else, and every
#' verb's door validator applies the same rule to a stored range
#' ([config_findings()]), so a range that clamps everything to one value, or
#' nothing to anything sensible, never reaches a prediction.
#'
#' @param r The range to check.
#'
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
is_valid_outcome_range <- function(r) {

  is.numeric(r) && length(r) == 2 && !anyNA(r) && r[1] < r[2]

}


#' Is this a usable weight budget?
#'
#' @description
#' One number, a whole number of at least 1, or `Inf` for no budget.
#' `configure()` refuses anything else, and every verb's door validator
#' applies the same rule to the stored budget ([config_findings()]).
#'
#' @param x The budget to check.
#'
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
is_valid_max_weights <- function(x) {

  is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 1 && (is.infinite(x) || x == round(x))

}


#' The outcome range an object's predictions are clamped to
#'
#' @description
#' Reads the `outcome_range` `configure()` recorded in `x$config$outcome_range`.
#' Every verb's door validator refuses a configured object whose range is
#' missing or breaks [is_valid_outcome_range()] ([config_findings()], #234),
#' so this only reads it.
#'
#' @param x A `horizons_data` object, or any subclass.
#'
#' @return `numeric(2)`.
#' @keywords internal
#' @noRd
outcome_range_setting <- function(x) {

  x$config$outcome_range

}


## ---------------------------------------------------------------------------
## The tuning ranges (configure(tune_ranges = ))
## ---------------------------------------------------------------------------

#' The tuned parameters of a set of models, and the models that tune each
#'
#' @param models Character. Short model names.
#' @return Named list: parameter id -> the models (character) tuning it, in
#'   the order the models tune them.
#' @keywords internal
#' @noRd
tuned_by <- function(models) {

  models <- unique(models)
  ids    <- unique(unlist(lapply(models, function(m) MODEL_SPECS[[m]]$tuned), use.names = FALSE))

  stats::setNames(lapply(ids, function(id) {
    models[vapply(models, function(m) id %in% MODEL_SPECS[[m]]$tuned, logical(1))]
  }), ids)

}

#' The ranges an object records: horizons' defaults with the user's over them
#'
#' @param given Named list of `c(lower, upper)`, or `NULL`.
#' @return Named list of double pairs, sorted by name.
#' @keywords internal
#' @noRd
merge_ranges <- function(given) {

  ranges <- utils::modifyList(TUNE_RANGES, given %||% list())

  lapply(ranges[sort(names(ranges), method = "radix")], function(r) as.double(unname(r)))

}

#' The configure tree's line of the ranges the configured models search
#'
#' @param ranges The recorded ranges.
#' @param models Character. The configured models.
#' @return Character(1), e.g. `"committees 1 to 25 (cubist); num_comp 1 to 30
#'   (plsr)"`, or `NULL` when no configured model tunes a ranged parameter.
#' @keywords internal
#' @noRd
configured_ranges_line <- function(ranges, models) {

  tuned <- tuned_by(models)
  ids   <- intersect(sort(names(ranges), method = "radix"), names(tuned))

  if (length(ids) == 0) return(NULL)

  paste(vapply(ids, function(id) {
    paste0(format_ranges(ranges[id]), " (", paste(tuned[[id]], collapse = ", "), ")")
  }, character(1), USE.NAMES = FALSE), collapse = "; ")

}

#' A set of ranges as one text, the form the checkpoint settings record
#'
#' @param ranges Named list of `c(lower, upper)`.
#' @return Character(1), e.g. `"committees 1 to 25; num_comp 1 to 30"`,
#'   sorted by name.
#' @keywords internal
#' @noRd
format_ranges <- function(ranges) {

  ids <- sort(names(ranges), method = "radix")

  paste(vapply(ids, function(id) {
    paste(id, as.character(as.double(ranges[[id]][1])), "to", as.character(as.double(ranges[[id]][2])))
  }, character(1), USE.NAMES = FALSE), collapse = "; ")

}

#' What is wrong with a set of tuning ranges
#'
#' @description
#' The one check of a set of ranges' shape, at `configure()`'s door and in
#' every verb's validator ([config_findings()]): a named list, its names
#' unique, each element two finite numbers with the lower below the upper,
#' and an `mtry`, `num_comp` or `hidden_units` range starting at 1 or more
#' (`BOUNDED_PARAMETERS`, which count what a fit uses). With `check_names`,
#' each name must be a parameter a model in `models` tunes (their ids in
#' `MODEL_SPECS`), so a misspelt one is refused rather than ignored. It
#' needs no package: whether a range is of its parameter's type is
#' [range_type_findings()]'s.
#'
#' @param ranges The ranges, as given.
#' @param models Character. The configured models, read only with
#'   `check_names`.
#' @param arg Character(1). How the messages name the ranges.
#' @param check_names Logical(1). Refuse a name no model in `models` tunes.
#' @return Character: one finding per problem, empty when there are none.
#' @keywords internal
#' @noRd
tune_range_findings <- function(ranges, models = character(0), arg = "tune_ranges", check_names = TRUE) {

  if (!is.list(ranges) || is.data.frame(ranges)) {
    return(cli::format_inline("{.arg {arg}} must be a named list of ranges, not {.obj_type_friendly {ranges}}."))
  }

  if (length(ranges) == 0) return(character(0))

  ids <- names(ranges)

  if (is.null(ids) || anyNA(ids) || !all(nzchar(ids))) {
    return(cli::format_inline("Every range in {.arg {arg}} must be named by the parameter it sets."))
  }

  findings <- character(0)
  repeated <- unique(ids[duplicated(ids)])

  if (length(repeated) > 0) {
    findings <- c(findings, cli::format_inline("{.arg {arg}} names {.field {repeated}} more than once."))
  }

  shaped <- vapply(ranges, is_range_pair, logical(1))

  for (id in ids[!shaped]) {
    got      <- paste(deparse(ranges[[id]]), collapse = " ")
    findings <- c(findings, cli::format_inline(
      "{.arg {arg}}'s {.field {id}} must be two finite numbers, the lower below the upper, not {.code {got}}."
    ))
  }

  ## The parameters the data bound (BOUNDED_PARAMETERS) count the
  ## predictors, components or hidden units a fit uses, so a range below 1
  ## holds values that are no model: every plsr count fails at num_comp = 0,
  ## from the one fit the counts share, ranger reads mtry = 0 as its own
  ## default, and a network of no hidden units has no hidden layer
  for (id in intersect(ids[shaped], BOUNDED_PARAMETERS)) {
    lower <- ranges[[id]][1]
    if (lower < 1) {
      findings <- c(findings, cli::format_inline(
        "{.arg {arg}}'s {.field {id}} must start at 1 or more, not {lower}: it counts the predictors, components or hidden units a fit uses, at least one."
      ))
    }
  }

  if (check_names) {

    unknown <- setdiff(ids, names(tuned_by(models)))

    if (length(unknown) > 0) {
      listed   <- paste0(vapply(unique(models), function(m) {
        paste0(paste(MODEL_SPECS[[m]]$tuned, collapse = ", "), " (", m, ")")
      }, character(1)), collapse = "; ")
      findings <- c(findings, cli::format_inline(
        "{.arg {arg}} names {cli::qty(length(unknown))}{?a parameter/parameters} no configured model tunes: {.field {unknown}}. The configured models tune {listed}."
      ))
    }

  }

  findings

}

#' Whether a range is two finite numbers, the lower below the upper
#'
#' @param r A range, as given.
#' @return Logical(1).
#' @keywords internal
#' @noRd
is_range_pair <- function(r) {

  is.numeric(r) && length(r) == 2 && all(is.finite(r)) && r[1] < r[2]

}

#' Which ranges are not of their parameters' types
#'
#' @description
#' Each well-shaped range ([tune_range_findings()]) is set on the parameter
#' of every model in `models` that tunes it ([dials::range_set()]), so a
#' range of the wrong type, a fractional bound on a whole-number parameter,
#' is refused in dials' words. Building a model's parameter set needs its
#' engine, so a model whose packages are not installed is skipped:
#' `configure()`, which checks no packages, types what it can, and
#' [evaluate()] and [fit()] type every range of the models they fit at
#' their doors, right after their package checks
#' ([check_config_packages()]), where nothing is skipped
#' ([check_range_types()]). The validators check the ranges' shape alone,
#' so a fitted object is validated without loading the engines of models
#' it holds no member of.
#'
#' @param ranges The ranges, as given or recorded.
#' @param models Character. The models whose parameters the ranges are set
#'   on.
#' @param arg Character(1). How the messages name the ranges.
#' @return Character: one finding per range a model's parameter refused,
#'   empty when there are none.
#' @keywords internal
#' @noRd
range_type_findings <- function(ranges, models, arg = "tune_ranges") {

  if (!is.list(ranges) || is.data.frame(ranges) || is.null(names(ranges))) return(character(0))

  ids    <- names(ranges)
  usable <- !is.na(ids) & nzchar(ids) & !duplicated(ids) & vapply(ranges, is_range_pair, logical(1))
  ranges <- ranges[usable]

  findings <- character(0)

  for (m in unique(models)) {

    ids_m <- intersect(names(ranges), MODEL_SPECS[[m]]$tuned)
    if (length(ids_m) == 0) next

    if (!all(vapply(MODEL_SPECS[[m]]$packages, requireNamespace, logical(1), quietly = TRUE))) next

    param_set <- hardhat::extract_parameter_set_dials(define_model_spec(m, isolate = FALSE))

    for (id in ids_m) {

      param   <- param_set$object[[which(param_set$id == id)]]
      refused <- tryCatch({
        dials::range_set(param, as.double(ranges[[id]]))
        NULL
      }, error = function(e) condition_summary(e))

      if (!is.null(refused)) {
        findings <- c(findings, cli::format_inline(
          "{.arg {arg}}'s {.field {id}} is not a range of {m}'s {.field {id}}: {refused}"
        ))
      }

    }

  }

  findings

}

#' Refuse recorded ranges the models a verb fits cannot take
#'
#' @description
#' The door check of [evaluate()] and [fit()], straight after their package
#' checks, so every engine it builds a parameter set for is installed: each
#' recorded range typed on the parameters of `models`
#' ([range_type_findings()]). `configure()` types a range only where the
#' model's engine is installed, so one it could not type, or one edited by
#' hand, is refused here before any work.
#'
#' @param ranges `config$tuning$ranges`.
#' @param models Character. The models the verb fits.
#' @param call The frame the refusal reports: the verb's.
#' @return Invisibly `NULL`; aborts with `horizons_error_validation`, its
#'   findings as `findings`.
#' @keywords internal
#' @noRd
check_range_types <- function(ranges, models, call = rlang::caller_env()) {

  findings <- range_type_findings(ranges, models, arg = "config$tuning$ranges")

  if (length(findings) > 0) {

    abort_validation(findings,
                     remedy = "Re-run {.fn configure} with {.arg tune_ranges} the models' parameters can take.",
                     call   = call)

  }

  invisible(NULL)

}


#' Which outcome values lie outside a range
#'
#' @description
#' The check `configure()`, `evaluate()`, `fit()` and `ensemble()` share.
#' Missing values are ignored, since no verb models them. Infinite values
#' count as breaches whatever the range: no range admits them, and one would
#' otherwise pass under an infinite bound and make [compute_response_bound()]
#' return `Inf`, which the fit validator refuses only after tuning. A
#' non-numeric outcome is not this check's to judge, and passes.
#'
#' @param values Numeric vector of outcome values.
#' @param outcome_range `numeric(2)`.
#'
#' @return `NULL` when every non-missing value is finite and lies inside the
#'   range. Otherwise a list with `n_below`, `n_above` and `n_infinite`
#'   (counts), `min` and `max` (of the non-missing values), and `lines`, one
#'   sentence per kind of breach for a message.
#' @keywords internal
#' @noRd
outcome_range_breach <- function(values, outcome_range) {

  if (!is.numeric(values)) return(NULL)

  y        <- values[!is.na(values)]
  finite   <- is.finite(y)
  infinite <- sum(!finite)
  below    <- sum(finite & y < outcome_range[1])
  above    <- sum(finite & y > outcome_range[2])

  if (below == 0 && above == 0 && infinite == 0) return(NULL)

  of_n <- function(k) paste0(k, " of ", length(y), " value", if (length(y) != 1) "s")

  lines <- c(
    if (below > 0) paste0(of_n(below), " below the lower bound ", outcome_range[1],
                          " (minimum ", signif(min(y[finite]), 4), ")"),
    if (above > 0) paste0(of_n(above), " above the upper bound ", outcome_range[2],
                          " (maximum ", signif(max(y[finite]), 4), ")"),
    if (infinite > 0) paste0(of_n(infinite), " infinite, which no range admits; ",
                             "correct or drop ", if (infinite == 1) "it" else "them")
  )

  list(n_below = below, n_above = above, n_infinite = infinite,
       min = min(y), max = max(y), lines = lines)

}


#' Refuse an outcome that lies outside the object's range
#'
#' @description
#' `evaluate()`, `fit()` and `ensemble()` call this on entry, before they
#' draw a split or fit anything, so an object whose outcome no longer fits its
#' range fails in a second rather than after the tuning cost (#76). The rows
#' are the ones the verbs model ([modelling_mask()]): missing outcomes and
#' flagged rows are ignored, since no model is fitted on them or scored
#' against them.
#'
#' @param x A configured `horizons_data` object.
#' @param verb `character(1)`. The calling verb, named in the message.
#' @param call The call the condition is attributed to. Default: the caller.
#'
#' @return `NULL`, invisibly. Aborts with class `horizons_error_input`.
#' @keywords internal
#' @noRd
check_outcome_range <- function(x, verb, call = rlang::caller_env()) {

  role_map    <- x$data$role_map
  outcome_col <- role_map$variable[role_map$role == "outcome"]

  ## No single outcome to check: the verbs' own gates say so.
  if (length(outcome_col) != 1 || !outcome_col %in% names(x$data$analysis)) {

    return(invisible(NULL))

  }

  ## A missing or unusable stored range is the door validator's finding.
  ## ensemble() runs this check before its validator, so it is left to the
  ## validator here rather than read.
  if (!is_valid_outcome_range(x$config$outcome_range)) {

    return(invisible(NULL))

  }

  outcome_range <- outcome_range_setting(x)
  modelled      <- modelling_mask(x$data$analysis, outcome_col)
  breach        <- outcome_range_breach(x$data$analysis[[outcome_col]][modelled], outcome_range)

  if (is.null(breach)) return(invisible(NULL))

  range_text <- format_outcome_range(outcome_range)

  ## The breach lines are built from numbers and the package's own words, but
  ## the column name is the user's, so both go in as values, not templates.
  abort_horizons(c(
    "{.field {outcome_col}} lies outside {.arg outcome_range}, {range_text}, so {.fn {verb}} would score it against predictions clamped to that range.",
    stats::setNames(sprintf("{breach$lines[%d]}", seq_along(breach$lines)),
                    rep("x", length(breach$lines))),
    "i" = "{.arg outcome_range} is the outcome's physical range, set by {.fn configure}. The default, {.code c(0, Inf)}, is for non-negative properties.",
    "i" = "Re-run {.fn configure} with the property's physical bounds as {.arg outcome_range}: {.code c(-Inf, Inf)} for a signed property."
  ), kind = "input", call = call)

}


#' Refuse rrmse as the ranking metric under a range that admits negatives
#'
#' @description
#' `rrmse` is `100 * rmse / mean(truth)`. Once the outcome can be negative its
#' mean can be zero or negative, the metric's sign flips, and ranking by its
#' minimum picks the worst configuration: in `evaluate()`, in `fit()`'s
#' member ranking, and in the ensemble's meta-learner selection and its
#' improvement over the best member, which read the rank metric `fit()`
#' recorded. Refused wherever a range with a negative lower bound meets it,
#' so the default range never trips it.
#'
#' @param metric `character(1)` or `NULL`. The metric the verb will rank by.
#' @param outcome_range `numeric(2)`.
#' @param verb `character(1)`. The calling verb, named in the message.
#' @param call The call the condition is attributed to. Default: the caller.
#'
#' @return `NULL`, invisibly. Aborts with class `horizons_error_input`.
#' @keywords internal
#' @noRd
check_rank_metric_range <- function(metric, outcome_range, verb,
                                    call = rlang::caller_env()) {

  if (!identical(metric, "rrmse") || outcome_range[1] >= 0) {

    return(invisible(NULL))

  }

  range_text <- format_outcome_range(outcome_range)

  abort_horizons(c(
    "{.fn {verb}} cannot rank by {.val rrmse} under {.arg outcome_range} {range_text}.",
    "x" = "{.val rrmse} divides the RMSE by the mean outcome, which can be zero or negative when the outcome can be, so a smaller value is not a better model.",
    "i" = "Rank by {.val rmse}, {.val mae}, {.val rpd}, {.val rsq} or {.val ccc}."
  ), kind = "input", call = call)

}


## ---------------------------------------------------------------------------
## Outcome variance: evaluate() and fit() refuse a constant outcome
## ---------------------------------------------------------------------------
## validate() reports a constant outcome (check P003), but nothing gates on
## its verdict and running it is optional, so the modelling verbs refuse one
## themselves (#132): on the rows they model, before any split, and on the
## rows a model is fitted on, which the split, a response trim or fit()'s
## calibration set can leave constant when the modelled rows are not.

#' The one value a constant outcome takes
#'
#' @param values Outcome values; `NA`s are dropped.
#' @return `NULL` when the non-missing values take at least two distinct
#'   values, or when there are fewer than two of them (the row floors judge
#'   those). Otherwise a list with `value` (the value every row has) and `n`
#'   (how many rows have it). Equality is exact, so a near-constant outcome
#'   is not constant.
#' @keywords internal
#' @noRd
constant_outcome <- function(values) {

  y <- values[!is.na(values)]

  if (length(y) < 2 || any(y != y[1])) return(NULL)

  list(value = y[1], n = length(y))

}


#' Format an outcome value for a message
#'
#' @param value A single outcome value.
#' @return `character(1)`.
#' @keywords internal
#' @noRd
format_outcome_value <- function(value) {

  if (is.numeric(value)) format(signif(value, 4)) else as.character(value)

}


#' Refuse an outcome with zero variance on the rows a verb models
#'
#' @description
#' `evaluate()` and `fit()` (on either path) call this beside
#' [check_outcome_range()], before they draw a split or fit anything, so a
#' constant outcome fails in a second rather than after the tuning cost or
#' with an error from inside a model. The rows are the ones the verbs model
#' ([modelling_mask()]): missing outcomes and flagged rows are ignored.
#'
#' @param x A configured `horizons_data` object.
#' @param verb `character(1)`. The calling verb, named in the message.
#' @param call The call the condition is attributed to. Default: the caller.
#' @return `NULL`, invisibly. Aborts with class `horizons_error_input`.
#' @keywords internal
#' @noRd
check_outcome_variance <- function(x, verb, call = rlang::caller_env()) {

  role_map    <- x$data$role_map
  outcome_col <- role_map$variable[role_map$role == "outcome"]

  ## No single outcome to check: the verbs' own gates say so.
  if (length(outcome_col) != 1 || !outcome_col %in% names(x$data$analysis)) {

    return(invisible(NULL))

  }

  modelled <- modelling_mask(x$data$analysis, outcome_col)
  constant <- constant_outcome(x$data$analysis[[outcome_col]][modelled])

  if (is.null(constant)) return(invisible(NULL))

  value_text <- format_outcome_value(constant$value)

  ### A flagged row with an outcome is not one of them, so the count says so
  rows_text <- if (any(!modelled & !is.na(x$data$analysis[[outcome_col]]))) "unflagged rows" else "rows"

  abort_horizons(c(
    "{.fn {verb}} cannot model {.field {outcome_col}}: it has zero variance.",
    "x" = "All {constant$n} {rows_text} with an observed {.field {outcome_col}} have the value {value_text}.",
    "i" = "A model fitted to a constant learns nothing from the spectra. Check that the outcome role names the right column and that its values were joined to the right samples."
  ), kind = "input", call = call)

}


#' Refuse training rows whose outcome does not vary
#'
#' @description
#' The modelled rows can vary while the rows a model is fitted on do not:
#' the few rows with another value can all fall in the test set, be trimmed
#' as response outliers, or be drawn into `fit()`'s calibration set. Every
#' model would then be fitted to a constant, and when the constant is a
#' finite lower bound of `outcome_range` the response bound equals that
#' bound, which the fit validator refused only after every model had been
#' fitted (#132). `evaluate()` calls this on the training part left after
#' the split and the trim, before its CV folds; `fit()` on the rows its
#' models are fitted on, on either path, before its folds.
#'
#' @param train_values Outcome values of the rows the models are fitted on.
#' @param held_out Named list of outcome vectors: the other modelled rows,
#'   by where they went. Each name completes "<count> ...", e.g.
#'   `"in the test set"`. A `NULL` element counts as no rows.
#' @param outcome_col `character(1)`. The outcome column, named in the
#'   message.
#' @param verb `character(1)`. The calling verb, named in the message.
#' @param rerun `character(1)`. The verb that applies the trim, named in the
#'   advice to drop it: `"evaluate"`, or `"fit"` on a cold start.
#' @param call The call the condition is attributed to. Default: the caller.
#' @return `NULL`, invisibly. Aborts with class `horizons_error_input`.
#' @keywords internal
#' @noRd
check_training_outcome_variance <- function(train_values, held_out, outcome_col,
                                            verb, rerun = "evaluate",
                                            call = rlang::caller_env()) {

  constant <- constant_outcome(train_values)

  if (is.null(constant)) return(invisible(NULL))

  value_text <- format_outcome_value(constant$value)

  ## Where the modelled rows with another value went
  n_other <- vapply(held_out, function(v) {
    sum(!is.na(v) & v != constant$value)
  }, integer(1))
  n_other <- n_other[n_other > 0]
  k_other <- sum(n_other)
  where   <- paste(n_other, names(n_other), collapse = ", ")

  ## The remedy depends on where those rows went: a trim or the calibration
  ## draw can be undone; rows in the test set only come back with more data
  remedies <- c(
    if ("trimmed as response outliers" %in% names(n_other)) {
      c("i" = "To keep the trimmed rows in training, re-run {.fn validate} without {.code remove_outliers = \"response\"}, then {.fn {rerun}}.")
    },
    if ("in the calibration set" %in% names(n_other)) {
      c("i" = "To keep the calibration rows in training, fit with {.code compute_uq = FALSE, compute_ad = FALSE}.")
    }
  )
  remedies <- c(remedies, c("i" = if (length(remedies) > 0) {
    "Otherwise {.field {outcome_col}} needs more samples where it varies."
  } else {
    "{.field {outcome_col}} needs more samples where it varies."
  }))

  abort_horizons(c(
    "{.fn {verb}} cannot fit {.field {outcome_col}}: all {constant$n} training rows have the value {value_text}.",
    "x" = if (k_other > 0) "The {k_other} modelled row{?s} with another value {?is/are all} outside the training rows: {where}.",
    "i" = "Every model would be fitted to a constant.",
    remedies
  ), kind = "input", call = call)

}


#' Format an outcome range the way it is typed
#'
#' @param outcome_range `numeric(2)`.
#' @return `character(1)`, e.g. `"c(0, Inf)"`.
#' @keywords internal
#' @noRd
format_outcome_range <- function(outcome_range) {

  paste0("c(", paste(as.character(outcome_range), collapse = ", "), ")")

}


#' Warn when the training rows were drawn for other properties
#'
#' @description
#' `select_training()` draws each target's `k` nearest pool rows per
#' property, among the rows that have that property measured. An outcome
#' outside `settings$properties` inherits rows drawn for something else, so
#' the per-target guarantee does not hold for it. `scope = "global"` draws
#' the whole pool, so there is no guarantee to lose.
#'
#' @param x `horizons_data`. The object being configured.
#' @param outcome_var `character(1)`. The outcome being configured.
#' @param collector `configure()`'s collector ([log_open()]), which the
#'   warning is added to.
#'
#' @return `NULL`, invisibly. Called for its warning.
#' @noRd

warn_selection_properties <- function(x, outcome_var, collector) {

  settings <- x$selection$settings

  if (is.null(settings$properties) || identical(settings$scope, "global") ||
      outcome_var %in% settings$properties) {

    return(invisible(NULL))

  }

  properties <- settings$properties

  log_warning(collector, "input", c(
    "The training rows were drawn by {.fn select_training} for {.field {properties}}, and {.field {outcome_var}} is not one of them, so each target's k nearest rows were not drawn for it.",
    "i" = "Re-run {.fn select_training} with {.field {outcome_var}} among {.arg properties} to draw rows for it."
  ))

  invisible(NULL)

}
