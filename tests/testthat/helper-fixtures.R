# Test Data Fixtures and Generators
# Helper functions to create consistent test data across the test suite


## ---------------------------------------------------------------------------
## Synthetic spectra with an outcome every preprocessing keeps
## ---------------------------------------------------------------------------

#' `n` synthetic spectra of `n_wn` bands and an outcome they carry, for any
#' fixture whose rows go through a configuration's recipe. Each spectrum is
#' one baseline shape shared by every row (half a cosine across the bands), a
#' peak whose height varies by row, and small noise; the outcome is the
#' peak's height rescaled, plus measurement noise drawn uniformly within
#' `outcome_noise` of it.
#'
#' The peak's apex is the band just past the middle and its flank the one
#' before it, so both sit in the bands the default window keeps
#' (transform_spectra_matrix() trims (window - 1) / 2 bands at each edge,
#' leaving the middle two of ten): the raw and smoothed bands read the
#' height, the first derivative the flank's slope, the second the apex's
#' curvature. The baseline gives SNV a shape to scale each spectrum against,
#' so the height survives its division. The outcome lies between
#' 1 - outcome_noise and 3 + outcome_noise by construction, inside the
#' default outcome_range, as a soil property does. Draws from the session's
#' random stream, so the caller seeds it. Returns `spectra`, a matrix named on
#' the fixtures' wavenumber axis, and `outcome`.
make_signal_spectra <- function(n, n_wn = 10, outcome_noise = 0.4) {

  band     <- seq_len(n_wn)
  baseline <- cos(pi * (band - 1) / max(n_wn - 1, 1))
  peak     <- exp(-(band - (n_wn %/% 2 + 1))^2 / 2)
  height   <- stats::runif(n)

  spectra <- outer(rep(1, n), baseline) + outer(height, peak) +
    matrix(stats::rnorm(n * n_wn, sd = 0.03), nrow = n)
  colnames(spectra) <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))

  list(spectra = spectra,
       outcome = 1 + 2 * height + stats::runif(n, -outcome_noise, outcome_noise))

}


## ---------------------------------------------------------------------------
## Evaluate Test Object
## ---------------------------------------------------------------------------

#' Build a minimal horizons_data object ready for evaluate()
make_eval_object <- function(n = 40, n_wn = 10, n_configs = 2,
                             add_validation = TRUE) {

  set.seed(42)

  ## Spectra and an outcome they carry
  sim      <- make_signal_spectra(n, n_wn)
  wn_names <- colnames(sim$spectra)

  df <- tibble::as_tibble(sim$spectra)
  df$sample_id <- paste0("S", sprintf("%03d", seq_len(n)))
  df$SOC       <- sim$outcome

  ## Role map
  roles <- tibble::tibble(
    variable = c("sample_id", wn_names, "SOC"),
    role     = c("id", rep("predictor", n_wn), "outcome")
  )

  ## Build configs
  models <- rep(c("rf", "cubist"), length.out = n_configs)
  configs <- tibble::tibble(
    config_id         = paste0("cfg_", sprintf("%03d", seq_len(n_configs))),
    model             = models,
    transformation    = "none",
    preprocessing     = "raw",
    feature_selection = "none"
  )

  ## Built from the constructor, so every key the contract declares (the log
  ## among them) arrives on its own; only what the tests set is written here.
  obj <- new_horizons_data(analysis = df, role_map = roles)

  obj$config$configs       <- configs
  obj$config$n_configs     <- n_configs
  obj$config$tuning        <- list(cv_folds = 3L, grid_size = 2L, bayesian_iter = 0L,
                                   final_bayesian_iter = DEFAULT_FINAL_BAYES_ITER,
                                   ranges = merge_ranges(NULL))

  ## What configure() writes beside the grid, at its defaults
  obj$config$recipe        <- list(sg_window     = DEFAULT_SG_WINDOW,
                                   pca_threshold = DEFAULT_PCA_THRESHOLD)
  obj$config$outcome_range <- DEFAULT_OUTCOME_RANGE

  obj$validation["passed"]    <- list(if (add_validation) TRUE)
  obj$validation["timestamp"] <- list(if (add_validation) Sys.time())

  obj

}

#' make_eval_object() on the grid the circuit breaker's rule is read on: the
#' three models with a thread argument (lightgbm, rf, xgboost), two
#' preprocessing methods and two feature selections (CARS and none), twelve
#' configurations in configure()'s order, each named by its parts
make_breaker_grid_object <- function(n = 60) {

  obj  <- make_eval_object(n = n, n_configs = 12)
  grid <- tidyr::crossing(model = c("lightgbm", "rf", "xgboost"), transformation = "none",
                          preprocessing = c("raw", "snv"), feature_selection = c("cars", "none"))

  grid$config_id     <- paste(grid$model, grid$preprocessing, grid$feature_selection, sep = "_")
  obj$config$configs <- grid[, c("config_id", "model", "transformation", "preprocessing", "feature_selection")]

  obj

}


## ---------------------------------------------------------------------------
## Fit Test Object: a minimal horizons_eval object ready for fit()
## ---------------------------------------------------------------------------

make_fit_object <- function(n = 60, n_wn = 10, n_configs = 2, seed = 42,
                            n_na = 0L, models = c("rf", "cubist"), feature_selection = "none") {

  set.seed(seed)

  ## Spectra and an outcome they carry
  sim      <- make_signal_spectra(n, n_wn)
  wn_names <- colnames(sim$spectra)

  df <- tibble::as_tibble(sim$spectra)
  df$sample_id <- paste0("S", sprintf("%03d", seq_len(n)))
  df$SOC       <- sim$outcome

  ## Rows with no measured outcome, as add_response() leaves them (#67)
  if (n_na > 0) df$SOC[seq_len(n_na)] <- NA_real_

  ## Role map
  roles <- tibble::tibble(
    variable = c("sample_id", wn_names, "SOC"),
    role     = c("id", rep("predictor", n_wn), "outcome")
  )

  ## Configs: rf + cubist by default (fast, tunable), with no selection
  ## unless one is given for each
  configs <- tibble::tibble(
    config_id         = paste0("cfg_", sprintf("%03d", seq_len(n_configs))),
    model             = models[seq_len(n_configs)],
    transformation    = "none",
    preprocessing     = "raw",
    feature_selection = rep_len(feature_selection, n_configs)
  )

  ## Built from the constructor, so every key the contract declares (the log
  ## among them) arrives on its own; only what the tests set is written here.
  obj <- new_horizons_data(analysis = df, role_map = roles)

  obj$config$configs       <- configs
  obj$config$n_configs     <- n_configs
  obj$config$tuning        <- list(cv_folds = 3L, grid_size = 2L, bayesian_iter = 0L,
                                   final_bayesian_iter = DEFAULT_FINAL_BAYES_ITER,
                                   ranges = merge_ranges(NULL))

  ## What configure() writes beside the grid, at its defaults
  obj$config$recipe        <- list(sg_window     = DEFAULT_SG_WINDOW,
                                   pca_threshold = DEFAULT_PCA_THRESHOLD)
  obj$config$outcome_range <- DEFAULT_OUTCOME_RANGE

  obj$validation$passed    <- TRUE
  obj$validation$timestamp <- Sys.time()

  ## Run evaluate() to populate evaluation slot
  ## prune = FALSE ensures every config gets status "success"
  suppressWarnings(
    evaluate(obj, prune = FALSE, seed = seed)
  )

}


## ---------------------------------------------------------------------------
## An evaluation ranked by hand
## ---------------------------------------------------------------------------

#' `obj` (an evaluated object) with its configurations ranked in the order of
#' `ids`, best first, for a test of fit() whose point is the order its
#' members run or print in, whatever the fixture's fits scored. Only the
#' cross-validated column of the evaluation's rank metric changes, set to 1
#' to `length(ids)` in the metric's direction, and best_config with it.
rank_configs_as <- function(obj, ids) {

  res    <- obj$evaluation$results
  metric <- obj$evaluation$rank_metric
  score  <- seq_along(ids)
  if (metric %in% HIGHER_BETTER_METRICS) score <- rev(score)

  res[[paste0("cv_", metric)]][match(ids, res$config_id)] <- score

  obj$evaluation$results     <- res
  obj$evaluation$best_config <- ids[1]
  obj

}


## ---------------------------------------------------------------------------
## The model table with some rows' fields replaced
## ---------------------------------------------------------------------------

#' MODEL_SPECS with the named rows' fields replaced, for the calling frame.
#' `rows` holds a list of fields per model, merged with utils::modifyList():
#' a field that is a list is replaced field by field (`threads`), any other
#' whole (`packages`). A package installed nowhere stands for a missing
#' engine, and a second engine's thread option for a table that declares
#' more than ranger's, whatever this machine has installed.
local_model_specs <- function(rows, .env = parent.frame()) {

  testthat::local_mocked_bindings(MODEL_SPECS = utils::modifyList(MODEL_SPECS, rows), .package = "horizons",
                                  .env = .env)

}


## ---------------------------------------------------------------------------
## A tune result scored by hand
## ---------------------------------------------------------------------------

#' A tune result with its candidates' fold RMSEs set by hand, so a test of
#' fit()'s re-tune choice fixes the means and standard errors it reads.
#' `rmse` takes one row of a fold's metrics (its parameters among its
#' columns) and returns that candidate's RMSE on every fold, in the result's
#' fold order.
set_fold_rmse <- function(results, rmse) {

  for (f in seq_len(nrow(results))) {

    m <- results$.metrics[[f]]

    for (r in which(m$.metric == "rmse")) m$.estimate[r] <- rmse(m[r, , drop = FALSE])[f]

    results$.metrics[[f]] <- m

  }

  results

}

#' tune_grid() as the re-tune calls it, its fold RMSEs then set by
#' set_fold_rmse(), for the calling frame
local_hand_scored_grid <- function(rmse, .env = parent.frame()) {

  real_grid <- tune::tune_grid

  testthat::local_mocked_bindings(tune_grid = function(...) set_fold_rmse(real_grid(...), rmse),
                                  .package = "tune", .env = .env)

}

## Three folds' RMSEs for a hand-scored candidate, and their standard error
## (the folds' sd over the square root of three): a candidate at
## LEAD_BASE + k * LEAD_SE trails one at LEAD_BASE by k of its standard errors
LEAD_BASE <- c(1.0, 1.2, 1.4)
LEAD_SE   <- 0.2 / sqrt(3)

#' A scorer for set_fold_rmse() that puts the best candidate `lead` of its
#' standard errors ahead of `incumbent` (evaluate()'s point, clamped): the
#' incumbent at LEAD_BASE + lead * LEAD_SE, the first other candidate the
#' scorer is asked for at LEAD_BASE, and the rest far behind. `$score` is the
#' scorer; `$other()` returns that first other candidate's tuned parameters
#' once the grid has been scored.
lead_scores <- function(incumbent, lead) {

  other <- NULL

  list(
    score = function(row) {
      point <- row[names(incumbent)]
      if (point_matches(incumbent, point)) return(LEAD_BASE + lead * LEAD_SE)
      if (is.null(other)) other <<- point
      if (point_matches(other, point)) LEAD_BASE else LEAD_BASE + 10
    },
    other = function() other
  )

}
