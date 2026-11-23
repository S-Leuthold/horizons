# Test Data Fixtures and Generators
# Helper functions to create consistent test data across the test suite


## ---------------------------------------------------------------------------
## Evaluate Test Object
## ---------------------------------------------------------------------------

#' Build a minimal horizons_data object ready for evaluate()
make_eval_object <- function(n = 40, n_wn = 10, n_configs = 2,
                             add_validation = TRUE) {

  set.seed(42)

  ## Spectral data
  wn_names <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
  spec_mat <- matrix(rnorm(n * n_wn), nrow = n)
  colnames(spec_mat) <- wn_names

  df <- tibble::as_tibble(spec_mat)
  df$sample_id <- paste0("S", sprintf("%03d", seq_len(n)))

  ## Outcome with weak signal
  df$SOC <- 2 + rowMeans(spec_mat[, 1:min(3, n_wn)]) * 0.5 + rnorm(n, sd = 0.5)

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


## ---------------------------------------------------------------------------
## Fit Test Object: a minimal horizons_eval object ready for fit()
## ---------------------------------------------------------------------------

make_fit_object <- function(n = 60, n_wn = 10, n_configs = 2, seed = 42,
                            n_na = 0L, models = c("rf", "cubist"), feature_selection = "none") {

  set.seed(seed)

  ## Spectral data
  wn_names <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
  spec_mat <- matrix(rnorm(n * n_wn), nrow = n)
  colnames(spec_mat) <- wn_names

  df <- tibble::as_tibble(spec_mat)
  df$sample_id <- paste0("S", sprintf("%03d", seq_len(n)))

  ## Outcome with weak signal from first 3 predictors
  df$SOC <- 2 + rowMeans(spec_mat[, 1:min(3, n_wn)]) * 0.5 + rnorm(n, sd = 0.5)

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
  ## prune = FALSE ensures configs get status "success" even with weak signal
  suppressWarnings(
    evaluate(obj, prune = FALSE, seed = seed)
  )

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
