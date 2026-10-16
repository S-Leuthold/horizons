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
  obj <- new_horizons_data(analysis = df, role_map = roles,
                           spectra_source = "test", spectra_type = "mir")

  obj$config$configs   <- configs
  obj$config$n_configs <- n_configs
  obj$config$tuning    <- list(cv_folds = 3L, grid_size = 2L, bayesian_iter = 0L)

  obj$validation["passed"]    <- list(if (add_validation) TRUE)
  obj$validation["timestamp"] <- list(if (add_validation) Sys.time())

  obj

}


## ---------------------------------------------------------------------------
## Fit Test Object: a minimal horizons_eval object ready for fit()
## ---------------------------------------------------------------------------

make_fit_object <- function(n = 60, n_wn = 10, n_configs = 2, seed = 42,
                            n_na = 0L) {

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

  ## Configs: rf + cubist (fast, tunable)
  models <- c("rf", "cubist")
  configs <- tibble::tibble(
    config_id         = paste0("cfg_", sprintf("%03d", seq_len(n_configs))),
    model             = models[seq_len(n_configs)],
    transformation    = "none",
    preprocessing     = "raw",
    feature_selection = "none"
  )

  ## Built from the constructor, so every key the contract declares (the log
  ## among them) arrives on its own; only what the tests set is written here.
  obj <- new_horizons_data(analysis = df, role_map = roles,
                           spectra_source = "test", spectra_type = "mir")

  obj$config$configs   <- configs
  obj$config$n_configs <- n_configs
  obj$config$tuning    <- list(cv_folds = 3L, grid_size = 2L, bayesian_iter = 0L)

  obj$validation$passed    <- TRUE
  obj$validation$timestamp <- Sys.time()

  ## Run evaluate() to populate evaluation slot
  ## prune = FALSE ensures configs get status "success" even with weak signal
  suppressWarnings(
    evaluate(obj, prune = FALSE, verbose = FALSE, seed = seed)
  )

}
