## ---------------------------------------------------------------------------
## Tests for pipeline-validate.R: validate() function
## ---------------------------------------------------------------------------

library(testthat)
library(horizons)


## ---------------------------------------------------------------------------
## Helpers: Fixture factory + quiet wrapper
## ---------------------------------------------------------------------------

#' Create a post-configure() horizons_data object for validation testing
#'
#' Default (n=100, 50 predictors, varied outcome) passes all checks cleanly.
#' Override parameters to create specific failure conditions.
#'
#' @param n_samples Number of samples (default 100)
#' @param n_predictors Number of spectral predictor columns (default 50)
#' @param outcome_name Name of outcome variable (default "SOC")
#' @param outcome_values Override outcome vector (length must match n_samples)
#' @param has_outcome Whether to include outcome (default TRUE)
#' @param cv_folds CV folds for tuning config (default 5L)
#' @param constant_cols Integer indices of predictors to make constant (0-var)
#' @param has_config Whether config$configs is non-NULL (default TRUE)
#'
#' @return horizons_data object ready for validate()
#' @noRd
make_configured_hd <- function(n_samples      = 100L,
                               n_predictors   = 50L,
                               outcome_name   = "SOC",
                               outcome_values = NULL,
                               has_outcome    = TRUE,
                               cv_folds       = 5L,
                               constant_cols  = integer(0),
                               has_config     = TRUE) {

  set.seed(42)

  ## Build sample IDs
  sample_ids <- sprintf("S%03d", seq_len(n_samples))

  ## Build predictor matrix (rnorm, varied). Decreasing wavenumber order:
  ## validate_horizons_data() (invariant I2) requires it, and validate() now
  ## calls it at the end of every run.
  wn_names <- as.character(seq(600 + n_predictors - 1, 600, by = -1))
  pred_mat <- matrix(stats::rnorm(n_samples * n_predictors, mean = 0.3, sd = 0.1),
                     nrow    = n_samples,
                     ncol    = n_predictors)
  colnames(pred_mat) <- wn_names

  ## Make constant columns if requested
  for (idx in constant_cols) {

    if (idx <= n_predictors) {

      pred_mat[, idx] <- 1.0

    }

  }

  ## Build analysis tibble
  analysis <- tibble::as_tibble(pred_mat)
  analysis <- tibble::add_column(analysis, sample_id = sample_ids, .before = 1)

  ## Build role_map
  role_map <- tibble::tibble(
    variable = c("sample_id", wn_names),
    role     = c("id", rep("predictor", n_predictors))
  )

  ## Add outcome if requested
  if (has_outcome) {

    if (is.null(outcome_values)) {

      outcome_values <- stats::rnorm(n_samples, mean = 15, sd = 5)

    }

    analysis[[outcome_name]] <- outcome_values

    role_map <- rbind(role_map, tibble::tibble(
      variable = outcome_name,
      role     = "outcome"
    ))

  }

  ## Build config section
  if (has_config) {

    configs <- tibble::tibble(
      config_id         = "rf_raw_none_none_abc123",
      model             = "rf",
      transformation    = "none",
      preprocessing     = "raw",
      feature_selection = "none",
      covariates        = NA_character_
    )

  } else {

    configs <- NULL

  }

  ## Assemble object; downstream slots in the constructor's shape
  contract <- new_horizons_data()

  obj <- list(
    data = list(
      analysis     = analysis,
      role_map     = role_map,
      n_rows       = n_samples,
      n_predictors = n_predictors,
      n_covariates = 0L,
      n_responses  = if (has_outcome) 0L else 0L
    ),
    provenance = list(
      spectra_source   = "test",
      spectra_type     = "mir",
      created          = Sys.time(),
      horizons_version = utils::packageVersion("horizons"),
      schema_version   = 1L
    ),
    config = list(
      configs   = configs,
      n_configs = if (!is.null(configs)) nrow(configs) else NULL,
      tuning    = list(grid_size = 10L, bayesian_iter = 15L, cv_folds = cv_folds)
    ),
    validation = list(
      passed    = NULL,
      checks    = NULL,
      timestamp = NULL,
      outliers  = list(
        spectral_ids   = NULL,
        response_ids   = NULL,
        removed_ids    = character(0),
        removal_detail = NULL,
        removed        = FALSE
      )
    ),
    evaluation = contract$evaluation,
    models     = contract$models,
    ensemble   = contract$ensemble,
    artifacts  = list(cache_dir = NULL)
  )

  class(obj) <- c("horizons_data", "list")
  obj

}


#' Silently run validate() — suppresses CLI tree output and warnings
#' @noRd
quiet_validate <- function(...) {

  suppressWarnings(
    invisible(capture.output(
      result <- validate(...)
    ))
  )
  result

}


## ===========================================================================
## 1. Input validation tests
## ===========================================================================

describe("validate() input validation", {

  test_that("rejects non-horizons_data input", {

    expect_error(
      capture.output(validate(data.frame(x = 1))),
      class = "horizons_validate_error"
    )

  })

  test_that("rejects unconfigured object (config$configs is NULL)", {

    hd <- make_configured_hd(has_config = FALSE)

    expect_error(
      capture.output(validate(hd)),
      "not configured",
      class = "horizons_validate_error"
    )

  })

  test_that("rejects invalid remove_outliers value", {

    hd <- make_configured_hd()

    expect_error(
      capture.output(validate(hd, remove_outliers = "both")),
      "remove_outliers",
      class = "horizons_validate_error"
    )

    expect_error(
      capture.output(validate(hd, remove_outliers = 42)),
      "remove_outliers",
      class = "horizons_validate_error"
    )

  })

  test_that("rejects invalid spectral_method", {

    hd <- make_configured_hd()

    expect_error(
      capture.output(validate(hd, spectral_method = "robust")),
      "spectral_method",
      class = "horizons_validate_error"
    )

  })

  test_that("rejects invalid response_method", {

    hd <- make_configured_hd()

    expect_error(
      capture.output(validate(hd, response_method = "zscore")),
      "response_method",
      class = "horizons_validate_error"
    )

  })

  test_that("rejects spectral_threshold out of (0, 1)", {

    hd <- make_configured_hd()

    expect_error(
      capture.output(validate(hd, spectral_threshold = -0.1)),
      "between 0 and 1",
      class = "horizons_validate_error"
    )

    expect_error(
      capture.output(validate(hd, spectral_threshold = 1.0)),
      "between 0 and 1",
      class = "horizons_validate_error"
    )

    expect_error(
      capture.output(validate(hd, spectral_threshold = NA)),
      "between 0 and 1",
      class = "horizons_validate_error"
    )

  })

  test_that("rejects non-positive response_threshold", {

    hd <- make_configured_hd()

    expect_error(
      capture.output(validate(hd, response_threshold = 0)),
      "must be positive",
      class = "horizons_validate_error"
    )

    expect_error(
      capture.output(validate(hd, response_threshold = -1)),
      "must be positive",
      class = "horizons_validate_error"
    )

  })

  test_that("warns on re-validation (overwriting previous results)", {

    hd <- make_configured_hd()
    r1 <- quiet_validate(hd)

    expect_warning(
      capture.output(validate(r1)),
      "Overwriting previous validation"
    )

  })

})


## ===========================================================================
## 2. Sample count checks (P001, P001b, P002)
## ===========================================================================

describe("validate() sample count checks", {

  ## P001 — Recommended sample count (n_total >= 50) ---------------------------

  test_that("P001 passes with n=100", {

    result <- quiet_validate(make_configured_hd(n_samples = 100))

    p001 <- result$validation$checks[result$validation$checks$check_id == "P001", ]
    expect_equal(p001$status, "pass")

  })

  test_that("P001 passes at boundary (n=50)", {

    result <- quiet_validate(make_configured_hd(n_samples = 50))

    p001 <- result$validation$checks[result$validation$checks$check_id == "P001", ]
    expect_equal(p001$status, "pass")

  })

  test_that("P001 fails below threshold (n=49)", {

    result <- quiet_validate(make_configured_hd(n_samples = 49))

    p001 <- result$validation$checks[result$validation$checks$check_id == "P001", ]
    expect_equal(p001$status, "fail")
    expect_equal(p001$severity, "WARNING")

  })

  ## P001b — CV feasibility (n_model >= cv_folds * 2) --------------------------

  test_that("P001b passes with sufficient samples", {

    result <- quiet_validate(make_configured_hd(n_samples = 100, cv_folds = 5L))

    p001b <- result$validation$checks[result$validation$checks$check_id == "P001b", ]
    expect_equal(p001b$status, "pass")

  })

  test_that("P001b passes at boundary (n_model=10, folds=5)", {

    result <- quiet_validate(make_configured_hd(n_samples = 10, cv_folds = 5L))

    p001b <- result$validation$checks[result$validation$checks$check_id == "P001b", ]
    expect_equal(p001b$status, "pass")

  })

  test_that("P001b fails below boundary (n_model=9, folds=5) -> passed=FALSE", {

    result <- quiet_validate(make_configured_hd(n_samples = 9, cv_folds = 5L))

    p001b <- result$validation$checks[result$validation$checks$check_id == "P001b", ]
    expect_equal(p001b$status, "fail")
    expect_equal(p001b$severity, "ERROR")
    expect_false(result$validation$passed)

  })

  test_that("P001b uses n_model (outcome-complete), not n_total", {

    ## 100 samples, 60% outcome NA → n_model = 40, cv_folds = 5 → 40 >= 10 → pass
    outcome_vals <- c(stats::rnorm(40, 15, 5), rep(NA, 60))
    result <- quiet_validate(make_configured_hd(
      n_samples      = 100,
      outcome_values = outcome_vals,
      cv_folds       = 5L
    ))

    p001b <- result$validation$checks[result$validation$checks$check_id == "P001b", ]
    expect_equal(p001b$status, "pass")

  })

  test_that("P001b fails when high NA reduces n_model below threshold", {

    ## 100 samples, 96% NA → n_model = 4, cv_folds = 5 → 4 < 10 → fail
    outcome_vals <- c(stats::rnorm(4, 15, 5), rep(NA, 96))
    result <- quiet_validate(make_configured_hd(
      n_samples      = 100,
      outcome_values = outcome_vals,
      cv_folds       = 5L
    ))

    p001b <- result$validation$checks[result$validation$checks$check_id == "P001b", ]
    expect_equal(p001b$status, "fail")
    expect_false(result$validation$passed)

  })

  ## P002 — Samples per fold ---------------------------------------------------

  test_that("P002 passes with 10+ samples per fold", {

    result <- quiet_validate(make_configured_hd(n_samples = 100, cv_folds = 5L))

    p002 <- result$validation$checks[result$validation$checks$check_id == "P002", ]
    expect_equal(p002$status, "pass")

  })

  test_that("P002 fails below 10 per fold", {

    ## n=49, folds=5 → 9 per fold → fail
    result <- quiet_validate(make_configured_hd(n_samples = 49, cv_folds = 5L))

    p002 <- result$validation$checks[result$validation$checks$check_id == "P002", ]
    expect_equal(p002$status, "fail")
    expect_equal(p002$severity, "WARNING")

  })

})


## ===========================================================================
## 3. Outcome checks (P003, P004, gating)
## ===========================================================================

describe("validate() outcome checks", {

  ## Gating behavior -----------------------------------------------------------

  test_that("skips P003/P004/P006 when no outcome in role_map", {

    hd     <- make_configured_hd(has_outcome = FALSE)
    result <- quiet_validate(hd)

    check_ids <- result$validation$checks$check_id
    expect_false("P003" %in% check_ids)
    expect_false("P004" %in% check_ids)
    expect_false("P006" %in% check_ids)

  })

  test_that("runs P003/P004/P006 when outcome exists", {

    result <- quiet_validate(make_configured_hd())

    check_ids <- result$validation$checks$check_id
    expect_true("P003" %in% check_ids)
    expect_true("P004" %in% check_ids)
    expect_true("P006" %in% check_ids)

  })

  ## P003 — Outcome variance ---------------------------------------------------

  test_that("P003 passes with normal variance", {

    result <- quiet_validate(make_configured_hd())

    p003 <- result$validation$checks[result$validation$checks$check_id == "P003", ]
    expect_equal(p003$status, "pass")

  })

  test_that("P003 fails with constant outcome -> passed=FALSE", {

    result <- quiet_validate(make_configured_hd(
      outcome_values = rep(5.0, 100)
    ))

    p003 <- result$validation$checks[result$validation$checks$check_id == "P003", ]
    expect_equal(p003$status, "fail")
    expect_equal(p003$severity, "ERROR")
    expect_false(result$validation$passed)

  })

  test_that("P003 computes variance on non-NA values only", {

    ## 3 varied values + 97 NAs → var of the 3 is > 0 → pass
    outcome_vals <- c(1, 5, 10, rep(NA, 97))
    result <- quiet_validate(make_configured_hd(outcome_values = outcome_vals))

    p003 <- result$validation$checks[result$validation$checks$check_id == "P003", ]
    expect_equal(p003$status, "pass")

  })

  test_that("P003 fails when only 1 non-NA value (var undefined)", {

    outcome_vals <- c(5.0, rep(NA, 99))
    result <- quiet_validate(make_configured_hd(outcome_values = outcome_vals))

    p003 <- result$validation$checks[result$validation$checks$check_id == "P003", ]
    expect_equal(p003$status, "fail")

  })

  ## P004 — Outcome NA proportion ----------------------------------------------

  test_that("P004 passes with 0% NA", {

    result <- quiet_validate(make_configured_hd())

    p004 <- result$validation$checks[result$validation$checks$check_id == "P004", ]
    expect_equal(p004$status, "pass")

  })

  test_that("P004 passes at threshold (20% NA)", {

    outcome_vals <- c(stats::rnorm(80, 15, 5), rep(NA, 20))
    result <- quiet_validate(make_configured_hd(outcome_values = outcome_vals))

    p004 <- result$validation$checks[result$validation$checks$check_id == "P004", ]
    expect_equal(p004$status, "pass")

  })

  test_that("P004 fails above threshold (25% NA)", {

    outcome_vals <- c(stats::rnorm(75, 15, 5), rep(NA, 25))
    result <- quiet_validate(make_configured_hd(outcome_values = outcome_vals))

    p004 <- result$validation$checks[result$validation$checks$check_id == "P004", ]
    expect_equal(p004$status, "fail")
    expect_equal(p004$severity, "WARNING")

  })

  test_that("P004 fails with 100% NA", {

    outcome_vals <- rep(NA_real_, 100)
    result <- quiet_validate(make_configured_hd(outcome_values = outcome_vals))

    p004 <- result$validation$checks[result$validation$checks$check_id == "P004", ]
    expect_equal(p004$status, "fail")

  })

})


## ===========================================================================
## 4. Predictor checks (P007)
## ===========================================================================

describe("validate() predictor checks", {

  test_that("P007 passes with normal predictors", {

    result <- quiet_validate(make_configured_hd())

    p007 <- result$validation$checks[result$validation$checks$check_id == "P007", ]
    expect_equal(p007$status, "pass")

  })

  test_that("P007 flags constant predictor columns", {

    result <- quiet_validate(make_configured_hd(constant_cols = c(1L, 2L, 3L)))

    p007 <- result$validation$checks[result$validation$checks$check_id == "P007", ]
    expect_equal(p007$status, "fail")
    expect_equal(p007$severity, "WARNING")
    expect_equal(p007$value, "3")

  })

  test_that("P007 flags predictor with var < 1e-10 but not var = 1e-9", {

    hd <- make_configured_hd()

    ## Make one predictor with variance < 1e-10 (effectively constant)
    hd$data$analysis[["600"]] <- 1.0 + seq_len(100) * 1e-12

    ## Make another predictor with variance ~ 1e-9 (just above threshold)
    hd$data$analysis[["601"]] <- 1.0 + seq_len(100) * 1e-6

    result <- quiet_validate(hd)

    p007 <- result$validation$checks[result$validation$checks$check_id == "P007", ]
    ## Only "600" should be flagged, not "601"
    expect_equal(p007$value, "1")

  })

  test_that("P007 only checks predictor columns, not outcome or id", {

    ## Create fixture where outcome is constant but predictors vary
    result <- quiet_validate(make_configured_hd(
      outcome_values = rep(5.0, 100)
    ))

    p007 <- result$validation$checks[result$validation$checks$check_id == "P007", ]
    ## P007 should pass — constant outcome doesn't affect predictor check
    expect_equal(p007$status, "pass")

  })

  test_that("P007 runs before P005 in check ordering", {

    result <- quiet_validate(make_configured_hd())

    checks <- result$validation$checks
    p007_row <- which(checks$check_id == "P007")
    p005_row <- which(checks$check_id == "P005")
    expect_true(p007_row < p005_row)

  })

  test_that("all predictors constant: P005 skips gracefully", {

    hd <- make_configured_hd(n_predictors = 5, constant_cols = 1:5)

    ## P005 should skip (< 2 clean cols after NZV removal) — issued as warning
    result <- suppressWarnings(quiet_validate(hd))

    p007 <- result$validation$checks[result$validation$checks$check_id == "P007", ]
    expect_equal(p007$value, "5")

    ## P005 should still be in checks (just with 0 flagged)
    expect_true("P005" %in% result$validation$checks$check_id)
    expect_equal(length(result$validation$outliers$spectral_ids), 0)

  })

})


## ===========================================================================
## 4b. Cubist feasibility check (P010)
## ===========================================================================

describe("validate() cubist feasibility check (P010)", {

  test_that("P010 passes on the default fixture (below threshold, model = rf)", {

    result <- quiet_validate(make_configured_hd())

    p010 <- result$validation$checks[result$validation$checks$check_id == "P010", ]
    expect_equal(p010$status, "pass")

  })

  test_that("P010 fires for cubist + feature_selection = 'none' above the cell threshold", {

    hd <- make_configured_hd()
    hd$config$configs$model             <- "cubist"
    hd$config$configs$feature_selection <- "none"

    ## Lower CUBIST_MAX_CELLS so the default fixture's 100 x 50 = 5,000 cells
    ## trips it without building a library-scale matrix.
    local_mocked_bindings(CUBIST_MAX_CELLS = 100, .package = "horizons")

    ## Don't use quiet_validate — need to capture the warning
    expect_warning(
      capture.output(result <- validate(hd)),
      "Cubist may not finish"
    )

    p010 <- result$validation$checks[result$validation$checks$check_id == "P010", ]
    expect_equal(p010$status, "fail")
    expect_equal(p010$severity, "WARNING")
    expect_equal(p010$value, "1 config(s); 100 x 50 = 5,000 cells")

  })

  test_that("P010 does not fire with feature_selection = 'pca'", {

    hd <- make_configured_hd()
    hd$config$configs$model             <- "cubist"
    hd$config$configs$feature_selection <- "pca"

    local_mocked_bindings(CUBIST_MAX_CELLS = 100, .package = "horizons")

    expect_no_warning(capture.output(result <- validate(hd)))

    p010 <- result$validation$checks[result$validation$checks$check_id == "P010", ]
    expect_equal(p010$status, "pass")

  })

  test_that("P010 does not fire below the cell threshold", {

    hd <- make_configured_hd()
    hd$config$configs$model             <- "cubist"
    hd$config$configs$feature_selection <- "none"
    ## Default CUBIST_MAX_CELLS (2e6) is far above 100 x 50 = 5,000 cells.

    expect_no_warning(capture.output(result <- validate(hd)))

    p010 <- result$validation$checks[result$validation$checks$check_id == "P010", ]
    expect_equal(p010$status, "pass")

  })

  test_that("P010 does not fire for non-cubist models even above the threshold", {

    hd <- make_configured_hd()
    hd$config$configs$model             <- "rf"
    hd$config$configs$feature_selection <- "none"

    local_mocked_bindings(CUBIST_MAX_CELLS = 100, .package = "horizons")

    expect_no_warning(capture.output(result <- validate(hd)))

    p010 <- result$validation$checks[result$validation$checks$check_id == "P010", ]
    expect_equal(p010$status, "pass")

  })

  test_that("P010 names only the affected configs in the warning", {

    hd <- make_configured_hd()
    hd$config$configs <- tibble::tibble(
      config_id         = c("cubist_raw_none_none_ab12cd", "rf_raw_none_none_ef34gh"),
      model             = c("cubist", "rf"),
      transformation    = "none",
      preprocessing     = "raw",
      feature_selection = "none",
      covariates        = NA_character_
    )

    local_mocked_bindings(CUBIST_MAX_CELLS = 100, .package = "horizons")

    warned <- testthat::capture_warnings(capture.output(result <- validate(hd)))
    combined <- paste(warned, collapse = "\n")

    expect_true(grepl("cubist_raw_none_none_ab12cd", combined, fixed = TRUE))
    expect_false(grepl("rf_raw_none_none_ef34gh", combined, fixed = TRUE))

    p010 <- result$validation$checks[result$validation$checks$check_id == "P010", ]
    expect_equal(p010$value, "1 config(s); 100 x 50 = 5,000 cells")

  })

})


## ===========================================================================
## 5. Spectral outlier detection (P005)
## ===========================================================================

describe("validate() spectral outlier detection", {

  test_that("P005 detects no outliers in clean data", {

    result <- quiet_validate(make_configured_hd())

    expect_equal(length(result$validation$outliers$spectral_ids), 0)

  })

  test_that("P005 detects injected spectral outliers", {

    hd <- make_configured_hd(n_samples = 100, n_predictors = 50)

    ## Inject 3 extreme spectral outliers — values far from the distribution
    ## (mean=0.3, sd=0.1 → 1000 is ~10000 SDs away)
    for (col in names(hd$data$analysis)[2:51]) {

      hd$data$analysis[[col]][1:3] <- 1000

    }

    result <- quiet_validate(hd)

    expect_true(length(result$validation$outliers$spectral_ids) >= 3)

  })

  test_that("P005 stores sample_id values, not integer indices", {

    hd <- make_configured_hd()

    ## Inject 1 outlier
    for (col in names(hd$data$analysis)[2:51]) {

      hd$data$analysis[[col]][1] <- 100

    }

    result <- quiet_validate(hd)

    ids <- result$validation$outliers$spectral_ids
    expect_type(ids, "character")
    if (length(ids) > 0) {

      expect_true(all(ids %in% hd$data$analysis$sample_id))

    }

  })

  test_that("P005 excludes NZV columns from PCA", {

    hd <- make_configured_hd(n_predictors = 10, constant_cols = c(1L, 2L, 3L))

    ## Should still run PCA on 7 remaining clean columns
    result <- suppressWarnings(quiet_validate(hd))
    expect_true("P005" %in% result$validation$checks$check_id)

  })

  test_that("P005 skips with fewer than 5 samples", {

    hd <- make_configured_hd(n_samples = 4, cv_folds = 2L)

    ## Don't use quiet_validate — we need to capture the warning
    expect_warning(
      capture.output(result <- validate(hd)),
      "Too few samples"
    )

    expect_equal(length(result$validation$outliers$spectral_ids), 0)

  })

  test_that("P005 runs with exactly 5 samples (boundary)", {

    hd <- make_configured_hd(n_samples = 5, cv_folds = 2L)

    ## Should NOT warn about too few samples
    output <- capture.output(
      result <- suppressWarnings(validate(hd))
    )

    expect_true("P005" %in% result$validation$checks$check_id)

  })

  test_that("P005 skips with fewer than 2 clean predictors after NZV", {

    ## 2 predictors, both constant → 0 clean → skip
    hd <- make_configured_hd(n_predictors = 2, constant_cols = c(1L, 2L))

    ## Don't use quiet_validate — we need to capture the warning
    expect_warning(
      capture.output(result <- validate(hd)),
      "Fewer than 2 predictor columns"
    )

    expect_equal(length(result$validation$outliers$spectral_ids), 0)

  })

  test_that("P005 skips with singular covariance matrix", {

    ## Create perfectly collinear predictors (duplicated columns)
    hd <- make_configured_hd(n_samples = 20, n_predictors = 10)
    base_col <- hd$data$analysis[["600"]]
    for (i in 2:10) {

      hd$data$analysis[[names(hd$data$analysis)[i + 1]]] <- base_col * i

    }

    result <- suppressWarnings(quiet_validate(hd))

    ## Should complete without error (may or may not detect outliers depending
    ## on PCA handling of collinear data)
    expect_true("P005" %in% result$validation$checks$check_id)

  })

  test_that("P005 is INFO severity and does not affect passed", {

    hd <- make_configured_hd()

    ## Inject outliers
    for (col in names(hd$data$analysis)[2:51]) {

      hd$data$analysis[[col]][1:5] <- 100

    }

    result <- quiet_validate(hd)

    expect_true("P005" %in% result$validation$checks$check_id)
    expect_true(result$validation$passed)

  })

  test_that("P005 respects spectral_threshold parameter", {

    hd <- make_configured_hd(n_samples = 200, n_predictors = 20)

    ## Inject moderate outliers
    for (col in names(hd$data$analysis)[2:21]) {

      hd$data$analysis[[col]][1:3] <- hd$data$analysis[[col]][1:3] + 5

    }

    result_strict <- quiet_validate(hd, spectral_threshold = 0.95)
    result_loose  <- quiet_validate(hd, spectral_threshold = 0.999)

    ## Stricter threshold should flag more or equal outliers
    expect_true(
      length(result_strict$validation$outliers$spectral_ids) >=
      length(result_loose$validation$outliers$spectral_ids)
    )

  })

  test_that("P005 returns character(0) when skipped, not NULL", {

    hd <- make_configured_hd(n_samples = 4, cv_folds = 2L)
    result <- suppressWarnings(quiet_validate(hd))

    expect_identical(result$validation$outliers$spectral_ids, character(0))

  })

})


## ===========================================================================
## 6. Response outlier detection (P006)
## ===========================================================================

describe("validate() response outlier detection", {

  test_that("P006 detects no outliers in clean outcome", {

    ## Use uniform-ish values that won't have IQR outliers
    outcome_vals <- seq(10, 20, length.out = 100)
    result <- quiet_validate(make_configured_hd(outcome_values = outcome_vals))

    expect_equal(length(result$validation$outliers$response_ids), 0)

  })

  test_that("P006 detects extreme high values", {

    outcome_vals <- c(stats::rnorm(97, 15, 2), 100, 200, 300)
    result <- quiet_validate(make_configured_hd(outcome_values = outcome_vals))

    expect_true(length(result$validation$outliers$response_ids) >= 2)

  })

  test_that("P006 detects extreme low values", {

    outcome_vals <- c(stats::rnorm(97, 15, 2), -100, -200, -300)
    result <- quiet_validate(make_configured_hd(outcome_values = outcome_vals))

    expect_true(length(result$validation$outliers$response_ids) >= 2)

  })

  test_that("P006 stores sample_id values", {

    outcome_vals <- c(stats::rnorm(97, 15, 2), 100, 200, 300)
    hd     <- make_configured_hd(outcome_values = outcome_vals)
    result <- quiet_validate(hd)

    ids <- result$validation$outliers$response_ids
    expect_type(ids, "character")
    expect_true(all(ids %in% hd$data$analysis$sample_id))

  })

  test_that("P006 respects response_threshold parameter", {

    outcome_vals <- c(stats::rnorm(95, 15, 2), 30, 35, 40, 45, 50)
    hd <- make_configured_hd(outcome_values = outcome_vals)

    result_strict <- quiet_validate(hd, response_threshold = 1.0)
    result_loose  <- quiet_validate(hd, response_threshold = 3.0)

    expect_true(
      length(result_strict$validation$outliers$response_ids) >=
      length(result_loose$validation$outliers$response_ids)
    )

  })

  test_that("P006 skipped when no outcome exists", {

    result <- quiet_validate(make_configured_hd(has_outcome = FALSE))

    expect_false("P006" %in% result$validation$checks$check_id)
    expect_equal(length(result$validation$outliers$response_ids), 0)

  })

  test_that("P006 is INFO severity and does not affect passed", {

    outcome_vals <- c(stats::rnorm(97, 15, 2), 100, 200, 300)
    result <- quiet_validate(make_configured_hd(outcome_values = outcome_vals))

    expect_true("P006" %in% result$validation$checks$check_id)
    expect_true(result$validation$passed)

  })

  test_that("P006 skips with fewer than 4 non-NA outcome values", {

    outcome_vals <- c(1.0, 2.0, 3.0, rep(NA, 97))

    ## Don't use quiet_validate — need to capture the warning
    expect_warning(
      capture.output(result <- validate(make_configured_hd(outcome_values = outcome_vals))),
      "Too few non-NA outcome values"
    )

    expect_equal(length(result$validation$outliers$response_ids), 0)

  })

})


## ===========================================================================
## 7. Outlier removal
## ===========================================================================

describe("validate() outlier removal", {

  ## Helper: create fixture with known outliers
  make_outlier_hd <- function() {

    hd <- make_configured_hd(n_samples = 100, n_predictors = 20)

    ## Inject 5 spectral outliers (samples 1-5)
    for (col in names(hd$data$analysis)[2:21]) {

      hd$data$analysis[[col]][1:5] <- 100

    }

    ## Inject 3 response outliers (samples 4-6, so sample 4-5 overlap)
    hd$data$analysis[["SOC"]][4:6] <- 500

    hd

  }

  test_that("remove_outliers=FALSE: no removal, outliers stored", {

    hd     <- make_outlier_hd()
    result <- quiet_validate(hd, remove_outliers = FALSE)

    expect_false(result$validation$outliers$removed)
    expect_equal(result$validation$outliers$removed_ids, character(0))
    expect_equal(result$data$n_rows, 100)

  })

  test_that("remove_outliers=TRUE: removes union of spectral + response", {

    hd     <- make_outlier_hd()
    result <- quiet_validate(hd, remove_outliers = TRUE)

    expect_true(result$validation$outliers$removed)
    expect_true(length(result$validation$outliers$removed_ids) > 0)
    expect_true(result$data$n_rows < 100)

  })

  test_that("remove_outliers='spectral': removes only spectral outliers", {

    hd     <- make_outlier_hd()
    result <- quiet_validate(hd, remove_outliers = "spectral")

    ## Only spectral outlier IDs should be removed
    removed <- result$validation$outliers$removed_ids
    spectral <- result$validation$outliers$spectral_ids
    expect_true(all(removed %in% spectral))

  })

  test_that("remove_outliers='response': removes only response outliers", {

    hd     <- make_outlier_hd()
    result <- quiet_validate(hd, remove_outliers = "response")

    removed  <- result$validation$outliers$removed_ids
    response <- result$validation$outliers$response_ids
    expect_true(all(removed %in% response))

  })

  test_that("removal updates data$analysis row count", {

    hd     <- make_outlier_hd()
    result <- quiet_validate(hd, remove_outliers = TRUE)

    n_removed <- length(result$validation$outliers$removed_ids)
    expect_equal(result$data$n_rows, 100 - n_removed)
    expect_equal(nrow(result$data$analysis), result$data$n_rows)

  })

  test_that("removal_detail maps sample_id to reason", {

    hd     <- make_outlier_hd()
    result <- quiet_validate(hd, remove_outliers = TRUE)

    detail <- result$validation$outliers$removal_detail
    expect_s3_class(detail, "tbl_df")
    expect_true("sample_id" %in% names(detail))
    expect_true("reason" %in% names(detail))
    expect_true(all(detail$reason %in% c("spectral", "response", "both")))

  })

  test_that("removal_detail correctly identifies overlap as 'both'", {

    hd     <- make_outlier_hd()
    result <- quiet_validate(hd, remove_outliers = TRUE)

    detail   <- result$validation$outliers$removal_detail
    spectral <- result$validation$outliers$spectral_ids
    response <- result$validation$outliers$response_ids
    overlap  <- intersect(spectral, response)

    if (length(overlap) > 0) {

      both_rows <- detail[detail$sample_id %in% overlap, ]
      expect_true(all(both_rows$reason == "both"))

    }

  })

  test_that("no outliers detected: remove_outliers=TRUE does nothing", {

    ## Use controlled outcome to ensure no IQR outliers
    outcome_vals <- seq(10, 20, length.out = 100)
    hd     <- make_configured_hd(outcome_values = outcome_vals)
    result <- quiet_validate(hd, remove_outliers = TRUE)

    expect_false(result$validation$outliers$removed)
    expect_equal(result$validation$outliers$removed_ids, character(0))
    expect_equal(result$data$n_rows, 100)

  })

  test_that("removed sample_ids no longer in data$analysis", {

    hd     <- make_outlier_hd()
    result <- quiet_validate(hd, remove_outliers = TRUE)

    removed_ids <- result$validation$outliers$removed_ids
    remaining   <- result$data$analysis$sample_id
    expect_false(any(removed_ids %in% remaining))

  })

  test_that("removal_detail records the outcome and threshold behind each removal", {

    ## Rows 1-3 are spectral only, 4-5 both, 6 response only
    hd     <- make_outlier_hd()
    result <- quiet_validate(hd, remove_outliers = TRUE,
                             spectral_threshold = 0.99, response_threshold = 2)

    detail        <- result$validation$outliers$removal_detail
    spectral_only <- detail[detail$reason == "spectral", ]
    response_side <- detail[detail$reason %in% c("response", "both"), ]

    expect_true(all(c("outcome", "spectral_threshold", "response_threshold") %in% names(detail)))
    expect_gt(nrow(spectral_only), 0)
    expect_gt(nrow(response_side), 0)

    ## A spectral removal does not depend on the outcome
    expect_true(all(is.na(spectral_only$outcome)))
    expect_true(all(is.na(spectral_only$response_threshold)))
    expect_true(all(spectral_only$spectral_threshold == 0.99))

    expect_true(all(response_side$outcome == "SOC"))
    expect_true(all(response_side$response_threshold == 2))

  })

})


## ===========================================================================
## 7b. The removal record across re-configure and re-validate
## ===========================================================================

describe("validate() keeps the removal record", {

  ## The outlier fixture plus a second response, pH, whose only outliers are
  ## rows 50 and 51 (so it can be configured after SOC's removals)
  make_two_response_hd <- function() {

    hd <- make_configured_hd(n_samples = 100, n_predictors = 20)

    for (col in names(hd$data$analysis)[2:21]) {

      hd$data$analysis[[col]][1:5] <- 100

    }

    hd$data$analysis[["SOC"]][4:6] <- 500

    analysis    <- hd$data$analysis
    analysis$pH <- round(seq(5.5, 7.0, length.out = nrow(analysis)), 3)
    analysis$pH[50:51] <- 60

    role_map <- dplyr::bind_rows(hd$data$role_map,
                                 tibble::tibble(variable = "pH", role = "response"))

    set_analysis(hd, analysis, role_map)

  }

  quiet_reconfigure <- function(x, outcome) {

    suppressWarnings(invisible(utils::capture.output(
      result <- configure(x, outcome = outcome)
    )))
    result

  }

  record_of <- function(x) x$validation$outliers[c("removed_ids", "removal_detail", "removed")]

  test_that("the record survives configure() and a second validate() (#70)", {

    ## validate(remove_outliers = TRUE) |> configure(outcome = "pH") |> validate()
    v1 <- quiet_validate(make_two_response_hd(), remove_outliers = TRUE)
    v2 <- quiet_validate(quiet_reconfigure(v1, "pH"))

    expect_true(v1$validation$outliers$removed)
    expect_identical(record_of(v2), record_of(v1))
    expect_false(any(v2$validation$outliers$removed_ids %in% v2$data$analysis$sample_id))

  })

  test_that("a second validate() that removes more rows adds to the record", {

    v1 <- quiet_validate(make_two_response_hd(), remove_outliers = TRUE)
    v3 <- quiet_validate(quiet_reconfigure(v1, "pH"), remove_outliers = "response")

    first  <- v1$validation$outliers
    out    <- v3$validation$outliers
    added  <- setdiff(out$removed_ids, first$removed_ids)

    expect_setequal(added, c("S050", "S051"))
    expect_true(all(first$removed_ids %in% out$removed_ids))
    expect_true(out$removed)

    ## One detail row per removed id, the earlier rows unchanged, the new
    ## ones attributed to the outcome that flagged them
    expect_identical(sort(out$removal_detail$sample_id), sort(out$removed_ids))
    expect_identical(out$removal_detail[seq_len(nrow(first$removal_detail)), ],
                     first$removal_detail)
    expect_true(all(out$removal_detail$outcome[out$removal_detail$sample_id %in% added] == "pH"))

    expect_false(any(out$removed_ids %in% v3$data$analysis$sample_id))

  })

  test_that("a spectral-only removal is not attributed to the outcome", {

    ## Rows 4 and 5 sit outside SOC's fences too, but "spectral" did not
    ## remove them for that, so they are "spectral" rows with no outcome
    v1     <- quiet_validate(make_two_response_hd(), remove_outliers = "spectral")
    detail <- v1$validation$outliers$removal_detail

    expect_true(all(c("S004", "S005") %in% detail$sample_id))
    expect_true(all(detail$reason == "spectral"))
    expect_true(all(is.na(detail$outcome)))
    expect_true(all(is.na(detail$response_threshold)))

    ## So changing the outcome has nothing stale to warn about
    warned <- testthat::capture_warnings(utils::capture.output(configure(v1, outcome = "pH")))

    expect_false(any(grepl("response outliers", warned)))

  })

  test_that("a row removed as both is not counted as a stale response removal", {

    ## Under TRUE, rows 4 and 5 are "both" (they would go as spectral
    ## outliers whatever the outcome) and row 6 is "response"
    v1     <- quiet_validate(make_two_response_hd(), remove_outliers = TRUE)
    detail <- v1$validation$outliers$removal_detail

    n_response <- sum(detail$reason == "response")

    expect_true(all(detail$reason[detail$sample_id %in% c("S004", "S005")] == "both"))
    expect_identical(detail$reason[detail$sample_id == "S006"], "response")

    warned <- testthat::capture_warnings(utils::capture.output(configure(v1, outcome = "pH")))

    expect_true(any(grepl(paste0("^", n_response, " row\\(s\\) were removed .*'SOC' \\(",
                                 n_response, "\\)"), warned)))

  })

})


## ===========================================================================
## 8. Validation passed logic + CLI output
## ===========================================================================

describe("validate() passed logic and CLI output", {

  test_that("passed=TRUE when all checks pass", {

    result <- quiet_validate(make_configured_hd())
    expect_true(result$validation$passed)

  })

  test_that("passed=TRUE with only WARNINGs (no ERRORs)", {

    ## n=40 fails P001 (WARNING) but no ERRORs
    result <- quiet_validate(make_configured_hd(n_samples = 40, cv_folds = 2L))

    expect_true(result$validation$passed)

  })

  test_that("passed=TRUE with only INFOs (outliers detected)", {

    hd <- make_configured_hd()
    for (col in names(hd$data$analysis)[2:51]) {

      hd$data$analysis[[col]][1:3] <- 100

    }

    result <- quiet_validate(hd)
    expect_true(result$validation$passed)

  })

  test_that("passed=FALSE when P001b ERROR fails", {

    result <- quiet_validate(make_configured_hd(n_samples = 9, cv_folds = 5L))
    expect_false(result$validation$passed)

  })

  test_that("passed=FALSE when P003 ERROR fails", {

    result <- quiet_validate(make_configured_hd(outcome_values = rep(5.0, 100)))
    expect_false(result$validation$passed)

  })

  test_that("CLI output shows PASSED when all clean", {

    hd     <- make_configured_hd()
    output <- capture.output(suppressWarnings(validate(hd)))
    combined <- paste(output, collapse = "\n")

    expect_true(grepl("PASSED", combined))

  })

  test_that("CLI output shows FAILED when ERROR present", {

    hd     <- make_configured_hd(outcome_values = rep(5.0, 100))
    output <- capture.output(suppressWarnings(validate(hd)))
    combined <- paste(output, collapse = "\n")

    expect_true(grepl("FAILED", combined))

  })

  test_that("CLI output shows outlier removal summary when removal happens", {

    hd <- make_configured_hd()
    for (col in names(hd$data$analysis)[2:51]) {

      hd$data$analysis[[col]][1:3] <- 100

    }

    output <- capture.output(suppressWarnings(validate(hd, remove_outliers = TRUE)))
    combined <- paste(output, collapse = "\n")

    expect_true(grepl("Removed", combined) || grepl("removal", combined, ignore.case = TRUE))

  })

})


## ===========================================================================
## 9. Edge cases and integration
## ===========================================================================

describe("validate() edge cases", {

  test_that("re-validation after removal operates on reduced dataset", {

    hd <- make_configured_hd()
    for (col in names(hd$data$analysis)[2:51]) {

      hd$data$analysis[[col]][1:5] <- 100

    }

    ## First validation with removal
    r1 <- quiet_validate(hd, remove_outliers = TRUE)
    n1 <- r1$data$n_rows

    ## Second validation on the reduced dataset
    r2 <- suppressWarnings(quiet_validate(r1))
    expect_equal(r2$data$n_rows, n1)  # no further removal

  })

  test_that("re-validation overwrites checks (not appends)", {

    hd <- make_configured_hd()
    r1 <- quiet_validate(hd)
    n_checks_1 <- nrow(r1$validation$checks)

    r2 <- suppressWarnings(quiet_validate(r1))
    n_checks_2 <- nrow(r2$validation$checks)

    expect_equal(n_checks_1, n_checks_2)

  })

  test_that("idempotency: same data validated twice gives same result", {

    hd <- make_configured_hd()
    r1 <- quiet_validate(hd)
    r2 <- suppressWarnings(quiet_validate(hd))

    expect_equal(r1$validation$checks$status, r2$validation$checks$status)
    expect_equal(r1$validation$passed, r2$validation$passed)

  })

  test_that("spectral + response overlap: union removes correct count", {

    hd <- make_configured_hd(n_samples = 100, n_predictors = 20)

    ## Inject spectral outliers on samples 1-5
    for (col in names(hd$data$analysis)[2:21]) {

      hd$data$analysis[[col]][1:5] <- 100

    }

    ## Inject response outliers on samples 4-6 (overlap on 4-5)
    hd$data$analysis[["SOC"]][4:6] <- 500

    result <- quiet_validate(hd, remove_outliers = TRUE)

    spectral <- result$validation$outliers$spectral_ids
    response <- result$validation$outliers$response_ids
    removed  <- result$validation$outliers$removed_ids

    ## Union removes unique samples
    expected_removed <- union(spectral, response)
    expect_equal(sort(removed), sort(expected_removed))

  })

  test_that("n=5 boundary: PCA runs successfully", {

    hd     <- make_configured_hd(n_samples = 5, n_predictors = 10, cv_folds = 2L)
    result <- quiet_validate(hd)

    expect_true("P005" %in% result$validation$checks$check_id)

  })

  test_that("validation$timestamp is POSIXct", {

    result <- quiet_validate(make_configured_hd())
    expect_s3_class(result$validation$timestamp, "POSIXct")

  })

  test_that("returns horizons_data class invisibly", {

    hd <- make_configured_hd()
    output <- capture.output(
      result <- suppressWarnings(validate(hd))
    )

    expect_s3_class(result, "horizons_data")

  })

})


## ===========================================================================
## 12. Outlier removal maintains the selection record
## ===========================================================================

describe("validate() and the selection record", {

  ## Helper: a configured object with spectral outliers and a selection record
  make_selected_hd <- function() {

    hd <- make_configured_hd(n_samples = 40L, n_predictors = 20L)

    ## Inject 5 spectral outliers
    for (col in names(hd$data$analysis)[2:21]) {

      hd$data$analysis[[col]][1:5] <- 100

    }

    hd$selection <- make_selection_record(pool_ids = hd$data$analysis$sample_id)

    hd

  }

  test_that("removing outliers refilters selection membership", {

    ## Arrange
    hd <- make_selected_hd()

    ## Act
    result <- quiet_validate(hd, remove_outliers = "spectral")

    ## Assert
    removed  <- result$validation$outliers$removed_ids
    surviving <- result$data$analysis$sample_id

    expect_true(length(removed) > 0)
    expect_false(any(result$selection$membership$pool_id %in% removed))
    expect_true(all(result$selection$membership$pool_id %in% surviving))

  })


  test_that("removing outliers refilters each group and recounts n_rows", {

    hd     <- make_selected_hd()
    result <- quiet_validate(hd, remove_outliers = "spectral")

    surviving <- result$data$analysis$sample_id

    for (i in seq_len(nrow(result$selection$groups))) {

      ids <- result$selection$groups$pool_ids[[i]]

      expect_true(all(ids %in% surviving))
      expect_identical(result$selection$groups$n_rows[i], length(ids))

    }

    expect_identical(sum(result$selection$groups$n_rows), length(surviving))

  })


  test_that("removing outliers recounts drawn and records rows_removed", {

    hd     <- make_selected_hd()
    result <- quiet_validate(hd, remove_outliers = "spectral")

    n_removed <- length(result$validation$outliers$removed_ids)

    expect_identical(result$selection$rows_removed, as.integer(n_removed))
    expect_true(all(result$selection$pool_sizes$drawn == result$data$n_rows))
    expect_identical(result$selection$pool_sizes$available,
                     hd$selection$pool_sizes$available)

  })


  test_that("removing nothing leaves the record untouched", {

    hd     <- make_selected_hd()
    result <- quiet_validate(hd, remove_outliers = FALSE)

    expect_identical(result$selection, hd$selection)

  })


  test_that("validate() aborts when asked to remove outliers from a fitted object", {

    ## Arrange — the row operation would strand the split and the row index
    hd <- make_selected_hd()
    hd$models$workflows <- list(cfg_a = "a fitted workflow")

    ## Act & Assert
    expect_error(
      suppressWarnings(capture.output(validate(hd, remove_outliers = "spectral"))),
      class = "horizons_input_error"
    )

  })

})


## ===========================================================================
## 9. Structural validation on return (#24)
## ===========================================================================

describe("validate() and the structural validator", {

  test_that("validate() catches a corrupt input the verb itself never checks (#24)", {

    ## Arrange — an increasing wavenumber axis. validate()'s own checks are
    ## about sample counts, outcome health and outliers; none of them read or
    ## enforce predictor order, so nothing here would ever catch this. The new
    ## end-of-verb validate_horizons_data() call (#24) is what catches it.
    hd <- make_configured_hd(n_samples = 20, n_predictors = 5)

    pred_rows <- which(hd$data$role_map$role == "predictor")
    hd$data$role_map[pred_rows, ] <- hd$data$role_map[rev(pred_rows), ]

    ## Act & Assert ------------------------------------------------------------

    expect_error(
      capture.output(validate(hd)),
      class = "horizons_validation_error"
    )

  })

})
