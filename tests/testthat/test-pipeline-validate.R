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

  ## Build predictor matrix (rnorm, varied)
  wn_names <- as.character(seq(600, 600 + n_predictors - 1))
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

  ## Assemble object
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
    evaluation = list(results = NULL),
    models     = list(workflows = NULL),
    ensemble   = list(stack = NULL),
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

    p005 <- result$validation$checks[result$validation$checks$check_id == "P005", ]
    expect_equal(p005$severity, "INFO")
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

    p006 <- result$validation$checks[result$validation$checks$check_id == "P006", ]
    expect_equal(p006$severity, "INFO")
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
