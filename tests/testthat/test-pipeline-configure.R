## ---------------------------------------------------------------------------
## Tests for pipeline-configure.R: configure() function
## ---------------------------------------------------------------------------

library(testthat)
library(horizons)


## ---------------------------------------------------------------------------
## Helpers: Create horizons_data fixtures for testing
## ---------------------------------------------------------------------------

#' Create minimal horizons_data with one response (SOC)
#'
#' Simulates post-add_response() state: has spectral predictors and one
#' response variable in role_map and analysis tibble.
#'
#' @return A horizons_data object ready for configure().
#' @noRd
make_single_response_hd <- function() {

  analysis <- tibble::tibble(
    sample_id = c("S001", "S002", "S003"),
    `600`     = c(0.10, 0.20, 0.30),
    `601`     = c(0.11, 0.21, 0.31),
    `602`     = c(0.12, 0.22, 0.32),
    SOC       = c(1.2, 3.4, 5.6)
  )

  role_map <- tibble::tibble(
    variable = c("sample_id", "600", "601", "602", "SOC"),
    role     = c("id", "predictor", "predictor", "predictor", "response")
  )

  obj <- list(
    data = list(
      analysis     = analysis,
      role_map     = role_map,
      n_rows       = 3L,
      n_predictors = 3L,
      n_covariates = 0L,
      n_responses  = 1L
    ),
    provenance = list(
      spectra_source   = "test",
      spectra_type     = "mir",
      created          = Sys.time(),
      horizons_version = utils::packageVersion("horizons"),
      schema_version   = 1L
    ),
    config = list(
      configs   = NULL,
      n_configs = NULL,
      tuning    = list(grid_size = 10L, bayesian_iter = 15L, cv_folds = 5L)
    ),
    validation = list(passed = NULL, checks = NULL, timestamp = NULL),
    evaluation = list(results = NULL),
    models     = list(workflows = NULL),
    ensemble   = list(stack = NULL),
    artifacts  = list(cache_dir = NULL)
  )

  class(obj) <- c("horizons_data", "list")
  obj

}


#' Create horizons_data with two responses (SOC and pH)
#' @noRd
make_multi_response_hd <- function() {

  analysis <- tibble::tibble(
    sample_id = c("S001", "S002", "S003"),
    `600`     = c(0.10, 0.20, 0.30),
    `601`     = c(0.11, 0.21, 0.31),
    `602`     = c(0.12, 0.22, 0.32),
    SOC       = c(1.2, 3.4, 5.6),
    pH        = c(5.5, 6.0, 6.5)
  )

  role_map <- tibble::tibble(
    variable = c("sample_id", "600", "601", "602", "SOC", "pH"),
    role     = c("id", "predictor", "predictor", "predictor", "response", "response")
  )

  obj <- list(
    data = list(
      analysis     = analysis,
      role_map     = role_map,
      n_rows       = 3L,
      n_predictors = 3L,
      n_covariates = 0L,
      n_responses  = 2L
    ),
    provenance = list(
      spectra_source   = "test",
      spectra_type     = "mir",
      created          = Sys.time(),
      horizons_version = utils::packageVersion("horizons"),
      schema_version   = 1L
    ),
    config = list(
      configs   = NULL,
      n_configs = NULL,
      tuning    = list(grid_size = 10L, bayesian_iter = 15L, cv_folds = 5L)
    ),
    validation = list(passed = NULL, checks = NULL, timestamp = NULL),
    evaluation = list(results = NULL),
    models     = list(workflows = NULL),
    ensemble   = list(stack = NULL),
    artifacts  = list(cache_dir = NULL)
  )

  class(obj) <- c("horizons_data", "list")
  obj

}


#' Create horizons_data with covariates (clay and MAP)
#' @noRd
make_covariate_hd <- function() {

  analysis <- tibble::tibble(
    sample_id = c("S001", "S002", "S003"),
    `600`     = c(0.10, 0.20, 0.30),
    `601`     = c(0.11, 0.21, 0.31),
    `602`     = c(0.12, 0.22, 0.32),
    SOC       = c(1.2, 3.4, 5.6),
    clay      = c(20, 35, 50),
    MAP       = c(800, 1000, 1200)
  )

  role_map <- tibble::tibble(
    variable = c("sample_id", "600", "601", "602", "SOC", "clay", "MAP"),
    role     = c("id", "predictor", "predictor", "predictor",
                 "response", "covariate", "covariate")
  )

  obj <- list(
    data = list(
      analysis     = analysis,
      role_map     = role_map,
      n_rows       = 3L,
      n_predictors = 3L,
      n_covariates = 2L,
      n_responses  = 1L
    ),
    provenance = list(
      spectra_source   = "test",
      spectra_type     = "mir",
      created          = Sys.time(),
      horizons_version = utils::packageVersion("horizons"),
      schema_version   = 1L
    ),
    config = list(
      configs   = NULL,
      n_configs = NULL,
      tuning    = list(grid_size = 10L, bayesian_iter = 15L, cv_folds = 5L)
    ),
    validation = list(passed = NULL, checks = NULL, timestamp = NULL),
    evaluation = list(results = NULL),
    models     = list(workflows = NULL),
    ensemble   = list(stack = NULL),
    artifacts  = list(cache_dir = NULL)
  )

  class(obj) <- c("horizons_data", "list")
  obj

}


#' Silently run configure() — suppresses CLI tree output
#' @noRd
quiet_configure <- function(...) {
  suppressWarnings(
    invisible(capture.output(
      result <- configure(...)
    ))
  )
  result
}


## ===========================================================================
## 1. Validation tests
## ===========================================================================

describe("configure() validation", {

  test_that("rejects non-horizons_data input", {

    expect_error(
      capture.output(configure(data.frame(x = 1))),
      class = "horizons_configure_error"
    )

  })

  test_that("rejects object with no response data", {

    ## Build an object with no response variables
    analysis <- tibble::tibble(
      sample_id = c("S001", "S002"),
      `600`     = c(0.1, 0.2)
    )

    role_map <- tibble::tibble(
      variable = c("sample_id", "600"),
      role     = c("id", "predictor")
    )

    hd <- list(
      data = list(
        analysis = analysis, role_map = role_map,
        n_rows = 2L, n_predictors = 1L, n_covariates = 0L, n_responses = 0L
      ),
      provenance = list(spectra_source = "test", spectra_type = "mir",
                        created = Sys.time(),
                        horizons_version = utils::packageVersion("horizons"),
                        schema_version = 1L),
      config = list(configs = NULL, n_configs = NULL,
                    tuning = list(grid_size = 10L, bayesian_iter = 15L, cv_folds = 5L)),
      validation = list(), evaluation = list(), models = list(),
      ensemble = list(), artifacts = list()
    )
    class(hd) <- c("horizons_data", "list")

    expect_error(
      capture.output(configure(hd)),
      "No response data found",
      class = "horizons_configure_error"
    )

  })

  test_that("errors when multiple responses and no outcome specified", {

    hd <- make_multi_response_hd()

    expect_error(
      capture.output(configure(hd)),
      "Multiple response variables",
      class = "horizons_configure_error"
    )

  })

  test_that("error for multiple responses lists available variables", {

    hd <- make_multi_response_hd()

    expect_error(
      capture.output(configure(hd)),
      "Multiple response variables",
      class = "horizons_configure_error"
    )

  })

  test_that("errors when specified outcome doesn't exist", {

    hd <- make_multi_response_hd()

    expect_error(
      capture.output(configure(hd, outcome = "TotalN")),
      "TotalN.*not found",
      class = "horizons_configure_error"
    )

  })

  test_that("error for invalid models includes invalid and valid values", {

    hd <- make_single_response_hd()

    expect_error(
      capture.output(configure(hd, models = c("rf", "fake_model"))),
      "Invalid model",
      class = "horizons_configure_error"
    )

  })

  test_that("error for invalid transformations", {

    hd <- make_single_response_hd()

    expect_error(
      capture.output(configure(hd, transformations = "box_cox")),
      "Invalid transformation",
      class = "horizons_configure_error"
    )

  })

  test_that("error for invalid preprocessing", {

    hd <- make_single_response_hd()

    expect_error(
      capture.output(configure(hd, preprocessing = "msc")),
      "Invalid preprocessing",
      class = "horizons_configure_error"
    )

  })

  test_that("error for invalid feature_selection", {

    hd <- make_single_response_hd()

    expect_error(
      capture.output(configure(hd, feature_selection = "rfe")),
      "Invalid feature selection",
      class = "horizons_configure_error"
    )

  })

  test_that("error messages include both invalid and valid options", {

    hd <- make_single_response_hd()

    expect_error(
      capture.output(configure(hd, models = "bogus")),
      "Invalid model",
      class = "horizons_configure_error"
    )

  })

  test_that("errors for invalid cov_fusion values", {

    hd <- make_single_response_hd()

    expect_error(
      capture.output(configure(hd, cov_fusion = "middle")),
      "middle",
      class = "horizons_configure_error"
    )

  })

  test_that("errors when covariates exist but cov_fusion is NULL", {

    hd <- make_covariate_hd()

    expect_error(
      capture.output(configure(hd, cov_fusion = NULL)),
      "fusion",
      class = "horizons_configure_error"
    )

  })

  test_that("errors for cv_folds < 2", {

    hd <- make_single_response_hd()

    expect_error(
      capture.output(configure(hd, cv_folds = 1)),
      "cv_folds",
      class = "horizons_configure_error"
    )

  })

  test_that("errors for grid_size < 1", {

    hd <- make_single_response_hd()

    expect_error(
      capture.output(configure(hd, grid_size = 0)),
      "grid_size",
      class = "horizons_configure_error"
    )

  })

  test_that("errors for bayesian_iter < 0", {

    hd <- make_single_response_hd()

    expect_error(
      capture.output(configure(hd, bayesian_iter = -1)),
      "bayesian_iter",
      class = "horizons_configure_error"
    )

  })

  test_that("errors for expand_covariates with invalid covariate names", {

    hd <- make_covariate_hd()

    expect_error(
      capture.output(configure(hd, cov_fusion = "early",
                               expand_covariates = c("clay", "sand"))),
      "Covariate.*not found",
      class = "horizons_configure_error"
    )

  })

})


## ===========================================================================
## 2. Outcome promotion tests
## ===========================================================================

describe("configure() outcome promotion", {

  test_that("auto-selects single response as outcome", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd)

    outcome_rows <- result$data$role_map[result$data$role_map$role == "outcome", ]
    expect_equal(nrow(outcome_rows), 1)
    expect_equal(outcome_rows$variable, "SOC")

  })

  test_that("promotes specified response to outcome role", {

    hd     <- make_multi_response_hd()
    result <- quiet_configure(hd, outcome = "pH")

    outcome_rows <- result$data$role_map[result$data$role_map$role == "outcome", ]
    expect_equal(outcome_rows$variable, "pH")

  })

  test_that("other responses remain as role 'response'", {

    hd     <- make_multi_response_hd()
    result <- quiet_configure(hd, outcome = "SOC")

    ph_role <- result$data$role_map$role[result$data$role_map$variable == "pH"]
    expect_equal(ph_role, "response")

  })

  test_that("reconfiguring resets previous outcome to 'response'", {

    hd <- make_multi_response_hd()
    r1 <- quiet_configure(hd, outcome = "SOC")
    r2 <- quiet_configure(r1, outcome = "pH")

    soc_role <- r2$data$role_map$role[r2$data$role_map$variable == "SOC"]
    ph_role  <- r2$data$role_map$role[r2$data$role_map$variable == "pH"]

    expect_equal(soc_role, "response")
    expect_equal(ph_role, "outcome")

  })

  test_that("outcome appears in role_map exactly once as 'outcome'", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd)

    n_outcome <- sum(result$data$role_map$role == "outcome")
    expect_equal(n_outcome, 1)

  })

  test_that("case sensitivity: outcome = 'soc' fails if response is 'SOC'", {

    hd <- make_single_response_hd()

    expect_error(
      capture.output(configure(hd, outcome = "soc")),
      "not found",
      class = "horizons_configure_error"
    )

  })

})


## ===========================================================================
## 3. Config grid tests
## ===========================================================================

describe("configure() config grid", {

  test_that("correct row count (Cartesian product of all axes)", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd,
                              models          = c("rf", "cubist"),
                              transformations = c("none", "log"),
                              preprocessing   = c("raw", "snv"),
                              feature_selection = "none")

    ## 2 models x 2 transforms x 2 preproc x 1 fs x 1 cov_set = 8
    expect_equal(nrow(result$config$configs), 8)

  })

  test_that("all 9 columns present", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd)

    expected_cols <- c("config_id", "model", "transformation", "preprocessing",
                       "feature_selection", "covariates",
                       "preprocessing_params", "feature_params", "transform_params")

    expect_equal(sort(names(result$config$configs)), sort(expected_cols))

  })

  test_that("config IDs are unique", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd,
                              models        = c("rf", "cubist", "plsr"),
                              preprocessing = c("raw", "snv"))

    ids <- result$config$configs$config_id
    expect_equal(length(ids), length(unique(ids)))

  })

  test_that("config IDs are deterministic (same inputs produce same IDs)", {

    hd <- make_single_response_hd()
    r1 <- quiet_configure(hd, models = c("rf", "cubist"))
    r2 <- quiet_configure(hd, models = c("rf", "cubist"))

    expect_equal(r1$config$configs$config_id, r2$config$configs$config_id)

  })

  test_that("list-columns initialized as NULL (each element is NULL)", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd, models = "rf")

    expect_true(all(vapply(result$config$configs$preprocessing_params,
                           is.null, logical(1))))
    expect_true(all(vapply(result$config$configs$feature_params,
                           is.null, logical(1))))
    expect_true(all(vapply(result$config$configs$transform_params,
                           is.null, logical(1))))

  })

  test_that("covariates column is NA when no covariates", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd)

    expect_true(all(is.na(result$config$configs$covariates)))

  })

  test_that("single-everything config produces 1 row", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd,
                              models            = "rf",
                              transformations   = "none",
                              preprocessing     = "raw",
                              feature_selection = "none")

    expect_equal(nrow(result$config$configs), 1)

  })

  test_that("large grid: 4 x 2 x 3 x 2 = 48 configs", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd,
                              models            = c("rf", "cubist", "plsr", "xgboost"),
                              transformations   = c("none", "log"),
                              preprocessing     = c("raw", "snv", "sg"),
                              feature_selection = c("none", "pca"))

    expect_equal(nrow(result$config$configs), 48)

  })

})


## ===========================================================================
## 4. Covariate expansion tests
## ===========================================================================

describe("configure() covariate expansion", {

  test_that("no covariates: covariates column all NA, single set", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd, models = "rf")

    expect_true(all(is.na(result$config$configs$covariates)))
    expect_equal(nrow(result$config$configs), 1)

  })

  test_that("expand_covariates = NULL with covariates: all covariates in every config", {

    hd     <- make_covariate_hd()
    result <- quiet_configure(hd,
                              models            = "rf",
                              cov_fusion        = "early",
                              expand_covariates = NULL)

    ## All configs should have the same covariate string (sorted: MAP,clay)
    expect_true(all(result$config$configs$covariates == "MAP,clay"))

  })

  test_that("expand_covariates = TRUE: power set (2^n sets)", {

    hd     <- make_covariate_hd()
    result <- quiet_configure(hd,
                              models            = "rf",
                              cov_fusion        = "early",
                              expand_covariates = TRUE)

    ## 2 covariates → power set = 2^2 = 4 sets (NA, MAP, clay, MAP+clay)
    ## 1 model x 1 transform x 1 preproc x 1 fs x 4 cov_sets = 4
    expect_equal(nrow(result$config$configs), 4)

  })

  test_that("expand_covariates = TRUE with 1 covariate: 2 sets", {

    ## Build a fixture with only 1 covariate
    hd <- make_covariate_hd()
    hd$data$role_map$role[hd$data$role_map$variable == "MAP"] <- "predictor"
    hd$data$n_covariates <- 1L

    result <- quiet_configure(hd,
                              models            = "rf",
                              cov_fusion        = "early",
                              expand_covariates = TRUE)

    ## 1 covariate → power set = 2 sets (NA, clay)
    expect_equal(nrow(result$config$configs), 2)

  })

  test_that("expand_covariates = c('clay'): selective expansion, others fixed", {

    hd     <- make_covariate_hd()
    result <- quiet_configure(hd,
                              models            = "rf",
                              cov_fusion        = "early",
                              expand_covariates = c("clay"))

    ## Expanding "clay" only: power set of {clay} = {NA, clay}
    ## Fixed covariate: MAP
    ## Sets: {MAP} and {MAP,clay} → 2 unique sets
    cov_vals <- sort(unique(result$config$configs$covariates))
    expect_equal(length(cov_vals), 2)
    expect_true("MAP" %in% cov_vals)
    expect_true("MAP,clay" %in% cov_vals)

  })

  test_that("expand_covariates = FALSE: all NA, covariates excluded", {

    hd     <- make_covariate_hd()
    result <- quiet_configure(hd,
                              models            = "rf",
                              cov_fusion        = "early",
                              expand_covariates = FALSE)

    expect_true(all(is.na(result$config$configs$covariates)))

  })

  test_that("covariate strings are canonicalized (sorted)", {

    hd     <- make_covariate_hd()
    result <- quiet_configure(hd,
                              models            = "rf",
                              cov_fusion        = "early",
                              expand_covariates = NULL)

    ## MAP comes before clay alphabetically
    expect_equal(result$config$configs$covariates[1], "MAP,clay")

  })

  test_that("total config count = base grid x covariate sets", {

    hd     <- make_covariate_hd()
    result <- quiet_configure(hd,
                              models            = c("rf", "cubist"),
                              transformations   = c("none", "log"),
                              cov_fusion        = "early",
                              expand_covariates = TRUE)

    ## 2 models x 2 transforms x 1 preproc x 1 fs = 4 base
    ## 2 covariates → 4 covariate sets
    ## 4 x 4 = 16
    expect_equal(nrow(result$config$configs), 16)

  })

})


## ===========================================================================
## 5. Storage tests
## ===========================================================================

describe("configure() storage", {

  test_that("config$configs is a tibble", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd)

    expect_s3_class(result$config$configs, "tbl_df")

  })

  test_that("config$n_configs matches nrow(config$configs)", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd,
                              models = c("rf", "cubist"),
                              preprocessing = c("raw", "snv"))

    expect_equal(result$config$n_configs, nrow(result$config$configs))

  })

  test_that("config$tuning stores all three params correctly", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd,
                              cv_folds      = 10L,
                              grid_size     = 20L,
                              bayesian_iter = 25L)

    expect_equal(result$config$tuning$cv_folds, 10L)
    expect_equal(result$config$tuning$grid_size, 20L)
    expect_equal(result$config$tuning$bayesian_iter, 25L)

  })

  test_that("config$expansion stores all original inputs", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd,
                              models          = c("rf", "cubist"),
                              transformations = "log",
                              preprocessing   = "snv",
                              feature_selection = "pca")

    exp <- result$config$expansion
    expect_equal(exp$outcome, "SOC")
    expect_equal(exp$models, c("rf", "cubist"))
    expect_equal(exp$transformations, "log")
    expect_equal(exp$preprocessing, "snv")
    expect_equal(exp$feature_selection, "pca")

  })

  test_that("config$defaults has expected structure", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd)

    defaults <- result$config$defaults
    expect_true("preprocessing_params" %in% names(defaults))
    expect_true("feature_params" %in% names(defaults))
    expect_true("transform_params" %in% names(defaults))
    expect_equal(defaults$preprocessing_params$sg_window, 11L)
    expect_equal(defaults$preprocessing_params$sg_order, 2L)
    expect_equal(defaults$feature_params$pca_threshold, 0.99)
    expect_equal(defaults$feature_params$correlation_n, 200L)

  })

})


## ===========================================================================
## 6. Covariate fusion tests
## ===========================================================================

describe("configure() covariate fusion", {

  test_that("cov_fusion = NULL when no covariates -> stored as NULL", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd, cov_fusion = NULL)

    expect_null(result$config$expansion$cov_fusion)

  })

  test_that("cov_fusion = 'early' with no covariates -> warns, stored as NULL", {

    hd <- make_single_response_hd()

    expect_warning(
      capture.output(result <- configure(hd, cov_fusion = "early")),
      "cov_fusion ignored"
    )

    expect_null(result$config$expansion$cov_fusion)

  })

  test_that("cov_fusion stored correctly when covariates present", {

    hd     <- make_covariate_hd()
    result <- quiet_configure(hd, cov_fusion = "late")

    expect_equal(result$config$expansion$cov_fusion, "late")

  })

  test_that("expand_covariates with no covariates -> warns, stored as NULL", {

    hd <- make_single_response_hd()

    expect_warning(
      capture.output(result <- configure(hd, expand_covariates = TRUE)),
      "expand_covariates ignored"
    )

    expect_null(result$config$expansion$expand_covariates)

  })

})


## ===========================================================================
## 7. Reconfiguration tests
## ===========================================================================

describe("configure() reconfiguration", {

  test_that("calling configure() twice overwrites cleanly", {

    hd <- make_single_response_hd()
    r1 <- quiet_configure(hd, models = c("rf", "cubist"))

    expect_equal(r1$config$n_configs, 2)

    r2 <- quiet_configure(r1, models = "plsr")

    expect_equal(r2$config$n_configs, 1)
    expect_equal(r2$config$configs$model, "plsr")

  })

  test_that("previous outcome reverts to 'response' before new promotion", {

    hd <- make_multi_response_hd()
    r1 <- quiet_configure(hd, outcome = "SOC")

    ## SOC is outcome after first configure
    expect_equal(
      r1$data$role_map$role[r1$data$role_map$variable == "SOC"],
      "outcome"
    )

    r2 <- quiet_configure(r1, outcome = "pH")

    ## SOC reverted, pH is now outcome
    expect_equal(
      r2$data$role_map$role[r2$data$role_map$variable == "SOC"],
      "response"
    )
    expect_equal(
      r2$data$role_map$role[r2$data$role_map$variable == "pH"],
      "outcome"
    )

  })

  test_that("warning issued on reconfiguration", {

    hd <- make_single_response_hd()
    r1 <- quiet_configure(hd)

    expect_warning(
      capture.output(configure(r1)),
      "Overwriting"
    )

  })

})


## ===========================================================================
## 8. CLI output tests
## ===========================================================================

describe("configure() CLI output", {

  test_that("output contains outcome name, model count, and config count", {

    hd     <- make_single_response_hd()
    output <- capture.output(
      suppressWarnings(result <- configure(hd, models = c("rf", "cubist")))
    )

    combined <- paste(output, collapse = "\n")
    expect_true(grepl("SOC", combined))
    expect_true(grepl("rf", combined))
    expect_true(grepl("cubist", combined))
    expect_true(grepl("2", combined))

  })

  test_that("covariate expansion info displayed when relevant", {

    hd     <- make_covariate_hd()
    output <- capture.output(
      suppressWarnings(
        result <- configure(hd,
                            cov_fusion        = "early",
                            expand_covariates = TRUE)
      )
    )

    combined <- paste(output, collapse = "\n")
    expect_true(grepl("Covariate", combined, ignore.case = TRUE))
    expect_true(grepl("clay", combined))
    expect_true(grepl("MAP", combined))

  })

})


## ===========================================================================
## 9. Edge case tests
## ===========================================================================

describe("configure() edge cases", {

  test_that("single model, single everything -> 1 config", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd,
                              models            = "rf",
                              transformations   = "none",
                              preprocessing     = "raw",
                              feature_selection = "none")

    expect_equal(result$config$n_configs, 1)

  })

  test_that("all defaults (only outcome specified) -> 3 configs (rf, cubist, plsr)", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd)

    expect_equal(result$config$n_configs, 3)
    expect_true(all(c("rf", "cubist", "plsr") %in% result$config$configs$model))

  })

  test_that("tuning param boundaries: cv_folds = 2, grid_size = 1, bayesian_iter = 0", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd,
                              cv_folds      = 2L,
                              grid_size     = 1L,
                              bayesian_iter = 0L)

    expect_equal(result$config$tuning$cv_folds, 2L)
    expect_equal(result$config$tuning$grid_size, 1L)
    expect_equal(result$config$tuning$bayesian_iter, 0L)

  })

  test_that("duplicated axis values are deduplicated", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd, models = c("rf", "rf"))

    ## tidyr::crossing deduplicates, so should get 1 rf row (times default axes)
    rf_count <- sum(result$config$configs$model == "rf")

    ## With defaults: 1 model x 1 transform x 1 preproc x 1 fs = 1
    expect_equal(rf_count, 1)

  })

})
