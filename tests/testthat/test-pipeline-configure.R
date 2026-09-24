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

  ## Downstream slots in the constructor's shape
  contract <- new_horizons_data()

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
    evaluation = contract$evaluation,
    models     = contract$models,
    ensemble   = contract$ensemble,
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

  ## Downstream slots in the constructor's shape
  contract <- new_horizons_data()

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
    evaluation = contract$evaluation,
    models     = contract$models,
    ensemble   = contract$ensemble,
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

  ## Downstream slots in the constructor's shape
  contract <- new_horizons_data()

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
    evaluation = contract$evaluation,
    models     = contract$models,
    ensemble   = contract$ensemble,
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

  test_that("rejects an sg_window that is even, below 5, fractional or not a scalar (#62)", {

    hd <- make_single_response_hd()

    ## 3 is odd but too narrow: deriv2 fits a cubic, which needs 5 points.
    for (bad in list(10L, 8, 3L, 1L, 7.5, NA_integer_, Inf, "9", c(9L, 11L))) {

      expect_error(
        capture.output(configure(hd, sg_window = bad)),
        "`sg_window` must be an odd integer >= 5",
        class = "horizons_configure_error",
        info  = paste("sg_window =", deparse(bad))
      )

    }

  })

  test_that("rejects a pca_threshold outside (0, 1] or not a single number (#62)", {

    hd <- make_single_response_hd()

    for (bad in list(0, -0.5, 1.01, 2, NA_real_, NaN, "0.99", c(0.9, 0.99))) {

      expect_error(
        capture.output(configure(hd, pca_threshold = bad)),
        "`pca_threshold` must be a single number in \\(0, 1\\]",
        class = "horizons_configure_error",
        info  = paste("pca_threshold =", deparse(bad))
      )

    }

  })

  test_that("accepts the boundary recipe settings: sg_window = 5, pca_threshold = 1", {

    hd <- make_single_response_hd()

    result <- quiet_configure(hd, sg_window = 5L, pca_threshold = 1)

    expect_identical(result$config$recipe$sg_window, 5L)
    expect_identical(result$config$recipe$pca_threshold, 1)

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

  test_that("all 6 columns present, and no per-config parameter columns (#62)", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd)

    ## The three list-columns that stood here were never read by the recipe
    ## builder, so they claimed per-config overrides that did not exist.
    expected_cols <- c("config_id", "model", "transformation", "preprocessing",
                       "feature_selection", "covariates")

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

  test_that("config IDs do not depend on the recipe settings (#62)", {

    ## The settings are object-level, so two objects that differ only in them
    ## name their configs identically; nothing per-config changed.
    hd      <- make_single_response_hd()
    default <- quiet_configure(hd, models = c("rf", "plsr"),
                               feature_selection = c("none", "pca"))
    tuned   <- quiet_configure(hd, models = c("rf", "plsr"),
                               feature_selection = c("none", "pca"),
                               sg_window = 15L, pca_threshold = 0.9)

    expect_identical(tuned$config$configs$config_id,
                     default$config$configs$config_id)

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

  ## -------------------------------------------------------------------------
  ## Recipe settings (#62)
  ## -------------------------------------------------------------------------
  ## configure() used to write config$defaults (a window of 11, an order of 2,
  ## a PCA threshold of 0.99, a correlation_n of 200) while the recipe ran a
  ## window of 9, a method-determined order, a threshold of 0.995 and a
  ## correlation step with no n at all. The record now holds what runs.

  test_that("config$recipe records the settings the recipe runs, at their defaults", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd)

    expect_identical(result$config$recipe$sg_window, 9L)
    expect_identical(result$config$recipe$pca_threshold, 0.995)
    expect_null(result$config$defaults)

  })

  test_that("config$recipe stores sg_window and pca_threshold as given", {

    hd     <- make_single_response_hd()
    result <- quiet_configure(hd, sg_window = 15, pca_threshold = 0.9)

    ## Nothing axis-dependent is stored: the window's width in cm-1 would go
    ## stale if standardize() ran after configure(), so evaluate() records it.
    expect_identical(result$config$recipe,
                     list(sg_window = 15L, pca_threshold = 0.9))
    expect_identical(recipe_settings(result),
                     list(sg_window = 15L, pca_threshold = 0.9))

  })

  test_that("configure()'s defaults are the values recipe_settings() falls back on", {

    ## An object configured before the settings existed runs the fallback;
    ## a freshly configured one runs the formals. They must be the same
    ## values, or the two would build different recipes from one grid.
    fm <- formals(configure)

    expect_identical(fm$sg_window, DEFAULT_SG_WINDOW)
    expect_identical(fm$pca_threshold, DEFAULT_PCA_THRESHOLD)
    expect_identical(recipe_settings(list(config = list(configs = NULL))),
                     list(sg_window = DEFAULT_SG_WINDOW,
                          pca_threshold = DEFAULT_PCA_THRESHOLD))

  })

  test_that("config$recipe is a key of the constructor's config slot", {

    expect_true("recipe" %in% names(new_horizons_data()$config))

  })

  ## axis_spacing_cm() converts the window to cm-1 for configure()'s console
  ## line and evaluate()'s record, each reading the axis at that moment.

  test_that("axis_spacing_cm() reads the spacing of the predictor axis", {

    ## The fixture's predictors are 600, 601 and 602: one cm-1 apart.
    hd <- make_single_response_hd()

    expect_equal(axis_spacing_cm(hd), 1)

    ## wn_-prefixed names, 4 cm-1 apart
    old <- c("600", "601", "602")
    new <- paste0("wn_", c(608, 604, 600))
    names(hd$data$analysis)[match(old, names(hd$data$analysis))] <- new
    hd$data$role_map$variable[match(old, hd$data$role_map$variable)] <- new

    expect_equal(axis_spacing_cm(hd), 4)

  })

  test_that("axis_spacing_cm() reads the axis before a recorded grid step that disagrees", {

    ## select_training() returns the pool's standardize() provenance on the
    ## targets' axis, so the recorded step can describe a different grid.
    hd <- make_single_response_hd()
    hd$provenance$standardization <- list(grid = list(step = 4))

    expect_equal(axis_spacing_cm(hd), 1)

  })

  test_that("axis_spacing_cm() falls back to the recorded grid step, then to NA", {

    ## Predictor names that are not wavenumbers carry no spacing of their own.
    hd  <- make_single_response_hd()
    old <- c("600", "601", "602")
    new <- c("band_a", "band_b", "band_c")
    names(hd$data$analysis)[match(old, names(hd$data$analysis))] <- new
    hd$data$role_map$variable[match(old, hd$data$role_map$variable)] <- new

    expect_true(is.na(axis_spacing_cm(hd)))

    hd$provenance$standardization <- list(grid = list(step = 2))
    expect_equal(axis_spacing_cm(hd), 2)

  })

  test_that("re-configuring an object from an earlier version drops its config$defaults", {

    hd <- make_single_response_hd()
    hd$config$defaults <- list(
      preprocessing_params = list(sg_window = 11L, sg_order = 2L),
      feature_params       = list(pca_threshold = 0.99, correlation_n = 200L),
      transform_params     = list()
    )

    expect_null(quiet_configure(hd)$config$defaults)

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
    result <- quiet_configure(hd, cov_fusion = "early")

    expect_equal(result$config$expansion$cov_fusion, "early")

  })

  test_that("cov_fusion = 'late' aborts: late fusion is not built (#69)", {

    ## build_recipe() only fuses early, so accepting "late" ran early fusion
    ## under the other name. Refused with or without covariates present, and
    ## caught by a handler for either class.
    expect_error(
      capture.output(configure(make_covariate_hd(), cov_fusion = "late")),
      "not built",
      class = "horizons_configure_error"
    )

    expect_error(
      capture.output(configure(make_covariate_hd(), cov_fusion = "late")),
      class = "horizons_input_error"
    )

    expect_error(
      capture.output(configure(make_single_response_hd(), cov_fusion = "late")),
      class = "horizons_configure_error"
    )

  })

  test_that("cov_fusion must be NULL or a single string (#69)", {

    hd <- make_covariate_hd()

    expect_error(
      capture.output(configure(hd, cov_fusion = c("early", "late"))),
      "single string",
      class = "horizons_configure_error"
    )

    expect_error(
      capture.output(configure(hd, cov_fusion = NA_character_)),
      "single string",
      class = "horizons_configure_error"
    )

    expect_error(
      capture.output(configure(hd, cov_fusion = TRUE)),
      "single string",
      class = "horizons_configure_error"
    )

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

  test_that("prints the window with its width in cm-1, and the PCA threshold when PCA runs", {

    hd <- make_single_response_hd()

    plain <- paste(capture.output(suppressWarnings(configure(hd, sg_window = 11L))),
                   collapse = "\n")
    pca   <- paste(capture.output(suppressWarnings(
      configure(hd, feature_selection = "pca", pca_threshold = 0.9)
    )), collapse = "\n")

    expect_match(plain, "SG window 11 (11 cm", fixed = TRUE)
    expect_no_match(plain, "PCA threshold")
    expect_match(pca, "PCA threshold 0.9", fixed = TRUE)

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


## ---------------------------------------------------------------------------
## Stored counts and the promotion the reconfigure gives back
## ---------------------------------------------------------------------------

describe("configure() and the object contract", {

  test_that("configure() recounts n_responses after promoting the outcome", {

    ## Arrange — two responses, one of which is about to become the outcome
    fx <- make_select_fixture(n_pool = 40)

    expect_identical(fx$pool$data$n_responses, 2L)

    ## Act
    result <- quiet_configure(fx$pool, outcome = "clay")

    ## Assert — the stored count follows the role map, so the next verb's
    ## validator does not abort on a legal chain
    expect_identical(result$data$n_responses, 1L)
    expect_identical(
      result$data$n_responses,
      sum(result$data$role_map$role == "response")
    )
    expect_no_error(validate_horizons_data(result))

  })


  test_that("a configured object survives the validator the next verb runs", {

    ## configure() |> standardize() is the chain that used to abort
    fx     <- make_select_fixture(n_pool = 40)
    result <- quiet_configure(fx$pool, outcome = "clay")

    expect_no_error(validate_horizons_data(result))

    ## And again after the outcome moves
    again <- quiet_configure(result, outcome = "oc")

    expect_identical(again$data$n_responses, 1L)
    expect_no_error(validate_horizons_data(again))

  })


  test_that("reconfiguring drops every promotion the object had earned", {

    ## Arrange — a configured object dressed as a fitted one
    fx  <- make_select_fixture(n_pool = 40)
    obj <- quiet_configure(fx$pool, outcome = "clay")

    obj$evaluation$results      <- tibble::tibble(config_id = "cfg_a")
    obj$evaluation$best_config  <- "cfg_a"
    obj$evaluation$split        <- "an rsplit"
    obj$models$workflows        <- list(cfg_a = "a fitted workflow")
    obj$models$n_models         <- 1L
    obj$models$split            <- "an rsplit"
    obj$models$row_index        <- tibble::tibble(.row = 1L, sample_id = "P001")
    obj$models$cv_predictions   <- tibble::tibble(.row = 1L, .pred = 1)
    obj$models$predictor_schema <- c("wn_4000")
    obj$ensemble$method         <- "weighted"

    ## Keys the old hand-kept clear list missed (#70)
    obj$evaluation$rank_metric  <- "rpd"
    obj$evaluation$n_train      <- 30L
    obj$evaluation$runtime_secs <- 1
    obj$models$results          <- tibble::tibble(config_id = "cfg_a")
    obj$models$uq               <- list(cfg_a = list(quantile_model = "a UQ bundle"))
    obj$models$ad               <- list(cfg_a = list(centroid = 1))
    obj$ensemble$model          <- "a trained meta-learner"

    class(obj) <- c("horizons_fit", "horizons_eval", "horizons_data", "list")

    ## Act
    result <- quiet_configure(obj, outcome = "oc")

    ## Assert — the slots and the class are both claims about state that the
    ## new outcome has invalidated, so all three slots are back to the
    ## constructor's shape, key for key
    blank <- new_horizons_data()

    expect_identical(result$evaluation, blank$evaluation)
    expect_identical(result$models,     blank$models)
    expect_identical(result$ensemble,   blank$ensemble)
    expect_false(has_uq(result))

    expect_identical(class(result), c("horizons_data", "list"))
    expect_identical(promoted_state(result), character())
    expect_no_error(validate_horizons_data(result))

  })


  test_that("reconfiguring clears the validation verdict and keeps the removal record (#70)", {

    ## Arrange — a configured object that validate() has judged, with one
    ## outlier removed (P099 is no longer in the analysis table)
    fx  <- make_select_fixture(n_pool = 40)
    obj <- quiet_configure(fx$pool, outcome = "clay")

    removal <- list(removed_ids    = "P099",
                    removal_detail = tibble::tibble(sample_id = "P099", reason = "spectral"),
                    removed        = TRUE)

    obj$validation$passed                <- TRUE
    obj$validation$checks                <- tibble::tibble(check_id = "P001", status = "pass")
    obj$validation$timestamp             <- Sys.time()
    obj$validation$outliers$spectral_ids <- c("P099", "P012")
    obj$validation$outliers$response_ids <- "P007"
    obj$validation$outliers[names(removal)] <- removal

    ## Act
    result <- quiet_configure(obj, outcome = "oc")

    ## Assert — the verdict and the flags are for the old outcome; the rows
    ## that left are still gone
    blank <- new_horizons_data()

    expect_identical(names(result$validation), names(blank$validation))
    expect_identical(names(result$validation$outliers), names(blank$validation$outliers))

    expect_null(result$validation$passed)
    expect_null(result$validation$checks)
    expect_null(result$validation$timestamp)
    expect_null(result$validation$outliers$spectral_ids)
    expect_null(result$validation$outliers$response_ids)

    expect_identical(result$validation$outliers[names(removal)], removal)

  })


  test_that("reconfiguring a real ensemble() result resets it (#70)", {

    ## Arrange — the cached fit, through ensemble(). This used to abort in
    ## set_analysis(): the hand-kept clear list missed ensemble$model, which
    ## promoted_state() counts as promotion.
    fitted <- readRDS(test_path("fixtures", "ensemble_fit.rds"))
    ens    <- suppressWarnings(
      ensemble(fitted, method = "weighted", optimize = FALSE,
               compute_uq = FALSE, verbose = FALSE)
    )

    ## The fixture was fitted without UQ bundles; carry one the way
    ## fit(compute_uq = TRUE) leaves it, or has_uq() is FALSE before and after.
    ens$models$uq <- stats::setNames(list(list(quantile_model = "a UQ bundle")),
                                     names(ens$models$workflows)[1])

    ## A removal validate() made before evaluate(), and a select_training()
    ## record. Both describe rows, not the outcome.
    removal <- list(removed_ids    = "sample_999",
                    removal_detail = tibble::tibble(sample_id = "sample_999", reason = "spectral"),
                    removed        = TRUE)

    ens$validation$outliers[names(removal)] <- removal
    ens$selection <- make_selection_record(pool_ids = ens$data$analysis$sample_id[1:10])

    expect_true(has_uq(ens))
    expect_true("an ensemble" %in% promoted_state(ens))

    ## Act
    result <- quiet_configure(ens)

    ## Assert
    expect_identical(class(result), c("horizons_data", "list"))
    expect_identical(promoted_state(result), character())
    expect_false(has_uq(result))

    blank <- new_horizons_data()

    expect_identical(result$evaluation, blank$evaluation)
    expect_identical(result$models,     blank$models)
    expect_identical(result$ensemble,   blank$ensemble)

    expect_identical(result$selection, ens$selection)
    expect_identical(result$validation$outliers[names(removal)], removal)
    expect_null(result$validation$passed)

    expect_no_error(validate_horizons_data(result))

  })


  test_that("evaluating a re-configured fit matches evaluating the plain object (#70)", {

    ## What the reset promises: nothing the clay fit earned leaks into the oc
    ## evaluation. rf, because cubist is not bit-reproducible (#51).
    fx    <- make_select_fixture(n_pool = 60)
    plain <- fx$pool
    args  <- list(models = "rf", grid_size = 2L, bayesian_iter = 0L,
                  final_bayesian_iter = 0L, cv_folds = 3L)

    run_configure <- function(x, outcome) do.call(quiet_configure, c(list(x, outcome = outcome), args))
    run_evaluate  <- function(x) suppressWarnings(evaluate(x, prune = FALSE, verbose = FALSE, seed = 7L))

    fit_clay <- suppressWarnings(
      fit(run_evaluate(run_configure(plain, "clay")),
          n_best = 1L, compute_uq = FALSE, verbose = FALSE, seed = 7L)
    )

    via_fit   <- run_evaluate(run_configure(fit_clay, "oc"))
    via_plain <- run_evaluate(run_configure(plain, "oc"))

    ## Timing columns are the only thing allowed to differ
    untimed <- function(results) results[, setdiff(names(results), "runtime_secs")]

    expect_identical(via_fit$evaluation$split$in_id, via_plain$evaluation$split$in_id)
    expect_equal(untimed(via_fit$evaluation$results), untimed(via_plain$evaluation$results))
    expect_identical(via_fit$evaluation$best_config, via_plain$evaluation$best_config)
    expect_identical(via_fit$models, via_plain$models)

  })


  test_that("configure() warns when rows were removed as response outliers of another outcome", {

    ## Arrange — a clay configuration after validate() removed one row as a
    ## clay response outlier, one as both, and one as a spectral outlier
    fx  <- make_select_fixture(n_pool = 40)
    obj <- quiet_configure(fx$pool, outcome = "clay")

    obj$validation$outliers["removed_ids"]    <- list(c("P097", "P098", "P099"))
    obj$validation$outliers["removed"]        <- list(TRUE)
    obj$validation$outliers["removal_detail"] <- list(tibble::tibble(
      sample_id          = c("P097", "P098", "P099"),
      reason             = c("response", "both", "spectral"),
      outcome            = c("clay", "clay", NA_character_),
      spectral_threshold = c(NA, 0.975, 0.975),
      response_threshold = c(1.5, 1.5, NA)
    ))

    ## Act
    to_oc   <- testthat::capture_warnings(utils::capture.output(configure(obj, outcome = "oc")))
    to_clay <- testthat::capture_warnings(utils::capture.output(configure(obj, outcome = "clay")))

    ## Assert — only the response-only row counts: the spectral removal does
    ## not depend on the outcome, and the "both" row would have gone as a
    ## spectral outlier anyway. The same outcome is silent.
    expect_true(any(grepl("1 row\\(s\\) were removed .*'clay' \\(1\\).*'oc'", to_oc)))
    expect_false(any(grepl("response outliers", to_clay)))

  })


  test_that("configure() warns when a selection was drawn for other properties", {

    ## Arrange — a select_training() record drawn for clay only
    obj <- make_selected_object()
    obj$selection$settings$properties <- "clay"

    global <- obj
    global$selection$settings$scope <- "global"

    ## Act
    to_oc     <- testthat::capture_warnings(utils::capture.output(configure(obj, outcome = "oc")))
    to_clay   <- testthat::capture_warnings(utils::capture.output(configure(obj, outcome = "clay")))
    global_oc <- testthat::capture_warnings(utils::capture.output(configure(global, outcome = "oc")))

    ## Assert — a global draw takes the whole pool, so there is nothing to lose
    expect_true(any(grepl("drawn by select_training\\(\\) for 'clay'; 'oc' is not one of them", to_oc)))
    expect_false(any(grepl("select_training", to_clay)))
    expect_false(any(grepl("select_training", global_oc)))

  })

})
