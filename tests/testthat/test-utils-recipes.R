## ---------------------------------------------------------------------------
## Tests: build_recipe() and parse_config_covariates()
## ---------------------------------------------------------------------------

## Helper: create minimal test data + role_map for recipe testing
make_test_data <- function(n = 20, n_wn = 50, covariates = NULL) {

  ## Fake spectral data: n samples x n_wn wavelengths
  wn_names <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
  spec_mat <- matrix(rnorm(n * n_wn), nrow = n)
  colnames(spec_mat) <- wn_names

  df <- tibble::as_tibble(spec_mat)
  df$sample_id <- paste0("S", sprintf("%03d", seq_len(n)))
  df$SOC       <- runif(n, 0.5, 10)

  ## Build role_map
  roles <- tibble::tibble(
    variable = c("sample_id", wn_names, "SOC"),
    role     = c("id", rep("predictor", n_wn), "outcome")
  )

  ## Optionally add covariates
  if (!is.null(covariates)) {

    for (cov in covariates) {
      df[[cov]] <- runif(n, 0, 100)
      roles <- rbind(roles, tibble::tibble(variable = cov, role = "covariate"))
    }

  }

  list(data = df, role_map = roles)

}

## Helper: create a config row
make_config_row <- function(model             = "rf",
                            transformation    = "none",
                            preprocessing     = "raw",
                            feature_selection = "none",
                            covariates        = NA_character_) {

  tibble::tibble(
    model             = model,
    transformation    = transformation,
    preprocessing     = preprocessing,
    feature_selection = feature_selection,
    covariates        = covariates
  )

}

## =========================================================================
## parse_config_covariates()
## =========================================================================

describe("parse_config_covariates()", {

  it("parses comma-separated covariate string", {

    result <- parse_config_covariates("pH,clay,sand")
    expect_equal(result, c("pH", "clay", "sand"))

  })

  it("trims whitespace", {

    result <- parse_config_covariates(" pH , clay ")
    expect_equal(result, c("pH", "clay"))

  })

  it("returns NULL for NA", {

    expect_null(parse_config_covariates(NA))

  })

  it("returns NULL for empty string", {

    expect_null(parse_config_covariates(""))

  })

  it("returns NULL for NULL", {

    expect_null(parse_config_covariates(NULL))

  })

  it("handles single covariate", {

    result <- parse_config_covariates("pH")
    expect_equal(result, "pH")

  })

})

## =========================================================================
## build_recipe()
## =========================================================================

describe("build_recipe()", {

  it("returns a recipe object", {

    td <- make_test_data()
    config <- make_config_row()

    rec <- build_recipe(config, td$data, td$role_map)

    expect_s3_class(rec, "recipe")

  })

  it("sets outcome role correctly", {

    td <- make_test_data()
    config <- make_config_row()

    rec <- build_recipe(config, td$data, td$role_map)

    ## Check that SOC is the outcome
    var_info <- rec$var_info
    outcome_vars <- var_info$variable[var_info$role == "outcome"]
    expect_equal(outcome_vars, "SOC")

  })

  it("sets id role correctly", {

    td <- make_test_data()
    config <- make_config_row()

    rec <- build_recipe(config, td$data, td$role_map)

    var_info <- rec$var_info
    id_vars <- var_info$variable[var_info$role == "id"]
    expect_equal(id_vars, "sample_id")

  })

  ## -----------------------------------------------------------------------
  ## Response transformation
  ## -----------------------------------------------------------------------

  it("adds step_log for log transformation", {

    td <- make_test_data()
    config <- make_config_row(transformation = "log")

    rec <- build_recipe(config, td$data, td$role_map)

    step_classes <- vapply(rec$steps, function(s) class(s)[1], character(1))
    expect_true("step_log" %in% step_classes)

  })

  it("adds step_sqrt for sqrt transformation", {

    td <- make_test_data()
    config <- make_config_row(transformation = "sqrt")

    rec <- build_recipe(config, td$data, td$role_map)

    step_classes <- vapply(rec$steps, function(s) class(s)[1], character(1))
    expect_true("step_sqrt" %in% step_classes)

  })

  it("adds no transformation step for 'none'", {

    td <- make_test_data()
    config <- make_config_row(transformation = "none")

    rec <- build_recipe(config, td$data, td$role_map)

    step_classes <- vapply(rec$steps, function(s) class(s)[1], character(1))
    expect_false("step_log" %in% step_classes)
    expect_false("step_sqrt" %in% step_classes)

  })

  ## -----------------------------------------------------------------------
  ## Spectral preprocessing
  ## -----------------------------------------------------------------------

  it("adds step_transform_spectra for all configs", {

    td <- make_test_data()

    for (preproc in c("raw", "snv", "deriv1")) {

      config <- make_config_row(preprocessing = preproc)
      rec <- build_recipe(config, td$data, td$role_map)

      step_classes <- vapply(rec$steps, function(s) class(s)[1], character(1))
      expect_true("step_transform_spectra" %in% step_classes,
                  info = paste("Missing step_transform_spectra for", preproc))

    }

  })

  ## -----------------------------------------------------------------------
  ## Feature selection
  ## -----------------------------------------------------------------------

  it("adds step_pca for pca feature selection", {

    td <- make_test_data()
    config <- make_config_row(feature_selection = "pca")

    rec <- build_recipe(config, td$data, td$role_map)

    step_classes <- vapply(rec$steps, function(s) class(s)[1], character(1))
    expect_true("step_pca" %in% step_classes)

  })

  it("adds no feature selection step for 'none'", {

    td <- make_test_data()
    config <- make_config_row(feature_selection = "none")

    rec <- build_recipe(config, td$data, td$role_map)

    step_classes <- vapply(rec$steps, function(s) class(s)[1], character(1))
    expect_false("step_pca" %in% step_classes)

  })

  it("adds step_select_correlation for correlation feature selection", {

    td <- make_test_data()
    config <- make_config_row(feature_selection = "correlation")

    rec <- build_recipe(config, td$data, td$role_map)

    step_classes <- vapply(rec$steps, function(s) class(s)[1], character(1))
    expect_true("step_select_correlation" %in% step_classes)

  })

  it("adds step_select_boruta for boruta feature selection", {

    td <- make_test_data()
    config <- make_config_row(feature_selection = "boruta")

    rec <- build_recipe(config, td$data, td$role_map)

    step_classes <- vapply(rec$steps, function(s) class(s)[1], character(1))
    expect_true("step_select_boruta" %in% step_classes)

  })

  it("adds step_select_cars for cars feature selection", {

    td <- make_test_data()
    config <- make_config_row(feature_selection = "cars")

    rec <- build_recipe(config, td$data, td$role_map)

    step_classes <- vapply(rec$steps, function(s) class(s)[1], character(1))
    expect_true("step_select_cars" %in% step_classes)

  })

  it("aborts on invalid feature selection method", {

    td <- make_test_data()
    config <- make_config_row(feature_selection = "deep_learning")

    expect_error(
      build_recipe(config, td$data, td$role_map),
      "Unsupported feature selection"
    )

  })

  ## -----------------------------------------------------------------------
  ## Covariate handling
  ## -----------------------------------------------------------------------

  it("includes requested covariates as predictors", {

    td <- make_test_data(covariates = c("pH", "clay", "sand"))
    config <- make_config_row(covariates = "pH,clay")

    rec <- build_recipe(config, td$data, td$role_map)

    ## pH and clay should be promoted to predictor
    var_info <- rec$var_info
    ph_role  <- var_info$role[var_info$variable == "pH"]
    clay_role <- var_info$role[var_info$variable == "clay"]

    expect_equal(ph_role, "predictor")
    expect_equal(clay_role, "predictor")

  })

  it("removes unrequested covariates via step_rm", {

    td <- make_test_data(covariates = c("pH", "clay", "sand"))
    config <- make_config_row(covariates = "pH")

    rec <- build_recipe(config, td$data, td$role_map)

    ## Should have a step_rm that removes clay and sand
    step_classes <- vapply(rec$steps, function(s) class(s)[1], character(1))
    expect_true("step_rm" %in% step_classes)

  })

  it("removes all covariates when config has NA covariates", {

    td <- make_test_data(covariates = c("pH", "clay"))
    config <- make_config_row(covariates = NA_character_)

    rec <- build_recipe(config, td$data, td$role_map)

    ## Should have a step_rm for all covariates
    step_classes <- vapply(rec$steps, function(s) class(s)[1], character(1))
    expect_true("step_rm" %in% step_classes)

  })

  it("works without any covariates in the data", {

    td <- make_test_data(covariates = NULL)
    config <- make_config_row(covariates = NA_character_)

    rec <- build_recipe(config, td$data, td$role_map)

    ## Should work fine without any covariate steps
    expect_s3_class(rec, "recipe")

  })

  it("aborts when config requests covariates not in data", {

    td <- make_test_data(covariates = c("pH", "clay"))
    config <- make_config_row(covariates = "pH,nitrogen")

    expect_error(
      build_recipe(config, td$data, td$role_map),
      "not available in data"
    )

  })

  ## -----------------------------------------------------------------------
  ## Step ordering
  ## -----------------------------------------------------------------------

  it("applies steps in correct order: transform → preprocess → feature_select", {

    td <- make_test_data()
    config <- make_config_row(
      transformation    = "log",
      preprocessing     = "raw",
      feature_selection = "pca"
    )

    rec <- build_recipe(config, td$data, td$role_map)

    step_classes <- vapply(rec$steps, function(s) class(s)[1], character(1))

    log_pos     <- which(step_classes == "step_log")
    spectra_pos <- which(step_classes == "step_transform_spectra")
    pca_pos     <- which(step_classes == "step_pca")

    expect_true(log_pos < spectra_pos)
    expect_true(spectra_pos < pca_pos)

  })

  ## -----------------------------------------------------------------------
  ## Column ordering assertion
  ## -----------------------------------------------------------------------

  it("aborts when predictor columns are not monotonically ordered", {

    td <- make_test_data(n_wn = 10)

    ## Scramble the predictor order in role_map
    pred_rows <- td$role_map$role == "predictor"
    pred_vars <- td$role_map$variable[pred_rows]
    td$role_map$variable[pred_rows] <- sample(pred_vars)

    config <- make_config_row()

    expect_error(
      build_recipe(config, td$data, td$role_map),
      "monotonically ordered"
    )

  })

  ## -----------------------------------------------------------------------
  ## Case insensitivity
  ## -----------------------------------------------------------------------

  it("is case-insensitive for transformation, preprocessing, feature_selection", {

    td <- make_test_data()

    config_upper <- make_config_row(
      transformation    = "LOG",
      preprocessing     = "RAW",
      feature_selection = "PCA"
    )

    rec <- build_recipe(config_upper, td$data, td$role_map)

    step_classes <- vapply(rec$steps, function(s) class(s)[1], character(1))
    expect_true("step_log" %in% step_classes)
    expect_true("step_pca" %in% step_classes)

  })

})
