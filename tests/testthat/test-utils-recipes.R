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

## =========================================================================
## Serialization footprint — regression guard
## =========================================================================
##
## A recipe holds references to its training data: the template, and whatever
## the step selectors captured from build_recipe()'s calling frame. Those cost
## nothing in memory, because R shares the underlying object — but R's
## serializer does NOT deduplicate data frames, so every reference becomes a
## full copy the moment the recipe is sent to a parallel worker or written to
## disk.
##
## On 2026-09-14 this was measured at 5.01x on a 14,228 x 1,701 spectral
## matrix: a 186 MB recipe serialized to 928 MB, which exhausted memory during
## parallel tuning and tripped R's 2 GB long-vector limit. Two magrittr pipes
## and two enquos()-captured frame references. Neither object.size() nor
## lobstr::obj_size() reports it, which is why it survived undetected.
##
## The ratio is scale-invariant, so a small fixture catches it. Test at a size
## where the data dominates fixed recipe overhead.


describe("build_recipe() serialization footprint", {

  it("serializes within a small multiple of its training data", {

    ## ~2.4 MB of predictors: large enough that data dominates overhead.
    td     <- make_test_data(n = 400, n_wn = 750)
    config <- make_config_row(preprocessing = "snv", feature_selection = "pca")

    rec <- build_recipe(config, td$data, td$role_map)

    data_bytes <- serialized_size(td$data)
    rec_bytes  <- serialized_size(rec)
    ratio      <- rec_bytes / data_bytes

    ## One copy is expected and legitimate: the recipe's own template.
    ## Anything approaching two means a reference leaked back in.
    expect_lt(ratio, 1.5)

  })

  it("does not retain the training data in step selector environments", {

    td     <- make_test_data(n = 400, n_wn = 750)
    config <- make_config_row(preprocessing = "snv", feature_selection = "pca")

    rec <- build_recipe(config, td$data, td$role_map)

    data_bytes <- serialized_size(td$data)

    ## Guard against a vacuous pass if the recipe ever has no steps.
    expect_gt(length(rec$steps), 0)

    ## Each step, serialized on its own, should be negligible against the
    ## training data. Before the fix, a single step carried 2x the table.
    for (step in rec$steps) {

      expect_lt(serialized_size(step) / data_bytes, 0.1)

    }

  })

  it("selects the same columns as an unstripped build, across the grid", {

    ## The footprint tests prove the recipe is small and the functional tests
    ## prove it preps — but neither proves it selects the RIGHT columns. PCA
    ## hides a wrong `wn_` set entirely. Comparing baked output against a build
    ## whose selector environments were left heavy is the assertion that would
    ## catch a binding this function failed to harvest.
    strip <- horizons:::strip_selector_envs

    for (fs in c("none", "pca")) {

      for (cov in list(NA_character_, "clay")) {

        td     <- make_test_data(n = 60, n_wn = 40, covariates = c("clay", "ph"))
        config <- make_config_row(preprocessing     = "snv",
                                  feature_selection = fs,
                                  covariates        = cov)

        stripped <- build_recipe(config, td$data, td$role_map)

        ## Rebuild with the strip neutered, by re-pointing every selector at an
        ## environment that still holds the caller's frame.
        unstripped <- local({
          on.exit(assignInNamespace("strip_selector_envs", strip,
                                    ns = "horizons"), add = TRUE)
          assignInNamespace("strip_selector_envs",
                            function(rec, frame) rec, ns = "horizons")
          build_recipe(config, td$data, td$role_map)
        })

        baked_stripped   <- recipes::bake(recipes::prep(stripped),   NULL)
        baked_unstripped <- recipes::bake(recipes::prep(unstripped), NULL)

        expect_equal(baked_stripped, baked_unstripped,
                     info = paste("fs =", fs, "cov =", as.character(cov)))

      }

    }

  })

  it("keeps covariate steps free of captured data", {

    ## step_rm() paths take their column names from build_recipe()'s frame too,
    ## so exercise a config that triggers them.
    td     <- make_test_data(n = 400, n_wn = 750, covariates = c("clay", "ph"))
    config <- make_config_row(feature_selection = "pca", covariates = "clay")

    rec <- build_recipe(config, td$data, td$role_map)

    ratio <- serialized_size(rec) / serialized_size(td$data)
    expect_lt(ratio, 1.5)

  })

  it("still resolves its selectors after the environments are stripped", {

    ## The footprint fix re-points selector quosures at a minimal environment.
    ## The risk it introduces is that a selector can no longer find the names it
    ## references, so prep() must still succeed and the spectral columns must
    ## still have been consumed by the steps.
    td     <- make_test_data(n = 100, n_wn = 60)
    config <- make_config_row(preprocessing = "snv", feature_selection = "pca")

    rec   <- build_recipe(config, td$data, td$role_map)
    baked <- recipes::bake(recipes::prep(rec), new_data = NULL)

    ## PCA ran: raw wavenumber columns are gone, components took their place.
    expect_false(any(grepl("^wn_", names(baked))))
    expect_true(any(grepl("^PC", names(baked))))
    expect_true("SOC" %in% names(baked))
    expect_equal(nrow(baked), nrow(td$data))

  })

  it("resolves covariate selectors after stripping", {

    ## step_rm() on covariates reads its column names from the same stripped
    ## environment, so a config that drops one must still drop exactly that one.
    td     <- make_test_data(n = 100, n_wn = 60, covariates = c("clay", "ph"))
    config <- make_config_row(feature_selection = "none", covariates = "clay")

    rec   <- build_recipe(config, td$data, td$role_map)
    baked <- recipes::bake(recipes::prep(rec), new_data = NULL)

    expect_true("clay" %in% names(baked))
    expect_false("ph" %in% names(baked))

  })

})

## =========================================================================
## step_transform_spectra: failures are visible, not silent
## =========================================================================
##
## bake() substitutes an all-NA row when a spectrum cannot be processed, and the
## fallback is built to the expected length — so the length-consistency check
## below it cannot detect one. Without an explicit warning a malformed sample
## returns NA predictions at predict time, or injects NA rows into the model
## matrix at train time, with no signal either way. See #52.

describe("step_transform_spectra() surfaces unusable spectra", {

  it("warns, naming the rows, when a spectrum cannot be processed", {

    td     <- make_test_data(n = 30, n_wn = 40)
    config <- make_config_row(preprocessing = "snv", feature_selection = "none")

    rec <- build_recipe(config, td$data, td$role_map)
    prepped <- recipes::prep(rec)

    ## A wholly non-finite spectrum is the case the length check is blind to:
    ## the fallback row is built to the expected width, so lengths agree and
    ## nothing downstream can tell it apart from a real result. Note a *single*
    ## NA does not reach this path — prospectr's SNV tolerates it.
    bad <- td$data
    wn  <- grep("^wn_", names(bad), value = TRUE)
    bad[3,  wn] <- NA_real_
    bad[11, wn] <- NA_real_

    expect_warning(recipes::bake(prepped, new_data = bad),
                   "produced no usable output")

    ## The message has to name the rows to be actionable.
    expect_warning(recipes::bake(prepped, new_data = bad), "3, 11")

  })

  it("stays quiet when every spectrum processes", {

    td     <- make_test_data(n = 30, n_wn = 40)
    config <- make_config_row(preprocessing = "snv", feature_selection = "none")

    rec     <- build_recipe(config, td$data, td$role_map)
    prepped <- recipes::prep(rec)

    expect_no_warning(recipes::bake(prepped, new_data = td$data))

  })

})

## =========================================================================
## transform_spectra_matrix: the matrix path equals the row-wise reference
## =========================================================================
##
## bake() used to loop process_spectra_row() over rows. The matrix call must
## give the same numbers for every method, the width prep() promises, and
## keep each row's fate independent of the others (2026-09-16).

describe("transform_spectra_matrix()", {

  methods <- c("raw", "sg", "snv", "deriv1", "deriv2", "snv_deriv1", "snv_deriv2")

  it("matches process_spectra_row() for every method", {

    set.seed(11)
    X <- matrix(rnorm(25 * 41), nrow = 25)

    for (m in methods) {

      ref <- do.call(rbind, lapply(seq_len(nrow(X)), function(i) {
        horizons:::process_spectra_row(X[i, ], preprocessing = m, window_size = 9)
      }))
      got <- horizons:::transform_spectra_matrix(X, preprocessing = m, window_size = 9)

      expect_equal(unname(got), unname(ref), tolerance = 1e-12, label = m)
      expect_equal(ncol(got), ncol(X) - 8, label = m)
      expect_null(dimnames(got))

    }

  })

  it("honours window_size in the output width for every method", {

    X <- matrix(rnorm(5 * 41), nrow = 5)

    for (w in c(5, 7, 11)) {
      for (m in methods) {
        got <- horizons:::transform_spectra_matrix(X, preprocessing = m, window_size = w)
        expect_equal(ncol(got), ncol(X) - (w - 1), label = paste(m, w))
      }
    }

  })

  it("keeps rows independent: an all-NA row is NA, its neighbours are not", {

    set.seed(12)
    X      <- matrix(rnorm(10 * 41), nrow = 10)
    clean  <- horizons:::transform_spectra_matrix(X, "snv_deriv1")
    X[4, ] <- NA_real_
    got    <- horizons:::transform_spectra_matrix(X, "snv_deriv1")

    expect_true(all(is.na(got[4, ])))
    expect_equal(got[-4, ], clean[-4, ], tolerance = 1e-12)

  })

  it("confines a single NA to that row's convolution window", {

    set.seed(13)
    X        <- matrix(rnorm(6 * 41), nrow = 6)
    X[2, 20] <- NA_real_
    got      <- horizons:::transform_spectra_matrix(X, "deriv1", window_size = 9)

    na_per_row <- rowSums(is.na(got))
    expect_equal(na_per_row[-2], rep(0, 5))
    expect_equal(na_per_row[2], 9)              # 2 * half_window + 1

  })

  it("returns the right shape for zero rows", {

    got <- horizons:::transform_spectra_matrix(matrix(numeric(0), 0, 41), "snv")
    expect_equal(dim(got), c(0L, 33L))

  })

  it("rejects an unknown method", {

    expect_error(horizons:::transform_spectra_matrix(matrix(rnorm(41), 1), "nope"),
                 "Unknown preprocessing type")

  })

})
