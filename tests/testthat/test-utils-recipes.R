## ---------------------------------------------------------------------------
## Tests: build_recipe() and parse_config_covariates()
## ---------------------------------------------------------------------------

## Helper: create minimal test data + role_map for recipe testing
##
## `covariates`, `meta` and `responses` add columns carrying the role map's
## non-predictor roles: covariates from add_covariates(), meta from
## select_training()'s provenance or spectra()'s user metadata, and responses
## from add_response() properties that configure() did not promote to outcome.
make_test_data <- function(n          = 20,
                           n_wn       = 50,
                           covariates = NULL,
                           meta       = NULL,
                           responses  = NULL) {

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

  ## Optionally add meta (training-only provenance / user metadata)
  if (!is.null(meta)) {

    for (m in meta) {
      df[[m]] <- paste0("m", seq_len(n))
      roles <- rbind(roles, tibble::tibble(variable = m, role = "meta"))
    }

  }

  ## Optionally add sibling lab responses (role "response", never the outcome)
  if (!is.null(responses)) {

    for (r in responses) {
      df[[r]] <- runif(n, 0.5, 10)
      roles <- rbind(roles, tibble::tibble(variable = r, role = "response"))
    }

  }

  list(data = df, role_map = roles)

}

## Helper: the role a built recipe assigns to a column, via summary()
recipe_role <- function(rec, variable) {

  info <- summary(rec)
  info$role[info$variable == variable]

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

  it("holds unrequested covariates in covariate_hold rather than removing them", {

    td <- make_test_data(covariates = c("pH", "clay", "sand"))
    config <- make_config_row(covariates = "pH")

    rec <- build_recipe(config, td$data, td$role_map)

    ## The role alone keeps them out of the model. A step_rm() would also
    ## require them at bake(), which breaks predict() on new data that never
    ## carries covariates.
    expect_equal(recipe_role(rec, "clay"), "covariate_hold")
    expect_equal(recipe_role(rec, "sand"), "covariate_hold")

    step_classes <- vapply(rec$steps, function(s) class(s)[1], character(1))
    expect_false("step_rm" %in% step_classes)

  })

  it("holds all covariates when config has NA covariates", {

    td <- make_test_data(covariates = c("pH", "clay"))
    config <- make_config_row(covariates = NA_character_)

    rec <- build_recipe(config, td$data, td$role_map)

    expect_equal(recipe_role(rec, "pH"),   "covariate_hold")
    expect_equal(recipe_role(rec, "clay"), "covariate_hold")

    step_classes <- vapply(rec$steps, function(s) class(s)[1], character(1))
    expect_false("step_rm" %in% step_classes)

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
## Non-predictor roles: assignment, bake requirements, and step isolation
## =========================================================================
##
## Three failure modes live together here, because they share one cause: a
## column that the role map names but `build_recipe()` does not re-role falls
## through `outcome ~ .` to predictor.
##
## (1) A sibling lab response (clay measured on the same samples as the SOC
##     being modelled) becomes a predictor — target leakage the pool-internal
##     CV cannot see, and a column that can never exist for a real target.
## (2) A non-predictor role left required at bake aborts `predict()` on any
##     batch that does not carry it, which is every batch: meta, sibling
##     responses and covariates are training-table columns.
## (3) `update_role()` is not sequenced with steps, so a covariate promoted to
##     predictor after a selection step was added is nonetheless inside that
##     step's `all_predictors()` at prep time.

describe("build_recipe() non-predictor roles", {

  it("assigns an explicit role to every non-spectral column", {

    td <- make_test_data(n          = 30,
                         n_wn       = 40,
                         covariates = c("clay", "ph"),
                         meta       = c(".drawn_by", ".group"),
                         responses  = c("oc"))

    config <- make_config_row(preprocessing     = "snv",
                              feature_selection = "pca",
                              covariates        = "clay")

    rec <- build_recipe(config, td$data, td$role_map)

    expect_equal(recipe_role(rec, "SOC"),       "outcome")
    expect_equal(recipe_role(rec, "sample_id"), "id")
    expect_equal(recipe_role(rec, ".drawn_by"), "meta")
    expect_equal(recipe_role(rec, ".group"),    "meta")
    expect_equal(recipe_role(rec, "oc"),        "response_hold")
    expect_equal(recipe_role(rec, "clay"),      "predictor")
    expect_equal(recipe_role(rec, "ph"),        "covariate_hold")

    ## The sibling response is the one that used to slip through silently:
    ## nothing outside the wavenumbers and the requested covariate may be a
    ## predictor.
    info      <- summary(rec)
    preds     <- info$variable[info$role == "predictor"]
    unexpected <- setdiff(preds, c(grep("^wn_", preds, value = TRUE), "clay"))

    expect_equal(unexpected, character(0))

  })

  it("does not require meta, sibling responses or covariates at bake", {

    td <- make_test_data(n          = 30,
                         n_wn       = 40,
                         covariates = c("clay", "ph"),
                         meta       = c(".drawn_by"),
                         responses  = c("oc"))

    config <- make_config_row(preprocessing     = "snv",
                              feature_selection = "pca")

    rec     <- build_recipe(config, td$data, td$role_map)
    prepped <- recipes::prep(rec)

    ## What predict() actually sees: identifier plus spectra, nothing else.
    wn_cols  <- grep("^wn_", names(td$data), value = TRUE)
    new_data <- td$data[, c("sample_id", wn_cols)]

    baked <- recipes::bake(prepped, new_data = new_data)

    expect_equal(nrow(baked), nrow(new_data))
    expect_true(any(grepl("^PC", names(baked))))
    expect_false("oc" %in% names(baked))

  })

  it("predicts through a workflow on new data lacking the held columns", {

    skip_if_not_installed("ranger")

    td <- make_test_data(n          = 40,
                         n_wn       = 40,
                         covariates = c("clay", "ph"),
                         meta       = c(".drawn_by"),
                         responses  = c("oc"))

    config <- make_config_row(preprocessing     = "snv",
                              feature_selection = "pca")

    rec <- build_recipe(config, td$data, td$role_map)

    ## The workflow path is the one that matters: hardhat::forge() shrinks new
    ## data to the blueprint's ptypes before recipes::bake() is reached, so a
    ## role still required at bake aborts before any step runs.
    wf <- workflows::workflow() |>
      workflows::add_recipe(rec) |>
      workflows::add_model(
        parsnip::set_engine(
          parsnip::set_mode(parsnip::rand_forest(trees = 20), "regression"),
          "ranger"
        )
      )

    fitted <- parsnip::fit(wf, td$data)

    wn_cols  <- grep("^wn_", names(td$data), value = TRUE)
    new_data <- td$data[, c("sample_id", wn_cols)]

    preds <- predict(fitted, new_data = new_data)

    expect_equal(nrow(preds), nrow(new_data))
    expect_false(anyNA(preds$.pred))

  })

  it("keeps a promoted covariate out of the spectral selection steps", {

    ## update_role() rewrites var_info for the whole recipe, so promoting
    ## `clay` in Step 5 would put it inside all_predictors() for the PCA step
    ## added in Step 4. Selecting spectra by name is what makes the bypass real.
    td <- make_test_data(n = 40, n_wn = 40, covariates = c("clay", "ph"))

    config <- make_config_row(preprocessing     = "snv",
                              feature_selection = "pca",
                              covariates        = "clay")

    rec     <- build_recipe(config, td$data, td$role_map)
    prepped <- recipes::prep(rec)

    step_classes <- vapply(rec$steps, function(s) class(s)[1], character(1))
    pca_number   <- which(step_classes == "step_pca")

    pca_terms <- unique(recipes::tidy(prepped, number = pca_number)$terms)

    expect_true(all(grepl("^spec[0-9]+$", pca_terms)))
    expect_false("clay" %in% pca_terms)
    expect_false("ph" %in% pca_terms)

    ## The promoted covariate survives PCA untransformed, and the outcome is
    ## still the outcome.
    baked <- recipes::bake(prepped, new_data = NULL)
    expect_true("clay" %in% names(baked))
    expect_equal(recipe_role(rec, "SOC"), "outcome")

  })

  it("keeps a promoted covariate out of correlation selection", {

    ## step_select_correlation()'s 3-wide rolling window assumes its input is
    ## contiguous spectra; a covariate inside the window is not a contiguity
    ## violation the step can detect.
    td <- make_test_data(n = 40, n_wn = 40, covariates = c("clay"))

    config <- make_config_row(preprocessing     = "snv",
                              feature_selection = "correlation",
                              covariates        = "clay")

    rec     <- build_recipe(config, td$data, td$role_map)
    prepped <- recipes::prep(rec)

    step_classes <- vapply(rec$steps, function(s) class(s)[1], character(1))
    sel_step     <- prepped$steps[[which(step_classes == "step_select_correlation")]]

    expect_true(all(grepl("^spec[0-9]+$", sel_step$selected_vars)))
    expect_false("clay" %in% sel_step$selected_vars)

  })

  it("does not let a sibling response reach the model matrix", {

    td <- make_test_data(n = 30, n_wn = 40, responses = c("oc"))

    for (fs in c("none", "pca")) {

      config <- make_config_row(preprocessing     = "snv",
                                feature_selection = fs)

      rec  <- build_recipe(config, td$data, td$role_map)
      info <- summary(rec)

      expect_false("oc" %in% info$variable[info$role == "predictor"],
                   info = paste("feature_selection =", fs))

      ## And the blueprint agrees: `oc` is not a modelled predictor.
      molded <- hardhat::mold(rec, td$data)
      expect_false("oc" %in% names(molded$predictors))

    }

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

    ## The covariate promotion reads its column names from the same stripped
    ## environment, so a config that uses one must still promote exactly that
    ## one and leave the other held.
    td     <- make_test_data(n = 100, n_wn = 60, covariates = c("clay", "ph"))
    config <- make_config_row(feature_selection = "none", covariates = "clay")

    rec   <- build_recipe(config, td$data, td$role_map)
    baked <- recipes::bake(recipes::prep(rec), new_data = NULL)

    expect_equal(recipe_role(rec, "clay"), "predictor")
    expect_equal(recipe_role(rec, "ph"),   "covariate_hold")

    ## A held column rides through bake() on training data — it is excluded by
    ## role, not by removal.
    expect_true("clay" %in% names(baked))
    expect_true("ph" %in% names(baked))

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


## =========================================================================
## process_spectra_row: each method equals its direct prospectr call
## =========================================================================
##
## The row function is the reference transform_spectra_matrix() is tested
## against, so it needs a reference of its own. Each method is written out
## here as the prospectr call it is meant to be, derivative order and
## polynomial degree spelled out, so a changed parameter fails here instead of
## moving both paths together. The length invariant is the arithmetic prep()
## uses to name the step's output. See #52.

describe("process_spectra_row() against prospectr", {

  sg <- function(x, m, p, w) {
    as.vector(prospectr::savitzkyGolay(matrix(x, nrow = 1), m = m, p = p, w = w))
  }

  snv <- function(x) {
    as.vector(prospectr::standardNormalVariate(matrix(x, nrow = 1)))
  }

  trim <- function(x, w) {
    h <- (w - 1) / 2
    x[(1 + h):(length(x) - h)]
  }

  reference <- list(
    raw        = function(x, w) trim(x, w),
    sg         = function(x, w) sg(x, m = 0, p = 1, w = w),
    snv        = function(x, w) trim(snv(x), w),
    deriv1     = function(x, w) sg(x, m = 1, p = 1, w = w),
    deriv2     = function(x, w) sg(x, m = 2, p = 3, w = w),
    snv_deriv1 = function(x, w) sg(snv(x), m = 1, p = 1, w = w),
    snv_deriv2 = function(x, w) sg(snv(x), m = 2, p = 3, w = w)
  )

  it("matches the direct prospectr call for each of the seven methods", {

    set.seed(52)
    x <- cumsum(rnorm(61))

    for (m in names(reference)) {

      expect_equal(horizons:::process_spectra_row(x, preprocessing = m, window_size = 9),
                   reference[[m]](x, 9),
                   tolerance = 1e-12, label = m)

    }

  })

  it("returns length(in) - (w - 1) for every method across odd windows", {

    ## Windows start at 5: the second-derivative methods fit a cubic, which
    ## needs a window wider than the polynomial degree.
    set.seed(53)
    x <- cumsum(rnorm(41))

    for (w in c(5, 7, 9, 11, 15, 21)) {

      for (m in names(reference)) {

        out <- horizons:::process_spectra_row(x, preprocessing = m, window_size = w)

        expect_equal(length(out), length(x) - (w - 1), label = paste(m, w))
        expect_equal(out, reference[[m]](x, w), tolerance = 1e-12,
                     label = paste(m, w))

      }

    }

  })

  it("agrees with the width prep() promises for the step's output", {

    td  <- make_test_data(n = 10, n_wn = 41)
    dat <- dplyr::select(td$data, -sample_id)

    for (w in c(5, 7, 9, 11, 15, 21)) {

      prepped <- recipes::recipe(SOC ~ ., data = dat) |>
        step_transform_spectra(dplyr::starts_with("wn_"),
                               preprocessing = "deriv2",
                               window_size   = w) |>
        recipes::prep()

      expect_equal(length(prepped$steps[[1]]$trained_columns), 41 - (w - 1),
                   label = paste("window", w))

    }

  })

})


## =========================================================================
## Selection steps: a selector that matches nothing is a configuration error
## =========================================================================
##
## build_recipe() selects the selection steps' inputs by the name pattern
## step_transform_spectra() gives its output. A zero match means that naming
## changed, and a selection step with nothing to select cannot train: the
## custom steps either error deep inside a modelling package or fall through
## their "retain everything" branch and become a silent no-op. Every branch
## now aborts at prep() instead.

describe("selection steps abort on a zero-column selector", {

  no_match <- "^nomatch[0-9]+$"

  base_recipe <- function() {

    td <- make_test_data(n = 20, n_wn = 20)
    recipes::recipe(SOC ~ ., data = dplyr::select(td$data, -sample_id))

  }

  it("step_select_correlation aborts naming itself", {

    rec <- base_recipe() |>
      step_select_correlation(dplyr::matches(no_match), outcome = "SOC")

    expect_error(recipes::prep(rec), "step_select_correlation")
    expect_error(recipes::prep(rec), "selected zero columns")

  })

  it("step_select_cars aborts naming itself", {

    rec <- base_recipe() |>
      step_select_cars(dplyr::matches(no_match), outcome = "SOC")

    expect_error(recipes::prep(rec), "selected zero columns")

  })

  it("step_select_boruta aborts naming itself", {

    rec <- base_recipe() |>
      step_select_boruta(dplyr::matches(no_match), outcome = "SOC")

    expect_error(recipes::prep(rec), "selected zero columns")

  })

  it("the pca branch aborts too, in the selector recipes::step_pca lacks", {

    ## step_pca() treats an empty selection as a pass-through no-op, so the
    ## gate lives in select_generated_spectra() rather than in a prep() this
    ## package owns. Without it a config asking for PCA would train on the
    ## untransformed spectral columns with nothing said.
    rec <- base_recipe() |>
      recipes::step_pca(select_generated_spectra(), threshold = 0.995)

    expect_error(recipes::prep(rec), "nothing to select")

  })

  it("the pca branch still preps when the pattern matches", {

    df <- as.data.frame(matrix(rnorm(200), nrow = 20))
    names(df) <- paste0("spec", 1:10)
    df$SOC    <- runif(20, 0.5, 10)

    rec <- recipes::recipe(SOC ~ ., data = df) |>
      recipes::step_pca(select_generated_spectra(), threshold = 0.995)

    prepped <- recipes::prep(rec)
    baked   <- recipes::bake(prepped, new_data = df)

    expect_true(any(grepl("^PC", names(baked))))

  })

})


## =========================================================================
## step_transform_spectra(): generated names must not collide, window must fit
## =========================================================================
##
## bake() binds the pass-through block to the transformed matrix. Under default
## name repair a pass-through column named like a generated one (a covariate
## called spec01) is resolved positionally: at training a band is silently
## displaced from everything downstream, and at predict time the bind dies
## inside vctrs without naming the column at fault.

describe("step_transform_spectra() name collisions and window size", {

  it("aborts, naming the covariate, when it collides with a generated name", {

    ## n_wn = 50 with the default window leaves 42 columns, which names0()
    ## writes as spec01 ... spec42 — so a covariate called spec01 collides.
    td     <- make_test_data(n = 20, n_wn = 50, covariates = "spec01")
    config <- make_config_row(covariates = "spec01")

    rec <- build_recipe(config, td$data, td$role_map)

    expect_error(recipes::prep(rec), "spec01")
    expect_error(recipes::prep(rec), class = "horizons_input_error")

  })

  it("is quiet when no pass-through column uses the generated pattern", {

    td     <- make_test_data(n = 20, n_wn = 50, covariates = "Clay")
    config <- make_config_row(covariates = "Clay")

    rec <- build_recipe(config, td$data, td$role_map)

    expect_no_error(recipes::prep(rec))

  })

  it("aborts, naming both numbers, when the window is wider than the spectrum", {

    td <- make_test_data(n = 20, n_wn = 10)

    rec <- recipes::recipe(SOC ~ ., data = dplyr::select(td$data, -sample_id)) |>
      step_transform_spectra(dplyr::starts_with("wn_"),
                             preprocessing = "raw",
                             window_size   = 21)

    expect_error(recipes::prep(rec), "10 spectral columns")
    expect_error(recipes::prep(rec), "window_size = 21")

  })

  it("aborts at prep when the window is exactly as wide as the spectrum", {

    ## prospectr::savitzkyGolay() needs w < ncol. A window equal to the column
    ## count used to pass prep (it leaves one column after trimming) and then
    ## fail at bake, inside tune, as a "Grid search failed" naming nothing.
    td <- make_test_data(n = 20, n_wn = 9)

    for (pp in c("raw", "deriv1", "deriv2")) {

      rec <- recipes::recipe(SOC ~ ., data = dplyr::select(td$data, -sample_id)) |>
        step_transform_spectra(dplyr::starts_with("wn_"), preprocessing = pp,
                               window_size = 9)

      expect_error(recipes::prep(rec), class = "horizons_input_error", label = pp)
      expect_error(recipes::prep(rec), "9 spectral columns", label = pp)

    }

    ## One column wider, and every method runs.
    td <- make_test_data(n = 20, n_wn = 11)

    rec <- recipes::recipe(SOC ~ ., data = dplyr::select(td$data, -sample_id)) |>
      step_transform_spectra(dplyr::starts_with("wn_"), preprocessing = "deriv2",
                             window_size = 9)

    expect_equal(ncol(recipes::bake(recipes::prep(rec), new_data = NULL)) - 1L, 3L)

  })

  it("refuses at construction a window that is even, under 5, fractional or not a scalar", {

    td   <- make_test_data(n = 20, n_wn = 30)
    base <- recipes::recipe(SOC ~ ., data = dplyr::select(td$data, -sample_id))

    for (bad in list(8, 10L, 3, 1L, 7.5, NA_real_, Inf, "9", c(9, 11))) {

      expect_error(
        step_transform_spectra(base, dplyr::starts_with("wn_"),
                               preprocessing = "raw", window_size = bad),
        "odd whole number of at least 5",
        class = "horizons_input_error",
        info  = paste("window_size =", deparse(bad))
      )

    }

    ## build_recipe() builds the step, so a direct caller is held to the same
    ## rule configure() applies.
    expect_error(build_recipe(make_config_row(), td$data, td$role_map, sg_window = 8L),
                 class = "horizons_input_error")

    expect_no_error(step_transform_spectra(base, dplyr::starts_with("wn_"),
                                           preprocessing = "deriv2", window_size = 5))

  })

})


## =========================================================================
## Round trip: a prepped recipe bakes the same predictor names on new data
## =========================================================================
##
## The predict path bakes new data through the recipe the model was fit with,
## so any divergence between the training-time and predict-time predictor
## names is a silent feature-space mismatch. Asserted across the selection
## methods and the preprocessing methods, because the failure modes differ:
## the transform step renames, and each selection step subsets.

describe("prepped recipes bake identical predictor names on train and new data", {

  selection_methods <- c("none", "pca", "correlation", "boruta", "cars")

  for (fs in selection_methods) {

    for (pp in c("raw", "snv", "deriv2")) {

      it(paste0("feature_selection = '", fs, "', preprocessing = '", pp, "'"), {

        if (fs == "boruta") skip_if_not_installed("Boruta")
        if (fs == "cars")   skip_if_not_installed("pls")

        set.seed(2026)
        train_td <- make_test_data(n = 40, n_wn = 60)
        new_td   <- make_test_data(n = 12, n_wn = 60)

        config  <- make_config_row(preprocessing     = pp,
                                   feature_selection = fs)

        rec     <- build_recipe(config, train_td$data, train_td$role_map)
        prepped <- suppressWarnings(
          recipes::prep(rec, training = train_td$data)
        )

        train_names <- names(recipes::bake(prepped,
                                           new_data = train_td$data,
                                           recipes::all_predictors()))

        new_names   <- names(recipes::bake(prepped,
                                           new_data = new_td$data,
                                           recipes::all_predictors()))

        expect_true(length(train_names) > 0)
        expect_identical(train_names, new_names)

      })

    }

  }

})


## =========================================================================
## Custom steps: selectors survive prep, bake reads the resolved names
## =========================================================================
##
## recipes' step contract keeps the selector quosures in `terms` for the life
## of the step and records the names they resolved to in `columns`, which is
## what bake() reads. The four custom steps used to overwrite their only copy
## of the selector with the resolved names at prep(), so a trained recipe could
## not be prepped a second time: `prep(fresh = TRUE)` handed the names back to
## recipes_eval_select() as if they were selectors, and it aborted. See #52.
##
## The other half of the contract is what keeps a stored model working.
## fit() keeps butchered workflows, and butcher re-points every step's `terms`
## at the base environment, so a butchered recipe cannot resolve its
## selectors again. That is expected of a butchered object, and harmless as
## long as bake() never needs them.
##
## Steps built before the fix are still in the wild, stored fits above all.
## prep() reads whichever layout a step has through step_selectors(), and
## bake() and print() must never come to need `terms`, or every stored
## pre-#52 fit stops predicting.

describe("custom steps keep their selectors through prep (#52)", {

  steps <- c("transform", "correlation", "boruta", "cars")

  step_fn <- function(step) {

    if (step == "transform") "step_transform_spectra" else paste0("step_select_", step)

  }

  ## A step as the release before #52 left it. Untrained, its selectors sat
  ## in `columns`; trained, `columns` held the resolved names and the
  ## selectors were gone. Neither had a `terms` slot, until butcher() added
  ## an empty one (`terms = list()`), so both shapes are covered.
  as_pre52 <- function(step_obj, butchered = FALSE) {

    s <- unclass(step_obj)
    if (!isTRUE(s$trained)) s$columns <- s$terms
    s$terms <- NULL
    if (butchered) s["terms"] <- list(list())
    structure(s, class = class(step_obj))

  }

  ## Swap the prepped recipe a fitted workflow predicts with.
  with_prepped_recipe <- function(wf, rec) {

    wf$pre$mold$blueprint$recipe <- rec
    wf

  }

  ## The step under test is always the recipe's last step. The selection
  ## steps sit on the transform step's output, as build_recipe() puts them.
  ## The transform step here selects through dplyr::all_of() on a local
  ## vector, a selector that needs its environment, so a test can see whether
  ## that environment was cut loose. build_recipe() itself now injects the
  ## names as a literal vector, which needs none; the re-prep of its own
  ## recipe is tested after this loop.
  step_recipe <- function(step, data) {

    wn_cols <- grep("^wn_", names(data), value = TRUE)

    rec <- recipes::recipe(SOC ~ ., data = data) |>
      step_transform_spectra(dplyr::all_of(wn_cols), preprocessing = "snv")

    switch(step,
      transform   = rec,
      correlation = step_select_correlation(rec, dplyr::matches("^spec[0-9]+$"),
                                            outcome = "SOC"),
      boruta      = step_select_boruta(rec, dplyr::matches("^spec[0-9]+$"),
                                       outcome = "SOC"),
      cars        = step_select_cars(rec, dplyr::matches("^spec[0-9]+$"),
                                     outcome = "SOC")
    )

  }

  step_data <- function(n = 40) {

    dplyr::select(make_test_data(n = n, n_wn = 60)$data, -sample_id)

  }

  skip_if_step_unavailable <- function(step) {

    if (step == "boruta") skip_if_not_installed("Boruta")
    if (step == "cars")   skip_if_not_installed("pls")

  }

  ## Boruta finds nothing on these noise spectra and says so with a
  ## horizons_boruta_warning (#75). That is the step working, and not what
  ## these lifecycle tests check, so that one class is muffled around each
  ## prep and fit; any other warning still surfaces.
  quiet_boruta <- function(expr) {

    suppressWarnings(expr, classes = "horizons_boruta_warning")

  }

  for (step in steps) {

    it(paste0(step, ": a trained recipe re-preps with fresh = TRUE"), {

      skip_if_step_unavailable(step)

      set.seed(52)
      d1 <- step_data()
      d2 <- step_data()

      rec <- step_recipe(step, d1)

      set.seed(1)
      trained <- quiet_boruta(recipes::prep(rec, training = d1))

      ## Re-prepped on other rows, the recipe has to land exactly where a
      ## first prep on those rows does: selectors re-resolved, state
      ## re-estimated, nothing carried over from d1.
      set.seed(2)
      refreshed <- quiet_boruta(recipes::prep(trained, training = d2, fresh = TRUE))

      set.seed(2)
      direct <- quiet_boruta(recipes::prep(rec, training = d2))

      expect_identical(recipes::bake(refreshed, new_data = NULL),
                       recipes::bake(direct,    new_data = NULL))

    })

    it(paste0(step, ": terms survive prep and columns hold the resolved names"), {

      skip_if_step_unavailable(step)

      set.seed(52)
      d    <- step_data()
      rec  <- step_recipe(step, d)
      last <- length(rec$steps)

      untrained <- rec$steps[[last]]

      expect_true(rlang::is_quosures(untrained$terms))
      expect_null(untrained$columns)

      set.seed(1)
      trained <- quiet_boruta(recipes::prep(rec, training = d))$steps[[last]]

      expect_identical(trained$terms, untrained$terms)
      expect_type(trained$columns, "character")
      expect_named(trained$columns, unname(trained$columns))

      pattern <- if (step == "transform") "^wn_" else "^spec[0-9]+$"
      expect_gt(length(trained$columns), 0)
      expect_true(all(grepl(pattern, trained$columns)))

    })

    it(paste0(step, ": prints untrained and trained"), {

      skip_if_step_unavailable(step)

      set.seed(52)
      d    <- step_data()
      rec  <- step_recipe(step, d)
      last <- length(rec$steps)

      set.seed(1)
      prepped <- quiet_boruta(recipes::prep(rec, training = d))

      untrained_says <- if (step == "transform") "Spectral transformation" else "not yet trained"
      trained_says   <- if (step == "transform") "Spectral transformation" else "retained"

      expect_output(print(rec$steps[[last]]),     untrained_says)
      expect_output(print(prepped$steps[[last]]), trained_says)

      ## And inside the recipe's own print method, which calls the step's.
      expect_no_error(suppressMessages(utils::capture.output(print(rec))))
      expect_no_error(suppressMessages(utils::capture.output(print(prepped))))

    })

    it(paste0(step, ": a butchered workflow still predicts"), {

      skip_if_step_unavailable(step)

      set.seed(52)
      train <- step_data(n = 80)
      new   <- step_data(n = 10)

      wf <- workflows::workflow(step_recipe(step, train), parsnip::linear_reg())

      set.seed(1)
      fitted   <- quiet_boruta(parsnip::fit(wf, data = train))
      expected <- predict(fitted, new_data = new)

      butchered <- butcher::butcher(fitted)

      ## The premise: butcher really did cut the selectors loose. The
      ## transform step's selector names a local vector that its environment
      ## held before butchering and does not hold after.
      q_before <- workflows::extract_recipe(fitted)$steps[[1]]$terms[[1]]
      q_after  <- workflows::extract_recipe(butchered)$steps[[1]]$terms[[1]]

      expect_true(exists("wn_cols", envir = rlang::quo_get_env(q_before),
                         inherits = FALSE))
      expect_false(exists("wn_cols", envir = rlang::quo_get_env(q_after),
                          inherits = FALSE))

      expect_equal(predict(butchered, new_data = new), expected)

    })

    it(paste0(step, ": a fit stored before #52 predicts, bakes and prints unchanged"), {

      skip_if_step_unavailable(step)

      set.seed(52)
      train <- step_data(n = 80)
      new   <- step_data(n = 10)

      wf <- workflows::workflow(step_recipe(step, train), parsnip::linear_reg())

      set.seed(1)
      fitted    <- quiet_boruta(parsnip::fit(wf, data = train))
      expected  <- predict(fitted, new_data = new)
      baked     <- recipes::bake(workflows::extract_recipe(fitted), new_data = new)
      butchered <- butcher::butcher(fitted)

      ## The guard: a future bake() or print() that reaches for `terms` fails
      ## here, for every stored pre-#52 fit, butchered or not.
      for (was_butchered in c(FALSE, TRUE)) {

        source_wf <- if (was_butchered) butchered else fitted

        old_rec       <- workflows::extract_recipe(source_wf)
        old_rec$steps <- lapply(old_rec$steps, as_pre52, butchered = was_butchered)
        old_wf        <- with_prepped_recipe(source_wf, old_rec)

        label <- if (was_butchered) "butchered" else "unbutchered"
        last  <- length(old_rec$steps)

        ## The layout really is the old one before anything is asserted on it.
        expect_identical(workflows::extract_recipe(old_wf)$steps[[last]]$terms,
                         if (was_butchered) list() else NULL, label = label)

        expect_equal(predict(old_wf, new_data = new), expected, label = label)
        expect_equal(recipes::bake(old_rec, new_data = new), baked, label = label)
        expect_output(print(old_rec$steps[[last]]))

      }

    })

    it(paste0(step, ": an untrained step from before #52 preps into the current layout"), {

      skip_if_step_unavailable(step)

      set.seed(52)
      d    <- step_data()
      rec  <- step_recipe(step, d)
      last <- length(rec$steps)

      set.seed(1)
      reference <- recipes::bake(quiet_boruta(recipes::prep(rec, training = d)), new_data = NULL)

      ## Selectors in `columns` and no `terms`, as the old release built it,
      ## and the same with the empty `terms` a butchered workflow's
      ## preprocessor carries.
      for (was_butchered in c(FALSE, TRUE)) {

        old       <- rec
        old$steps <- lapply(old$steps, as_pre52, butchered = was_butchered)

        expect_true(rlang::is_quosures(old$steps[[last]]$columns))

        set.seed(1)
        prepped <- quiet_boruta(recipes::prep(old, training = d))

        expect_identical(recipes::bake(prepped, new_data = NULL), reference)

        ## It comes out in the current layout, so it can be re-prepped.
        expect_identical(prepped$steps[[last]]$terms, rec$steps[[last]]$terms)
        expect_no_error(quiet_boruta(recipes::prep(prepped, training = d, fresh = TRUE)))

      }

    })

    it(paste0(step, ": a trained step from before #52 refuses to re-prep, naming why"), {

      skip_if_step_unavailable(step)

      set.seed(52)
      d <- step_data()

      set.seed(1)
      trained <- quiet_boruta(recipes::prep(step_recipe(step, d), training = d))
      last    <- length(trained$steps)

      ## Only the step under test is old, so the error is its own and not
      ## the transform step's ahead of it.
      for (was_butchered in c(FALSE, TRUE)) {

        old <- trained
        old$steps[[last]] <- as_pre52(old$steps[[last]], butchered = was_butchered)

        expect_error(recipes::prep(old, training = d, fresh = TRUE),
                     class = "horizons_input_error")
        expect_error(recipes::prep(old, training = d, fresh = TRUE),
                     "earlier version of horizons")
        expect_error(recipes::prep(old, training = d, fresh = TRUE),
                     step_fn(step), fixed = TRUE)

      }

    })

  }

  it("a recipe build_recipe() made, literal selector and all, re-preps with fresh = TRUE", {

    ## build_recipe() gives the transform step its columns as a literal name
    ## vector. That quosure has to survive prep, as the #52 contract requires
    ## of `terms`, and resolve again on a fresh re-prep to the same columns.
    set.seed(52)
    td <- make_test_data(n = 40, n_wn = 60, covariates = "clay")

    for (fs in c("none", "pca", "correlation")) {

      rec <- build_recipe(make_config_row(preprocessing = "snv_deriv1",
                                          feature_selection = fs,
                                          covariates = "clay"),
                          td$data, td$role_map)

      prepped <- recipes::prep(rec, training = td$data)
      again   <- recipes::prep(prepped, training = td$data, fresh = TRUE)

      expect_identical(prepped$steps[[1]]$terms, rec$steps[[1]]$terms, label = fs)
      expect_identical(again$steps[[1]]$columns, prepped$steps[[1]]$columns, label = fs)
      expect_identical(recipes::bake(again, new_data = NULL),
                       recipes::bake(prepped, new_data = NULL), label = fs)

    }

  })

  it("step_selectors() reads each layout, and refuses the unrecoverable one", {

    sel <- horizons:::step_selectors
    q   <- rlang::quos(dplyr::starts_with("wn_"))

    ## 1. `terms` holds the selectors: as built, after butcher() strips the
    ##    quosures class, and empty (a step called with no selectors).
    expect_identical(sel(list(terms = q, columns = NULL), "s"), q)
    expect_identical(sel(list(terms = unclass(q), columns = c(wn_1 = "wn_1")), "s"),
                     unclass(q))
    expect_identical(sel(list(terms = rlang::quos(), columns = NULL), "s"),
                     rlang::quos())

    ## 2. Untrained before #52: the selectors are in `columns`, with or
    ##    without butcher()'s empty `terms`.
    expect_identical(sel(list(columns = q), "s"), q)
    expect_identical(sel(list(terms = list(), columns = q), "s"), q)

    ## 3. Trained before #52: no selectors anywhere.
    expect_error(sel(list(columns = c(wn_1 = "wn_1")), "step_x"),
                 class = "horizons_input_error")
    expect_error(sel(list(terms = list(), columns = c(wn_1 = "wn_1")), "step_x"),
                 "earlier version of horizons")

  })

})


## =========================================================================
## Recipe settings: configure()'s sg_window and pca_threshold reach the steps
## =========================================================================
##
## Before #62 build_recipe() hardcoded both: step_transform_spectra() ran its
## own default window of 9 and step_pca() a threshold of 0.995, whatever
## configure() recorded. The settings are now arguments, threaded from
## config$recipe, with the old values as defaults.

describe("build_recipe() recipe settings (#62)", {

  methods <- c("raw", "sg", "snv", "deriv1", "deriv2", "snv_deriv1", "snv_deriv2")

  it("passes sg_window to the transform step: a window of 11 leaves p - 10 columns", {

    td <- make_test_data(n = 20, n_wn = 40)

    for (pp in methods) {

      config <- make_config_row(preprocessing = pp)
      rec    <- build_recipe(config, td$data, td$role_map, sg_window = 11L)

      expect_identical(rec$steps[[1]]$window_size, 11L, label = pp)

      baked <- recipes::bake(recipes::prep(rec), new_data = NULL)

      expect_equal(sum(grepl("^spec[0-9]+$", names(baked))), 40 - 10, label = pp)

    }

  })

  it("passes pca_threshold to step_pca, where it sets the component count", {

    set.seed(62)
    td     <- make_test_data(n = 40, n_wn = 40)
    config <- make_config_row(preprocessing = "snv", feature_selection = "pca")

    n_components <- function(threshold) {

      rec <- build_recipe(config, td$data, td$role_map, pca_threshold = threshold)
      pca <- rec$steps[[which(vapply(rec$steps, inherits, logical(1), "step_pca"))]]

      expect_identical(pca$threshold, threshold)

      sum(grepl("^PC", names(recipes::bake(recipes::prep(rec), new_data = NULL))))

    }

    ## Noise spectra spread their variance over many components, so a lower
    ## threshold keeps strictly fewer.
    expect_lt(n_components(0.5), n_components(0.995))

  })

  it("builds, at its defaults, the recipe the hardcoded values built", {

    ## The reference is the call build_recipe() made before the settings were
    ## arguments: step_transform_spectra() with no window_size, so the step's
    ## own default of 9, and step_pca() at 0.995. The baked output must be
    ## identical, so the default behaviour did not move.
    td <- make_test_data(n = 40, n_wn = 40)
    wn <- td$role_map$variable[td$role_map$role == "predictor"]

    for (pp in c("raw", "snv", "deriv2")) {

      for (fs in c("none", "pca")) {

        reference <- recipes::recipe(SOC ~ ., data = td$data) |>
          recipes::update_role(sample_id, new_role = "id") |>
          step_transform_spectra(dplyr::all_of(wn), preprocessing = pp)

        if (fs == "pca") {

          reference <- reference |>
            recipes::step_pca(select_generated_spectra(), threshold = 0.995,
                              options = list(scale. = TRUE, center = TRUE))

        }

        built <- build_recipe(make_config_row(preprocessing = pp, feature_selection = fs),
                              td$data, td$role_map)

        expect_identical(recipes::bake(recipes::prep(built), new_data = NULL),
                         recipes::bake(recipes::prep(reference), new_data = NULL),
                         label = paste(pp, fs))

      }

    }

  })

})


## =========================================================================
## tune reads the recipe without tidyselect's all_of() deprecation
## =========================================================================
##
## evaluate_single_config() and fit_single_config() call
## workflows::extract_parameter_set_dials(), which reaches every step argument
## through recipes:::find_tune_id(). That evaluates the selector quosures
## outside a selecting context, and the transform step's selector used to be
## dplyr::all_of(predictor_cols), so every config raised "Using `all_of()`
## outside of a selecting function" (about 26 warnings in the suite). The
## lifecycle verbosity is forced so the check does not depend on whether
## lifecycle has already warned once this session.

describe("build_recipe() under tune's parameter extraction", {

  it("raises no warning for any feature selection, with or without a covariate", {

    rlang::local_options(lifecycle_verbosity = "warning")

    td   <- make_test_data(n = 20, n_wn = 30, covariates = "clay")
    spec <- define_model_spec("rf")

    for (fs in c("none", "pca", "correlation", "boruta", "cars")) {

      for (cov in list(NA_character_, "clay")) {

        rec <- build_recipe(make_config_row(preprocessing = "deriv1",
                                            feature_selection = fs,
                                            covariates = cov),
                            td$data, td$role_map)

        wf <- workflows::workflow() |>
          workflows::add_recipe(rec) |>
          workflows::add_model(spec)

        expect_no_warning(workflows::extract_parameter_set_dials(wf),
                          message = "outside of a selecting function")

      }

    }

  })

  it("still selects exactly the spectral columns, leaving a promoted covariate alone", {

    td  <- make_test_data(n = 20, n_wn = 30, covariates = "clay")
    rec <- build_recipe(make_config_row(covariates = "clay"), td$data, td$role_map)

    prepped <- recipes::prep(rec)
    wn      <- td$role_map$variable[td$role_map$role == "predictor"]

    expect_identical(unname(prepped$steps[[1]]$columns), wn)
    expect_true("clay" %in% names(recipes::bake(prepped, new_data = NULL)))

  })

})
