## ---------------------------------------------------------------------------
## Tests: Model specification utilities
## ---------------------------------------------------------------------------

## Helper: check if a parsnip arg is a tune() call
is_tune_arg <- function(arg) {

  expr <- rlang::quo_get_expr(arg)
  is.call(expr) && grepl("^tune", deparse(expr[[1]]))

}

describe("define_model_spec()", {

  it("returns a regression model_spec with the correct engine and tune() placeholders for each valid model", {

    expected_engines <- list(
      rf          = "ranger",
      cubist      = "Cubist",
      xgboost     = "xgboost",
      plsr        = "pls",
      elastic_net = "glmnet",
      svm_rbf     = "kernlab",
      mars        = "earth",
      lightgbm    = "lightgbm",
      mlp         = "nnet"
    )

    for (model in VALID_MODELS) {

      spec <- define_model_spec(model)
      expect_s3_class(spec, "model_spec")
      expect_equal(spec$mode, "regression")
      expect_equal(spec$engine, expected_engines[[model]])

      has_tune <- any(vapply(spec$args, is_tune_arg, logical(1)))
      expect_true(has_tune, info = paste("Model", model, "should have tune() params"))

    }

  })

  ## -------------------------------------------------------------------------
  ## Model-specific tests
  ## -------------------------------------------------------------------------

  it("creates random forest with 500 fixed trees", {

    spec <- define_model_spec("rf")
    trees_arg <- rlang::eval_tidy(spec$args$trees)
    expect_equal(trees_arg, 500)

  })

  it("creates cubist with committees, neighbors, and max_rules tunable", {

    spec <- define_model_spec("cubist")
    expect_true(is_tune_arg(spec$args$committees))
    expect_true(is_tune_arg(spec$args$neighbors))
    expect_true(is_tune_arg(spec$args$max_rules))

  })

  it("creates xgboost with 500 fixed trees and 6 tunable params, and lightgbm with 500 fixed trees", {

    spec <- define_model_spec("xgboost")
    trees_arg <- rlang::eval_tidy(spec$args$trees)
    expect_equal(trees_arg, 500)

    tune_count <- sum(vapply(spec$args, is_tune_arg, logical(1)))
    expect_equal(tune_count, 6)

    expect_identical(rlang::eval_tidy(define_model_spec("lightgbm")$args$trees), 500)

  })

  it("creates elastic_net with penalty and mixture tunable", {

    spec <- define_model_spec("elastic_net")
    expect_true(is_tune_arg(spec$args$penalty))
    expect_true(is_tune_arg(spec$args$mixture))

  })

  it("creates plsr with num_comp tunable", {

    spec <- define_model_spec("plsr")
    expect_true(is_tune_arg(spec$args$num_comp))

  })

  it("creates mlp with MaxNWts = 10000 engine arg", {

    spec <- define_model_spec("mlp")
    expect_equal(spec$eng_args$MaxNWts, rlang::quo(10000))

  })

  ## An unknown model is refused at every verb's door (config_findings(),
  ## test-class-core.R), before a spec is built.

})


## ---------------------------------------------------------------------------
## check_packages() — the missing-package gate every verb shares (#65)
## ---------------------------------------------------------------------------

describe("check_packages() - missing package gate", {

  it("aborts cleanly, naming the package and the model that needs it", {

    err <- tryCatch(
      check_packages(list(horizonsFakePkgXYZ123 = "cubist"), "predict"),
      horizons_error_missing_package = function(e) e
    )

    expect_s3_class(err, "horizons_error_missing_package")
    expect_s3_class(err, "horizons_error_dependency")
    expect_match(conditionMessage(err), "horizonsFakePkgXYZ123")
    expect_match(conditionMessage(err), "cubist")

  })

  it("collects every missing package and aborts once, not on the first miss", {

    err <- tryCatch(
      check_packages(list(
        horizonsFakePkgXYZ123 = "cubist",
        horizonsFakePkgABC456 = "mars"
      ), "predict"),
      horizons_error_missing_package = function(e) e
    )

    expect_s3_class(err, "horizons_error_missing_package")
    expect_s3_class(err, "horizons_error_dependency")
    expect_match(conditionMessage(err), "horizonsFakePkgXYZ123")
    expect_match(conditionMessage(err), "horizonsFakePkgABC456")

  })

  it("does not abort when every needed package is installed", {

    expect_no_error(check_packages(list(ranger = "rf"), "predict"))

  })

  it("is a no-op for an empty needed list", {

    expect_no_error(check_packages(list(), "predict"))

  })

})


## ---------------------------------------------------------------------------
## needed_packages() — pure model and selection -> package mapping (#65)
## ---------------------------------------------------------------------------

describe("needed_packages()", {

  it("matches MODEL_PACKAGES for a single model", {

    expect_identical(needed_packages("rf"), list(ranger = "rf"))

  })

  it("unions packages across models and dedupes reasons", {

    needed <- needed_packages(c("rf", "cubist", "rf"))

    expect_setequal(names(needed), c("ranger", "rules", "Cubist"))
    expect_identical(needed$ranger, "rf")

  })

  it("adds ranger, reasoned as prediction intervals, only when asked", {

    without_uq <- needed_packages("elastic_net")
    with_uq    <- needed_packages("elastic_net", needs_ranger_for_uq = TRUE)

    expect_false("ranger" %in% names(without_uq))
    expect_true("ranger" %in% names(with_uq))
    expect_identical(with_uq$ranger, "prediction intervals")

  })

  it("adds the packages a selection step trains with, named for the selection", {

    needed <- needed_packages(c("plsr", "elastic_net"), c("boruta", "none"))

    expect_identical(needed$Boruta, "boruta feature selection")
    expect_setequal(needed$ranger, "boruta feature selection")
    expect_identical(needed_packages("plsr", "none"), list(pls = "plsr"))

  })

  it("SELECTION_PACKAGES covers exactly the feature-selection methods", {

    expect_setequal(names(SELECTION_PACKAGES), VALID_FEATURE_SELECTION)

  })

})


## ---------------------------------------------------------------------------
## The package tables against what the models and steps require
## ---------------------------------------------------------------------------
## MODEL_PACKAGES and SELECTION_PACKAGES are what the door checks. Each entry
## is read here against what parsnip and recipes say the real objects need, so
## a table that drifts from an engine or a step fails the suite, rather than
## letting a package past the door to fail mid-run.

describe("MODEL_PACKAGES - against parsnip's required_pkgs()", {

  for (m in setdiff(VALID_MODELS, "lightgbm")) {

    it(paste0("lists what parsnip requires for model '", m, "', less horizons"), {

      required <- parsnip::required_pkgs(define_model_spec(m), infra = FALSE)

      expect_setequal(MODEL_PACKAGES[[m]], setdiff(required, "horizons"))

    })

  }

  it("lists what parsnip requires for model 'lightgbm', which it learns from bonsai", {

    ## bonsai registers the engine with parsnip when it loads
    skip_if_not_installed("bonsai")

    required <- parsnip::required_pkgs(define_model_spec("lightgbm"), infra = FALSE)

    expect_setequal(MODEL_PACKAGES[["lightgbm"]], setdiff(required, "horizons"))

  })

})

describe("SELECTION_PACKAGES - against the selection steps' required_pkgs()", {

  ## The recipe build_recipe() builds for a feature selection, on 20 rows it
  ## never preps; the selection's steps are the ones the "none" recipe lacks
  wn    <- paste0("wn_", seq(4000, by = -2, length.out = 20))
  data  <- tibble::as_tibble(matrix(seq_len(400) / 400, nrow = 20, dimnames = list(NULL, wn)))
  data$sample_id <- paste0("S", seq_len(20))
  data$SOC       <- seq_len(20) / 2
  roles <- tibble::tibble(variable = c("sample_id", wn, "SOC"),
                          role     = c("id", rep("predictor", length(wn)), "outcome"))

  recipe_for <- function(fs) {
    build_recipe(tibble::tibble(model = "plsr", transformation = "none", preprocessing = "raw",
                                feature_selection = fs),
                 data, roles)
  }

  step_class <- function(step) class(step)[1]

  for (fs in VALID_FEATURE_SELECTION) {

    it(paste0("lists what the step for '", fs, "' requires, less horizons"), {

      base  <- vapply(recipe_for("none")$steps, step_class, character(1))
      steps <- Filter(function(s) !step_class(s) %in% base, recipe_for(fs)$steps)

      ## Every selection but none adds its step
      expect_length(steps, if (fs == "none") 0L else 1L)

      required <- unique(as.character(unlist(lapply(steps, recipes::required_pkgs))))

      expect_setequal(SELECTION_PACKAGES[[fs]], setdiff(required, "horizons"))

    })

  }

})
