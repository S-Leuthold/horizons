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
