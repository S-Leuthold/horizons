## ---------------------------------------------------------------------------
## Tests: Model specification utilities
## ---------------------------------------------------------------------------

## Helper: check if a parsnip arg is a tune() call
is_tune_arg <- function(arg) {

  expr <- rlang::quo_get_expr(arg)
  is.call(expr) && grepl("^tune", deparse(expr[[1]]))

}

describe("define_model_spec()", {

  it("returns a parsnip model_spec for each valid model", {

    for (model in VALID_MODELS) {

      spec <- define_model_spec(model)
      expect_s3_class(spec, "model_spec")

    }

  })

  it("sets mode to regression for all models", {

    for (model in VALID_MODELS) {

      spec <- define_model_spec(model)
      expect_equal(spec$mode, "regression")

    }

  })

  it("sets the correct engine for each model", {

    expected_engines <- list(
      rf          = "ranger",
      cubist      = "Cubist",
      xgboost     = "xgboost",
      plsr        = "mixOmics",
      elastic_net = "glmnet",
      svm_rbf     = "kernlab",
      mars        = "earth",
      lightgbm    = "lightgbm",
      mlp         = "nnet"
    )

    for (model in names(expected_engines)) {

      spec <- define_model_spec(model)
      expect_equal(spec$engine, expected_engines[[model]])

    }

  })

  it("includes tune() placeholders in all tunable specs", {

    for (model in VALID_MODELS) {

      spec <- define_model_spec(model)

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

  it("creates xgboost with 500 fixed trees and 6 tunable params", {

    spec <- define_model_spec("xgboost")
    trees_arg <- rlang::eval_tidy(spec$args$trees)
    expect_equal(trees_arg, 500)

    tune_count <- sum(vapply(spec$args, is_tune_arg, logical(1)))
    expect_equal(tune_count, 6)

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

  ## -------------------------------------------------------------------------
  ## Error handling
  ## -------------------------------------------------------------------------

  it("aborts on unknown model name", {

    expect_error(
      define_model_spec("deep_forest"),
      "Unknown model"
    )

  })

  it("aborts on NULL model name", {

    expect_error(define_model_spec(NULL))

  })

})
