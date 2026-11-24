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

    ## cubist's fits are isolated (MODEL_SPECS), through its engine's twin
    expected_engines <- list(
      rf          = "ranger",
      cubist      = "Cubist_isolated",
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

  it("takes each model's own engine under isolate = FALSE, and changes nothing else", {

    for (model in VALID_MODELS) {

      off <- define_model_spec(model, isolate = FALSE)
      on  <- define_model_spec(model)

      expect_identical(off$engine, MODEL_SPECS[[model]]$engine, label = model)
      expect_identical(names(off$args), names(on$args), label = model)
      expect_identical(lapply(off$eng_args, rlang::quo_get_expr), lapply(on$eng_args, rlang::quo_get_expr),
                       label = model)

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

  it("carries nothing of its caller's frame, so a tune task stays small, whatever it leaves unread", {

    ## tune serializes the spec to a worker with every task, and its tune()
    ## arguments keep define_model_spec()'s frame. rf has no weight limit,
    ## so it never reads max_weights; left a promise, the argument would
    ## carry its caller's frame with it: here 40 MB, in the verbs the folds
    ## and the training rows.
    in_a_heavy_frame <- function(model) {
      heavy <- numeric(5e6)
      define_model_spec(model, isolate = FALSE, threads = length(heavy[1]), max_weights = heavy[1] + 10000)
    }

    for (model in c("rf", "mlp")) {
      expect_lt(length(serialize(in_a_heavy_frame(model), NULL)), 1e6, label = model)
    }

  })

  it("gives mlp's engine the weight budget as its own limit, 10000 unless told otherwise", {

    expect_identical(rlang::eval_tidy(define_model_spec("mlp")$eng_args$MaxNWts), 10000)
    expect_identical(rlang::eval_tidy(define_model_spec("mlp", max_weights = 20)$eng_args$MaxNWts), 20)
    expect_identical(rlang::eval_tidy(define_model_spec("mlp", max_weights = Inf)$eng_args$MaxNWts), Inf)

    ## A model with no weights trait takes no such argument
    expect_null(define_model_spec("rf", max_weights = 20)$eng_args$MaxNWts)

  })

  it("builds each model's spec from its row: fn's class, the fixed, engine, thread and weight arguments, and a tune() for each tuned one", {

    for (model in VALID_MODELS) {

      row  <- MODEL_SPECS[[model]]
      spec <- define_model_spec(model)

      expect_s3_class(spec, row$fn)
      expect_identical(spec$mode, "regression", label = model)
      expect_identical(spec$engine, model_engine(model), label = model)

      set <- names(spec$args)[!vapply(spec$args, rlang::quo_is_null, logical(1))]
      expect_setequal(set, c(names(row$fixed), row$tuned))

      for (arg in names(row$fixed)) {
        expect_identical(rlang::eval_tidy(spec$args[[arg]]), row$fixed[[arg]], label = paste(model, arg))
      }

      for (arg in row$tuned) {
        expect_identical(rlang::quo_get_expr(spec$args[[arg]]), quote(tune::tune()), label = paste(model, arg))
      }

      ## The engine arguments, the thread argument at the package's count,
      ## and the weight limit at the default budget
      engine_args <- row$engine_args
      if (!is.null(row$threads$argument)) engine_args[[row$threads$argument]] <- ENGINE_THREADS
      if (!is.null(row$weights$argument)) engine_args[[row$weights$argument]] <- DEFAULT_MAX_WEIGHTS

      expect_setequal(names(spec$eng_args) %||% character(0), names(engine_args) %||% character(0))

      for (arg in names(engine_args)) {
        expect_identical(rlang::eval_tidy(spec$eng_args[[arg]]), engine_args[[arg]], label = paste(model, arg))
      }

    }

  })

  it("sets each engine's thread argument to the count it is given, and one by default", {

    ## rf, xgboost and lightgbm, the rows with a thread argument
    with_arg <- Filter(function(model) !is.null(MODEL_SPECS[[model]]$threads$argument), VALID_MODELS)
    expect_setequal(with_arg, c("rf", "xgboost", "lightgbm"))

    for (model in with_arg) {
      arg <- MODEL_SPECS[[model]]$threads$argument
      expect_identical(rlang::eval_tidy(define_model_spec(model, threads = 4)$eng_args[[arg]]), 4L, label = model)
      expect_identical(rlang::eval_tidy(define_model_spec(model)$eng_args[[arg]]), 1L, label = model)
    }

  })

  it("refuses a model the table does not hold, as an internal error", {

    ## An unknown model is refused at every verb's door first
    ## (config_findings(), test-class-core.R)
    expect_error(define_model_spec("not_a_model"), class = "horizons_error_internal")

  })

})

describe("engine_thread_options()", {

  it("gives each thread option the rows declare, at the package's thread count", {

    expect_identical(engine_thread_options(), list(ranger.num.threads = 1L))

    local({
      local_mocked_bindings(ENGINE_THREADS = 3L, .package = "horizons")
      expect_identical(engine_thread_options(), list(ranger.num.threads = 3L))
    })

    local_model_specs(list(xgboost = list(threads = list(option = "horizons.test.threads"))))

    expect_identical(engine_thread_options(), list(ranger.num.threads = 1L, horizons.test.threads = 1L))

  })

  it("gives them a count it is given, as a whole number", {

    expect_identical(engine_thread_options(4), list(ranger.num.threads = 4L))

  })

})

describe("engine_spec()", {

  it("finds each engine's row, its isolated twin's too, and none for an engine no row names", {

    for (model in VALID_MODELS) {

      row <- MODEL_SPECS[[model]]

      expect_identical(engine_spec(row$engine), row, label = model)
      expect_identical(engine_spec(isolated_engine_name(row$engine)), row, label = model)

    }

    expect_null(engine_spec("not_an_engine"))

  })

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

  it("returns NULL invisibly when every needed package is installed", {

    out <- expect_invisible(check_packages(list(ranger = "rf"), "predict"))
    expect_null(out)

  })

  it("is a no-op for an empty needed list, returning NULL invisibly", {

    out <- expect_invisible(check_packages(list(), "predict"))
    expect_null(out)

  })

})


## ---------------------------------------------------------------------------
## needed_packages() — pure model and selection -> package mapping (#65)
## ---------------------------------------------------------------------------

describe("needed_packages()", {

  it("gives a single model's packages, each needed for it", {

    expect_identical(needed_packages("rf"), list(ranger = "rf"))

  })

  it("unions packages across models and dedupes reasons", {

    needed <- needed_packages(c("rf", "cubist", "rf"))

    expect_setequal(names(needed), c("ranger", "rules", "Cubist"))
    expect_identical(needed$ranger, "rf")

  })

  it("needs bonsai, which registers the engine, and lightgbm for model 'lightgbm'", {

    ## The check against parsnip below runs only with bonsai installed
    expect_identical(needed_packages("lightgbm"), list(bonsai = "lightgbm", lightgbm = "lightgbm"))

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
## Each model's `packages` and SELECTION_PACKAGES are what the door checks. Each entry
## is read here against what parsnip and recipes say the real objects need, so
## a table that drifts from an engine or a step fails the suite, rather than
## letting a package past the door to fail mid-run.

describe("each model's packages - against parsnip's required_pkgs()", {

  for (m in setdiff(VALID_MODELS, "lightgbm")) {

    it(paste0("lists what parsnip requires for model '", m, "', less horizons"), {

      required <- parsnip::required_pkgs(define_model_spec(m), infra = FALSE)

      expect_setequal(MODEL_SPECS[[m]]$packages, setdiff(required, "horizons"))

    })

  }

  it("lists what parsnip requires for model 'lightgbm', which it learns from bonsai", {

    ## bonsai registers the engine with parsnip when it loads
    skip_if_not_installed("bonsai")

    required <- parsnip::required_pkgs(define_model_spec("lightgbm"), infra = FALSE)

    expect_setequal(MODEL_SPECS[["lightgbm"]]$packages, setdiff(required, "horizons"))

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
