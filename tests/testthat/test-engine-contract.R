## ---------------------------------------------------------------------------
## Tests: the engine contract, every engine in MODEL_SPECS
## ---------------------------------------------------------------------------
## horizons makes four promises about every engine it offers, whatever the
## engine's own code does:
##
##   (a) repeats: the same seed and data give the same predictions, fit after
##       fit in one process and in a fresh one;
##   (b) memory: a process's memory stays flat over many fits;
##   (c) the engine loads and fits on a mirai daemon started from an
##       installed build;
##   (d) it runs on the package's thread count.
##
## Each model is fitted through the package's own path for one fit, the calls
## fit_single_config()'s final fit makes. An engine that breaks a promise is
## contained by a mechanism its MODEL_SPECS row declares (for cubist,
## `isolate`) and is held to the promise through it. Each promise the row
## names under `isolate` is run with the containment off and must still fail
## there (a canary): once the engine's own fix lands it passes, the canary
## fails, and its message says to drop the reason.
##
## (a), (b), (c) and the canaries start fresh processes (run_isolated()),
## which load horizons from the library R_LIBS names: an installed build, in
## the slow tier. (d) and the checks on the table run in the default tier.

## ---------------------------------------------------------------------------
## What the contract fits
## ---------------------------------------------------------------------------

## One small fixed point per model, quick to fit at the contract's shape. A
## new model in MODEL_SPECS needs its point here (a test below holds the two
## to the same names).
CONTRACT_PARAMS <- list(
  rf          = list(mtry = 17L, min_n = 5L),
  cubist      = list(committees = 2L, neighbors = 5L, max_rules = 50L),
  xgboost     = list(tree_depth = 2L, learn_rate = 0.1, mtry = 17L, min_n = 5L, loss_reduction = 0,
                     sample_size = 0.8),
  plsr        = list(num_comp = 5L),
  elastic_net = list(penalty = 0.01, mixture = 0.5),
  svm_rbf     = list(cost = 1, rbf_sigma = 0.01),
  mlp         = list(hidden_units = 3L, penalty = 0.01, epochs = 50L),
  lightgbm    = list(tree_depth = 3L, learn_rate = 0.1, mtry = 17L, min_n = 5L),
  mars        = list(prod_degree = 1L, num_terms = 10L)
)

## The promises an `isolate` reason can name
CONTRACT_PROMISES <- c("repeats", "memory")

## The fits a memory sequence makes, the fits its slope is read over, and the
## slope above which a process's memory is not flat (MB a fit). The caller's
## allocator settles over the first twenty fits: an isolated cubist fit hands
## back a fitted object of over a megabyte, and its caller's resident memory
## climbs for about 25 fits and then stops. Measured over fits 21 to 40 in
## three processes per engine, the worst flat slope was 0.37 (one allocator
## step of 5 MB) and Cubist's leak in one process 1.48.
CONTRACT_FITS      <- 40L
CONTRACT_WINDOW    <- 21:40
CONTRACT_RSS_SLOPE <- 0.75

## 200 rows by 300 predictors to fit, and 150 by 40 fitted between: a fit of
## another row count moves the stream Cubist's C code keeps, and at 300
## predictors the leak it leaves is about 1.4 MB a fit. The 40 rows to
## predict are drawn at 1 to 1.3 times the training rows' spread, so some lie
## past the edge of the training cloud: Cubist's neighbour cutoff (`maxd`)
## reaches only a case whose nearest training rows lie near it.
build_contract_data <- function() {

  set.seed(11)

  table_of <- function(x, y) {
    names(x) <- paste0("wn_", seq(4000, by = -2, length.out = ncol(x)))
    df <- tibble::as_tibble(x)
    df$sample_id <- sprintf("S%04d", seq_len(nrow(x)))
    df$SOC <- y
    df
  }

  roles_of <- function(df) {
    wn <- grep("^wn_", names(df), value = TRUE)
    tibble::tibble(variable = c("sample_id", wn, "SOC"), role = c("id", rep("predictor", length(wn)), "outcome"))
  }

  x <- matrix(stats::rnorm(200 * 300), 200)
  y <- 2 + drop(x[, 1:10] %*% seq(0.5, 0.05, length.out = 10)) + stats::rnorm(200, sd = 0.5)

  new <- matrix(stats::rnorm(40 * 300), 40) * seq(1, 1.3, length.out = 40)
  o_x <- matrix(stats::rnorm(150 * 40), 150)

  train <- table_of(as.data.frame(x), y)
  other <- table_of(as.data.frame(o_x), 2 + o_x[, 1] + stats::rnorm(150, sd = 0.5))

  list(train    = train,
       new      = table_of(as.data.frame(new), NA_real_),
       role_map = roles_of(train),
       other    = list(train = other, role_map = roles_of(other)))

}

contract_data <- function() memo_fixture("contract_data", build_contract_data)

## A model's fits in one fresh process, as one quoted call, so it runs the
## same in every process it is sent to: the model's packages loaded as the
## verbs' door loads them, then `n_fits` fits of the 300-predictor set (the
## 40-predictor set fitted once after the first), each read for its
## predictions and, after gc(), the process's resident memory.
CONTRACT_SEQUENCE <- quote({

  horizons:::check_packages(horizons:::needed_packages(model), "the engine contract")

  fit_once <- function(data, role_map) {
    config <- tibble::tibble(config_id = "contract", model = model, transformation = "none",
                             preprocessing = "raw", feature_selection = "none")
    recipe <- horizons:::build_recipe(config, data, role_map)
    spec   <- tune::finalize_model(horizons:::define_model_spec(model, isolate = isolate),
                                   tibble::as_tibble(params))
    stage  <- c("contract", model, "final-fit")
    horizons:::with_unit_seed(seed, stage, workflows::fit(
      horizons:::seed_engine(workflows::workflow(recipe, spec), horizons:::unit_seed(seed, stage)),
      data = data))
  }

  rss_now <- function() {
    gc(verbose = FALSE)
    gc(verbose = FALSE)
    ps::ps_memory_info()[["rss"]]
  }

  rss   <- numeric(n_fits)
  preds <- vector("list", n_fits)

  for (i in seq_len(n_fits)) {

    fitted     <- fit_once(d$train, d$role_map)
    preds[[i]] <- stats::predict(fitted, d$new)$.pred
    rm(fitted)

    if (i == 1L && n_fits > 1L) invisible(fit_once(d$other$train, d$other$role_map))

    rss[i] <- rss_now()

  }

  list(preds = preds, rss = rss)

})

## A model's fits in a fresh process, isolation on or off, built once per
## test process
build_contract_run <- function(model, isolate, n_fits) {

  suppressWarnings(suppressMessages(horizons:::run_isolated(
    CONTRACT_SEQUENCE,
    args = list(model = model, isolate = isolate, n_fits = n_fits, d = contract_data(),
                params = CONTRACT_PARAMS[[model]], seed = 307L),
    what = paste("the engine contract's", model, "fits")
  )))

}

contract_run <- function(model, isolate = TRUE, n_fits = CONTRACT_FITS) {
  memo_fixture("contract_run", build_contract_run, model = model, isolate = isolate, n_fits = n_fits)
}

## ---------------------------------------------------------------------------
## Each promise, read from a model's fits: why it fails, or NULL when it holds
## ---------------------------------------------------------------------------

## (a) Fits 1, 2 and the last in one process, and one fit in another, predict
## identically
broken_repeats <- function(model, isolate) {

  run   <- contract_run(model, isolate)
  again <- contract_run(model, isolate, n_fits = 1L)
  last  <- length(run$preds)

  if (!identical(run$preds[[2]], run$preds[[1]])) return("the second fit predicts differently from the first")
  if (!identical(run$preds[[last]], run$preds[[1]])) return(paste0("fit ", last, " predicts differently from the first"))
  if (!identical(again$preds[[1]], run$preds[[1]])) return("a fit in a fresh process predicts differently")

  NULL

}

## (b) The least-squares slope of resident memory over the window is under
## the threshold
contract_slope <- function(rss) {
  i <- CONTRACT_WINDOW
  unname(stats::coef(stats::lm(rss[i] / 1e6 ~ i))[2])
}

broken_memory <- function(model, isolate) {

  slope <- contract_slope(contract_run(model, isolate)$rss)

  if (slope >= CONTRACT_RSS_SLOPE) {
    return(sprintf("resident memory grows %.2f MB a fit over fits %d to %d", slope, min(CONTRACT_WINDOW),
                   max(CONTRACT_WINDOW)))
  }

  NULL

}

promise_broken <- list(repeats = broken_repeats, memory = broken_memory)

## A model's tests skip, by name, where its packages are not installed
skip_without_packages <- function(model) {
  for (pkg in MODEL_SPECS[[model]]$packages) testthat::skip_if_not_installed(pkg)
}

skip_unless_contract_runs <- function() {
  skip_unless_slow_tier()
  skip_on_cran()
  skip_if_dev_package()
}

## ---------------------------------------------------------------------------
## The table
## ---------------------------------------------------------------------------

describe("the engine contract's table", {

  it("has a fixed point for every model, and every isolate reason names a promise it checks", {

    expect_setequal(names(CONTRACT_PARAMS), names(MODEL_SPECS))

    for (model in names(MODEL_SPECS)) {
      expect_setequal(names(CONTRACT_PARAMS[[model]]) %||% character(0), MODEL_SPECS[[model]]$tuned %||% character(0))
      expect_true(all(names(MODEL_SPECS[[model]]$isolate) %in% CONTRACT_PROMISES), label = model)
      expect_true(all(nzchar(MODEL_SPECS[[model]]$isolate)), label = model)
    }

  })

})

## ---------------------------------------------------------------------------
## (d) Threads, and an isolated engine's twin
## ---------------------------------------------------------------------------

describe("the engine contract: (d) threads", {

  for (model in names(MODEL_SPECS)) {

    it(paste0("gives ", model, " the package's thread count through the argument its row declares, and no other"), {

      skip_without_packages(model)
      check_packages(needed_packages(model), "the engine contract")

      ## parsnip's translation of the spec is the call the engine's fit
      ## receives. An engine whose fit takes a thread argument the row does
      ## not declare is caught here, at whatever its registration defaults
      ## it to.
      row  <- MODEL_SPECS[[model]]
      spec <- tune::finalize_model(define_model_spec(model), tibble::as_tibble(CONTRACT_PARAMS[[model]]))
      args <- parsnip::translate(spec)$method$fit$args

      threads <- names(args)[grepl("thread", names(args), ignore.case = TRUE)]

      expect_identical(threads, row$threads$argument %||% character(0))

      for (arg in threads) expect_identical(rlang::eval_tidy(args[[arg]]), ENGINE_THREADS, label = paste(model, arg))

    })

  }

})

describe("the engine contract: an isolated engine's twin", {

  for (model in names(Filter(isolates, MODEL_SPECS))) {

    it(paste0("tunes ", model, "'s twin as its engine, and reads its engine's traits for it"), {

      skip_without_packages(model)
      check_packages(needed_packages(model), "the engine contract")

      twin <- define_model_spec(model)
      own  <- define_model_spec(model, isolate = FALSE)

      ## tidymodels code that branches on an engine's name would treat the
      ## twin as another engine
      expect_identical(twin$engine, isolated_engine_name(own$engine))
      expect_identical(tune::tunable(twin), tune::tunable(own))
      expect_identical(engine_spec(twin$engine), MODEL_SPECS[[model]])

    })

  }

})

## ---------------------------------------------------------------------------
## (a) Repeats and (b) memory, through each engine's containment
## ---------------------------------------------------------------------------

describe("the engine contract: (a) repeats", {

  for (model in names(MODEL_SPECS)) {

    it(paste0("gives ", model, " the same predictions fit after fit in one process and in a fresh one (installed build)"), {

      skip_unless_contract_runs()
      skip_without_packages(model)

      expect_null(broken_repeats(model, isolate = TRUE), label = model)

    })

  }

})

describe("the engine contract: (b) memory", {

  for (model in names(MODEL_SPECS)) {

    it(paste0("keeps a process's memory flat over ", CONTRACT_FITS, " ", model, " fits (installed build)"), {

      skip_unless_contract_runs()
      skip_without_packages(model)
      ## The one reading this promise needs from the platform
      skip_if_not(ps::ps_is_supported(), "ps cannot read a process's memory on this platform")

      expect_null(broken_memory(model, isolate = TRUE), label = model)

    })

  }

})

## ---------------------------------------------------------------------------
## (c) Loads and fits on a daemon started from an installed build
## ---------------------------------------------------------------------------

describe("the engine contract: (c) a fresh daemon", {

  for (model in names(MODEL_SPECS)) {

    it(paste0("runs a ", model, " configuration as a configs-axis worker's first task in a fresh process (installed build)"), {

      skip_unless_contract_runs()
      skip_without_packages(model)

      shared <- worker_payload()
      shared$configs$model <- model

      ## A process started for the one call, which is sent a path and
      ## nothing of this session, so a package the model needs is loaded
      ## there by the worker or not at all. bonsai registers lightgbm's
      ## engine with parsnip only once loaded.
      res <- with_deadline(horizons:::run_isolated(quote({
        before <- intersect(packages, loadedNamespaces())
        row    <- suppressWarnings(horizons:::evaluate_config_worker(1L, payload_path))
        list(before = before, status = row$status, error = row$error_message)
      }), args = list(packages = MODEL_SPECS[[model]]$packages, payload_path = worker_payload_file(shared)),
      what = paste("a", model, "worker")), 120)

      expect_length(res$before, 0L)
      expect_identical(res$status, "success", info = res$error)

    })

  }

})

## ---------------------------------------------------------------------------
## The canaries: each reason a row gives for its containment still holds
## ---------------------------------------------------------------------------

describe("the engine contract: the canaries", {

  for (model in names(Filter(isolates, MODEL_SPECS))) {

    for (promise in names(MODEL_SPECS[[model]]$isolate)) {

      it(paste0("finds ", model, " still breaking '", promise, "' without isolation (installed build)"), {

        skip_unless_contract_runs()
        skip_without_packages(model)
        if (promise == "memory") skip_if_not(ps::ps_is_supported(), "ps cannot read a process's memory on this platform")

        why <- promise_broken[[promise]](model, isolate = FALSE)

        ## Strict: the promise holding is the failure
        if (is.null(why)) {
          fail(sprintf(paste0(
            "%s no longer breaks '%s' without isolation, so the containment has outlived this reason ",
            "(upstream: %s). Remove `%s` from `MODEL_SPECS$%s$isolate`; with no reason left, remove `isolate`."),
            MODEL_SPECS[[model]]$label, promise, MODEL_SPECS[[model]]$isolate[[promise]], promise, model))
        } else {
          succeed()
        }

      })

    }

  }

})
