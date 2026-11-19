## ---------------------------------------------------------------------------
## Tests: isolated engines (R/isolated-engines.R)
## ---------------------------------------------------------------------------
## A model MODEL_SPECS marks `isolate` is fitted through its engine's twin,
## whose fit function runs the engine's own registered fit function in a
## process of its own. cubist is the one marked.

## A small regression problem, as parsnip hands it to a fit function
isolated_data <- function(n = 120, p = 12) {
  set.seed(3)
  x <- as.data.frame(matrix(stats::rnorm(n * p), n, p))
  names(x) <- paste0("wn_", seq_len(p))
  list(x = x, y = rowSums(x[, 1:3]) + stats::rnorm(n))
}

## mirai::mirai as it is, for a test that mocks it and then sends
mirai_real <- mirai::mirai

cubist_spec <- function(engine, neighbors = 3) {
  parsnip::cubist_rules(committees = 2, neighbors = neighbors) |>
    parsnip::set_engine(engine) |>
    parsnip::set_mode("regression")
}

describe("model_engine()", {

  it("names the twin for a model marked isolate, only while isolation is on, and each other model's own engine", {

    expect_identical(model_engine("cubist"), "Cubist_isolated")
    expect_identical(model_engine("cubist", isolate = FALSE), "Cubist")

    for (model in setdiff(VALID_MODELS, "cubist")) {
      expect_identical(model_engine(model), MODEL_SPECS[[model]]$engine, label = model)
    }

    expect_identical(isolated_models(c("rf", "cubist", "cubist", "plsr")), "cubist")
    expect_identical(isolated_models("rf"), character(0))

  })

})

describe("register_isolated_engine()", {

  it("copies the engine's registration under the twin's name, with only its fit function changed", {

    env  <- parsnip::get_model_env()
    pick <- function(table, engine) {
      rows <- env[[paste0("cubist_rules_", table)]]
      rows <- rows[rows$engine == engine, , drop = FALSE]
      rows$engine <- NULL
      rows
    }

    ## The arguments, neighbors as the submodel tune predicts across
    expect_identical(pick("args", "Cubist_isolated"), pick("args", "Cubist"))
    expect_true(pick("args", "Cubist_isolated")$has_submodel[pick("args", "Cubist")$parsnip == "neighbors"])

    ## Both prediction types, the engine's own
    expect_identical(pick("predict", "Cubist_isolated"), pick("predict", "Cubist"))
    expect_setequal(pick("predict", "Cubist_isolated")$type, c("numeric", "raw"))

    ## The encoding
    enc <- parsnip::get_encoding("cubist_rules")
    expect_identical(enc[enc$engine == "Cubist_isolated", -2], enc[enc$engine == "Cubist", -2])

    ## The packages, with horizons
    pkgs <- pick("pkgs", "Cubist_isolated")$pkg[[1]]
    expect_setequal(pkgs, c(pick("pkgs", "Cubist")$pkg[[1]], "horizons"))

    ## The fit: horizons' function, given the engine's own and its name
    fit_twin <- pick("fit", "Cubist_isolated")$value[[1]]
    fit_own  <- pick("fit", "Cubist")$value[[1]]
    expect_identical(fit_twin$func, c(pkg = "horizons", fun = "isolated_fit"))
    expect_identical(fit_twin$defaults, c(fit_own$defaults, list(.isolated_fit = fit_own$func, .isolated_engine = "Cubist")))
    expect_identical(fit_twin[c("interface", "protect")], fit_own[c("interface", "protect")])

  })

  it("registers nothing a second time, and nothing for an engine that is not registered", {

    env    <- parsnip::get_model_env()
    before <- as.list(env)[grep("^cubist_rules", ls(env), value = TRUE)]

    register_isolated_engines()
    register_isolated_engine("cubist_rules", "NotAnEngine")

    expect_identical(as.list(env)[grep("^cubist_rules", ls(env), value = TRUE)], before)

  })

})

describe("isolated_fit()", {

  it("fits in a process of its own what the engine's fit function would fit there, from one draw of the caller's stream", {

    skip_if_not_installed("callr")

    d <- isolated_data()

    set.seed(21)
    fitted <- with_deadline(parsnip::fit_xy(cubist_spec("Cubist_isolated"), x = d$x, y = d$y), 60)
    after  <- stats::runif(1)

    ## The engine's own fit function in a fresh process, seeded with the one
    ## integer the caller's stream gives
    set.seed(21)
    seed <- sample.int(100000L, 1L)
    expect_identical(stats::runif(1), after)

    reference <- callr::r(function(x, y, seed) {
      set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
      rules::cubist_fit(x = x, y = y, committees = 2, neighbors = 3)
    }, list(x = d$x, y = d$y, seed = seed))

    ## Identical but for the time the names file says it was written
    stamp <- function(fit) sub("\n\\| on [^\n]*\n", "\n", fit$names)
    expect_identical(stamp(fitted$fit), stamp(reference))
    expect_identical(fitted$fit[setdiff(names(reference), "names")], reference[setdiff(names(reference), "names")])
    expect_s3_class(fitted$fit, "cubist")

    ## It predicts here, through the engine's own code, at the neighbours tune
    ## predicts across without refitting
    own <- parsnip::fit_xy(cubist_spec("Cubist"), x = d$x, y = d$y)
    own$fit <- fitted$fit
    expect_identical(predict(fitted, d$x[1:5, ]), predict(own, d$x[1:5, ]))
    expect_identical(parsnip::multi_predict(fitted, d$x[1:3, ], neighbors = c(0, 5)),
                     parsnip::multi_predict(own, d$x[1:3, ], neighbors = c(0, 5)))

  })

  it("sends the engine's fit function and the data alone, and its process loads no horizons", {

    d    <- isolated_data()
    sent <- NULL

    local_mocked_bindings(mirai = function(.expr, ..., .args = list(), .compute = NULL) {
      sent <<- list(expr = .expr, args = .args, compute = .compute)
      rlang::abort("sent", class = "isolated_sent")
    }, .package = "mirai")

    expect_error(parsnip::fit_xy(cubist_spec("Cubist_isolated"), x = d$x, y = d$y), class = "isolated_sent")

    ## The arguments: the data, the model's arguments, and the runner's own
    expect_setequal(names(sent$args), c("x", "y", "committees", "neighbors", ".horizons_libs",
                                        ".horizons_options", ".horizons_seed"))
    expect_true(is.call(sent$expr))
    expect_identical(sent$compute, isolated_profile())

    ## No function or environment travels, and nothing beyond the data
    holds <- function(v) is.function(v) || is.environment(v) || (is.list(v) && any(vapply(v, holds, logical(1))))
    expect_false(any(vapply(sent$args, holds, logical(1))))
    data_bytes <- length(serialize(d$x, NULL)) + length(serialize(d$y, NULL))
    expect_lt(length(serialize(sent$args, NULL)), data_bytes + 10000)
    expect_lt(length(serialize(sent$expr, NULL)), 10000)

    ## The engine's own fit function, called by name
    expect_match(gsub("\\s+", " ", paste(deparse(sent$expr), collapse = " ")),
                 "rules::cubist_fit(x = x, y = y, committees = committees, neighbors = neighbors)", fixed = TRUE)

    ## Sent again, as it was, with the process's namespaces read after the fit
    ### mirai sends a call held in a variable, and one written in place as written
    probe  <- bquote({ .(sent$expr); loadedNamespaces() })
    loaded <- with_deadline(mirai_real(
      .expr    = probe,
      .args    = sent$args,
      .compute = "horizons-test-loaded"
    )[], 60)

    expect_true("rules" %in% loaded)
    expect_false("horizons" %in% loaded)

  })

  it("gives the fit's process the engines' thread options this session has set, and no others", {

    d    <- isolated_data()
    sent <- NULL

    local_mocked_bindings(run_isolated = function(expr, args = list(), what, options = list(), ...) {
      sent <<- options
      rlang::abort("sent", class = "isolated_sent")
    })

    ## A second engine's option, unset here
    specs <- MODEL_SPECS
    specs$xgboost$threads$option <- "horizons.test.threads"
    local_mocked_bindings(MODEL_SPECS = specs)

    withr::local_options(ranger.num.threads = 3L, horizons.test.threads = NULL)
    expect_error(parsnip::fit_xy(cubist_spec("Cubist_isolated"), x = d$x, y = d$y), class = "isolated_sent")
    expect_identical(sent, list(ranger.num.threads = 3L))

    options(ranger.num.threads = NULL)
    expect_error(parsnip::fit_xy(cubist_spec("Cubist_isolated"), x = d$x, y = d$y), class = "isolated_sent")
    expect_length(sent, 0L)

  })

})

describe("a cubist fit through define_model_spec()", {

  it("does not depend on the Cubist fits made before it in the session", {

    ## Seven isolated fits at 300 predictors, several seconds
    skip_unless_slow_tier()
    skip_on_cran()

    ## Cubist's neighbour cutoff (`maxd`) comes from a generator in its C code
    ## that carries on from every fit in a process; at 300 predictors it moves
    ## by several units from one fit to the next there. Each fit in a process
    ## of its own starts it at the same point.
    d     <- isolated_data(n = 200, p = 300)
    other <- isolated_data(n = 150, p = 40)

    fit_once <- function(data) {
      spec <- tune::finalize_model(define_model_spec("cubist"),
                                   tibble::tibble(committees = 2L, neighbors = 5L, max_rules = 50L))
      set.seed(4)
      parsnip::fit_xy(spec, x = data$x, y = data$y)
    }

    first <- with_deadline(fit_once(d), 60)
    again <- with_deadline(lapply(1:3, function(i) {
      fit_once(other)
      fit_once(d)
    }), 120)

    for (later in again) {
      expect_identical(later$fit$maxd, first$fit$maxd)
      expect_identical(predict(later, d$x[1:20, ]), predict(first, d$x[1:20, ]))
    }

  })

})

describe("the progress tree and the log on isolated fits", {

  it("says which models fit in a process of their own, and with isolation off says so and notes it", {

    expect_identical(
      isolation_tree_line(c("rf", "cubist"), TRUE),
      "Isolated fits: cubist, each fit in a process of its own (control_horizons(isolate = FALSE) turns it off)"
    )
    expect_identical(isolation_tree_line(c("rf", "cubist"), FALSE),
                     "Isolated fits: off, as asked; cubist fitted in the process that ran it")
    expect_null(isolation_tree_line("rf", TRUE))

    collector <- log_open("evaluate")
    log_isolation_off(collector, c("rf", "cubist"), TRUE)
    log_isolation_off(collector, "rf", FALSE)
    expect_length(collector$events, 0L)

    log_isolation_off(collector, c("rf", "cubist"), FALSE)
    rows <- log_close(collector)

    expect_identical(rows$type, "note")
    expect_identical(rows$ids, list("cubist"))
    expect_match(rows$message, "\"cubist\" was fitted in the process that ran it, as `control_horizons(isolate = FALSE)` asked",
                 fixed = TRUE)

  })

})
