## ---------------------------------------------------------------------------
## Tests: memoised fixtures (helper-memo.R)
## ---------------------------------------------------------------------------
## Each test runs against an empty cache of its own, so these tests neither
## see nor clear fixtures that other files in the same process have built.

local_memo_sandbox <- function(.env = parent.frame()) {

  saved_cache <- .horizons_memo$cache
  saved_root  <- .horizons_memo$root
  saved_jit   <- .horizons_memo$jit_level

  .horizons_memo$cache     <- new.env(parent = emptyenv())
  .horizons_memo$root      <- NULL
  .horizons_memo$jit_level <- NULL

  ## memo_reset() also restores the JIT level if a verify-mode test here
  ## turned the JIT off.
  withr::defer({
    memo_reset()
    .horizons_memo$cache     <- saved_cache
    .horizons_memo$root      <- saved_root
    .horizons_memo$jit_level <- saved_jit
  }, envir = .env)

}

## A build counter the builders below can bump without taking it as an
## argument, which would put it in the key.
new_counter <- function() {

  counter   <- new.env(parent = emptyenv())
  counter$n <- 0L
  counter

}

global_seed <- function() get(".Random.seed", envir = globalenv())


describe("memo_fixture()", {

  it("builds once per key; arguments, name and builder body each give a new build", {

    local_memo_sandbox()
    calls <- new_counter()

    build <- function(n) {
      calls$n <- calls$n + 1L
      seq_len(n)
    }

    ## `n` also checks that a builder argument is not partially matched to
    ## memo_fixture()'s own formals.
    first  <- memo_fixture("seq", build, n = 3)
    second <- memo_fixture("seq", build, n = 3)

    expect_identical(calls$n, 1L)
    expect_identical(second, first)

    expect_identical(memo_fixture("seq", build, n = 4), 1:4)
    expect_identical(calls$n, 2L)

    memo_fixture("seq-other", build, n = 3)
    expect_identical(calls$n, 3L)

    edited <- function(n) {
      calls$n <- calls$n + 1L
      rev(seq_len(n))
    }

    expect_identical(memo_fixture("seq", edited, n = 3), 3:1)
    expect_identical(calls$n, 4L)

    ## The original entry is still there.
    expect_identical(memo_fixture("seq", build, n = 3), 1:3)
    expect_identical(calls$n, 4L)

  })

  it("leaves the RNG where a rebuild would on a hit", {

    local_memo_sandbox()
    withr::local_preserve_seed()

    build <- function() {
      set.seed(7)
      stats::runif(3)
    }

    set.seed(1)
    memo_fixture("rng", build)
    after_build <- global_seed()

    stats::runif(10)
    expect_false(identical(global_seed(), after_build))

    memo_fixture("rng", build)
    after_hit <- global_seed()

    ## A different name is a different key, so this is a real rebuild.
    set.seed(99)
    memo_fixture("rng-rebuild", build)
    after_rebuild <- global_seed()

    expect_identical(after_hit, after_build)
    expect_identical(after_hit, after_rebuild)

  })

  it("leaves the caller's RNG stream alone when the builder does not use it", {

    local_memo_sandbox()
    withr::local_preserve_seed()

    build <- function() withr::with_seed(3, stats::runif(2))

    set.seed(1)
    value <- memo_fixture("seedless", build)

    set.seed(2)
    before_hit <- global_seed()
    expect_identical(memo_fixture("seedless", build), value)
    expect_identical(global_seed(), before_hit)

  })

  it("caches a build error and re-signals it to every later caller", {

    local_memo_sandbox()
    calls <- new_counter()

    build <- function() {
      calls$n <- calls$n + 1L
      stop("no reference data")
    }

    expect_error(memo_fixture("broken", build),
                 class = "horizons_fixture_error")

    for (i in 1:2) {
      cnd <- expect_error(memo_fixture("broken", build),
                          class = "horizons_fixture_error")
      expect_identical(cnd$fixture, "broken")
      expect_match(conditionMessage(cnd), "broken")
      expect_match(conditionMessage(cnd$parent), "no reference data")
    }

    expect_identical(calls$n, 1L)

  })

  it("lets a skip during the build propagate and does not cache it", {

    local_memo_sandbox()
    calls <- new_counter()

    build <- function() {
      calls$n <- calls$n + 1L
      testthat::skip("optional dependency missing")
    }

    expect_condition(memo_fixture("skipped", build), class = "skip")
    expect_condition(memo_fixture("skipped", build), class = "skip")
    expect_identical(calls$n, 2L)

  })

  it("aborts a build that emits a warning or a message, naming the fixture", {

    local_memo_sandbox()

    expect_error(
      memo_fixture("loud-warning", function() {
        warning("convergence")
        1
      }),
      class = "horizons_fixture_error", regexp = "loud-warning"
    )

    expect_error(
      memo_fixture("loud-message", function() {
        message("fitting 4 configs")
        1
      }),
      class = "horizons_fixture_error", regexp = "emitted a message"
    )

    ## Noise muffled inside the builder is fine, and package startup
    ## messages pass through rather than abort.
    expect_identical(
      memo_fixture("muffled", function() suppressWarnings({
        warning("convergence")
        1
      })),
      1
    )

    expect_message(
      value <- memo_fixture("startup", function() {
        packageStartupMessage("method overwritten")
        2
      }),
      class = "packageStartupMessage"
    )
    expect_identical(value, 2)

  })

  it("in verify mode, aborts on a hit after an environment in the value was modified in place", {

    local_memo_sandbox()
    withr::local_envvar(HORIZONS_MEMO_VERIFY = "true")

    build <- function() {
      state   <- new.env()
      state$n <- 1
      list(state = state, x = 1:3, scale = function(v) v * state$n)
    }

    value <- memo_fixture("stateful", build)

    ## Copy-on-modify leaves the cached list alone, new objects in the
    ## builder's enclosing environment are context, not part of the value,
    ## and calling a stored closure (which the JIT would compile in place)
    ## changes nothing.
    value$x[1] <- 99L
    defined_later <- TRUE
    for (i in 1:3) expect_identical(value$scale(2), 2)
    expect_no_error(memo_fixture("stateful", build))
    expect_identical(memo_fixture("stateful", build)$scale(3), 3)
    expect_no_error(memo_fixture("stateful", build))

    value$state$n <- 2
    expect_error(memo_fixture("stateful", build),
                 class = "horizons_fixture_modified", regexp = "stateful")

  })

  it("outside verify mode, serves a value modified in place without checking", {

    local_memo_sandbox()
    withr::local_envvar(HORIZONS_MEMO_VERIFY = "false")

    build <- function() {
      state   <- new.env()
      state$n <- 1
      list(state = state)
    }

    value <- memo_fixture("stateful", build)
    value$state$n <- 2

    expect_identical(memo_fixture("stateful", build)$state$n, 2)

  })

})


describe("memo_dir()", {

  it("builds the template once and gives each caller a private copy", {

    local_memo_sandbox()
    calls <- new_counter()

    build <- function(dir) {
      calls$n <- calls$n + 1L
      writeLines("template", file.path(dir, "a.txt"))
      writeLines("hidden", file.path(dir, ".state"))
      dir.create(file.path(dir, "sub"))
      writeLines("nested", file.path(dir, "sub", "b.txt"))
      "built"
    }

    first <- memo_dir("checkpoints", build)
    expect_identical(first$value, "built")

    writeLines("changed", file.path(first$dir, "a.txt"))
    writeLines("extra", file.path(first$dir, "c.txt"))
    unlink(file.path(first$dir, "sub"), recursive = TRUE)

    second <- memo_dir("checkpoints", build)

    expect_false(identical(second$dir, first$dir))
    expect_identical(readLines(file.path(second$dir, "a.txt")), "template")
    expect_identical(readLines(file.path(second$dir, ".state")), "hidden")
    expect_identical(readLines(file.path(second$dir, "sub", "b.txt")), "nested")
    expect_false(file.exists(file.path(second$dir, "c.txt")))
    expect_identical(calls$n, 1L)

    ## The copy belongs to the calling frame and goes when it exits.
    copy_in_frame <- function() memo_dir("checkpoints", build)$dir
    copy <- copy_in_frame()
    expect_false(dir.exists(copy))
    expect_identical(calls$n, 1L)

  })

  it("keeps a copy made through an accessor alive in the test that called it", {

    local_memo_sandbox()

    build <- function(dir) {
      writeLines("template", file.path(dir, "a.txt"))
      "built"
    }

    ## The accessor form in the header's rule 1.
    checkpoints <- function(.env = parent.frame()) {
      memo_dir("checkpoints", build, .env = .env)
    }

    copy <- checkpoints()$dir
    expect_true(dir.exists(copy))
    expect_identical(readLines(file.path(copy, "a.txt")), "template")

    ## ... and it goes when that test's frame exits.
    in_a_test <- function() checkpoints()$dir
    expect_false(dir.exists(in_a_test()))

  })

  it("in verify mode, aborts when a test wrote into the template", {

    local_memo_sandbox()
    withr::local_envvar(HORIZONS_MEMO_VERIFY = "true")

    build <- function(dir) {
      writeLines("template", file.path(dir, "a.txt"))
      dir
    }

    first <- memo_dir("checkpoints", build)
    expect_no_error(memo_dir("checkpoints", build))

    ## Writing through the value's path reaches the shared template.
    writeLines("changed", file.path(first$value, "a.txt"))
    expect_error(memo_dir("checkpoints", build),
                 class = "horizons_fixture_modified", regexp = "template")

  })

})


describe("parallel builds", {

  it("are not aborted by parallelly's once-per-process warning on the first parallel call", {

    skip_on_cran()
    skip_if_not_installed("callr")

    ## A fresh process, so this build makes its first parallel call. On a
    ## machine where parallelly warns about the cgroups CPU set, the build
    ## would abort without the helper's priming call; elsewhere the test
    ## passes either way.
    helper <- normalizePath(test_path("helper-memo.R"))

    worker_pid <- callr::r(function(helper) {
      source(helper)
      memo_fixture("first-parallel-call", function() {
        old <- future::plan(future::multisession, workers = 2)
        on.exit(future::plan(old))
        future::value(future::future(Sys.getpid()))
      })
    }, args = list(helper = helper))

    expect_type(worker_pid, "integer")

  })

})


describe("memo_reset()", {

  it("clears every entry and removes the template directories", {

    local_memo_sandbox()
    calls <- new_counter()

    build <- function(dir) {
      calls$n <- calls$n + 1L
      writeLines("template", file.path(dir, "a.txt"))
      dir
    }

    template <- memo_dir("checkpoints", build)$value
    memo_reset()

    expect_false(dir.exists(template))
    memo_dir("checkpoints", build)
    expect_identical(calls$n, 2L)

  })

  it("turns the JIT back on after verify mode turned it off", {

    local_memo_sandbox()
    withr::local_envvar(HORIZONS_MEMO_VERIFY = "true")

    ## Start from R's default level whatever earlier tests left, and put
    ## the old level back afterwards. enableJIT(-1) reports the level
    ## without changing it.
    jit_outside <- compiler::enableJIT(3)
    withr::defer(compiler::enableJIT(jit_outside))

    memo_fixture("verified", function() 1)
    expect_identical(compiler::enableJIT(-1), 0L)

    memo_reset()
    expect_identical(compiler::enableJIT(-1), 3L)

  })

})
