## ---------------------------------------------------------------------------
## Tests: the log, the collector, run_unit() and unit_step()
## ---------------------------------------------------------------------------

describe("the collector", {

  it("closes onto the log as one row per topic, in the order topics first arose", {

    col <- log_open("evaluate")
    log_add(col, "partial_failure", "3 of 40 configurations failed.", ids = c("a", "b", "c"))
    log_add(col, "quality", "The smallest assessment fold has 8 rows.")
    log_add(col, "quality", "Every member is below the prune threshold.", ids = "m1")

    log <- log_close(col)

    expect_named(log, names(new_log()))
    expect_equal(log$verb, c("evaluate", "evaluate"))
    expect_equal(log$topic, c("partial_failure", "quality"))
    expect_equal(log$ids, list(c("a", "b", "c"), "m1"))
    expect_equal(log$n, c(3L, 2L))
    expect_match(log$message[2], "The smallest assessment fold has 8 rows.", fixed = TRUE)
    expect_match(log$message[2], "Every member is below the prune threshold.", fixed = TRUE)

  })

  it("appends to the log it is given and leaves it as it was when nothing arose", {

    first <- log_open("standardize")
    log_add(first, "flagged", "3 samples are non-finite.", ids = c("s1", "s2", "s3"))
    log <- log_close(first)

    expect_identical(log_close(log_open("average"), log), log)

    second <- log_open("evaluate")
    log_add(second, "quality", "Small folds.")

    expect_equal(log_close(second, log)$verb, c("standardize", "evaluate"))

  })

  it("renders the message where it is added, and keeps upstream text as text", {

    n_failed <- 2L
    cause    <- "column {wn_600} could not be read"

    col <- log_open("evaluate")
    log_add(col, "partial_failure", c("{n_failed} configurations failed.", "x" = "{cause}"))

    expect_match(log_close(col)$message, "2 configurations failed.", fixed = TRUE)
    expect_match(log_close(col)$message, "column {wn_600} could not be read", fixed = TRUE)
    expect_warning(signal_log(col), "{wn_600}", fixed = TRUE,
                   class = "horizons_warning_partial_failure")

  })

  it("refuses a topic it does not know, as a bug in its caller", {

    expect_error(log_add(log_open("fit"), "misc", "x"), "topic",
                 class = "horizons_error_internal")

  })

})

describe("signal_log()", {

  it("raises one warning per topic, each carrying its log row", {

    col <- log_open("evaluate")
    log_add(col, "partial_failure", "3 of 40 configurations failed.", ids = c("a", "b", "c"))
    log_add(col, "quality", "Small folds.")
    log_add(col, "quality", "Weak members.")

    raised <- list()
    withCallingHandlers(
      signal_log(col),
      warning = function(w) {
        raised[[length(raised) + 1L]] <<- w
        invokeRestart("muffleWarning")
      }
    )

    expect_length(raised, 2L)
    expect_equal(vapply(raised, function(w) class(w)[1], character(1)),
                 c("horizons_warning_partial_failure", "horizons_warning_quality"))
    expect_true(all(vapply(raised, inherits, logical(1), "horizons_warning")))
    expect_equal(raised[[1]]$log$ids, list(c("a", "b", "c")))
    expect_equal(raised[[2]]$log$n, 2L)

  })

  it("is silent when nothing arose", {

    expect_no_warning(signal_log(log_open("fit")))

  })

  it("keeps the record whether or not the warning is seen", {

    col <- log_open("fit")
    log_add(col, "skipped", "Intervals were not built for 2 members.", ids = c("m1", "m2"))

    suppressWarnings(signal_log(col))

    expect_equal(log_close(col)$ids, list(c("m1", "m2")))

  })

  it("aborts under warn = 2 with the rows and the finished object, rather than losing them", {

    col <- log_open("evaluate")
    log_add(col, "partial_failure", "3 of 40 configurations failed.", ids = c("a", "b", "c"))

    withr::local_options(warn = 2)

    err <- expect_error(signal_log(col, object = list(result = "kept")),
                        class = "horizons_error_promoted_warnings", inherit = FALSE)

    expect_s3_class(err, "horizons_error_run")
    expect_equal(err$object$result, "kept")
    expect_equal(err$log$topic, "partial_failure")

  })

  it("words a multi-warning topic and the warn = 2 error readably", {

    col <- log_open("evaluate")
    log_add(col, "partial_failure", "3 of 40 configurations failed.", ids = c("a", "b", "c"))
    log_add(col, "quality", "The smallest assessment fold has 8 rows.")
    log_add(col, "quality", "Every member is below the prune threshold.")

    expect_snapshot(signal_log(col))

    ### warn = 2 only around the call, so testthat's own warnings stay warnings.
    expect_snapshot(withr::with_options(list(warn = 2), signal_log(col)), error = TRUE)

  })

})

describe("run_unit()", {

  it("returns the value with no records when the unit runs cleanly", {

    out <- run_unit(1 + 1, "configuration", "cfg_1")

    expect_equal(out$value, 2)
    expect_false(out$failed)
    expect_equal(nrow(out$records), 0L)
    expect_named(out$records, names(new_unit_records()))

  })

  it("records a warning with its step and class, muffles it, and goes on", {

    out <- expect_no_warning(run_unit({
      unit_step("prep", warning("near-zero variance in wn_600"))
      unit_step("fit", "fitted")
    }, "configuration", "cfg_1"))

    expect_equal(out$value, "fitted")
    expect_false(out$failed)
    expect_equal(out$records$type, "warning")
    expect_equal(out$records$step, "prep")
    expect_equal(out$records$class, "simpleWarning")
    expect_equal(out$records$message, "near-zero variance in wn_600")

  })

  it("contains a run failure, naming the step and the root cause's class", {

    rec <- recipes::recipe(mpg ~ ., data = mtcars) |>
      recipes::step_mutate(boom = stop("singular matrix"))

    out <- run_unit(unit_step("prep", recipes::prep(rec)), "configuration", "cfg_2")

    expect_true(out$failed)
    expect_null(out$value)
    expect_equal(out$records$type, "failure")
    expect_equal(out$records$step, "prep")
    expect_equal(out$records$class, "simpleError")
    expect_match(out$records$message, "singular matrix", fixed = TRUE)
    expect_null(out$records$trace[[1]])

  })

  it("keeps a failure's backtrace as text, and only when asked", {

    out <- run_unit(stop("model failed"), "member", "m1", keep_traces = TRUE)

    expect_type(out$records$trace[[1]], "character")
    expect_gt(length(out$records$trace[[1]]), 0L)

  })

  it("never contains an internal, input or dependency error, even wrapped by a recipe step", {

    expect_error(run_unit(abort_horizons("Broken.", kind = "internal"), "member", "m1"),
                 class = "horizons_error_internal")
    expect_error(run_unit(abort_horizons("Wrong.", kind = "input"), "member", "m1"),
                 class = "horizons_error_input")
    expect_error(run_unit(abort_horizons("Missing.", kind = "dependency"), "member", "m1"),
                 class = "horizons_error_dependency")

    rec <- recipes::recipe(mpg ~ ., data = mtcars) |>
      recipes::step_mutate(boom = abort_horizons("Rows misaligned.", kind = "internal"))

    err <- expect_error(run_unit(recipes::prep(rec), "configuration", "cfg_3"))
    expect_true(rlang::cnd_inherits(err, "horizons_error_internal"))

  })

  it("contains an add-on unit's failure inside its member, and lets a bug through both", {

    member <- function(addon) {
      uq <- run_unit(addon(), "uq", "m1")
      list(fitted = TRUE, uq = uq)
    }

    out <- run_unit(member(function() stop("quantile model failed")), "member", "m1")

    expect_false(out$failed)
    expect_true(out$value$uq$failed)
    expect_equal(out$value$uq$records$message, "quantile model failed")

    expect_error(
      run_unit(member(function() abort_horizons("Broken.", kind = "internal")), "member", "m1"),
      class = "horizons_error_internal"
    )

  })

  it("names only the unit's own steps, not an outer unit's", {

    out <- unit_step("fit_members", run_unit(stop("failed"), "uq", "m1"))

    expect_true(is.na(out$records$step))

  })

  it("contains nothing under debug, so the first failure stops the call", {

    expect_error(run_unit(stop("model failed"), "configuration", "cfg_1", debug = TRUE),
                 "model failed")

  })

  it("leaves the step labels as they were after a failure", {

    before <- unit_steps$stack

    run_unit(unit_step("a", unit_step("b", stop("failed"))), "configuration", "cfg_1")

    expect_identical(unit_steps$stack, before)

  })

  it("does not record the warn setting's promotion of a unit's warning", {

    withr::local_options(warn = 2)

    out <- run_unit({ warning("noted"); "done" }, "configuration", "cfg_1")

    expect_equal(out$value, "done")
    expect_equal(out$records$type, "warning")

  })

})
