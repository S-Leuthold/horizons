## ---------------------------------------------------------------------------
## Tests: the log, the collector, run_unit() and unit_step()
## ---------------------------------------------------------------------------

describe("the collector", {

  it("closes onto the log as one row per topic, in the order topics first arose", {

    col <- log_open("evaluate")
    log_warning(col, "partial_failure", "3 of 40 configurations failed.", ids = c("a", "b", "c"))
    log_warning(col, "quality", "The smallest assessment fold has 8 rows.")
    log_warning(col, "quality", "Every member is below the prune threshold.", ids = "m1")

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
    log_warning(first, "flagged", "3 samples are non-finite.", ids = c("s1", "s2", "s3"))
    log <- log_close(first)

    expect_identical(log_close(log_open("average"), log), log)

    second <- log_open("evaluate")
    log_warning(second, "quality", "Small folds.")

    expect_equal(log_close(second, log)$verb, c("standardize", "evaluate"))

  })

  it("stamps every row of a call with the one time it closed, and leaves earlier rows' times alone", {

    first <- log_open("spectra")
    log_note(first, "Read a data frame.")
    log <- log_close(first)

    Sys.sleep(0.05)

    second <- log_open("average")
    log_warning(second, "quality", "1 sample averaged though every replicate failed QC.", ids = "s3")
    log_note(second, "Averaged 6 scans into 2 samples.")

    before <- Sys.time()
    log2   <- log_close(second, log)
    after  <- Sys.time()

    expect_s3_class(log2$time, "POSIXct")
    expect_false(anyNA(log2$time))

    ### The earlier call's row keeps its time; this call's rows share one
    expect_identical(log2$time[1], log$time[1])
    expect_identical(log2$time[2], log2$time[3])
    expect_true(log2$time[2] >= before && log2$time[2] <= after)
    expect_true(log2$time[2] > log2$time[1])

    ### The warning carries its row as the log holds it, time included
    w <- expect_warning(signal_log(second), class = "horizons_warning_quality")
    expect_identical(w$log$time, log2$time[2])

  })

  it("renders the message where it is added, and keeps upstream text as text", {

    n_failed <- 2L
    cause    <- "column {wn_600} could not be read"

    col <- log_open("evaluate")
    log_warning(col, "partial_failure", c("{n_failed} configurations failed.", "x" = "{cause}"))

    log <- log_close(col)

    expect_match(log$message, "2 configurations failed.", fixed = TRUE)
    expect_match(log$message, "column {wn_600} could not be read", fixed = TRUE)
    expect_warning(signal_log(col), "{wn_600}", fixed = TRUE,
                   class = "horizons_warning_partial_failure")

  })

  it("refuses a topic it does not know, as a bug in its caller", {

    expect_error(log_warning(log_open("fit"), "misc", "x"), "topic",
                 class = "horizons_error_internal")

  })

  it("keeps a note as a row of its own, beside the warnings, in the order they arose", {

    col <- log_open("average")
    log_note(col, "2 samples dropped: every replicate failed QC.", ids = c("s1", "s2"))
    log_warning(col, "quality", "1 sample averaged though every replicate failed QC.", ids = "s3")
    log_note(col, "1 metadata column dropped: it varies within a sample.", ids = "site")

    log <- log_close(col)

    expect_equal(log$type, c("note", "warning", "note"))
    expect_equal(log$topic, c(NA, "quality", NA))
    expect_equal(log$ids, list(c("s1", "s2"), "s3", "site"))

  })

  it("never raises a note", {

    col <- log_open("average")
    log_note(col, "2 samples dropped as asked.", ids = c("s1", "s2"))
    log_warning(col, "quality", "1 sample averaged though every replicate failed QC.", ids = "s3")
    log_close(col)

    raised <- list()
    withCallingHandlers(
      signal_log(col),
      warning = function(w) {
        raised[[length(raised) + 1L]] <<- w
        invokeRestart("muffleWarning")
      }
    )

    expect_length(raised, 1L)
    expect_s3_class(raised[[1]], "horizons_warning_quality")

    only_notes <- log_open("average")
    log_note(only_notes, "2 samples dropped as asked.", ids = c("s1", "s2"))
    log_close(only_notes)

    expect_no_warning(signal_log(only_notes))

    ## Nor does warn = 2 promote one
    notes_under_warn2 <- log_open("average")
    log_note(notes_under_warn2, "2 samples dropped as asked.", ids = c("s1", "s2"))
    log_close(notes_under_warn2)

    expect_no_error(withr::with_options(list(warn = 2), signal_log(notes_under_warn2)))

  })

  it("refuses, as a bug in the verb, to add to or close a collector twice", {

    col <- log_open("fit")
    log_close(col)

    expect_error(log_warning(col, "quality", "Late."), "after it was closed",
                 class = "horizons_error_internal")
    expect_error(log_close(col), "closed twice", class = "horizons_error_internal")

  })

})

describe("signal_log()", {

  closed <- function(verb, ...) {
    col <- log_open(verb)
    for (event in list(...)) do.call(log_warning, c(list(col), event))
    log_close(col)
    col
  }

  it("raises one warning per topic, each carrying its log row", {

    col <- closed("evaluate",
                  list("partial_failure", "3 of 40 configurations failed.", ids = c("a", "b", "c")),
                  list("quality", "Small folds."),
                  list("quality", "Weak members."))

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

    expect_no_warning(signal_log(closed("fit")))

  })

  it("raises only what log_close() wrote, once", {

    col <- log_open("fit")
    log_warning(col, "skipped", "Intervals were not built for 2 members.", ids = c("m1", "m2"))

    expect_error(signal_log(col), "before", class = "horizons_error_internal")

    log <- log_close(col)
    suppressWarnings(signal_log(col))

    expect_equal(log$ids, list(c("m1", "m2")))
    expect_error(signal_log(col), "twice", class = "horizons_error_internal")

  })

  it("aborts under warn = 2 with the rows and the finished object, rather than losing them", {

    col <- closed("evaluate",
                  list("partial_failure", "3 of 40 configurations failed.", ids = c("a", "b", "c")))

    err <- expect_error(
      withr::with_options(list(warn = 2), signal_log(col, object = list(result = "kept"))),
      class = "horizons_error_promoted_warnings", inherit = FALSE
    )

    expect_s3_class(err, "horizons_error_run")
    expect_equal(err$object$result, "kept")
    expect_equal(err$log$topic, "partial_failure")

  })

  it("lets the caller's handlers take a warning before warn = 2 promotes it", {

    events <- list(list("partial_failure", "3 of 40 configurations failed.", ids = "a"),
                   list("quality", "Small folds."))

    ## Muffled: nothing is left to promote
    expect_no_error(withr::with_options(list(warn = 2),
      suppressWarnings(signal_log(do.call(closed, c("evaluate", events))))))

    ## Caught: the handler unwinds, as it would for any warning
    caught <- withr::with_options(list(warn = 2), tryCatch(
      signal_log(do.call(closed, c("evaluate", events))),
      horizons_warning_partial_failure = function(w) "caught"
    ))
    expect_identical(caught, "caught")

    ## One topic muffled: only the other is promoted
    err <- expect_error(withr::with_options(list(warn = 2), withCallingHandlers(
      signal_log(do.call(closed, c("evaluate", events))),
      horizons_warning_partial_failure = function(w) invokeRestart("muffleWarning")
    )), class = "horizons_error_promoted_warnings")
    expect_equal(err$log$topic, "quality")

  })

  it("words a multi-warning topic and the warn = 2 error readably", {

    events <- list(list("partial_failure", "3 of 40 configurations failed.", ids = c("a", "b", "c")),
                   list("quality", "The smallest assessment fold has 8 rows."),
                   list("quality", "Every member is below the prune threshold."))

    expect_snapshot(signal_log(do.call(closed, c("evaluate", events))))

    ### warn = 2 only around the call, so testthat's own warnings stay warnings.
    expect_snapshot(
      withr::with_options(list(warn = 2), signal_log(do.call(closed, c("evaluate", events)))),
      error = TRUE
    )

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

  it("keeps a record's cause and location apart, as the message joins them", {

    rec <- recipes::recipe(mpg ~ ., data = mtcars) |>
      recipes::step_mutate(boom = stop("singular matrix"))

    out <- run_unit(recipes::prep(rec), "configuration", "cfg_2")

    expect_identical(out$records$cause, "singular matrix")
    expect_identical(out$records$where, "`step_mutate()`")
    expect_identical(out$records$message, "singular matrix (in `step_mutate()`)")

    clean <- run_unit(warning("no wrapper"), "configuration", "cfg_1")
    expect_identical(clean$records$where, NA_character_)

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

  it("finds no backtrace in a chain that holds none, however deep", {

    expect_null(chain_trace(simpleError("no trace here")))

    ## Deeper than the walk's bound, as a chain that looped would be
    deep <- simpleError("0")
    for (i in 1:60) {
      outer        <- simpleError(as.character(i))
      outer$parent <- deep
      deep         <- outer
    }
    expect_null(chain_trace(deep))

  })

  it("keeps the backtrace of a wrapped failure, which its parent holds", {

    ## recipes wraps a step's error, and rlang then traces the parent only
    rec <- recipes::recipe(mpg ~ ., data = mtcars) |>
      recipes::step_mutate(boom = stop("singular matrix"))

    out <- run_unit(recipes::prep(rec), "configuration", "cfg_2", keep_traces = TRUE)

    expect_type(out$records$trace[[1]], "character")
    expect_true(any(grepl("prep", out$records$trace[[1]], fixed = TRUE)))

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

  it("raises a package missing where the unit ran as horizons_error_missing_package, chained, as base R or rlang reports it, and wrapped by a recipe step", {

    ## base R's packageNotFoundError, as library() raises it on tune's workers
    err <- expect_error(run_unit(loadNamespace("horizonsFakePkgXYZ123"), "configuration", "cfg_1"),
                        class = "horizons_error_missing_package", inherit = FALSE)

    expect_s3_class(err, "horizons_error_dependency")
    expect_s3_class(err$parent, "packageNotFoundError")
    msg <- gsub("\\s+", " ", conditionMessage(err))
    expect_match(msg, "A package needed by configuration \"cfg_1\" is not installed where it ran.", fixed = TRUE)
    expect_match(msg, "horizonsFakePkgXYZ123", fixed = TRUE)

    ## rlang's, as a step's check_installed() raises it
    rlang::local_interactive(FALSE)
    err <- expect_error(run_unit(rlang::check_installed("horizonsFakePkgXYZ123"), "meta_learner", "weighted"),
                        class = "horizons_error_missing_package", inherit = FALSE)
    expect_true(rlang::cnd_inherits(err, "rlib_error_package_not_found"))
    expect_match(conditionMessage(err), "meta learner", fixed = TRUE)

    ## Inside a recipe step, which wraps it
    rec <- recipes::recipe(mpg ~ ., data = mtcars) |>
      recipes::step_mutate(boom = loadNamespace("horizonsFakePkgXYZ123"))
    expect_error(run_unit(recipes::prep(rec), "configuration", "cfg_2"),
                 class = "horizons_error_missing_package", inherit = FALSE)

    ## An error that only names a package is the unit's failure
    out <- run_unit(stop("a column called package is missing"), "configuration", "cfg_3")
    expect_true(out$failed)
    expect_identical(out$records$class, "simpleError")

  })

  it("never contains a worker's death, which future raises as a FutureError", {

    died <- structure(class = c("FutureInterruptError", "FutureError", "error", "condition"),
                      list(message = "A future worker died.", call = NULL))

    expect_error(run_unit(stop(died), "configuration", "cfg_1"),
                 class = "FutureInterruptError")

  })

  it("never contains one that was chained to an upstream parent", {

    ## try_fetch() offers a declined condition's parent to the same handler,
    ## so a dependency error chained to a simpleError must still propagate
    upstream <- simpleError("could not find function \"xgb.train\"")

    err <- expect_error(
      run_unit(abort_horizons("Missing on a worker.", kind = "dependency", parent = upstream),
               "configuration", "cfg_7"),
      class = "horizons_error_dependency", inherit = FALSE
    )
    expect_identical(err$parent, upstream)

    expect_error(
      run_unit(run_unit(abort_horizons("Broken.", kind = "internal", parent = upstream), "uq", "m1"),
               "member", "m1"),
      class = "horizons_error_internal", inherit = FALSE
    )

  })

  it("raises an internal error, rather than a unit failure, when a record cannot be built", {

    local_mocked_bindings(condition_parts = function(...) stop("summary broke"))

    expect_error(run_unit({ warning("noted"); 1 }, "configuration", "cfg_1"),
                 "Recording a warning", class = "horizons_error_internal")
    expect_error(run_unit(stop("model failed"), "configuration", "cfg_1"),
                 "Recording a failure", class = "horizons_error_internal")

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

  it("contains nothing under debug, so the first failure stops the call, and still records warnings", {

    expect_error(run_unit(stop("model failed"), "configuration", "cfg_1", debug = TRUE),
                 "model failed")

    out <- expect_no_warning(
      run_unit(unit_step("fit", { warning("did not converge"); "fitted" }),
               "configuration", "cfg_1", debug = TRUE)
    )

    expect_equal(out$value, "fitted")
    expect_identical(out$failed, FALSE)
    expect_equal(out$records$step, "fit")
    expect_identical(out$records$type, "warning")
    expect_equal(out$records$message, "did not converge")

  })

  it("refuses a unit or id that is not a single string, as a bug in its caller", {

    expect_error(run_unit(1, "configuration", 7L),
                 "`unit` and `id` must each be a single string.", fixed = TRUE,
                 class = "horizons_error_internal")

    ## Inside an outer unit, the bad inner id is a bug, not the outer unit's failure
    expect_error(run_unit(run_unit(1, "uq", NULL), "member", "m1"),
                 class = "horizons_error_internal")

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

describe("warn_in_unit() and log_unit_records()", {

  it("raises a horizons warning that a unit records under its topic's class", {

    cause <- "column {wn_600} could not be read"

    out <- run_unit({
      warn_in_unit("substitution", "Fell back: {cause}")
      "done"
    }, "configuration", "cfg_1")

    expect_equal(out$value, "done")
    expect_identical(out$records$class, "horizons_warning_substitution")
    expect_identical(out$records$message, "Fell back: column {wn_600} could not be read")

    ## Outside a unit it is an ordinary warning of its topic
    expect_warning(warn_in_unit("quality", "Weak."), "Weak.",
                   class = "horizons_warning_quality")

  })

  it("refuses a topic it does not know, as a bug in its caller", {

    expect_error(warn_in_unit("misc", "x"), "topic", class = "horizons_error_internal")

  })

  it("lifts only horizons warnings, one event per distinct message, naming the units that raised it", {

    unit <- function(id, expr) run_unit(expr, "configuration", id)$records

    records <- dplyr::bind_rows(
      unit("cfg_1", { warn_in_unit("substitution", "Kept the grid's choice."); warning("ranger warned") }),
      unit("cfg_2", warn_in_unit("substitution", "Kept the grid's choice.")),
      unit("cfg_3", warn_in_unit("substitution", "Kept {.val braces} as text.")),
      unit("cfg_4", stop("failed"))
    )

    col <- log_open("evaluate")
    log_unit_records(col, records)
    log_unit_records(col, NULL)
    log <- log_close(col)

    ## One topic, one row: the other package's warning and the failure stay
    ## on their units' rows
    expect_identical(log$topic, "substitution")
    expect_identical(log$ids[[1]], c("cfg_1", "cfg_2", "cfg_3"))
    expect_identical(log$n, 3L)
    expect_match(log$message, "Kept the grid's choice. In 2 configurations: \"cfg_1\" and \"cfg_2\".",
                 fixed = TRUE)
    expect_match(log$message, "Kept \"braces\" as text. In 1 configuration: \"cfg_3\".",
                 fixed = TRUE)
    expect_false(grepl("ranger warned", log$message, fixed = TRUE))

  })

  it("names an ensemble's units for what they are, not by their internal names", {

    records <- dplyr::bind_rows(
      run_unit(warn_in_unit("quality", "The meta-learner warned."), "meta_learner", "penalized")$records,
      run_unit(warn_in_unit("quality", "The intervals warned."), "intervals", "ensemble")$records,
      run_unit(warn_in_unit("quality", "The combination warned."), "combination", "weighted")$records
    )

    col <- log_open("ensemble")
    log_unit_records(col, records)
    log <- log_close(col)

    expect_match(log$message, "The meta-learner warned. In the ensemble's meta-learner (\"penalized\").",
                 fixed = TRUE, all = FALSE)
    expect_match(log$message, "The intervals warned. In the ensemble's prediction intervals.",
                 fixed = TRUE, all = FALSE)
    expect_match(log$message, "The combination warned. In the ensemble's combination (\"weighted\").",
                 fixed = TRUE, all = FALSE)
    expect_false(any(grepl("meta_learner|In 1 ", log$message)))

  })

})

describe("unit_failure_message()", {

  it("names the step a unit failed in, the unit itself outside every step, or neither", {

    in_step <- run_unit(unit_step("final_fit", stop("no fit")), "member", "cfg_1")$records
    outside <- run_unit(stop("no data"), "ad", "cfg_1")$records

    expect_match(unit_failure_message(in_step, "Fit"), "^Final model fit failed: no fit")
    expect_match(unit_failure_message(outside, "Fit"), "^Fit failed: no data")

    ## An add-on's own condition says what failed, so it stands alone
    expect_match(unit_failure_message(outside, NULL), "^no data")

  })

})

describe("note_in_unit()", {

  it("is recorded as a note by the unit it is raised in, and silent outside one", {

    out <- run_unit({
      note_in_unit("Stored whole: {.val {'butcher'}} failed")
      "done"
    }, "member", "cfg_1")

    expect_equal(out$value, "done")
    expect_identical(out$records$type, "note")
    expect_identical(out$records$class, "horizons_note")
    expect_identical(out$records$message, "Stored whole: \"butcher\" failed")

    ## Under debug too
    debugged <- run_unit(note_in_unit("A note."), "member", "cfg_1", debug = TRUE)
    expect_identical(debugged$records$type, "note")

    ## Outside a unit nothing handles it: no message, no warning
    expect_silent(note_in_unit("Nobody listens."))

    ## The same text whatever colours the session reports: cli quotes a file
    ## name only when it cannot colour it
    withr::local_envvar(R_CLI_NUM_COLORS = "256")
    coloured <- run_unit(note_in_unit("Read {.file notes.txt}."), "member", "cfg_1")
    expect_identical(coloured$records$message, "Read 'notes.txt'.")

  })

  it("is recorded once, by the innermost unit, not again by an enclosing one", {

    inner <- NULL
    outer <- run_unit({
      inner <- run_unit(note_in_unit("Inner note."), "uq", "cfg_1")
      "done"
    }, "member", "cfg_1")

    expect_identical(inner$records$type, "note")
    expect_identical(nrow(outer$records), 0L)

    ## Under debug too, where an add-on's unit runs inside its parent's
    inner <- NULL
    outer <- run_unit({
      inner <- run_unit(note_in_unit("Inner note."), "uq", "cfg_1", debug = TRUE)
      "done"
    }, "member", "cfg_1", debug = TRUE)

    expect_identical(inner$records$type, "note")
    expect_identical(nrow(outer$records), 0L)

  })

  it("is lifted to the log as a note, one per distinct message, beside the warnings", {

    unit <- function(id, expr) run_unit(expr, "member", id)$records

    records <- dplyr::bind_rows(
      unit("cfg_1", { note_in_unit("Stored whole."); warn_in_unit("skipped", "No UQ.") }),
      unit("cfg_2", note_in_unit("Stored whole."))
    )

    col <- log_open("fit")
    log_unit_records(col, records)
    log <- log_close(col)

    expect_identical(log$type, c("note", "warning"))
    expect_identical(log$topic, c(NA_character_, "skipped"))
    expect_identical(log$ids[[1]], c("cfg_1", "cfg_2"))
    expect_match(log$message[1], "Stored whole. In 2 members", fixed = TRUE)

    ## A note is recorded, never raised: the one warning is the skipped one
    raised <- list()
    withCallingHandlers(signal_log(col), warning = function(w) {
      raised[[length(raised) + 1L]] <<- w
      invokeRestart("muffleWarning")
    })
    expect_length(raised, 1L)
    expect_s3_class(raised[[1]], "horizons_warning_skipped")

  })

})


## ---------------------------------------------------------------------------
## missing_package_signal(): tune's own refusals, by their text
## ---------------------------------------------------------------------------
## On the configs axis tune runs in the worker as in a session, and refuses
## at its own door, unclassed, a workflow whose packages are not installed.
## Each refusal below comes from tune's own code, so a change in its wording
## fails here rather than letting a worker's missing package read as each
## configuration's own failure again.

describe("missing_package_signal() - tune's refusals", {

  wf <- workflows::workflow() |>
    workflows::add_formula(mpg ~ .) |>
    workflows::add_model(parsnip::linear_reg() |> parsnip::set_engine("lm"))

  it("recognises tune's refusal of the packages a workflow and its recipe require", {

    set.seed(1)
    folds <- rsample::vfold_cv(mtcars, v = 2)
    run   <- function() {
      tune::fit_resamples(wf, folds, control = tune::control_resamples(pkgs = "horizonsFakePkgXYZ123"))
    }

    err <- expect_error(run(), "Some package installs are needed: horizonsFakePkgXYZ123", fixed = TRUE)

    expect_true(missing_package_signal(err))
    expect_error(run_unit(run(), "configuration", "cfg_1"),
                 class = "horizons_error_missing_package", inherit = FALSE)

    ## Anywhere in the chain: under a condition that wraps it and says
    ## something else
    expect_true(missing_package_signal(
      rlang::catch_cnd(rlang::abort("The recipe step failed.", parent = err))
    ))
    expect_error(run_unit(rlang::abort("The recipe step failed.", parent = err),
                          "configuration", "cfg_1"),
                 class = "horizons_error_missing_package", inherit = FALSE)

  })

  it("recognises tune's refusal of a model's engine packages", {

    set.seed(1)
    folds <- rsample::vfold_cv(mtcars, v = 2)

    ## The lm engine's dependencies gain a package installed nowhere
    real <- parsnip::get_dependency
    local_mocked_bindings(get_dependency = function(model) {
      deps     <- real(model)
      deps$pkg <- lapply(deps$pkg, function(p) c(p, "horizonsFakePkgXYZ123"))
      deps
    }, .package = "parsnip")

    err <- expect_error(tune::fit_resamples(wf, folds),
                        "Package install is required for horizonsFakePkgXYZ123.", fixed = TRUE)

    expect_true(missing_package_signal(err))

  })

  it("reads no other failure as a missing package", {

    expect_false(missing_package_signal(simpleError("A package install directory was not set.")))
    expect_false(missing_package_signal(rlang::catch_cnd(rlang::abort("Grid search failed: singular matrix"))))

  })

})

