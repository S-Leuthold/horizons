## ---------------------------------------------------------------------------
## Tests: control_horizons() and resolve_control()
## ---------------------------------------------------------------------------
## The constructor checks what it is given; resolve_control() gives each verb
## its own fields with the defaults filled in, and refuses a field set that
## the verb does not use. The verbs' use of the fields is tested with each
## verb.


describe("control_horizons()", {

  it("has one argument per row of the field table, every one NULL by default", {

    ctrl <- control_horizons()

    expect_s3_class(ctrl, "horizons_control")
    expect_identical(names(formals(control_horizons)), c("...", names(CONTROL_FIELDS)))
    expect_identical(names(ctrl), names(CONTROL_FIELDS))
    expect_true(all(vapply(unclass(ctrl), is.null, logical(1))))

  })

  it("names only verbs that take a control object", {

    used <- unique(unlist(lapply(CONTROL_FIELDS, `[[`, "verbs")))

    expect_true(all(used %in% CONTROL_VERBS))

    for (verb in CONTROL_VERBS) {
      expect_true("control" %in% names(formals(get(verb))), label = verb)
    }

  })

  it("refuses a value outside each field's domain, naming the field", {

    bad <- list(
      allow_par            = "yes",
      parallelize_over     = "folds",
      verbose              = NA,
      output_dir           = "",
      debug                = 1,
      breaker              = 1,
      keep_traces          = c(TRUE, FALSE),
      chunk_size           = 0.5,
      min_calib_rows       = 1,
      min_ad_rows          = "10",
      min_degradation_rows = -5,
      min_resemblance_rows = NA_real_,
      max_worker_payload   = 0
    )

    expect_setequal(names(bad), names(CONTROL_FIELDS))

    for (field in names(bad)) {

      err <- expect_error(do.call(control_horizons, bad[field]),
                          class = "horizons_error_input", label = field)
      expect_match(conditionMessage(err), field, fixed = TRUE, label = field)

    }

  })

  it("takes its fields by name only", {

    expect_error(control_horizons(TRUE), "1 value was passed by position",
                 class = "horizons_error_input")
    expect_error(control_horizons(alow_par = TRUE), "no field alow_par",
                 class = "horizons_error_input")

  })

  it("refuses parallelize_over = 'both' with the nesting hint, and anything but one axis", {

    expect_error(control_horizons(parallelize_over = "both"), "Nested parallelism",
                 class = "horizons_error_input")
    expect_error(control_horizons(parallelize_over = c("configs", "resamples")), "parallelize_over",
                 class = "horizons_error_input")

  })

  it("floors each row minimum at what its computation needs", {

    ## A 90% split-conformal interval needs 9 calibration rows
    expect_error(control_horizons(min_calib_rows = 8L), "larger than or equal to 9",
                 class = "horizons_error_input")
    expect_identical(control_horizons(min_calib_rows = 9L)$min_calib_rows, 9L)

    expect_error(control_horizons(min_degradation_rows = 4L), "larger than or equal to 5",
                 class = "horizons_error_input")
    expect_error(control_horizons(min_ad_rows = 1L), "larger than or equal to 2",
                 class = "horizons_error_input")

  })

  it("takes FALSE or a whole number of at least 2 as the breaker", {

    expect_false(control_horizons(breaker = FALSE)$breaker)
    expect_identical(control_horizons(breaker = 5L)$breaker, 5L)

    ## Each refusal names what it was given
    expect_error(control_horizons(breaker = TRUE), "whole number of at least 2, not TRUE\\.",
                 class = "horizons_error_input")
    expect_error(control_horizons(breaker = 2.5), "not 2\\.5\\.", class = "horizons_error_input")
    expect_error(control_horizons(breaker = c(3, 4)), "not a double\\s+vector",
                 class = "horizons_error_input")

  })

  it("words a refusal as the user's call, naming the field", {

    expect_snapshot(control_horizons(min_calib_rows = 8L), error = TRUE)
    expect_snapshot(control_horizons(TRUE, alow_par = TRUE), error = TRUE)
    expect_snapshot(control_horizons(parallelize_over = "both"), error = TRUE)
    expect_snapshot(evaluate(make_eval_object(), control = control_horizons(chunk_size = 10L)),
                    error = TRUE)
    expect_snapshot(evaluate(make_eval_object(), control = list(allow_par = TRUE)), error = TRUE)

    ctrl <- control_horizons()
    ctrl$alow_par <- TRUE
    expect_snapshot(evaluate(make_eval_object(), control = ctrl), error = TRUE)

  })

  it("prints the fields that are set, and returns the control invisibly", {

    expect_output(print(control_horizons()), "every field at its default")
    expect_output(print(control_horizons(allow_par = TRUE)), "allow_par: TRUE")
    expect_snapshot(print(control_horizons(allow_par = TRUE, parallelize_over = "resamples", chunk_size = 10L)))

    ctrl <- control_horizons(allow_par = TRUE)
    expect_invisible(out <- withr::with_output_sink(nullfile(), print(ctrl)))
    expect_identical(out, ctrl)

  })

})


describe("resolve_control()", {

  it("gives each verb its own fields, with the defaults filled in", {

    for (verb in CONTROL_VERBS) {

      resolved <- resolve_control(NULL, verb)
      used     <- names(CONTROL_FIELDS)[vapply(CONTROL_FIELDS, function(f) verb %in% f$verbs, logical(1))]

      expect_s3_class(resolved, "horizons_control")
      expect_identical(names(resolved), used, label = verb)

    }

    fit_ctrl <- resolve_control(NULL, "fit")
    expect_false(fit_ctrl$allow_par)
    expect_false(fit_ctrl$keep_traces)
    expect_identical(fit_ctrl$breaker, BREAKER_FAILURES)
    expect_identical(fit_ctrl$min_calib_rows, N_CALIB_MIN)

  })

  it("keeps a value that is set", {

    resolved <- resolve_control(control_horizons(allow_par = TRUE, min_calib_rows = 12L), "fit")

    expect_true(resolved$allow_par)
    expect_identical(resolved$min_calib_rows, 12L)

  })

  it("refuses a field set that the verb does not use, naming the verbs that do", {

    err <- expect_error(resolve_control(control_horizons(output_dir = "out", chunk_size = 10L), "fit"),
                        class = "horizons_error_input")

    expect_match(conditionMessage(err), "output_dir: evaluate()", fixed = TRUE)
    expect_match(conditionMessage(err), "chunk_size: select_training()", fixed = TRUE)

  })

  it("refuses anything that is not a horizons_control, and a field it does not know", {

    expect_error(resolve_control(list(allow_par = TRUE), "evaluate"), "control_horizons",
                 class = "horizons_error_input")

    ## An edit after the constructor, with a typo
    ctrl <- control_horizons()
    ctrl$alow_par <- TRUE
    expect_error(resolve_control(ctrl, "evaluate"), "field alow_par",
                 class = "horizons_error_input")

  })

  it("reads verbose from horizons.verbose when it is not set, and is verbose without it", {

    ## The suite's setup sets the option to FALSE
    withr::local_options(horizons.verbose = NULL)
    expect_true(resolve_control(NULL, "evaluate")$verbose)

    withr::local_options(horizons.verbose = TRUE)
    expect_true(resolve_control(NULL, "evaluate")$verbose)

    withr::local_options(horizons.verbose = FALSE)
    expect_false(resolve_control(NULL, "evaluate")$verbose)
    expect_true(resolve_control(control_horizons(verbose = TRUE), "evaluate")$verbose)

    ## A bad option is reported as the option's, and a set field still wins
    withr::local_options(horizons.verbose = "loud")
    expect_error(resolve_control(NULL, "evaluate"), "getOption(\"horizons.verbose\")",
                 fixed = TRUE, class = "horizons_error_input")
    expect_false(resolve_control(control_horizons(verbose = FALSE), "evaluate")$verbose)

  })

  it("keeps a larger future.globals.maxSize as the payload limit, not tune's smaller one, and reports a bad one as the option's", {

    withr::local_options(future.globals.maxSize = NULL)
    expect_identical(resolve_control(NULL, "evaluate")$max_worker_payload, EVAL_WORKER_PAYLOAD_LIMIT)

    ## What tune sets when it loads
    withr::local_options(future.globals.maxSize = 1e9)
    expect_identical(resolve_control(NULL, "evaluate")$max_worker_payload, EVAL_WORKER_PAYLOAD_LIMIT)

    withr::local_options(future.globals.maxSize = 2 * 1024^3)
    expect_identical(resolve_control(NULL, "evaluate")$max_worker_payload, 2 * 1024^3)

    ## A bad option is reported as the option's, and a set field still wins
    withr::local_options(future.globals.maxSize = NA_real_)
    expect_error(resolve_control(NULL, "evaluate"), "getOption(\"future.globals.maxSize\")",
                 fixed = TRUE, class = "horizons_error_input")
    expect_identical(resolve_control(control_horizons(max_worker_payload = 5e8), "evaluate")$max_worker_payload,
                     5e8)

  })

})
