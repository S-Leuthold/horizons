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
    expect_identical(names(formals(control_horizons)), names(CONTROL_FIELDS))
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

  it("refuses parallelize_over = 'both' with the nesting hint", {

    expect_error(control_horizons(parallelize_over = "both"), "Nested parallelism",
                 class = "horizons_error_input")

  })

  it("takes FALSE or a whole number of at least 2 as the breaker", {

    expect_false(control_horizons(breaker = FALSE)$breaker)
    expect_identical(control_horizons(breaker = 5L)$breaker, 5L)
    expect_error(control_horizons(breaker = TRUE), "whole number of at least 2",
                 class = "horizons_error_input")

  })

  it("prints the fields that are set", {

    expect_output(print(control_horizons()), "every field at its default")
    expect_output(print(control_horizons(allow_par = TRUE)), "allow_par: TRUE")

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

  it("refuses anything that is not a horizons_control", {

    expect_error(resolve_control(list(allow_par = TRUE), "evaluate"), "control_horizons",
                 class = "horizons_error_input")

  })

  it("reads verbose from horizons.verbose when it is not set", {

    withr::local_options(horizons.verbose = TRUE)
    expect_true(resolve_control(NULL, "evaluate")$verbose)

    withr::local_options(horizons.verbose = FALSE)
    expect_false(resolve_control(NULL, "evaluate")$verbose)
    expect_true(resolve_control(control_horizons(verbose = TRUE), "evaluate")$verbose)

    withr::local_options(horizons.verbose = "loud")
    expect_error(resolve_control(NULL, "evaluate"), "verbose",
                 class = "horizons_error_input")

  })

  it("leaves a future.globals.maxSize the user set as the payload limit", {

    withr::local_options(future.globals.maxSize = NULL)
    expect_identical(resolve_control(NULL, "evaluate")$max_worker_payload, EVAL_WORKER_PAYLOAD_LIMIT)

    withr::local_options(future.globals.maxSize = 2 * 1024^3)
    expect_identical(resolve_control(NULL, "evaluate")$max_worker_payload, 2 * 1024^3)

    expect_identical(resolve_control(control_horizons(max_worker_payload = 5e8), "evaluate")$max_worker_payload,
                     5e8)

  })

})
