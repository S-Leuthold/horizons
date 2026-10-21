## ---------------------------------------------------------------------------
## Tests: Error handling utilities
## ---------------------------------------------------------------------------

describe("create_failed_result()", {

  it("creates a single-row tibble with expected columns", {

    result <- create_failed_result(
      config_id = "CFG_abc123",
      error     = simpleError("model diverged")
    )

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 1)
    expect_true("config_id" %in% names(result))
    expect_true("status" %in% names(result))
    expect_true("error_message" %in% names(result))

    ## Status failed, the error's message kept, every metric and the
    ## runtime a double NA (so the row binds with evaluate()'s scored rows),
    ## and best_params a list holding NULL
    expect_equal(result$status, "failed")
    expect_equal(result$error_message, "model diverged")

    for (col in c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae",
                  "cv_rmse", "cv_rrmse", "cv_rsq", "cv_ccc", "cv_rpd", "cv_mae",
                  "runtime_secs")) {
      expect_identical(result[[col]], NA_real_, info = col)
    }

    expect_true("best_params" %in% names(result))
    expect_true(is.list(result$best_params))
    expect_null(result$best_params[[1]])

    ## The failure's class and backtrace, which a unit's failure fills in
    expect_identical(result$class, "simpleError")
    expect_true(is.list(result$trace))
    expect_null(result$trace[[1]])

  })

  it("handles NULL error gracefully", {

    result <- create_failed_result(
      config_id = "CFG_001",
      error     = NULL
    )

    expect_equal(result$status, "failed")
    expect_identical(result$error_message, NA_character_)

  })

  it("records a string error as it is, and any other value as text", {

    result <- create_failed_result(
      config_id = "CFG_001",
      error     = "something went wrong"
    )

    expect_equal(result$error_message, "something went wrong")

    expect_identical(create_failed_result("CFG_001", 42)$error_message, "42")

  })

})

## ---------------------------------------------------------------------------
## distinct_config_errors() — finished, brace-safe cli bullets
## ---------------------------------------------------------------------------
## The helper is the one place the brace-safety rule is enforced for the
## all-failed aborts, so its bullets must render literally whatever the
## upstream text carries.

describe("distinct_config_errors()", {

  results <- tibble::tibble(
    config_id     = c("a", "b", "c", "d", "e", "f"),
    error_message = c("Grid {wn_600} failed", "Grid {wn_600} failed", NA,
                      "second", "third", "fourth")
  )

  bullets <- distinct_config_errors(results)

  it("returns one x bullet per shown message and an i bullet for the rest", {

    expect_identical(names(bullets), c("x", "x", "x", "i"))
    expect_identical(bullets[["i"]], "1 more distinct error message not shown.")

  })

  it("renders upstream braces literally through cli", {

    expect_identical(cli::format_inline(bullets[[1]]), "Grid {wn_600} failed (a, b)")

    err <- tryCatch(cli::cli_abort(c("header", bullets)), error = function(e) e)
    expect_match(conditionMessage(err), "Grid {wn_600} failed (a, b)", fixed = TRUE)

  })

  it("is empty when no row has a message", {

    expect_length(distinct_config_errors(results[3, ]), 0)
    expect_length(distinct_config_errors(tibble::tibble(config_id = "a")), 0)

  })

})


## ---------------------------------------------------------------------------
## check_rows_aligned() / abort_misaligned() — positional binds (#140)
## ---------------------------------------------------------------------------

describe("check_rows_aligned()", {

  it("is silent when the counts match, and the ids where both sides have them", {

    expect_no_error(check_rows_aligned("Values", "the rows", n = 5L, n_expected = 5L))
    expect_null(check_rows_aligned("Values", "the rows", n = 5L, n_expected = 5L))

    ## The ids match position by position
    ids <- c("a", "b", "c")

    expect_no_error(check_rows_aligned("Values", "the rows",
                                       ids = ids, expected_ids = ids))

    ## Either side has no ids: the count alone is checked
    expect_no_error(check_rows_aligned("Values", "the rows", n = 2L, n_expected = 2L,
                                       ids = NULL, expected_ids = c("a", "b")))

  })

  it("aborts with an internal error naming the bind and both counts", {

    err <- expect_error(
      check_rows_aligned("Interval columns for config 'cfg_1'", "the rows of new_data",
                         n = 7L, n_expected = 8L),
      class = "horizons_error_internal"
    )

    msg <- conditionMessage(err)
    expect_match(msg, "Interval columns for config 'cfg_1'", fixed = TRUE)
    expect_match(msg, "the rows of new_data", fixed = TRUE)
    expect_match(msg, "Got 7 rows for 8", fixed = TRUE)
    expect_match(msg, "internal error that was detected in the horizons package", fixed = TRUE)

  })

  it("aborts when the same ids are in a different order, naming the first", {

    err <- expect_error(
      check_rows_aligned("Provenance columns", "the drawn rows",
                         ids = c("a", "c", "b"), expected_ids = c("a", "b", "c")),
      class = "horizons_error_internal"
    )

    msg <- conditionMessage(err)
    expect_match(msg, "2 of 3 positions", fixed = TRUE)
    expect_match(msg, "row 2", fixed = TRUE)

  })

  it("checks the count before the order", {

    expect_error(
      check_rows_aligned("Values", "the rows", ids = c("a", "b"),
                         expected_ids = c("a", "b", "c")),
      "Got 2 rows for 3", class = "horizons_error_internal"
    )

  })

  it("treats an NA id against a real one as a difference", {

    expect_error(
      check_rows_aligned("Values", "the rows", ids = c("a", NA),
                         expected_ids = c("a", "b")),
      class = "horizons_error_internal"
    )

  })

  it("reports the caller's frame, not its own", {

    caller <- function() check_rows_aligned("Values", "the rows", n = 1L, n_expected = 2L)
    err    <- tryCatch(caller(), error = function(e) e)

    expect_identical(as.character(err$call[[1]]), "caller")

  })

  it("interpolates ids and labels as values, so braces survive", {

    err <- expect_error(
      check_rows_aligned("Values for config '{x}'", "the rows",
                         ids = c("{a}", "b"), expected_ids = c("b", "{a}")),
      class = "horizons_error_internal"
    )

    expect_match(conditionMessage(err), "config '{x}'", fixed = TRUE)
    expect_match(conditionMessage(err), "{a}", fixed = TRUE)

  })

})

describe("abort_horizons()", {

  it("raises the kind's class under the base class, with a subclass in front", {

    err <- expect_error(
      abort_horizons("No configurations scored.", kind = "run",
                     class = "horizons_error_all_configs_failed"),
      class = "horizons_error_all_configs_failed", inherit = FALSE
    )

    expect_equal(class(err)[1:3], c("horizons_error_all_configs_failed",
                                     "horizons_error_run", "horizons_error"))

  })

  it("interpolates the message in the caller's environment", {

    n_rows <- 7L
    report <- function() abort_horizons("Got {n_rows} rows.", kind = "input")

    expect_error(report(), "Got 7 rows.", class = "horizons_error_input")

  })

  it("passes fields through to the condition", {

    err <- expect_error(
      abort_horizons("Failed.", kind = "run", records = data.frame(id = "a")),
      class = "horizons_error_run"
    )

    expect_equal(err$records$id, "a")

  })

  it("adds rlang's internal-error footer only to the internal kind", {

    internal <- expect_error(abort_horizons("Broken.", kind = "internal"),
                             class = "horizons_error_internal")
    input    <- expect_error(abort_horizons("Wrong.", kind = "input"),
                             class = "horizons_error_input")

    expect_match(conditionMessage(internal), "internal error", fixed = TRUE)
    expect_no_match(conditionMessage(input), "internal error", fixed = TRUE)

  })

  it("refuses a kind or subclass it does not know, as a bug in its caller", {

    expect_error(abort_horizons("x", kind = "user"), "kind",
                 class = "horizons_error_internal")
    expect_error(abort_horizons("x", kind = "run", class = "my_error"), "class",
                 class = "horizons_error_internal")
    expect_error(abort_horizons("x", kind = "run", class = "horizons_error_input"),
                 "class", class = "horizons_error_internal")

  })

})

describe("check_arg()", {

  it("raises rlang's message with horizons' input class, naming the user's argument", {

    verb <- function(allow_par) check_arg(rlang::check_bool, allow_par)

    err <- expect_error(verb("yes"), class = "horizons_error_input")

    expect_match(conditionMessage(err), "`allow_par` must be `TRUE` or `FALSE`",
                 fixed = TRUE)
    expect_equal(rlang::call_name(err$call), "verb")

  })

  it("passes the checker's options through and returns quietly on a valid value", {

    expect_no_error(check_arg(rlang::check_number_whole, 3, min = 1))
    expect_no_error(check_arg(rlang::check_bool, NULL, allow_null = TRUE))
    expect_error(check_arg(rlang::check_number_whole, 0, min = 1),
                 class = "horizons_error_input")

  })

})
