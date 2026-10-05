## ---------------------------------------------------------------------------
## Tests: Error handling utilities (safely_execute, handle_results)
## ---------------------------------------------------------------------------

describe("safely_execute()", {

  it("returns result on successful evaluation", {

    result <- safely_execute({ 1 + 1 })

    expect_equal(result$result, 2)
    expect_null(result$error)
    expect_null(result$warnings)
    expect_null(result$messages)
    expect_equal(result$n_warnings, 0)
    expect_equal(result$n_messages, 0)

  })

  it("returns default_value and captures error on failure", {

    result <- safely_execute(
      { stop("something broke") },
      default_value = NA_real_,
      log_error     = FALSE
    )

    expect_true(is.na(result$result))
    expect_false(is.null(result$error))
    expect_s3_class(result$error, "simpleError")
    expect_true(grepl("something broke", result$error$message))

  })

  it("returns NULL as default when no default_value specified", {

    result <- safely_execute(
      { stop("fail") },
      log_error = FALSE
    )

    expect_null(result$result)
    expect_false(is.null(result$error))

  })

  it("evaluates expression in the caller's environment", {

    x <- 10
    y <- 20

    result <- safely_execute(
      { x + y },
      log_error = FALSE
    )

    expect_equal(result$result, 30)

  })

  it("captures warnings when capture_conditions = TRUE", {

    result <- safely_execute(
      {
        warning("first warning")
        warning("second warning")
        42
      },
      capture_conditions = TRUE,
      log_error          = FALSE
    )

    expect_equal(result$result, 42)
    expect_null(result$error)
    expect_equal(length(result$warnings), 2)
    expect_equal(result$n_warnings, 2)
    expect_true(grepl("first warning", result$warnings[[1]]))
    expect_true(grepl("second warning", result$warnings[[2]]))

  })

  it("captures messages when capture_conditions = TRUE", {

    result <- safely_execute(
      {
        message("info message")
        "done"
      },
      capture_conditions = TRUE,
      log_error          = FALSE
    )

    expect_equal(result$result, "done")
    expect_equal(length(result$messages), 1)
    expect_equal(result$n_messages, 1)
    expect_true(grepl("info message", result$messages[[1]]))

  })

  it("does not capture conditions when capture_conditions = FALSE", {

    ## Warnings will propagate normally when not captured
    result <- suppressWarnings(safely_execute(
      {
        warning("uncaptured")
        99
      },
      capture_conditions = FALSE,
      log_error          = FALSE
    ))

    expect_equal(result$result, 99)
    expect_null(result$warnings)
    expect_equal(result$n_warnings, 0)

  })

  it("logs error as warning when log_error = TRUE", {

    expect_warning(
      safely_execute(
        { stop("broken") },
        log_error     = TRUE,
        error_message = "Model fitting failed"
      ),
      "Model fitting failed"
    )

  })

  it("does not log when log_error = FALSE", {

    expect_silent(
      safely_execute(
        { stop("broken") },
        log_error = FALSE
      )
    )

  })

  it("interpolates error_message with caller environment variables", {

    config_id <- "CFG_001"

    expect_warning(
      safely_execute(
        { stop("convergence failure") },
        log_error     = TRUE,
        error_message = "Config {config_id} failed"
      ),
      "CFG_001"
    )

  })

})

describe("handle_results()", {

  it("returns result on success", {

    safe_result <- list(
      result     = data.frame(a = 1:3),
      error      = NULL,
      warnings   = NULL,
      messages   = NULL,
      n_warnings = 0,
      n_messages = 0
    )

    result <- handle_results(safe_result)
    expect_equal(result, data.frame(a = 1:3))

  })

  it("validates input structure", {

    expect_error(
      handle_results(list(foo = "bar")),
      "Invalid safe_result"
    )

    expect_error(
      handle_results("not a list"),
      "Invalid safe_result"
    )

    expect_error(
      handle_results(NULL),
      "Invalid safe_result"
    )

  })

  it("aborts with error_title when result is NULL and abort_on_null = TRUE", {

    safe_result <- list(
      result = NULL,
      error  = simpleError("underlying failure")
    )

    err <- expect_error(
      handle_results(safe_result, error_title = "Model training failed"),
      "Model training failed"
    )

    ## The underlying error's message is in the abort too
    expect_match(conditionMessage(err), "underlying failure")

  })

  it("returns NULL when result is NULL and abort_on_null = FALSE", {

    safe_result <- list(
      result = NULL,
      error  = simpleError("underlying failure")
    )

    result <- handle_results(safe_result, abort_on_null = FALSE, silent = TRUE)
    expect_null(result)

  })

  it("includes hints in abort message", {

    safe_result <- list(
      result = NULL,
      error  = simpleError("memory exhausted")
    )

    expect_error(
      handle_results(
        safe_result,
        error_title = "Training failed",
        error_hints = c("Reduce grid size", "Try fewer covariates")
      ),
      "Training failed"
    )

  })

  it("surfaces warnings from successful results when silent = FALSE", {

    safe_result <- list(
      result     = 42,
      error      = NULL,
      warnings   = list("minor issue detected"),
      messages   = NULL,
      n_warnings = 1,
      n_messages = 0
    )

    ## Warnings from safe_result are re-emitted
    expect_warning(
      handle_results(safe_result, silent = FALSE),
      "minor issue"
    )

  })

  it("suppresses warnings when silent = TRUE", {

    safe_result <- list(
      result     = 42,
      error      = NULL,
      warnings   = list("minor issue"),
      messages   = NULL,
      n_warnings = 1,
      n_messages = 0
    )

    expect_silent(
      handle_results(safe_result, silent = TRUE)
    )

  })

})

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
    ## runtime NA, and best_params a list holding NULL
    expect_equal(result$status, "failed")
    expect_equal(result$error_message, "model diverged")

    expect_true(is.na(result$rmse))
    expect_true(is.na(result$rpd))
    expect_true(is.na(result$rsq))
    expect_true(is.na(result$ccc))
    expect_true(is.na(result$rrmse))
    expect_true(is.na(result$mae))

    expect_true(is.na(result$runtime_secs))

    expect_true("best_params" %in% names(result))
    expect_true(is.list(result$best_params))
    expect_null(result$best_params[[1]])

  })

  it("handles NULL error gracefully", {

    result <- create_failed_result(
      config_id = "CFG_001",
      error     = NULL
    )

    expect_equal(result$status, "failed")
    expect_true(is.na(result$error_message))

  })

  it("handles string error messages", {

    result <- create_failed_result(
      config_id = "CFG_001",
      error     = "something went wrong"
    )

    expect_equal(result$error_message, "something went wrong")

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
      class = "horizons_internal_error"
    )

    msg <- conditionMessage(err)
    expect_match(msg, "Interval columns for config 'cfg_1'", fixed = TRUE)
    expect_match(msg, "the rows of new_data", fixed = TRUE)
    expect_match(msg, "Got 7 rows for 8", fixed = TRUE)
    expect_match(msg, "bug in horizons", fixed = TRUE)

  })

  it("aborts when the same ids are in a different order, naming the first", {

    err <- expect_error(
      check_rows_aligned("Provenance columns", "the drawn rows",
                         ids = c("a", "c", "b"), expected_ids = c("a", "b", "c")),
      class = "horizons_internal_error"
    )

    msg <- conditionMessage(err)
    expect_match(msg, "2 of 3 positions", fixed = TRUE)
    expect_match(msg, "row 2", fixed = TRUE)

  })

  it("checks the count before the order", {

    expect_error(
      check_rows_aligned("Values", "the rows", ids = c("a", "b"),
                         expected_ids = c("a", "b", "c")),
      "Got 2 rows for 3", class = "horizons_internal_error"
    )

  })

  it("treats an NA id against a real one as a difference", {

    expect_error(
      check_rows_aligned("Values", "the rows", ids = c("a", NA),
                         expected_ids = c("a", "b")),
      class = "horizons_internal_error"
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
      class = "horizons_internal_error"
    )

    expect_match(conditionMessage(err), "config '{x}'", fixed = TRUE)
    expect_match(conditionMessage(err), "{a}", fixed = TRUE)

  })

})
