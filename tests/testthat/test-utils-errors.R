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

  it("handles complex default values", {

    default_df <- data.frame(x = 1:3, y = 4:6)

    result <- safely_execute(
      { stop("fail") },
      default_value = default_df,
      log_error     = FALSE
    )

    expect_equal(result$result, default_df)

  })

  it("works inside purrr::map workflows", {

    inputs <- list(1, "not_a_number", 3)

    results <- purrr::map(inputs, function(val) {
      safely_execute(
        { log(val) },
        default_value = NA_real_,
        log_error     = FALSE
      )$result
    })

    expect_equal(results[[1]], log(1))
    expect_true(is.na(results[[2]]))
    expect_equal(results[[3]], log(3))

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

    expect_error(
      handle_results(safe_result, error_title = "Model training failed"),
      "Model training failed"
    )

  })

  it("returns NULL when result is NULL and abort_on_null = FALSE", {

    safe_result <- list(
      result = NULL,
      error  = simpleError("underlying failure")
    )

    result <- handle_results(safe_result, abort_on_null = FALSE, silent = TRUE)
    expect_null(result)

  })

  it("includes error details in abort message", {

    safe_result <- list(
      result = NULL,
      error  = simpleError("convergence failed after 100 iterations")
    )

    expect_error(
      handle_results(safe_result, error_title = "Fitting failed"),
      "convergence failed"
    )

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

  })

  it("marks status as 'failed'", {

    result <- create_failed_result(
      config_id = "CFG_001",
      error     = simpleError("timeout")
    )

    expect_equal(result$status, "failed")

  })

  it("captures the error message", {

    result <- create_failed_result(
      config_id = "CFG_001",
      error     = simpleError("out of memory")
    )

    expect_equal(result$error_message, "out of memory")

  })

  it("sets metric columns to NA", {

    result <- create_failed_result(
      config_id = "CFG_001",
      error     = simpleError("fail")
    )

    expect_true(is.na(result$rmse))
    expect_true(is.na(result$rpd))
    expect_true(is.na(result$rsq))
    expect_true(is.na(result$ccc))
    expect_true(is.na(result$rrmse))
    expect_true(is.na(result$mae))

  })

  it("sets runtime to NA", {

    result <- create_failed_result(
      config_id = "CFG_001",
      error     = simpleError("fail")
    )

    expect_true(is.na(result$runtime_secs))

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

  it("includes best_params as list(NULL)", {

    result <- create_failed_result(config_id = "CFG_001")

    expect_true("best_params" %in% names(result))
    expect_true(is.list(result$best_params))
    expect_null(result$best_params[[1]])

  })

})
