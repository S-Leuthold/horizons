## ---------------------------------------------------------------------------
## Tests: the daemon tests' deadline (helper-parallel.R)
## ---------------------------------------------------------------------------
## with_deadline() is what turns a hang in a daemon test into a failure, so
## it is held to failing where a unit's containment swallows the time limit.

describe("with_deadline()", {

  it("fails a call whose time limit a unit contained, once its time is up", {

    ## The limit fires inside the unit, which records it as the unit's
    ## failure and returns: the call ends as if it had not hung
    expect_error(
      with_deadline(run_unit(repeat Sys.sleep(0.05), "configuration", "cfg_001"), 1),
      class = "horizons_test_deadline"
    )

  })

  it("fails a call that raised its own error after its time was up, with no parent to match through", {

    late <- function() {
      tryCatch(repeat Sys.sleep(0.05), error = function(e) NULL)
      rlang::abort("The expected error, raised late.", class = "horizons_error_systemic")
    }

    err <- expect_error(with_deadline(late(), 1), class = "horizons_test_deadline")
    expect_false(inherits(err, "horizons_error_systemic"))
    expect_null(err$parent)

  })

  it("lets a call that ends in time through, its value and its error alike", {

    expect_identical(with_deadline(1 + 1, 10), 2)
    expect_error(with_deadline(stop("In time."), 10), "In time.", fixed = TRUE)

  })

})
