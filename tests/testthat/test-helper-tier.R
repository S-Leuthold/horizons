## ---------------------------------------------------------------------------
## Tests: the slow-tier gate (helper-tier.R)
## ---------------------------------------------------------------------------

## The reason skip_unless_slow_tier() skips with, as the reporters print it
## (testthat prefixes the condition's message with "Reason: "), or NULL when
## it does not skip.
tier_skip_message <- function() {
  tryCatch({
    skip_unless_slow_tier()
    NULL
  }, skip = function(cnd) sub("^Reason: ", "", conditionMessage(cnd)))
}

describe("skip_unless_slow_tier()", {

  it("skips with the pinned reason when HORIZONS_SLOW_TESTS is unset, false or not a logical", {

    ## CI greps test output for this exact text to prove the tier ran; change
    ## it only together with the "Check the slow tier ran" steps in
    ## .github/workflows/R-CMD-check.yaml and test-coverage.yaml.
    pinned <- "slow tier (set HORIZONS_SLOW_TESTS=true to run)"

    withr::local_envvar(HORIZONS_SLOW_TESTS = NA)
    expect_identical(tier_skip_message(), pinned)

    for (value in c("false", "FALSE", "", "no", "ture")) {
      withr::local_envvar(HORIZONS_SLOW_TESTS = value)
      expect_identical(tier_skip_message(), pinned, info = value)
    }

  })

  it("does not skip when HORIZONS_SLOW_TESTS is true", {

    for (value in c("true", "TRUE", "True")) {
      withr::local_envvar(HORIZONS_SLOW_TESTS = value)
      expect_null(tier_skip_message(), info = value)
    }

  })

})
