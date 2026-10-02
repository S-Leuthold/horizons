## ---------------------------------------------------------------------------
## Test helper: the slow tier
## ---------------------------------------------------------------------------
## Tests that take several seconds each on their own (multisession dispatch,
## optimize = TRUE ensembles, the select_training() chain) run only when the
## environment variable HORIZONS_SLOW_TESTS is true. CI turns the tier on for
## every push to development or main, the nightly run, coverage, and pull
## requests that change a path in .github/slow-tier-paths.txt, change a test
## file holding tiered tests, or carry the slow-tests label. Locally:
##
##   HORIZONS_SLOW_TESTS=true Rscript -e 'devtools::test()'
##
## Make skip_unless_slow_tier() the first line of a tiered test, before any
## other skip and before any memoised fixture accessor (helper-memo.R), so the
## fast tier never builds a fixture that only tiered tests read.
##
## The skip reason is pinned by test-helper-tier.R. CI fails a run that should
## have had the tier on if this reason appears in its test output, so a
## misspelt or dropped variable cannot quietly turn the tier into skips.

slow_tier_skip_reason <- "slow tier (set HORIZONS_SLOW_TESTS=true to run)"

slow_tier_on <- function() {
  isTRUE(as.logical(Sys.getenv("HORIZONS_SLOW_TESTS", "false")))
}

skip_unless_slow_tier <- function() {
  testthat::skip_if_not(slow_tier_on(), slow_tier_skip_reason)
}
