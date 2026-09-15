## ---------------------------------------------------------------------------
## Test helpers: registered future plans and warning filtering
## ---------------------------------------------------------------------------
## The package never registers a plan; tests that need one register it for
## the duration of the calling test and restore the previous one. Two
## workers is the CRAN-conformant maximum under _R_CHECK_LIMIT_CORES_. There
## is no withr::local_plan(); future::plan() returns the previous strategy
## when setting a new one, which is what the restore relies on.

local_plan <- function(..., .env = parent.frame()) {
  old <- future::plan(...)
  withr::defer(future::plan(old), envir = .env)
  invisible(old)
}

## Evaluate `expr` letting only warnings that match `keep` (a condition class
## or a message regex) propagate. The small fixtures trip rsample's
## stratification warnings, which are not the subject of any test here; this
## muffles them so expect_warning() sees only the one under test.
keep_only_warning <- function(expr, keep) {
  withCallingHandlers(expr, warning = function(w) {
    matches <- inherits(w, keep) || grepl(keep, conditionMessage(w))
    if (!matches) invokeRestart("muffleWarning")
  })
}
