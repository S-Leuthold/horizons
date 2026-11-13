## ---------------------------------------------------------------------------
## Test helpers: mirai daemons, deadlines and warning filtering
## ---------------------------------------------------------------------------
## The package never sets daemons; tests that need some set them for the
## duration of the calling test and reset them on its exit. Two daemons is
## the CRAN-conformant maximum under _R_CHECK_LIMIT_CORES_. Daemons are new
## Rscript processes, which load the horizons R_LIBS names, so the tests that
## run work on them need an installed build with R_LIBS on it.

## Set `n` daemons on the default profile until the calling frame exits, and
## wait until they are connected. autoexit = NA: a daemon finishes exiting
## when the reset drops its connection, where mirai's default kills it 200 ms
## after daemons(0), cutting off the trace file a daemon writes on exit under
## covr. Named apart from mirai's own local_daemons(), which switches the
## active profile instead. Setting daemons moves the caller's random stream by
## one draw, so a test of the stream sets its seed after this.
local_test_daemons <- function(n = 2L, ..., .env = parent.frame()) {

  mirai::daemons(n, ..., autoexit = NA)
  withr::defer(mirai::daemons(0), envir = .env)

  with_deadline(
    while (connected_daemons() < n) Sys.sleep(0.05),
    seconds = 30
  )

  invisible(n)

}

## A task no daemon will ever take, for the stops a wait makes when no daemon
## is left, without starting one: daemons set on a URL that no daemon dials,
## so the dispatcher holds the task and none is connected. The daemons are
## reset when the calling frame exits.
local_unresolved_task <- function(.env = parent.frame()) {

  mirai::daemons(url = mirai::local_url())
  withr::defer(mirai::daemons(0), envir = .env)

  mirai::mirai(NULL)

}

## What daemon_reports() returns from daemons that load horizons `version`
## and lack nothing, one report per process id: the mock of the door's check
## for tests that do not start daemons.
daemons_on <- function(version = as.character(utils::packageVersion("horizons")), pids = 1:2) {
  lapply(as.integer(pids), function(pid) list(pid = pid, version = version, missing = character(0)))
}

matching_daemons <- function(...) daemons_on()

## Evaluate `expr` with a deadline, so a wait that never ends fails the test
## instead of stalling the run. setTimeLimit() ends both a polling wait and
## mirai's blocking collect, with a plain error that fires once. Inside a
## unit that error is contained as the unit's failure, and the call goes on,
## so the deadline is also read from the clock: whatever `expr` returned or
## raised, once the time is up the test fails with a class no expectation
## matches, and no parent an expectation could match through. A wait after
## the limit fired has none; the CI job's timeout is the backstop for it.
with_deadline <- function(expr, seconds = 60) {

  start <- Sys.time()
  setTimeLimit(elapsed = seconds, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf), add = TRUE)

  passed <- function() {
    if (difftime(Sys.time(), start, units = "secs") >= seconds) {
      rlang::abort(sprintf("The test's deadline of %s seconds passed.", seconds),
                   class = "horizons_test_deadline")
    }
  }

  value <- withCallingHandlers(expr, error = function(e) passed())
  passed()

  value

}

## The default profile's daemons as a run found or left them: how many were
## set, whether any are, their dispatcher (a new one after any reset, even to
## the same size) and how many are connected
daemons_record <- function() {

  list(n          = mirai::nextget("n"),
       set        = mirai::daemons_set(),
       dispatcher = mirai::nextget("dispatcher"),
       connected  = connected_daemons())

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

## Evaluate `expr`, muffling every warning, and return its value with the
## warnings of `class` whose message matches `pattern`, as conditions: for a
## test that a warning is raised once, with its class. By default, every
## warning.
collect_warnings <- function(expr, pattern = "", class = "warning") {
  warned <- list()
  value  <- withCallingHandlers(expr, warning = function(w) {
    if (inherits(w, class) && grepl(pattern, conditionMessage(w))) warned[[length(warned) + 1L]] <<- w
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = warned)
}
