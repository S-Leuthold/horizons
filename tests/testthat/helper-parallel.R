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
## one draw, so a test of the stream sets its seed after this. The machine is
## read as having a petabyte available meanwhile, so a run on these daemons
## does not warn that they need more than a small machine has: the memory
## check has tests of its own (test-utils-parallel.R).
local_test_daemons <- function(n = 2L, ..., .env = parent.frame()) {

  testthat::local_mocked_bindings(memory_available = function() 1e15, .package = "horizons", .env = .env)

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
## for tests that do not start daemons. By default they are on another
## machine, so the memory check counts none of them and a test's result does
## not depend on this machine's free memory; a test of the check puts them
## here (`host = this_host()`).
daemons_on <- function(version = as.character(utils::packageVersion("horizons")), pids = 1:2,
                       host = "a-test-host") {
  lapply(as.integer(pids), function(pid) list(pid = pid, host = host, version = version, missing = character(0)))
}

matching_daemons <- function(...) daemons_on()

## The name the memory check knows this machine by
this_host <- function() Sys.info()[["nodename"]]

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

## Whether a process is running: signal 0 checks without sending anything.
## For the tests of the processes horizons starts for isolated work, which
## skip on Windows, where tools::pskill() sends no signal 0.
process_alive <- function(pid) {
  isTRUE(tools::pskill(as.integer(pid), 0L))
}

## Wait up to `seconds` for every process in `pids` to end; TRUE when they
## all have
processes_end <- function(pids, seconds = 5) {
  until <- Sys.time() + seconds
  while (any(vapply(pids, process_alive, logical(1))) && Sys.time() < until) Sys.sleep(0.05)
  !any(vapply(pids, process_alive, logical(1)))
}

## A library holding another horizons, until the calling frame exits: a
## package of that name at version 0.0.0.1, holding nothing else, installed
## with R CMD INSTALL (a few seconds). What a process started with only the
## environment's library paths would load first when R_LIBS names it.
local_decoy_horizons <- function(.env = parent.frame()) {

  src <- withr::local_tempdir(.local_envir = .env)
  lib <- withr::local_tempdir(.local_envir = .env)

  dir.create(file.path(src, "R"))
  writeLines(c("Package: horizons", "Version: 0.0.0.1", "Title: Another horizons",
               "Description: A package of the same name, for a test.", "License: MIT",
               "Encoding: UTF-8"),
             file.path(src, "DESCRIPTION"))
  writeLines("export(decoy)", file.path(src, "NAMESPACE"))
  writeLines("decoy <- function() TRUE", file.path(src, "R", "decoy.R"))

  status <- system2(file.path(R.home("bin"), "R"),
                    c("CMD", "INSTALL", "--no-test-load", "--no-docs", paste0("--library=", shQuote(lib)), shQuote(src)),
                    stdout = FALSE, stderr = FALSE)

  if (!identical(as.integer(status), 0L) || !dir.exists(file.path(lib, "horizons"))) {
    testthat::skip("another horizons could not be installed for the test")
  }

  lib

}

## Replace the isolated engines' fit with one whose process kills itself on
## the calls `dies()` picks (it is given the call's number), so a test sees a
## real death of an isolated fit's process, until the calling frame exits.
## The other calls fit as the engine would.
local_dying_fits <- function(dies, .env = parent.frame()) {

  calls     <- 0L
  real_fit  <- isolated_fit

  testthat::local_mocked_bindings(isolated_fit = function(..., .isolated_fit, .isolated_engine) {
    calls <<- calls + 1L
    if (dies(calls)) {
      run_isolated(quote(tools::pskill(Sys.getpid(), 9L)), what = paste0("a ", .isolated_engine, " fit"))
    }
    real_fit(..., .isolated_fit = .isolated_fit, .isolated_engine = .isolated_engine)
  }, .package = "horizons", .env = .env)

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
