## ---------------------------------------------------------------------------
## Parallelism helpers: axis resolution, the daemons, the configs axis's
## dispatch, thread pinning
## ---------------------------------------------------------------------------
##
## The rule these implement: the user owns the mirai daemons; the package owns
## only which axis of its work they are applied to. horizons never calls
## mirai::daemons(), and never uses with(daemons()), with_daemons() or
## local_daemons(), whose scope would send tune's own tasks to another
## profile. It reads the pool with mirai::info() by name, never status(), and
## every call on the pool names `.compute = NULL`, the current profile, which
## is the one tune reads.

## ---------------------------------------------------------------------------
## resolve_parallel_axis
## ---------------------------------------------------------------------------

#' Resolve the parallel axis for a run
#'
#' @description
#' Pure function from the caller's request and the object's shape to the
#' concrete dispatch decision. It never reads the daemons, so it is
#' unit-testable with no backend and no installed build.
#'
#' `"auto"` picks by the measured cost model (2026-09-15): one config run
#' sequentially costs about T; on the configs axis wall time is roughly
#' ceiling(n_configs / workers) * T, on the resamples axis roughly
#' n_configs * T / k with k the effective inner speedup, which is capped at
#' `cv_folds`. Configs wins whenever `n_configs >= k`, and always once
#' `n_configs` exceeds the worker count, so the worker count cancels and the
#' rule is `n_configs >= cv_folds`. Dispatching configs needs a checkpoint
#' directory, so `"auto"` chooses it only when there is one (#233).
#'
#' On the configs axis the worker runs tune sequentially: a daemon has no
#' daemons of its own. `control_horizons()` refuses `"both"`.
#'
#' @param parallelize_over One of `"auto"`, `"configs"`, `"resamples"`, as
#'   `control_horizons()` checked it.
#' @param allow_par Logical. `FALSE` short-circuits to sequential.
#' @param n_configs Integer. Number of configs to evaluate.
#' @param cv_folds Integer. CV folds per config.
#' @param has_output_dir Logical. Whether the run checkpoints.
#'
#' @return A list: `axis` (`"sequential"`, `"configs"` or `"resamples"`),
#'   `dispatch_configs` (logical; dispatch the configurations to the
#'   daemons), `tune_allow_par`
#'   (logical; what the single-config functions pass to tune's control), and
#'   `tune_parallel_over` (`"resamples"` or `NULL`; the value *requested* of
#'   tune, which may rewrite it, see the note on `tune:::.update_parallel_over`
#'   in the spec).
#' @keywords internal
resolve_parallel_axis <- function(parallelize_over, allow_par, n_configs, cv_folds,
                                  has_output_dir) {

  ### control_horizons() checked parallelize_over and allow_par; n_configs is
  ### the configured grid's row count, and cv_folds config$tuning's, both of
  ### which the door validator checks.

  if (!allow_par) {

    return(list(
      axis               = "sequential",
      dispatch_configs   = FALSE,
      tune_allow_par     = FALSE,
      tune_parallel_over = NULL
    ))

  }

  axis <- if (parallelize_over == "auto") {

    if (n_configs >= cv_folds && has_output_dir) "configs" else "resamples"

  } else {

    parallelize_over

  }

  if (axis == "configs") {

    list(
      axis               = "configs",
      dispatch_configs   = TRUE,
      tune_allow_par     = FALSE,
      tune_parallel_over = NULL
    )

  } else {

    list(
      axis               = "resamples",
      dispatch_configs   = FALSE,
      tune_allow_par     = TRUE,
      tune_parallel_over = "resamples"
    )

  }

}

## ---------------------------------------------------------------------------
## The cost table: config_costs, dispatch_order
## ---------------------------------------------------------------------------

#' The seconds each configuration is expected to take
#'
#' @description
#' Each configuration's cell of `CONFIG_COST_SECONDS`, the seconds a
#' configuration of its model and feature selection took on a measured grid.
#' The configs axis starts the longest first by it ([dispatch_order()]), and
#' `monitor_evaluate()` weights its ETA by it, from the run's manifest.
#'
#' @param configs The configurations, with `model` and `feature_selection`
#'   columns.
#' @return Numeric, one unnamed cost per row of `configs`.
#' @keywords internal
#' @noRd
config_costs <- function(configs) {

  keys <- paste(configs$model, configs$feature_selection, sep = "/")
  cost <- unname(CONFIG_COST_SECONDS[keys])

  ### configure() admits only the models and selections the table holds, so a
  ### key it lacks is a cell missing from the table
  if (anyNA(cost)) {

    missing <- unique(keys[is.na(cost)])

    abort_horizons(
      "{.code CONFIG_COST_SECONDS} has no cost for {.val {missing}}.",
      kind = "internal"
    )

  }

  cost

}

#' The order the configs axis starts its configurations in
#'
#' @description
#' Longest first: by [config_costs()], so the longest start first and the
#' short ones fill the daemons at the end. Ties keep grid order, so the order
#' is deterministic. The results do not depend on it: every draw is seeded
#' from the run's seed and its own name, each fit of an engine that carries
#' state from one fit to the next (`cubist`) runs in a process of its own,
#' and the rows are put back in configuration order before they are bound.
#'
#' @param configs The configurations to run, with `model` and
#'   `feature_selection` columns.
#' @return Integer: the row positions of `configs`, in the order to start
#'   them.
#' @keywords internal
#' @noRd
dispatch_order <- function(configs) {

  ### order() leaves ties in their original order, which is grid order
  order(-config_costs(configs))

}

## ---------------------------------------------------------------------------
## The daemons
## ---------------------------------------------------------------------------

#' The mirai daemons connected now
#'
#' @description
#' `mirai::info()[["connections"]]` on the current profile, which is the
#' number tune reads when it chooses mirai. The one place horizons reads the
#' pool's size, so tests mock it here.
#'
#' @return Integer: `0L` when no daemons are set.
#' @keywords internal
#' @noRd
connected_daemons <- function() {

  counts <- mirai::info(.compute = NULL)

  if (is.null(counts)) return(0L)

  as.integer(counts[["connections"]])

}

#' Whether the daemons have mirai's dispatcher
#'
#' @description
#' With `mirai::daemons(dispatcher = FALSE)` tasks go to the daemons in turn
#' as they are sent, and `mirai::info()` reports only the connections, its
#' other counts `NA`: the configs axis then cannot start the longest
#' configurations first or cancel those still running.
#'
#' @return Logical(1): `FALSE` when no daemons are set.
#' @keywords internal
#' @noRd
daemons_dispatch <- function() {

  counts <- mirai::info(.compute = NULL)

  !is.null(counts) && !is.na(counts[["cumulative"]])

}

#' Check that a parallel run has daemons to run on
#'
#' @description
#' Called only when the caller asked for parallelism. The gate is two
#' connected daemons: with one, tune runs sequentially, silently, which was
#' issue #37. A `future::plan()` is not read: tune is given `allow_par = TRUE`
#' only when this passes, and then takes mirai, so it never uses a plan.
#'
#' Running sequentially is a substitution (something other than what was
#' asked ran), so it is recorded on the verb's collector as a
#' `substitution` warning, which the verb raises at its end.
#'
#' @param verb Character. The verb, for the message (`"evaluate"`).
#' @param collector The verb's collector, from `log_open()`.
#' @return `TRUE` if parallel dispatch is possible, else `FALSE`, with the
#'   warning recorded.
#' @keywords internal
check_parallel_backend <- function(verb, collector) {

  n <- connected_daemons()

  if (n >= 2L) return(TRUE)

  log_warning(collector, "substitution", c(
    "{.fn {verb}} was asked to run in parallel ({.code allow_par = TRUE}) but {n} mirai daemon{?s} {?is/are} connected, so it ran sequentially.",
    "i" = "Set daemons before calling, for example {.code mirai::daemons(4)}. A {.code future::plan()} is no longer used."
  ))

  FALSE

}

#' Wait until one of the tasks in flight resolves, or stop when no daemon is left
#'
#' @description
#' Every wait horizons owns on the daemons goes through this one, so none
#' blocks for ever. A task queued with no daemon connected waits for ever
#' under mirai, which relaunches no daemon that exits (under `maxtasks`,
#' `idletime` or `walltime`) or dies. So the wait polls, reading the
#' connected daemons at every poll, and calls `no_daemons()`, which stops
#' the run, when none is left while a task is unresolved. `race_mirai()`
#' would block in that case.
#'
#' @param tasks List of `mirai` objects, at least one.
#' @param no_daemons A function of no arguments that stops the run.
#' @return `NULL`, invisibly, once a task has resolved.
#' @keywords internal
#' @noRd
await_round <- function(tasks, no_daemons) {

  repeat {

    if (!all(vapply(tasks, mirai::unresolved, logical(1)))) return(invisible(NULL))

    if (connected_daemons() == 0L) no_daemons()

    Sys.sleep(DISPATCH_POLL_SECONDS)

  }

}

#' Stop the call because no daemon is left to run its work
#'
#' @description
#' Raised by a wait with work remaining and no daemon connected
#' ([await_round()]), and before a unit of work tune runs on the daemons
#' with fewer than two connected (`stop_on_worker_death()`), the gate
#' [check_parallel_backend()] applies before the run. The likely cause is a
#' daemon's own limit, which mirai does not restart it from; horizons starts
#' no daemons of its own.
#'
#' @param results The rows finished so far, carried as `results`, or `NULL`
#'   when the verb has none to carry.
#' @param n_left Integer(1) or `NULL`. The configurations still to run, when
#'   the caller knows them.
#' @param output_dir The run's `output_dir`, or `NULL`.
#' @param verb Character(1). The verb that stops.
#' @param connected Integer(1). The daemons connected: 0, or 1 before a unit
#'   that needs two.
#' @param call The frame the error reports.
#' @return Does not return; aborts with class `horizons_error_no_daemons`
#'   (then `horizons_error_systemic`, kind `run`).
#' @keywords internal
#' @noRd
abort_no_daemons <- function(results, n_left = NULL, output_dir = NULL, verb = "evaluate",
                             connected = 0L, call = rlang::caller_env()) {

  abort_horizons(c(
    if (connected > 0L) {
      "Only one mirai daemon is connected, and {.fn {verb}} needs two to run its work in parallel, so it stopped before its work was done."
    } else if (is.null(n_left)) {
      "No mirai daemon is connected, so {.fn {verb}} stopped before its work was done."
    } else {
      "No mirai daemon is connected, so {.fn {verb}} stopped with {n_left} configuration{?s} still to run."
    },
    "i" = "A daemon exits after {.arg maxtasks} tasks, or past {.arg idletime} or {.arg walltime}, and mirai does not start it again. Set daemons without those limits, or launch more with {.code mirai::launch_local()}.",
    ### The verbs that check every daemon at the door spend a task on each
    "i" = if (verb %in% c("evaluate", "fit")) "{.fn {verb}} spends one task on each daemon checking it before the run.",
    resume_hint(results, output_dir)
  ), kind = "run", class = c("horizons_error_no_daemons", "horizons_error_systemic"),
  results = results, call = call)

}

#' Where a stopped run's finished work is, for its message
#'
#' @param results The rows finished so far, or `NULL` when the verb has none.
#' @param output_dir The run's `output_dir`, or `NULL`.
#' @return A named character vector of cli lines (`"i"`), empty when there is
#'   nothing to point to, interpolated where the caller's `output_dir` and
#'   `verb` are: the checkpoints when the run has an `output_dir`, and the
#'   `results` field whenever it holds rows, since a row whose checkpoint
#'   could not be written is there and not on disk.
#' @keywords internal
#' @noRd
resume_hint <- function(results, output_dir) {

  c(
    if (!is.null(output_dir)) {
      c("i" = "The configurations finished are checkpointed in {.path {output_dir}}; calling {.fn {verb}} again with it resumes from them.")
    },
    if (!is.null(results) && nrow(results) > 0) {
      c("i" = "The rows finished so far are in this error's {.field results} field.")
    }
  )

}

#' The condition a daemon's error was, raised again in this session
#'
#' @description
#' mirai returns an error raised in a task as a `miraiError`: a string whose
#' attributes are the condition's fields (`message`, `call`, `parent`,
#' `results` and the rest) and its classes (`condition.class`). Rebuilt from
#' them, it is caught by its class and prints its message as it would have
#' in the task, so `horizons_error_missing_package`, the kinds no unit
#' contains and a version refusal keep working across the boundary.
#'
#' @param err A `miraiError`.
#' @return A condition object.
#' @keywords internal
#' @noRd
mirai_condition <- function(err) {

  fields <- attributes(err)
  fields <- fields[setdiff(names(fields), c("class", "condition.class", "stack.trace"))]

  structure(fields, class = err$condition.class)

}

#' A task's value, or the condition it ended in, raised
#'
#' @description
#' mirai returns what ends a task as its value: a `miraiError` for the
#' task's own error, raised again as the condition it was
#' ([mirai_condition()]), and any other error value for a failure of the
#' task's transport, its daemon's death (`errorValue` 19) among them, which
#' `transport` raises. A `miraiError` is an error value too, so it is read
#' first.
#'
#' @param value A task's value (`task$data`, once resolved).
#' @param transport A function of the error value that stops the call.
#' @return `value`, when it is neither.
#' @keywords internal
#' @noRd
task_value <- function(value, transport) {

  if (mirai::is_mirai_error(value)) stop(mirai_condition(value))

  if (mirai::is_error_value(value)) transport(value)

  value

}

## ---------------------------------------------------------------------------
## The memory the daemons on this machine need
## ---------------------------------------------------------------------------
## The user's daemons(n) is the run's budget of cores, and each daemon holds
## its own copy of the run's data, and, while it fits a model whose fits are
## isolated, that fit's process too. A parallel evaluate() or fit() warns at
## its door when the daemons on this machine would need more memory than the
## machine has, and suggests a count that fits. The run goes on: the
## estimate is from measured runs, and the user knows the machine.

#' The memory available on this machine
#'
#' @description
#' `ps::ps_system_memory()`'s `avail`: what the system can give processes
#' without swapping. Under a container's or `systemd-run`'s memory cap it is
#' the machine's, not the cap's.
#'
#' @return Numeric(1), in bytes, or `NA` when it cannot be read. A seam, so a
#'   test can give another reading.
#' @keywords internal
#' @noRd
memory_available <- function() {

  tryCatch(as.numeric(ps::ps_system_memory()[["avail"]]), error = function(e) NA_real_)

}

#' The processes one daemon holds for a run
#'
#' @description
#' The daemon, and an isolated fit's process beside it while it fits a model
#' whose fits are isolated (each daemon fits one at a time). The one place a
#' daemon's processes are counted, so work that starts more of them adds its
#' count here.
#'
#' @param models Character. The models the run fits.
#' @param isolate Logical(1). `control_horizons(isolate = )`.
#' @return Named integer: one count per row of `DAEMON_MEMORY`.
#' @keywords internal
#' @noRd
daemon_processes <- function(models, isolate) {

  c(daemon   = 1L,
    isolated = as.integer(isTRUE(isolate) && length(isolated_models(models)) > 0))

}

#' The memory one daemon needs for a run
#'
#' @param processes Named integer, from [daemon_processes()].
#' @param n_rows,n_predictors The run's data: its rows and predictors.
#' @return Numeric(1), in bytes: each process's `DAEMON_MEMORY` line at the
#'   run's data, times how many of it the daemon holds.
#' @keywords internal
#' @noRd
daemon_need <- function(processes, n_rows, n_predictors) {

  bytes <- 8 * as.numeric(n_rows) * as.numeric(n_predictors)

  sum(vapply(names(processes), function(p) {
    processes[[p]] * (DAEMON_MEMORY[[p]][["base"]] + DAEMON_MEMORY[[p]][["per_byte"]] * bytes)
  }, numeric(1)))

}

#' Warn when the daemons on this machine would need more memory than it has
#'
#' @description
#' Run at a parallel `evaluate()`'s or `fit()`'s door, once the daemons have
#' reported ([daemon_reports()]). The daemons counted are those on this
#' machine, by the host name each reported; daemons elsewhere are not
#' checked, since only this machine's memory can be read here. When those
#' daemons' need ([daemon_need()]) is more than the memory available
#' ([memory_available()]), an `input` warning is recorded on the verb's
#' collector, raised once at its end, suggesting the most daemons that fit,
#' or a sequential run below two. A reading that cannot be made is a note.
#'
#' @param reports The daemons' reports, one per task.
#' @param models Character. The models the run fits.
#' @param isolate Logical(1).
#' @param n_rows,n_predictors The run's data.
#' @param collector The verb's collector.
#' @param verb Character(1).
#' @return Logical(1): `TRUE` when it warned, for the tree's parallel line.
#' @keywords internal
#' @noRd
check_daemon_memory <- function(reports, models, isolate, n_rows, n_predictors, collector, verb) {

  here    <- Sys.info()[["nodename"]]
  local   <- Filter(function(r) identical(r$host, here), reports)
  n_local <- length(unique(vapply(local, function(r) r$pid, integer(1))))

  if (n_local == 0L) return(FALSE)

  available <- memory_available()

  if (!is.finite(available)) {
    log_note(collector, "{.fn {verb}} could not read the memory available on this machine, so it did not check that its mirai daemons fit in it.")
    return(FALSE)
  }

  processes <- daemon_processes(models, isolate)
  need      <- daemon_need(processes, n_rows, n_predictors)
  total     <- n_local * need

  if (total <= available) return(FALSE)

  fits <- floor(available / need)
  gb   <- function(bytes) format(round(bytes / 1e9, 1), nsmall = 1)
  iso  <- isolated_models(models)

  log_warning(collector, "input", c(
    "The {n_local} mirai daemon{?s} on this machine would need about {gb(need)} GB each for this run, {gb(total)} GB in all, and {gb(available)} GB is available.",
    "i" = if (processes[["isolated"]] > 0) {
      "Each holds a copy of the run's data, and each fit of {.val {iso}} runs in a process of its own beside it."
    } else {
      "Each holds a copy of the run's data."
    },
    "i" = if (fits >= 2) {
      "{.code mirai::daemons({fits})} fits; the estimate is from measured runs, so it is approximate."
    } else {
      "Fewer than two daemons fit, so a sequential run does ({.code control_horizons(allow_par = FALSE)}); the estimate is from measured runs, so it is approximate."
    }
  ))

  TRUE

}

## ---------------------------------------------------------------------------
## Isolated work: one call in a process of its own
## ---------------------------------------------------------------------------
## Some work runs in a process started for it alone and ended once its value
## is back: a fit whose engine carries state from one fit to the next
## (MODEL_SPECS' `isolate`, R/isolated-engines.R), and select_training()'s
## draw, so this session never holds the library. It is one mirai() call to a
## profile no daemons() call sets, which mirai answers by starting a new R
## process for that one call (mirai's ephemeral daemon, the path it takes
## when no daemons are set). horizons never sets that profile, or any other,
## and the user's daemons are not touched: the process works the same in
## this session, on a configs-axis daemon and in one of tune's fold tasks.

#' The compute profile isolated work is sent to
#'
#' @description
#' A name no `mirai::daemons()` call sets, so every `mirai()` sent to it
#' starts a process of its own. The process id makes it this process's, so a
#' daemon running isolated work uses its own, and no profile a user set by
#' name can be taken for it.
#'
#' @return Character(1).
#' @keywords internal
#' @noRd
isolated_profile <- function() {

  paste0("horizons-isolated-", Sys.getpid())

}

#' Run one call in a process of its own
#'
#' @description
#' Sends `expr` to a new R process ([isolated_profile()]) and returns its
#' value here, as if it had run in this session:
#'
#' * The process is given this session's library paths (`libraries`) first,
#'   so it loads what this session would (a process started by mirai sees
#'   `R_LIBS` and the startup files, not paths set at run time), then any
#'   `options`.
#' * Its random stream is seeded with one integer drawn here, from the
#'   caller's stream, under the kinds every unit pins ([with_unit_seed()]),
#'   so what it draws follows the caller's seed, and this session's stream
#'   moves by that one draw.
#' * The warnings and messages it raises are kept in order and muffled
#'   there, and what it prints is captured; here the output is printed, then
#'   each condition is raised again with its class and fields, then its
#'   error, if it ended in one, is raised as the condition it was. mirai
#'   relays none of these on its own.
#' * A process that ends without returning (killed, out of memory,
#'   `errorValue` 19) is `horizons_error_process_died` ([abort_process_died()]).
#' * Leaving before the value is back (an error, an interrupt) drops the
#'   call, which ends its process.
#'
#' The call sent is a base-R body around `expr`, and `args` are plain
#' values the body finds by name: nothing of horizons travels, so a process
#' running a fit loads only what `expr` names. A function or a closure among
#' `args` would carry its environment with it.
#'
#' @param expr A call, quoted, evaluated in the process with `args` in scope.
#' @param args Named list of the values `expr` reads.
#' @param what Character(1). What runs, for the death's message: `"a Cubist
#'   fit"`, `"the library draw"`.
#' @param options Named list of options to set in the process first.
#' @param libraries Character. The library paths the process loads from.
#' @param call The frame a death reports.
#' @return The value of `expr`.
#' @keywords internal
#' @noRd
run_isolated <- function(expr, args = list(), what, options = list(), libraries = .libPaths(),
                         call = rlang::caller_env()) {

  reserved <- intersect(names(args), ISOLATED_NAMES)

  if (length(args) > 0 && (is.null(names(args)) || any(!nzchar(names(args))) || length(reserved) > 0)) {

    abort_horizons("Every argument sent to an isolated call must have a name of its own.",
                   kind = "internal")

  }

  ## One draw from the caller's stream, as resolve_seed() draws a root
  seed <- sample.int(100000L, 1L)

  ### mirai takes a call held in a variable as the call to send; a call
  ### written in place would be sent as written
  body <- isolated_body(expr)

  task <- mirai::mirai(
    .expr    = body,
    .args    = c(args, list(.horizons_libs    = libraries,
                            .horizons_options = options,
                            .horizons_seed    = seed)),
    .compute = isolated_profile()
  )

  ## Dropped and collected unless its value came back, so the process ends:
  ## mirai closes the call's socket when the task is collected, and the
  ## process ends with it, inside compiled code too
  collected <- FALSE
  on.exit(if (!collected) {
    mirai::stop_mirai(task)
    rm(task)
    gc(verbose = FALSE)
  }, add = TRUE)

  value     <- mirai::collect_mirai(task)
  collected <- TRUE

  ### A miraiError is the body's own failure (an option it could not set),
  ### raised as the condition it was; any other error value, the process's
  ### end without a result
  value <- task_value(value, function(v) abort_process_died(v, what, call = call))

  if (length(value$output) > 0) cat(value$output, sep = "\n")

  for (cnd in value$conditions) {

    if (inherits(cnd, "warning")) warning(cnd) else if (inherits(cnd, "message")) message(cnd)

  }

  if (!is.null(value$error)) stop(value$error)

  value$value

}

#' The names an isolated call's body keeps for itself
#'
#' @keywords internal
#' @noRd
ISOLATED_NAMES <- c(".horizons_libs", ".horizons_options", ".horizons_seed", ".horizons_value",
                    ".horizons_error", ".horizons_conditions", ".horizons_output")

#' The body an isolated call runs, around its expression
#'
#' @description
#' Base R alone, so the process loads nothing the expression does not name.
#' A condition is kept without its call and trace, which can hold the data
#' the call was given.
#'
#' @param expr A call.
#' @return A call returning a list: `value`, `error` (the condition the call
#'   ended in, or `NULL`), `conditions` (the warnings and messages raised, in
#'   order) and `output` (what it printed, as lines).
#' @keywords internal
#' @noRd
isolated_body <- function(expr) {

  bquote({

    .libPaths(.horizons_libs)
    if (length(.horizons_options) > 0) options(.horizons_options)
    set.seed(.horizons_seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
             sample.kind = "Rejection")

    .horizons_error      <- NULL
    .horizons_conditions <- list()

    .horizons_output <- utils::capture.output(
      .horizons_value <- tryCatch(
        withCallingHandlers(
          .(expr),
          warning = function(w) {
            w$call <- NULL
            w$trace <- NULL
            .horizons_conditions[[length(.horizons_conditions) + 1L]] <<- w
            if (!is.null(findRestart("muffleWarning"))) invokeRestart("muffleWarning")
          },
          message = function(m) {
            m$call <- NULL
            .horizons_conditions[[length(.horizons_conditions) + 1L]] <<- m
            if (!is.null(findRestart("muffleMessage"))) invokeRestart("muffleMessage")
          }
        ),
        error = function(e) {
          .horizons_error <<- e
          NULL
        }
      )
    )

    list(value = .horizons_value, error = .horizons_error,
         conditions = .horizons_conditions, output = .horizons_output)

  })

}

#' Stop the call because an isolated process ended without its result
#'
#' @description
#' The process [run_isolated()] started for one call ended without sending
#' its value back: killed, most often by the system for memory, which mirai
#' reads as `errorValue` 19 (Connection reset). A daemon of the user's pool
#' did not die, so no pool shrank. The message starts with `PROCESS_DIED`, by
#' which [check_tune_result()] finds it in tune's notes, where tune keeps an
#' error raised inside a fit as text.
#'
#' @param value The task's error value.
#' @param what Character(1). What ran in the process.
#' @param call The frame the error reports.
#' @return Does not return; aborts with class `horizons_error_process_died`
#'   (kind `run`).
#' @keywords internal
#' @noRd
abort_process_died <- function(value, what, call = rlang::caller_env()) {

  code   <- as.integer(value)
  reason <- if (identical(code, 19L)) "{.code errorValue} 19, Connection reset" else "{.code errorValue} {code}"

  abort_horizons(c(
    paste0(PROCESS_DIED, " {what} ended without returning its result (mirai's ", reason, ")."),
    "i" = "A process ends this way when it is killed, most often by the system when memory runs out.",
    "i" = "{.code control_horizons(isolate = FALSE)} runs the work in this session instead."
  ), kind = "run", class = "horizons_error_process_died", call = call)

}

## ---------------------------------------------------------------------------
## The configs axis's dispatch
## ---------------------------------------------------------------------------

#' Run configurations on the daemons, a window at a time
#'
#' @description
#' The configurations start longest first ([dispatch_order()]): with mirai's
#' dispatcher a task starts in the order it was submitted, so the short ones
#' fill the daemons at the end. At most as many are in flight as there are
#' daemons connected, read again each round, so the window narrows when a
#' daemon leaves and widens when one joins. Each task carries its
#' configuration's index and the path of the file `evaluate()` wrote the
#' payload to, which its daemon reads, so this session holds the
#' configurations and the path, and none of the data a running
#' configuration was given. Each is collected as it finishes and the next
#' submitted. Every wait goes through [await_round()], so a window left with
#' no daemon stops the run rather than waiting for ever.
#'
#' What a task returns is read here ([task_value()]). A row is appended to
#' `finished$rows`. A `miraiError` is the task's own error, raised again as
#' the condition it was. Any other error value is the transport, and the
#' window stops the run itself, as `horizons_error_systemic` with the rows
#' so far: `errorValue` 19 is the daemon's death. A daemon that could not
#' read the payload returns what went wrong instead of a row, and the
#' window stops the run on it the same way ([abort_payload_unread()]).
#' Every task that finished in a pass is collected before the first that
#' ended in an error is raised, so a stop carries every row that came in
#' with it. A pool left with no daemon stops the run here too
#' ([abort_no_daemons()]).
#'
#' When the window exits by an error, an interrupt or a stop of its own, it
#' cancels every task still in flight (`mirai::stop_mirai()`), so a
#' configuration still running stops before it writes its checkpoint. It is
#' an `on.exit()`, not a calling handler, so it runs on all three. A
#' configuration in an isolated fit stops at once, since its daemon waits on
#' the fit's process in R, and the process is ended as the wait is left
#' ([run_isolated()]). One inside another compiled fit stops only when that
#' fit returns, and writes nothing, since the interrupt is raised once R
#' checks for it, long before the checkpoint. Cancelling needs mirai's dispatcher: without it
#' `stop_mirai()` does nothing and the task runs to its end, which
#' `evaluate()` warns of ([daemons_dispatch()]).
#'
#' Each task sends the worker function and its arguments, the
#' configuration's row index and the payload's path, not a closure: the
#' function's environment is horizons' namespace, which a daemon resolves to
#' its installed horizons, so nothing in this frame or the caller's travels.
#'
#' @param configs The configurations still to run, the payload's `configs`.
#' @param payload_path Character(1). The absolute path of the file holding
#'   the payload every task reads ([evaluate_config_worker()]).
#' @param finished An environment whose `rows` the window appends each row
#'   to, in the order they finish.
#' @param rows_so_far A function of no arguments returning the results table
#'   a stop carries: the rows resumed and those in `finished$rows`.
#' @param output_dir The run's `output_dir`, for a stop's message.
#' @param call The frame a stop reports: the verb's.
#' @return `NULL`, invisibly, once every configuration's row is in
#'   `finished$rows`.
#' @keywords internal
#' @noRd
dispatch_configs <- function(configs, payload_path, finished, rows_so_far, output_dir = NULL,
                             call = rlang::caller_env()) {

  n_configs <- nrow(configs)
  start     <- dispatch_order(configs)

  ### The task's call, quoted here: its names are the task's arguments
  task_call <- quote(worker(config_i, payload_path))
  tasks     <- list()
  next_i    <- 1L
  n_done    <- 0L

  no_daemons <- function() {
    abort_no_daemons(rows_so_far(), n_configs - n_done, output_dir, call = call)
  }

  ## On an error, an interrupt or a stop from the window itself; on its
  ## return nothing is in flight
  on.exit(mirai::stop_mirai(tasks), add = TRUE)

  repeat {

    ## Collect what has finished, the whole pass before any stop
    ended <- NULL

    for (key in names(tasks)) {

      if (mirai::unresolved(tasks[[key]])) next

      value        <- tasks[[key]]$data
      tasks[[key]] <- NULL

      ### A miraiError is an error value too; a payload the daemon could not
      ### read is neither, and stops the run as they do
      if (mirai::is_error_value(value) || inherits(value, "horizons_payload_unread")) {
        if (is.null(ended)) ended <- list(value = value, config_id = configs$config_id[as.integer(key)])
        next
      }

      finished$rows <- c(finished$rows, list(value))
      n_done        <- n_done + 1L

    }

    if (!is.null(ended)) {
      if (inherits(ended$value, "horizons_payload_unread")) {
        abort_payload_unread(ended$value$path, ended$value$reason, ended$config_id, rows_so_far(),
                             output_dir, call = call)
      }
      task_value(ended$value, function(value) {
        abort_task_ended(value, ended$config_id, rows_so_far(), output_dir, call = call)
      })
    }

    if (n_done == n_configs) return(invisible(NULL))

    ## Submit while there are daemons free
    width <- connected_daemons()

    while (next_i <= n_configs && length(tasks) < width) {

      i      <- start[next_i]
      next_i <- next_i + 1L

      tasks[[as.character(i)]] <- mirai::mirai(
        task_call,
        .args    = list(worker = evaluate_config_worker, config_i = i, payload_path = payload_path),
        .compute = NULL
      )

    }

    if (length(tasks) == 0L) no_daemons()

    await_round(tasks, no_daemons)

  }

}

#' Stop the call because a task on the daemons ended without a result
#'
#' @description
#' A task's transport failed: its daemon died (`errorValue` 19), or another
#' error value came back in place of its result. Raised where the task is
#' read, by the window ([dispatch_configs()]) and by the check of the
#' daemons at the door ([daemon_reports()]), with the rows so far, as every
#' other stop on the daemons is. Its message names the error value.
#'
#' @param value The task's error value.
#' @param config_id Character(1), the configuration the task ran, or `NULL`
#'   for the check at the door.
#' @param results The rows finished so far, carried as `results`.
#' @param output_dir The run's `output_dir`, or `NULL`.
#' @param verb Character(1). The verb that stops.
#' @param call The frame the error reports.
#' @return Does not return; aborts with class `horizons_error_systemic`
#'   (kind `run`).
#' @keywords internal
#' @noRd
abort_task_ended <- function(value, config_id, results, output_dir = NULL, verb = "evaluate",
                             call = rlang::caller_env()) {

  code <- as.integer(value)
  died <- identical(code, 19L)

  abort_horizons(c(
    if (is.null(config_id) && died) {
      "A mirai daemon stopped while {.fn {verb}} checked the daemons before the run ({.code errorValue} 19, Connection reset), so the run cannot go on."
    } else if (is.null(config_id)) {
      "A task checking the daemons before the run ended in mirai's {.code errorValue} {code} rather than a result, so {.fn {verb}} cannot go on."
    } else if (died) {
      "The mirai daemon running configuration {.val {config_id}} stopped while it ran ({.code errorValue} 19, Connection reset), so {.fn {verb}} cannot go on."
    } else {
      "Configuration {.val {config_id}}'s task ended in mirai's {.code errorValue} {code} rather than a result, so {.fn {verb}} cannot go on."
    },
    resume_hint(results, output_dir)
  ), kind = "run", class = "horizons_error_systemic", results = results, call = call)

}

#' Stop the call because a daemon could not read the configurations' data
#'
#' @description
#' On the configs axis each task reads the configurations' data from the file
#' `evaluate()` wrote under `output_dir`, and a daemon that cannot read it
#' returns what went wrong instead of a row ([evaluate_config_worker()]).
#' Every configuration after it would fail the same way, so the window stops
#' the run here, as systemic, with the rows so far, as it stops on a daemon's
#' death ([abort_task_ended()]). The message says what the axis needs: each
#' daemon reads the file at the path this session gives it, and no other run
#' is started into the same `output_dir`, which would remove the file.
#'
#' @param path Character(1). The file the daemon was given.
#' @param reason Character(1). What base R said when the daemon read it.
#' @param config_id Character(1). The configuration the task ran.
#' @param results The rows finished so far, carried as `results`.
#' @param output_dir The run's `output_dir`.
#' @param verb Character(1). The verb that stops.
#' @param call The frame the error reports.
#' @return Does not return; aborts with class `horizons_error_systemic`
#'   (kind `run`).
#' @keywords internal
#' @noRd
abort_payload_unread <- function(path, reason, config_id, results, output_dir = NULL, verb = "evaluate",
                                 call = rlang::caller_env()) {

  abort_horizons(c(
    "The mirai daemon running configuration {.val {config_id}} could not read the configurations' data from {.path {path}}, so {.fn {verb}} cannot go on.",
    "x" = "{reason}",
    "i" = "On the configs axis each daemon reads the configurations' data from {.arg output_dir}, at the path this session gives it, so daemons on another machine need {.arg output_dir} on a file system they share with it, at that path. {.code parallelize_over = \"resamples\"} runs without it. One {.arg output_dir} serves one run at a time, and a run started into it removes an earlier run's file.",
    resume_hint(results, output_dir)
  ), kind = "run", class = "horizons_error_systemic", results = results, call = call)

}

#' Rows in configuration order
#'
#' @param rows List of result rows, each with a `config_id`.
#' @param config_ids Character. Every configuration's id, in configuration
#'   order.
#' @return `rows`, ordered as `config_ids`.
#' @keywords internal
#' @noRd
in_config_order <- function(rows, config_ids) {

  ids <- vapply(rows, function(row) row$config_id, character(1))

  rows[order(match(ids, config_ids))]

}

## ---------------------------------------------------------------------------
## Thread pinning (parent side)
## ---------------------------------------------------------------------------

#' Pin BLAS, OpenMP, data.table and ranger threads in the parent
#'
#' @description
#' Pinning has to happen in the parent before workers are forked or spawned.
#' OpenBLAS reads its thread count when it loads, so setting
#' `OPENBLAS_NUM_THREADS` inside a worker after the library is loaded is dead
#' code (proven 2026-09-15: the environment variable changed nothing, the
#' runtime call did). `RhpcBLASctl` provides the runtime call and is a
#' Suggests; without it the parent relies on the environment variables being
#' set before R starts.
#'
#' @return A function that restores the previous settings; call it from
#'   `on.exit()`.
#' @keywords internal
pin_parent_threads <- function() {

  restore <- list()

  if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {

    restore$blas <- tryCatch(RhpcBLASctl::blas_get_num_procs(), error = function(e) NULL)
    restore$omp  <- tryCatch(RhpcBLASctl::omp_get_max_threads(), error = function(e) NULL)

    tryCatch(RhpcBLASctl::blas_set_num_threads(1L), error = function(e) NULL)
    tryCatch(RhpcBLASctl::omp_set_num_threads(1L),  error = function(e) NULL)

  }

  if (requireNamespace("data.table", quietly = TRUE)) {

    restore$dt <- data.table::getDTthreads()
    data.table::setDTthreads(1L)

  }

  restore$ranger <- getOption("ranger.num.threads")
  options(ranger.num.threads = 1L)

  function() {

    if (!is.null(restore$blas)) {
      tryCatch(RhpcBLASctl::blas_set_num_threads(restore$blas), error = function(e) NULL)
    }

    if (!is.null(restore$omp)) {
      tryCatch(RhpcBLASctl::omp_set_num_threads(restore$omp), error = function(e) NULL)
    }

    if (!is.null(restore$dt)) data.table::setDTthreads(restore$dt)

    options(ranger.num.threads = restore$ranger)

    invisible(NULL)

  }

}
