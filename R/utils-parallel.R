## ---------------------------------------------------------------------------
## Parallelism helpers: axis resolution, the daemons, the configs axis's
## dispatch, thread pinning
## ---------------------------------------------------------------------------
##
## The rule these implement: the user owns the mirai daemons; the package owns
## which axis of its work they are applied to, and how the work is arranged
## across them. horizons never sets, resizes or resets the user's daemons. It
## calls mirai::daemons() in one place, inside a configuration's task on one
## of them (start_inner_daemons()): the inner daemons a long configuration
## runs its folds on, set on that daemon's own default profile, which no
## other process sees, and reset as the task ends. It never uses
## with(daemons()), with_daemons() or local_daemons(), whose scope would send
## tune's own tasks to another profile. It reads the pool with mirai::info()
## by name, never status(), and every call on the pool names
## `.compute = NULL`, the current profile, which is the one tune reads.

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
#' On the configs axis the worker runs tune sequentially, unless the run's
#' plan gives its configuration inner daemons (`split_plan()`), which it
#' starts on its own daemon. `control_horizons()` refuses `"both"`.
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
#' Each configuration's cell of `CONFIG_COST_SECONDS` (the seconds a
#' configuration of its model and feature selection took on the measured
#' local grid), times the run's rows over the grid's (`CONFIG_ROWS_REF`)
#' raised to its model's `CONFIG_ROW_EXPONENT`. The configs axis starts the
#' longest first by it ([dispatch_order()]) and gives the long ones inner
#' daemons by it ([split_plan()]), and `monitor_evaluate()` weights its ETA
#' by it, from the run's manifest. Only each configuration's share of the
#' run is read, so the scale of the seconds does not matter. One
#' configuration's estimate can be off by a factor of two either way, from
#' when its Bayesian search stops, which nothing knows before it runs.
#'
#' @param configs The configurations, with `model` and `feature_selection`
#'   columns.
#' @param n_rows Numeric(1). The rows the configurations are tuned on: the
#'   run's training part.
#' @return Numeric, one unnamed cost per row of `configs`.
#' @keywords internal
#' @noRd
config_costs <- function(configs, n_rows) {

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

  cost * (as.numeric(n_rows) / CONFIG_ROWS_REF)^unname(CONFIG_ROW_EXPONENT[configs$model])

}

#' The order the configs axis starts its configurations in
#'
#' @description
#' Longest first: by their costs ([config_costs()]), so the longest start
#' first and the short ones fill the daemons at the end. Ties keep grid
#' order, so the order is deterministic. The results do not depend on it:
#' every draw is seeded from the run's seed and its own name, each fit of an
#' engine that carries state from one fit to the next (`cubist`) runs in a
#' process of its own, and the rows are put back in configuration order
#' before they are bound.
#'
#' @param costs Numeric: each configuration's cost, in grid order.
#' @return Integer: the positions of `costs`, in the order to start them.
#' @keywords internal
#' @noRd
dispatch_order <- function(costs) {

  ### order() leaves ties in their original order, which is grid order
  order(-costs)

}

#' How many daemons each configuration's folds run on
#'
#' @description
#' With one configuration a daemon, a run finishes no sooner than its
#' longest configuration. A configuration whose share of the run's work is
#' more than `SPLIT_SHARE / n` therefore runs its folds on inner daemons
#' started on its own daemon, taking that many of the window's `n` slots
#' ([dispatch_units()]), so no more than `n` cores are busy. It gets the
#' smallest divisor of the fold count, at most `n`, that brings its cost
#' under `SPLIT_SHARE` times the run's work over `n`, else the largest such
#' divisor: tune sends a call's folds out at once, so a count that divides
#' them leaves no daemon idle in the last round. With no divisor between 1
#' and `n` (the default 5 folds on fewer than 5 daemons) nothing is split.
#' The plan is made once, from the configurations still to run, and holds
#' for the run: a daemon that joins or leaves changes the window, not the
#' plan.
#'
#' @param costs Numeric: the configurations' costs ([config_costs()]).
#' @param n Integer(1). The daemons connected.
#' @param folds Integer(1). The cross-validation folds.
#' @return Integer, one per cost: the daemons each configuration's folds
#'   run on, 1 for a configuration that runs as one task.
#' @keywords internal
#' @noRd
split_plan <- function(costs, n, folds) {

  share    <- SPLIT_SHARE * sum(costs) / n
  divisors <- seq_len(folds)[folds %% seq_len(folds) == 0L]
  divisors <- divisors[divisors > 1L & divisors <= n]

  vapply(costs, function(cost) {

    if (length(divisors) == 0L || cost <= share) return(1L)

    under <- divisors[cost / divisors <= share]

    as.integer(if (length(under) > 0L) min(under) else max(divisors))

  }, integer(1), USE.NAMES = FALSE)

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

#' Say that the daemons have no dispatcher, once, on the verb's log
#'
#' @description
#' The window horizons runs its own tasks through ([dispatch_units()])
#' starts the longest first and cancels those in flight when it stops, both
#' through mirai's dispatcher: without it the work runs as asked, but loses
#' both, which the run says once, as an `input` warning, and on the tree's
#' parallel line. Said wherever the window runs: the configs axis's
#' configurations, `evaluate()`'s fold preparations on the resamples axis,
#' and `fit()`'s.
#'
#' @param collector The verb's collector, from `log_open()`.
#' @param what Character(1). What the window runs, e.g. `"configurations"`.
#' @param verb Character(1). The verb that runs it.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
log_no_dispatcher <- function(collector, what, verb) {

  log_warning(collector, "input", c(
    "The mirai daemons were set with {.code dispatcher = FALSE}, so {.fn {verb}} cannot start the longest {what} first or cancel the ones still running when it stops.",
    "i" = "mirai keeps its dispatcher unless {.code dispatcher = FALSE} is set; set the daemons again without it."
  ))

  invisible(NULL)

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
#' daemon's own limit, which neither mirai nor horizons restarts it from.
#'
#' @param results The rows finished so far, carried as `results`, or `NULL`
#'   when the verb has none to carry.
#' @param n_left Integer(1) or `NULL`. What is still to run, when the caller
#'   knows it: `evaluate()`'s configurations, or `fit()`'s members.
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
    } else if (identical(verb, "fit")) {
      "No mirai daemon is connected, so {.fn {verb}} stopped with {n_left} member{?s} still to fit."
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
#' daemon's processes are counted, so work that starts more of them counts
#' them in its units: a split configuration's inner daemons are each counted
#' as one of these ([split_need()]).
#'
#' @param models Character. The models the run fits.
#' @param isolate Logical(1). `control_horizons(isolate = )`.
#' @return Named integer: one count per process of `DAEMON_MEMORY`, the
#'   `daemon` and its `isolated` fit.
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

#' The daemons on this machine and the memory they need of it
#'
#' @description
#' The one rule both memory checks count by ([check_daemon_memory()],
#' [check_split_memory()]): the daemons counted are those on this machine,
#' by the host name and process id each reported; daemons elsewhere are not
#' checked, since only this machine's memory can be read here.
#'
#' @param reports The daemons' reports, one per task ([daemon_reports()]).
#' @param processes Named integer, from [daemon_processes()].
#' @param n_rows,n_predictors The run's data.
#' @return A list: `n` (the daemons on this machine), `need` (what each
#'   needs, [daemon_need()]) and `available` ([memory_available()], `NA`
#'   when it cannot be read; not read when `n` is 0).
#' @keywords internal
#' @noRd
local_daemon_memory <- function(reports, processes, n_rows, n_predictors) {

  here  <- Sys.info()[["nodename"]]
  local <- Filter(function(r) identical(r$host, here), reports)
  n     <- length(unique(vapply(local, function(r) r$pid, integer(1))))

  list(n         = n,
       need      = daemon_need(processes, n_rows, n_predictors),
       available = if (n > 0L) memory_available() else NA_real_)

}

#' Warn when the daemons on this machine would need more memory than it has
#'
#' @description
#' Run at a parallel `evaluate()`'s or `fit()`'s door, once the daemons have
#' reported ([daemon_reports()]). The daemons counted are those on this
#' machine ([local_daemon_memory()]). When those daemons' need
#' ([daemon_need()]) is more than the memory available
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

  processes <- daemon_processes(models, isolate)
  local     <- local_daemon_memory(reports, processes, n_rows, n_predictors)
  n_local   <- local$n

  if (n_local == 0L) return(FALSE)

  available <- local$available

  if (!is.finite(available)) {
    log_note(collector, "{.fn {verb}} could not read the memory available on this machine, so it did not check that its mirai daemons fit in it.")
    return(FALSE)
  }

  need  <- local$need
  total <- n_local * need

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

#' The predictors each configuration's preparation keeps
#'
#' @description
#' What [split_need()] sizes a configuration's fold frames by. A
#' configuration with no feature selection keeps every predictor of the role
#' map. One with a selection keeps what its fold preparations kept, the mean
#' over its folds, which the fold stage knows before any configuration runs
#' ([fold_stage_record()]); for one the stage built nothing for, every
#' predictor too, which is the most it can keep.
#'
#' @param configs The configurations, with `preprocessing`,
#'   `feature_selection` and `transformation` columns.
#' @param role_map Tibble with `variable` and `role` columns.
#' @param record The fold stage's record, or `NULL` when it built nothing.
#' @return Numeric, one per row of `configs`.
#' @keywords internal
#' @noRd
config_kept <- function(configs, role_map, record = NULL) {

  selection <- configs$feature_selection
  kept      <- rep(as.numeric(sum(role_map$role == "predictor")), nrow(configs))

  if (is.null(record) || nrow(record) == 0) return(kept)

  ## The record names each prep by its preprocessing, selection and, where
  ## the selection reads the outcome, transformation: the unit's name
  ## without the verb and the fold
  folds   <- record[record$fold != "training", , drop = FALSE]
  by_prep <- tapply(folds$n_predictors, folds$prep, mean)

  label <- vapply(seq_len(nrow(configs)), function(i) {
    paste(fold_prep_scope("evaluate", configs[i, ])[-(1:2)], collapse = "/")
  }, character(1))

  found <- selection != "none" & label %in% names(by_prep)
  kept[found] <- unname(by_prep[label[found]])

  kept

}

#' The memory a run's inner daemons add on this machine
#'
#' @description
#' What the split adds to its daemons' own need ([daemon_need()]), for the
#' split configurations that can be in flight at once: taken in start
#' order, each while its daemons fit in `n` beside those taken, as the
#' window takes them. For each, its inner daemons, each counted as a daemon
#' of the run with its isolated fit's process ([daemon_processes()]), and
#' what its own daemon holds while tune's fold tasks wait for them: the
#' `queue` line of `DAEMON_MEMORY` per byte of its folds' frames, `folds`
#' frames of the training rows by the predictors it keeps.
#'
#' @param inner Integer: each configuration's daemons ([split_plan()]).
#' @param order Integer: the configurations' start order ([dispatch_order()]).
#' @param n Integer(1). The daemons connected.
#' @param processes Named integer, from [daemon_processes()].
#' @param n_rows,n_predictors The run's data: its rows and predictors.
#' @param n_train Numeric(1). The training rows the folds are cut from.
#' @param kept Numeric: the predictors each configuration keeps
#'   ([config_kept()]).
#' @param folds Integer(1). The cross-validation folds.
#' @return Numeric(1), in bytes.
#' @keywords internal
#' @noRd
split_need <- function(inner, order, n, processes, n_rows, n_predictors, n_train, kept, folds) {

  taken <- integer(0)
  used  <- 0L

  for (i in order[inner[order] > 1L]) {
    if (used + inner[[i]] <= n) {
      taken <- c(taken, i)
      used  <- used + inner[[i]]
    }
  }

  daemon <- daemon_need(processes, n_rows, n_predictors)
  queue  <- DAEMON_MEMORY$queue[["base"]] +
    DAEMON_MEMORY$queue[["per_byte"]] * folds * 8 * as.numeric(n_train) * as.numeric(kept[taken])

  sum(inner[taken] * daemon + queue)

}

#' Split nothing when the inner daemons would not fit in this machine's memory
#'
#' @description
#' A split is horizons' choice, not the user's, so it is made only when it
#' fits: when the daemons on this machine ([check_daemon_memory()]) and what
#' the split adds ([split_need()]) together need more than the memory
#' available, no configuration is split, with a note naming the figures. When
#' the daemons alone already need more, the check at the door has warned,
#' and nothing is split either. A reading that cannot be made leaves the
#' plan, as the door's check does, which noted it.
#'
#' @param reports The daemons' reports, one per task.
#' @param extra Numeric(1). What the split adds, in bytes ([split_need()]).
#' @param processes Named integer, from [daemon_processes()].
#' @param n_rows,n_predictors The run's data.
#' @param n_split Integer(1). The configurations the plan splits.
#' @param collector The verb's collector.
#' @param verb Character(1).
#' @return Logical(1): `TRUE` when the split fits, `FALSE` when the plan is
#'   to be emptied, which the note says.
#' @keywords internal
#' @noRd
check_split_memory <- function(reports, extra, processes, n_rows, n_predictors, n_split, collector, verb) {

  local     <- local_daemon_memory(reports, processes, n_rows, n_predictors)
  n_local   <- local$n
  available <- local$available

  if (n_local == 0L || !is.finite(available)) return(TRUE)

  need <- n_local * local$need

  if (need + extra <= available) return(TRUE)

  gb <- function(bytes) format(round(bytes / 1e9, 1), nsmall = 1)

  log_note(collector, c(
    "{.fn {verb}} ran each configuration on one daemon: the inner daemons it would start for {n_split} long configuration{?s} would need about {gb(extra)} GB on this machine beside the {gb(need)} GB its {n_local} daemon{?s} {?needs/need}, and {gb(available)} GB is available."
  ))

  FALSE

}

#' The progress tree's lines on how a run goes: its parallel axis and its
#' isolated fits
#'
#' @description
#' The one place `evaluate()`'s and `fit()`'s trees say it, so the two verbs
#' say it alike: the Parallel line for a parallel axis (its daemons, and
#' whether they have no dispatcher or would need more memory than the
#' machine has), or that debug turned it off; then [isolation_tree_line()].
#'
#' @param axis Character(1). `"configs"`, `"resamples"` or `"sequential"`.
#' @param n_daemons Integer(1). The daemons the run began with.
#' @param models Character. The models the run fits.
#' @param isolate Logical(1). `control_horizons(isolate = )`.
#' @param no_dispatcher,short_memory Logical(1). The daemons have no
#'   dispatcher ([daemons_dispatch()]); they would need more memory than
#'   this machine has ([check_daemon_memory()]).
#' @param debug_off Logical(1). A parallel run asked for, run sequentially
#'   since debug runs in this session.
#' @return Character: the lines, without the tree's prefix.
#' @keywords internal
#' @noRd
parallel_tree_lines <- function(axis, n_daemons, models, isolate, no_dispatcher = FALSE,
                                short_memory = FALSE, debug_off = FALSE) {

  parallel <- if (axis != "sequential") {
    paste0("Parallel: over ", axis, " on mirai (", n_daemons, " daemons",
           if (no_dispatcher) ", no dispatcher",
           if (short_memory) ", more memory than available", ")")
  } else if (debug_off) {
    "Parallel: off, since debug runs in this session"
  }

  c(parallel, isolation_tree_line(models, isolate))

}

#' The progress tree's lines on the configurations run on inner daemons
#'
#' @description
#' How many long configurations run their folds on inner daemons, and on how
#' many each, with the setting that turns it off; or, when the plan would
#' have split some, that the setting turned it off, or that the inner
#' daemons would not fit in this machine's memory. When some are split and
#' BLAS cannot be pinned ([blas_pinnable()]), a second line says so, with
#' how to pin it. No line when the plan splits nothing. Read from the plan,
#' before any configuration runs.
#'
#' @param planned Integer: the daemons [split_plan()] gave each
#'   configuration.
#' @param inner Integer: the daemons each runs on.
#' @param setting Logical(1). `control_horizons(inner_daemons = )`.
#' @param pinned Logical(1). Whether BLAS and OpenMP can be pinned at run
#'   time ([blas_pinnable()]).
#' @return Character: the lines, without the tree's prefix, or `NULL`.
#' @keywords internal
#' @noRd
inner_daemons_tree_line <- function(planned, inner, setting, pinned) {

  n_planned <- sum(planned > 1L)
  n_split   <- sum(inner > 1L)

  if (n_split > 0L) {
    counts <- inner[inner > 1L]
    c(paste0("Inner daemons: ", n_split, " long configuration", if (n_split != 1L) "s",
             " run", if (n_split == 1L) "s", " ", if (n_split == 1L) "its" else "their", " folds on ",
             if (n_split == 1L) counts else paste0(paste(counts[-n_split], collapse = ", "), " and ", counts[n_split]),
             " (control_horizons(inner_daemons = FALSE) turns it off)"),
      if (!isTRUE(pinned)) {
        paste0("BLAS threads: not pinned, so a configuration on inner daemons can differ from an unsplit run in ",
               "rounding (install RhpcBLASctl, or Sys.setenv(OPENBLAS_NUM_THREADS = 1) before mirai::daemons())")
      })
  } else if (n_planned > 0L && !isTRUE(setting)) {
    paste0("Inner daemons: off, as asked; ", n_planned, " long configuration", if (n_planned != 1L) "s",
           " run", if (n_planned == 1L) "s on one daemon" else " each on one daemon")
  } else if (n_planned > 0L) {
    "Inner daemons: none, since they would not fit in this machine's memory (see the log)"
  }

}

#' Say on the log which configurations ran their folds on inner daemons
#'
#' @description
#' Written once the window has run, from what each configuration's task
#' says of its inner daemons ([evaluate_config_worker()]): a note naming
#' each configuration whose inner daemons connected and ran its folds, and
#' on how many, with the setting that turns it off; a note naming each the
#' plan split whose inner daemons did not connect, so it ran its folds on
#' its own daemon; and, when BLAS could not be pinned ([blas_pinnable()])
#' and some ran split, a note saying they can differ from an unsplit run in
#' rounding, with how to pin it. With the setting off, read from the plan
#' alone: a note when it would have split some, as the run notes isolation
#' turned off. When the memory check emptied the plan it has said so itself
#' ([check_split_memory()]).
#'
#' @param collector The verb's collector, from `log_open()`.
#' @param ids Character: the configurations, in the plan's order.
#' @param planned,inner,setting,pinned As [inner_daemons_tree_line()]'s.
#' @param records Named list: each configuration's record of its inner
#'   daemons, from its row, by configuration (`finished$inner`).
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
log_inner_daemons <- function(collector, ids, planned, inner, setting, pinned, records) {

  if (!isTRUE(setting)) {

    would <- ids[planned > 1L]
    n     <- length(would)

    if (n > 0L) {
      log_note(collector,
        "{.fn evaluate} ran each configuration on one daemon, as {.code control_horizons(inner_daemons = FALSE)} asked; it would have run the folds of {n} long configuration{?s} on inner daemons.",
        ids = would)
    }

    return(invisible(NULL))

  }

  started   <- vapply(ids, function(id) isTRUE(records[[id]]$started), logical(1), USE.NAMES = FALSE)
  split     <- ids[inner > 1L & started]
  unstarted <- ids[inner > 1L & !started]

  if (length(split) > 0L) {

    n       <- length(split)
    details <- paste0(split, " on ", inner[match(split, ids)])

    log_note(collector, c(
      "{.fn evaluate} ran the folds of {n} long configuration{?s} on inner daemons it started on {?its/their} daemon{?s}, within the daemons set: {details}.",
      "i" = "{.code control_horizons(inner_daemons = FALSE)} runs each configuration on one daemon."
    ), ids = split)

  }

  if (length(unstarted) > 0L) {

    n <- length(unstarted)

    log_note(collector,
      "The inner daemons {.fn evaluate} started for {n} configuration{?s} did not connect within {INNER_START_SECONDS} seconds, so {cli::qty(n)}{?it/they} ran {?its/their} folds on {?its/their} own daemon{?s}: {.val {unstarted}}.",
      ids = unstarted)

  }

  if (length(split) > 0L && !isTRUE(pinned)) {

    n <- length(split)

    log_note(collector, c(
      "{.fn evaluate} could not pin BLAS and OpenMP to one thread on its daemons, since {.pkg RhpcBLASctl} is not installed, so the {n} configuration{?s} run on inner daemons can differ in floating-point rounding from the same run without them: {.val {split}}.",
      "i" = "Install {.pkg RhpcBLASctl} ({.code install.packages(\"RhpcBLASctl\")}), or set {.code Sys.setenv(OPENBLAS_NUM_THREADS = 1)} before {.code mirai::daemons()}, so every process runs BLAS on one thread."
    ), ids = split)

  }

  invisible(NULL)

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

#' Set the environment a process about to start inherits
#'
#' @description
#' A process mirai starts (an isolated call's, an inner daemon) reads its
#' library paths from `R_LIBS` and its thread counts from the variables
#' BLAS and OpenMP read as they load, not from settings made at run time in
#' this one. So each start sets the variables it needs here, and they are put
#' back as they were once the process has started.
#'
#' @param vars Named character: the variables and their values.
#' @return A function of no arguments that puts every variable back as it
#'   was, unset where it was unset.
#' @keywords internal
#' @noRd
launch_env <- function(vars) {

  was <- Sys.getenv(names(vars), unset = NA, names = TRUE)

  do.call(Sys.setenv, as.list(vars))

  function() {

    for (v in names(was)) {
      if (is.na(was[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, stats::setNames(list(was[[v]]), v))
    }

    invisible(NULL)

  }

}

#' Run one call in a process of its own
#'
#' @description
#' Sends `expr` to a new R process ([isolated_profile()]) and returns its
#' value here, as if it had run in this session:
#'
#' * The process starts with this session's library paths (`libraries`):
#'   `R_LIBS` names them while it is started, and its body sets them first,
#'   so it loads what this session would (a process started by mirai sees
#'   `R_LIBS` and the startup files, not paths set at run time), whatever
#'   its arguments hold. Then any `options` are set.
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
#'   `errorValue` 19) is `horizons_error_process_died` ([abort_process_died()]),
#'   and is recorded in this process ([note_process_death()]), where a
#'   Bayesian search reads it.
#' * Leaving before the value is back (an error, an interrupt) drops the
#'   call, which ends its process.
#'
#' The call sent is a base-R body around `expr`, and `args` are plain
#' values the body finds by name: nothing of horizons is in the body, so a
#' process running a fit loads only what `expr` names. A function or a
#' fitted object among `args` carries its environment or its package with
#' it, which the process loads from `libraries` as it reads its message.
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

  ## The process starts with these libraries on its startup path, not only
  ## given them in the body: a value sent that holds a function or a fitted
  ## object loads its package as the process reads the message, before the
  ## body runs. mirai starts the process inside mirai(), which it inherits
  ## this variable from.
  restore_env <- launch_env(c(R_LIBS = paste(libraries, collapse = .Platform$path.sep)))
  on.exit(restore_env(), add = TRUE)

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
  ### end without a result, recorded here as it is raised
  value <- task_value(value, function(v) {
    withCallingHandlers(abort_process_died(v, what, call = call),
                        horizons_error_process_died = note_process_death)
  })

  if (length(value$output) > 0) cat(value$output, sep = "\n")

  for (cnd in value$conditions) {

    if (inherits(cnd, "warning")) warning(cnd) else if (inherits(cnd, "message")) message(cnd)

  }

  if (!is.null(value$error)) stop(value$error)

  value$value

}

#' A call to `fn` with each of `args` passed by its own name
#'
#' @description
#' How isolated work is called: each argument is a symbol of its own name,
#' which the process finds among the values [run_isolated()] sends it (and
#' the session among the same values, for work it runs itself), so the call
#' holds none of the data, and nor does a trace that records it.
#'
#' @param fn The function, by name: a symbol, or a `pkg::fun` call.
#' @param args Named list: the values the call takes, every one named.
#' @return A call.
#' @keywords internal
#' @noRd
call_by_name <- function(fn, args) {

  if (length(args) > 0 && (is.null(names(args)) || any(!nzchar(names(args))))) {

    abort_horizons("Every argument of an isolated call must have a name of its own.", kind = "internal")

  }

  as.call(c(list(fn), stats::setNames(lapply(names(args), as.name), names(args))))

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
#' A warning or a message is kept without its call (and a warning without
#' its trace), which can hold the data the call was given; the error the
#' call ends in keeps both, since the caller reports a refusal's call.
#'
#' The body is parsed from text ([ISOLATED_BODY]) at each call rather than
#' written as code in this function, so it is the same plain call however
#' horizons was installed: code here would carry the source references of a
#' keep-source install, and under covr the counts it adds, both of which
#' travel with the body and make the process load horizons.
#'
#' @param expr A call.
#' @return A call returning a list: `value`, `error` (the condition the call
#'   ended in, or `NULL`), `conditions` (the warnings and messages raised, in
#'   order) and `output` (what it printed, as lines).
#' @keywords internal
#' @noRd
isolated_body <- function(expr) {

  ### substitute() through do.call(), so it works on the parsed call rather
  ### than on the symbol holding it
  do.call(substitute, list(str2lang(ISOLATED_BODY), list(.horizons_expr = expr)))

}

#' The text of the body an isolated call runs
#'
#' @description
#' [isolated_body()] parses it and puts the call to run in place of
#' `.horizons_expr`. Text, not code, for the reason given there.
#'
#' @keywords internal
#' @noRd
ISOLATED_BODY <- '{

  .libPaths(.horizons_libs)
  if (length(.horizons_options) > 0) options(.horizons_options)
  set.seed(.horizons_seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
           sample.kind = "Rejection")

  .horizons_error      <- NULL
  .horizons_conditions <- list()

  .horizons_output <- utils::capture.output(
    .horizons_value <- tryCatch(
      withCallingHandlers(
        .horizons_expr,
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

}'

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
## The deaths of isolated work's processes, where they were met
## ---------------------------------------------------------------------------
## tune keeps an error raised inside a fit as a note, where check_tune_result()
## finds a process's death, except in a Bayesian search, which drops a
## candidate that failed in every fold, note and all. So each death is also
## recorded in the process whose call met it (run_isolated()), and the search
## reads the record there once it returns: here, or on each daemon when its
## folds ran on them (bayes_search()).

#' The deaths of the processes `run_isolated()` started from this process
#'
#' @description
#' `messages` holds each death's message, as tune would keep it in a note,
#' oldest first, until [take_process_deaths()] takes them.
#'
#' @keywords internal
#' @noRd
process_deaths <- new.env(parent = emptyenv())
process_deaths$messages <- character(0)

#' Record the death of a process `run_isolated()` started
#'
#' @param cnd The `horizons_error_process_died` being raised.
#' @return `NULL`, invisibly; the condition goes on being raised.
#' @keywords internal
#' @noRd
note_process_death <- function(cnd) {

  process_deaths$messages <- c(process_deaths$messages, conditionMessage(cnd))

  invisible(NULL)

}

#' Take the deaths recorded in this process, leaving none
#'
#' @return Character: each death's message, oldest first.
#' @keywords internal
#' @noRd
take_process_deaths <- function() {

  taken <- process_deaths$messages
  process_deaths$messages <- character(0)

  taken

}

#' Take the deaths recorded here and, for work run on them, on each daemon
#'
#' @description
#' This process's record ([take_process_deaths()]) and, when `on_daemons`,
#' each connected daemon's, read with one task on each, as the door's check
#' reaches them ([daemon_reports()]), and left empty. A daemon gone before
#' its task returns is not waited for: its death is the verb's to read from
#' the count of connected daemons.
#'
#' @param on_daemons Logical(1). Whether the work ran on the user's daemons
#'   (tune's folds on the resamples axis).
#' @return Character: the deaths' messages.
#' @keywords internal
#' @noRd
isolated_deaths <- function(on_daemons) {

  here <- take_process_deaths()

  ### tune runs the folds here when no daemons are set, whatever allow_par
  ### says
  if (!isTRUE(on_daemons) || !mirai::daemons_set(.compute = NULL) || connected_daemons() == 0L) {

    return(here)

  }

  ## A mirai_map, whose elements are plain mirai. Called through the
  ## namespace, which every daemon that ran tune's folds has loaded
  tasks <- unclass(mirai::everywhere(
    get("take_process_deaths", envir = asNamespace("horizons"))(),
    .compute = NULL
  ))

  while (any(vapply(tasks, mirai::unresolved, logical(1))) && connected_daemons() > 0L) {
    Sys.sleep(DISPATCH_POLL_SECONDS)
  }

  there <- lapply(tasks, function(task) {
    if (!mirai::unresolved(task) && is.character(task$data)) task$data else character(0)
  })

  c(here, unlist(there, use.names = FALSE))

}

## ---------------------------------------------------------------------------
## The window: units of work on the daemons, a window at a time
## ---------------------------------------------------------------------------

#' Run units of work on the daemons, a window at a time
#'
#' @description
#' The one window horizons runs its own tasks through: the configs axis's
#' configurations ([dispatch_configs()]) and a run's fold preparations
#' ([build_fold_stage()]). The units start in `order` (longest first, by a
#' cost estimate): with mirai's dispatcher a task starts in the order it was
#' submitted, so the short ones fill the daemons at the end. Each unit takes
#' `slots(i)` of the window's slots, one for a unit that is one task and
#' more for a configuration that runs its folds on inner daemons
#' ([split_plan()]), and the slots in flight are at most the daemons
#' connected, read again each round, so the window narrows when a daemon
#' leaves and widens when one joins. A unit whose slots are not free is
#' passed over for the next in `order` that fits, and stays first in line;
#' one wider than the daemons connected starts once nothing is in flight, so
#' a pool that shrank cannot strand it. Each is collected as it finishes and
#' the next submitted. Every wait goes through [await_round()], so a window
#' left with no daemon stops the run rather than waiting for ever.
#'
#' What a task returns is read here ([task_value()]). A value is handed to
#' `collect()`. A `miraiError` is the task's own error, raised again as the
#' condition it was. Any other error value is the transport, and the window
#' stops the run itself, as `horizons_error_systemic` with the rows so far:
#' `errorValue` 19 is the daemon's death ([abort_task_ended()]). A daemon
#' that could not read a file the run wrote for it returns what went wrong
#' instead of a value, and the window stops the run on it the same way
#' ([abort_run_file_unread()]), and so does a configuration whose inner
#' daemons fell ([abort_inner_daemon_died()]). Every task that finished in a pass is
#' collected before the first that ended in an error is raised, so a stop
#' carries every row that came in with it. A pool left with no daemon stops
#' the run here too ([abort_no_daemons()]).
#'
#' When the window exits by an error, an interrupt or a stop of its own, it
#' cancels every task still in flight (`mirai::stop_mirai()`), so a unit
#' still running stops before it writes anything. It is an `on.exit()`, not
#' a calling handler, so it runs on all three. A configuration in an
#' isolated fit stops at once, since its daemon waits on the fit's process in
#' R, and the process is ended as the wait is left ([run_isolated()]). One
#' inside another compiled fit stops only when that fit returns, and writes
#' nothing, since the interrupt is raised once R checks for it, long before
#' the checkpoint. Cancelling needs mirai's dispatcher: without it
#' `stop_mirai()` does nothing and the task runs to its end, which the verb
#' warns of ([daemons_dispatch()]).
#'
#' Each task sends a worker function and its arguments, not a closure: the
#' function's environment is horizons' namespace, which a daemon resolves to
#' its installed horizons, so nothing in this frame or the caller's travels.
#'
#' @param n Integer(1). The units.
#' @param order Integer. Their positions, in the order to start them.
#' @param task A function of a unit's position giving its task: a list with
#'   `call` (the call, quoted, naming the worker and its arguments) and
#'   `args` (the worker as `worker`, and the arguments).
#' @param collect A function of a unit's position and the value its task
#'   returned.
#' @param label A function of a unit's position giving it for a stop's
#'   message: a list with `what` (e.g. `"configuration"`) and `id`.
#' @param rows_so_far A function of no arguments returning the results table
#'   a stop carries.
#' @param output_dir The run's `output_dir`, for a stop's message.
#' @param n_left Integer(1) or `NULL`. What is still to run, for a stop's
#'   message ([abort_no_daemons()]): `evaluate()`'s configurations, or
#'   `fit()`'s members; `NULL` counts the units not collected.
#' @param verb Character(1). The verb, for a stop's message.
#' @param slots A function of a unit's position giving the window's slots it
#'   takes, each a daemon: 1 for every unit by default.
#' @param call The frame a stop reports: the verb's.
#' @return `NULL`, invisibly, once every unit's value has been collected.
#' @keywords internal
#' @noRd
dispatch_units <- function(n, order, task, collect, label, rows_so_far, output_dir = NULL,
                           n_left = NULL, verb = "evaluate", slots = function(i) 1L,
                           call = rlang::caller_env()) {

  tasks   <- list()
  waiting <- order
  n_done  <- 0L

  no_daemons <- function() {
    abort_no_daemons(rows_so_far(), n_left %||% (n - n_done), output_dir, verb = verb, call = call)
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

      ### A miraiError is an error value too; a run file the daemon could not
      ### read, and an inner daemon's death, are neither, and stop the run as
      ### they do
      if (mirai::is_error_value(value) ||
          inherits(value, c("horizons_run_file_unread", "horizons_inner_daemon_died"))) {
        if (is.null(ended)) ended <- list(value = value, i = as.integer(key))
        next
      }

      collect(as.integer(key), value)
      n_done <- n_done + 1L

    }

    if (!is.null(ended)) {
      if (inherits(ended$value, "horizons_run_file_unread")) {
        abort_run_file_unread(ended$value, label(ended$i), rows_so_far(), output_dir, verb = verb, call = call)
      }
      if (inherits(ended$value, "horizons_inner_daemon_died")) {
        abort_inner_daemon_died(ended$value, label(ended$i), rows_so_far(), output_dir, verb = verb, call = call)
      }
      task_value(ended$value, function(value) {
        abort_task_ended(value, label(ended$i), rows_so_far(), output_dir, verb = verb, call = call)
      })
    }

    if (n_done == n) return(invisible(NULL))

    ## Submit, in order, each unit whose slots are free: one that does not
    ## fit is passed over and stays first in line, and one wider than the
    ## pool starts once nothing is in flight
    width <- connected_daemons()
    busy  <- sum(vapply(names(tasks), function(key) slots(as.integer(key)), integer(1)))

    for (i in waiting) {

      if (busy >= width && width > 0L) break

      need <- slots(i)

      if (width == 0L || (busy + need > width && busy > 0L)) next

      waiting <- waiting[waiting != i]
      busy    <- busy + need
      spec    <- task(i)

      ### mirai() takes the call from a symbol bound to it; any other
      ### expression it would send as written
      task_call <- spec$call
      tasks[[as.character(i)]] <- mirai::mirai(task_call, .args = spec$args, .compute = NULL)

    }

    if (length(tasks) == 0L) no_daemons()

    await_round(tasks, no_daemons)

  }

}

#' Run configurations on the daemons, a window at a time
#'
#' @description
#' The configs axis's configurations through the window ([dispatch_units()]),
#' longest first ([dispatch_order()]), each taking as many of the window's
#' slots as the daemons its folds run on (`inner`). Each task carries its
#' configuration's index and the path of the file `evaluate()` wrote the
#' payload to, which its daemon reads, so this session holds the
#' configurations and the path, and none of the data a running configuration
#' was given. A row is appended to `finished$rows` as it finishes, and what
#' it says of its inner daemons ([evaluate_config_worker()]) to
#' `finished$inner`, by configuration.
#'
#' @param configs The configurations still to run, the payload's `configs`.
#' @param costs Numeric: each configuration's cost ([config_costs()]), in
#'   the order of `configs`.
#' @param inner Integer: the daemons each configuration's folds run on
#'   ([split_plan()]), in the order of `configs`, which are the window's
#'   slots it takes.
#' @param payload_path Character(1). The absolute path of the file holding
#'   the payload every task reads ([evaluate_config_worker()]).
#' @param finished An environment whose `rows` the window appends each row
#'   to, in the order they finish, and `inner` each row's record of its
#'   inner daemons, named by configuration.
#' @param rows_so_far A function of no arguments returning the results table
#'   a stop carries: the rows resumed and those in `finished$rows`.
#' @param output_dir The run's `output_dir`, for a stop's message.
#' @param call The frame a stop reports: the verb's.
#' @return `NULL`, invisibly, once every configuration's row is in
#'   `finished$rows`.
#' @keywords internal
#' @noRd
dispatch_configs <- function(configs, costs, payload_path, finished, rows_so_far, output_dir = NULL,
                             inner = rep(1L, nrow(configs)), call = rlang::caller_env()) {

  ### The task's call, quoted here: its names are the task's arguments
  task_call <- quote(worker(config_i, payload_path, inner))

  dispatch_units(
    n       = nrow(configs),
    order   = dispatch_order(costs),
    task    = function(i) {
      list(call = task_call,
           args = list(worker = evaluate_config_worker, config_i = i, payload_path = payload_path,
                       inner = inner[[i]]))
    },
    ## What the row says of its inner daemons is kept apart from it
    collect = function(i, row) {
      finished$inner <- c(finished$inner, stats::setNames(list(attr(row, "inner_daemons")), configs$config_id[i]))
      attr(row, "inner_daemons") <- NULL
      finished$rows <- c(finished$rows, list(row))
    },
    label   = function(i) list(what = "configuration", id = configs$config_id[i]),
    rows_so_far = rows_so_far,
    output_dir  = output_dir,
    slots       = function(i) inner[[i]],
    call        = call
  )

}

#' Stop the call because a task on the daemons ended without a result
#'
#' @description
#' A task's transport failed: its daemon died (`errorValue` 19), or another
#' error value came back in place of its result. Raised where the task is
#' read, by the window ([dispatch_units()]) and by the check of the
#' daemons at the door ([daemon_reports()]), with the rows so far, as every
#' other stop on the daemons is. Its message names the error value.
#'
#' @param value The task's error value.
#' @param unit The unit the task ran, a list with `what` and `id`
#'   (`what` `"configuration"`, say), or `NULL` for the check at the door.
#' @param results The rows finished so far, carried as `results`.
#' @param output_dir The run's `output_dir`, or `NULL`.
#' @param verb Character(1). The verb that stops.
#' @param call The frame the error reports.
#' @return Does not return; aborts with class `horizons_error_systemic`
#'   (kind `run`).
#' @keywords internal
#' @noRd
abort_task_ended <- function(value, unit, results, output_dir = NULL, verb = "evaluate",
                             call = rlang::caller_env()) {

  code <- as.integer(value)
  died <- identical(code, 19L)
  what <- unit$what
  What <- if (!is.null(what)) paste0(toupper(substr(what, 1, 1)), substring(what, 2))
  id   <- unit$id

  abort_horizons(c(
    if (is.null(unit) && died) {
      "A mirai daemon stopped while {.fn {verb}} checked the daemons before the run ({.code errorValue} 19, Connection reset), so the run cannot go on."
    } else if (is.null(unit)) {
      "A task checking the daemons before the run ended in mirai's {.code errorValue} {code} rather than a result, so {.fn {verb}} cannot go on."
    } else if (died) {
      "The mirai daemon running {what} {.val {id}} stopped while it ran ({.code errorValue} 19, Connection reset), so {.fn {verb}} cannot go on."
    } else {
      "{What} {.val {id}}'s task ended in mirai's {.code errorValue} {code} rather than a result, so {.fn {verb}} cannot go on."
    },
    resume_hint(results, output_dir)
  ), kind = "run", class = "horizons_error_systemic", results = results, call = call)

}

#' Stop the call because a daemon could not read a file the run wrote for it
#'
#' @description
#' On the configs axis each task reads the configurations' data from the
#' file `evaluate()` wrote under `output_dir`, and each configuration's
#' daemon its fold preparations from the same directory. A daemon that
#' cannot read one returns what went wrong instead of a value
#' ([evaluate_config_worker()], [fold_prep_worker()]). Every unit after it
#' would fail the same way, so the window stops the run here, as systemic,
#' with the rows so far, as it stops on a daemon's death
#' ([abort_task_ended()]). The message says what the axis needs: each daemon
#' reads the files at the paths this session gives it, and no other run is
#' started into the same `output_dir`, which would remove them.
#'
#' @param unread The daemon's value: a list of class
#'   `horizons_run_file_unread` with `path` (the file it was given),
#'   `reason` (what base R said when it read it) and `file` (what the file
#'   holds, e.g. `"the configurations' data"`).
#' @param unit The unit the task ran, a list with `what` and `id`.
#' @param results The rows finished so far, carried as `results`.
#' @param output_dir The run's `output_dir`.
#' @param verb Character(1). The verb that stops.
#' @param call The frame the error reports.
#' @return Does not return; aborts with class `horizons_error_systemic`
#'   (kind `run`).
#' @keywords internal
#' @noRd
abort_run_file_unread <- function(unread, unit, results, output_dir = NULL, verb = "evaluate",
                                  call = rlang::caller_env()) {

  path   <- unread$path
  reason <- unread$reason
  file   <- unread$file
  what   <- unit$what
  id     <- unit$id

  abort_horizons(c(
    "The mirai daemon running {what} {.val {id}} could not read {file} from {.path {path}}, so {.fn {verb}} cannot go on.",
    "x" = "{reason}",
    "i" = "On the configs axis each daemon reads the configurations' data and their fold preparations from {.arg output_dir}, at the paths this session gives it, so daemons on another machine need {.arg output_dir} on a file system they share with it, at that path. {.code parallelize_over = \"resamples\"} runs without it. One {.arg output_dir} serves one run at a time, and a run started into it removes an earlier run's files.",
    resume_hint(results, output_dir)
  ), kind = "run", class = "horizons_error_systemic", results = results, call = call)

}

## ---------------------------------------------------------------------------
## Inner daemons: a long configuration's folds on daemons of its own
## ---------------------------------------------------------------------------
## A configuration the plan splits (split_plan()) starts its inner daemons
## inside its task, on its daemon's default profile, which tune reads and no
## other process sees (a daemon has no daemons of its own), and the window
## holds that many of its slots, so the cores busy stay within the user's
## daemons: the configuration's daemon waits in tune while its inner daemons
## fit the folds. The task that starts them resets them on its exit, however
## it ends; a daemon that dies takes them with it (mirai's autoexit).

#' Start inner daemons on this daemon for its configuration's folds
#'
#' @description
#' Each inner daemon starts with this process's library paths, so it loads
#' the horizons this daemon runs, and with BLAS and OpenMP on
#' `ENGINE_THREADS`, set before they load ([launch_env()]). Waits for all
#' `n` to connect, polling, up to `seconds`; past it the profile is reset and
#' the configuration runs its folds here instead. Once they are connected,
#' each is set up as a daemon is for a unit ([prepare_worker()]: the
#' engines' thread options and the configuration's packages), with BLAS and
#' OpenMP pinned at run time too ([pin_blas_threads()]), through
#' `mirai::everywhere()`, whose settings outlast a daemon's cleanup and
#' which tune's tasks wait behind; this waits for it, so what the inner
#' daemons have completed is their set-up alone. The caller registers the
#' reset on its own exit before calling.
#'
#' @param n Integer(1). The inner daemons, at least 2.
#' @param models,selections The configuration's model and feature selection
#'   ([prepare_worker()]).
#' @param seconds Numeric(1). How long they have to connect.
#' @return Logical(1): `TRUE` when all `n` connected.
#' @keywords internal
#' @noRd
start_inner_daemons <- function(n, models, selections, seconds = INNER_START_SECONDS) {

  threads     <- as.character(ENGINE_THREADS)
  restore_env <- launch_env(c(R_LIBS               = paste(.libPaths(), collapse = .Platform$path.sep),
                              OMP_NUM_THREADS      = threads,
                              OPENBLAS_NUM_THREADS = threads))
  on.exit(restore_env(), add = TRUE)

  mirai::daemons(url = mirai::local_url(), .compute = NULL)
  mirai::launch_local(n, .compute = NULL)

  until <- Sys.time() + seconds

  while (connected_daemons() < n) {

    if (Sys.time() >= until) {
      mirai::daemons(0, .compute = NULL)
      return(FALSE)
    }

    Sys.sleep(DISPATCH_POLL_SECONDS)

  }

  mirai::everywhere({
    pin()
    prepare(models, selections)
  }, .args = list(pin = pin_blas_threads, prepare = prepare_worker, models = models, selections = selections),
  .compute = NULL)[]

  TRUE

}

#' Run a configuration on its inner daemons, reading their deaths
#'
#' @description
#' tune does not raise an inner daemon's death: it binds the dead task's
#' value, which fails the configuration with dplyr's "Argument 1 must be a
#' data frame", or, inside a Bayesian iteration, ends the search quietly.
#' So the inner daemons connected are read after the configuration, on its
#' return and on its error, and fewer than `n` is a death: the value is then
#' the death's record, which the window stops the run on in the session,
#' where the rows so far are ([abort_inner_daemon_died()]).
#'
#' @param expr The configuration's work.
#' @param n Integer(1). The inner daemons connected before it.
#' @param id Character(1). The configuration's id.
#' @return The value of `expr`, or a list of class
#'   `horizons_inner_daemon_died` holding `id`, `started` (`n`) and
#'   `connected` (those left).
#' @keywords internal
#' @noRd
watch_inner_daemons <- function(expr, n, id) {

  died <- function() {
    structure(list(id = id, started = n, connected = connected_daemons()),
              class = "horizons_inner_daemon_died")
  }

  value <- rlang::try_fetch(expr, error = function(e) if (connected_daemons() < n) died() else rlang::zap())

  if (!inherits(value, "horizons_inner_daemon_died") && connected_daemons() < n) died() else value

}

#' Stop the call because a configuration's inner daemon died
#'
#' @param died The task's value, from [watch_inner_daemons()].
#' @param unit The unit the task ran, a list with `what` and `id`.
#' @param results The rows finished so far, carried as `results`.
#' @param output_dir The run's `output_dir`, or `NULL`.
#' @param verb Character(1). The verb that stops.
#' @param call The frame the error reports.
#' @return Does not return; aborts with class `horizons_error_systemic`
#'   (kind `run`).
#' @keywords internal
#' @noRd
abort_inner_daemon_died <- function(died, unit, results, output_dir = NULL, verb = "evaluate",
                                    call = rlang::caller_env()) {

  what <- unit$what
  id   <- unit$id
  n    <- died$started
  left <- died$connected

  abort_horizons(c(
    "An inner mirai daemon running {what} {.val {id}}'s folds stopped while it ran, so {.fn {verb}} cannot go on.",
    "i" = "{n - left} of the {n} inner daemon{?s} {.fn {verb}} started on its daemon {cli::qty(n - left)}{?was/were} gone after it.",
    "i" = "{.fn {verb}} runs a long configuration's folds on daemons it starts on that configuration's daemon, within the daemons set; {.code control_horizons(inner_daemons = FALSE)} runs each configuration on one daemon.",
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

#' Pin BLAS, OpenMP, data.table and the engines' threads in the parent
#'
#' @description
#' Pinning has to happen in the parent before workers are forked or spawned.
#' OpenBLAS reads its thread count when it loads, so setting
#' `OPENBLAS_NUM_THREADS` inside a worker after the library is loaded is dead
#' code (proven 2026-09-15: the environment variable changed nothing, the
#' runtime call did). `RhpcBLASctl` provides the runtime call and is a
#' Suggests; without it the parent relies on the environment variables being
#' set before R starts. The engines' thread options are the ones
#' `MODEL_SPECS` declares (`engine_thread_options()`), set to one.
#'
#' @return A function that restores the previous settings; call it from
#'   `on.exit()`.
#' @keywords internal
pin_parent_threads <- function() {

  restore <- list(blas = pin_blas_threads())

  if (requireNamespace("data.table", quietly = TRUE)) {

    restore$dt <- data.table::getDTthreads()
    data.table::setDTthreads(1L)

  }

  restore$options <- options(engine_thread_options())

  function() {

    restore$blas()

    if (!is.null(restore$dt)) data.table::setDTthreads(restore$dt)

    options(restore$options)

    invisible(NULL)

  }

}

#' Pin BLAS and OpenMP to one thread in this process, at run time
#'
#' @description
#' The one runtime pin of BLAS and OpenMP, for every process that fits: the
#' session before dispatch ([pin_parent_threads()]), a configs-axis
#' configuration's daemon while its task runs, and its inner daemons
#' ([start_inner_daemons()]), so a configuration computes alike wherever its
#' folds and its final fit run. `RhpcBLASctl` makes the call and is a
#' Suggests; without it nothing is pinned here, and a process keeps the
#' threads it started with ([blas_pinnable()]).
#'
#' @return A function of no arguments that puts both counts back as they
#'   were.
#' @keywords internal
#' @noRd
pin_blas_threads <- function() {

  restore <- list()

  if (blas_pinnable()) {

    restore$blas <- tryCatch(RhpcBLASctl::blas_get_num_procs(), error = function(e) NULL)
    restore$omp  <- tryCatch(RhpcBLASctl::omp_get_max_threads(), error = function(e) NULL)

    tryCatch(RhpcBLASctl::blas_set_num_threads(1L), error = function(e) NULL)
    tryCatch(RhpcBLASctl::omp_set_num_threads(1L),  error = function(e) NULL)

  }

  function() {

    if (!is.null(restore$blas)) {
      tryCatch(RhpcBLASctl::blas_set_num_threads(restore$blas), error = function(e) NULL)
    }

    if (!is.null(restore$omp)) {
      tryCatch(RhpcBLASctl::omp_set_num_threads(restore$omp), error = function(e) NULL)
    }

    invisible(NULL)

  }

}

#' Whether BLAS and OpenMP can be pinned at run time here
#'
#' @return Logical(1): `TRUE` when `RhpcBLASctl` is installed.
#' @keywords internal
#' @noRd
blas_pinnable <- function() {

  requireNamespace("RhpcBLASctl", quietly = TRUE)

}
