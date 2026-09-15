## ---------------------------------------------------------------------------
## Parallelism helpers: axis resolution, plan inspection, thread pinning
## ---------------------------------------------------------------------------
##
## The rule these implement (evaluate-design.md, Parallelism): the user owns
## the backend and the topology; the package owns only which axis of its work
## the registered future plan is applied to. Nothing here ever calls
## future::plan() to *set* a plan.

## ---------------------------------------------------------------------------
## resolve_parallel_axis
## ---------------------------------------------------------------------------

#' Resolve the parallel axis for a run
#'
#' @description
#' Pure function from the caller's request and the object's shape to the
#' concrete dispatch decision. It never inspects the registered plan, so it is
#' unit-testable with no backend and no installed build.
#'
#' `"auto"` picks by the measured cost model (2026-09-15): one config run
#' sequentially costs about T; on the configs axis wall time is roughly
#' ceiling(n_configs / workers) * T, on the resamples axis roughly
#' n_configs * T / k with k the effective inner speedup, which is capped at
#' `cv_folds`. Configs wins whenever `n_configs >= k`, and always once
#' `n_configs` exceeds the worker count, so the worker count cancels and the
#' rule is `n_configs >= cv_folds`.
#'
#' `"both"` (a nested plan, configs across the outer level and folds across
#' the inner) is not in the vocabulary for v1. A user who wants nesting
#' registers a nested plan and uses `"configs"`; the package does not build
#' or endorse one until the topology bake-off (S1) shows it is worth it.
#'
#' @param parallelize_over One of `"auto"`, `"configs"`, `"resamples"`.
#' @param allow_par Logical. `FALSE` short-circuits to sequential.
#' @param n_configs Integer. Number of configs to evaluate.
#' @param cv_folds Integer. CV folds per config.
#'
#' @return A list: `axis` (`"sequential"`, `"configs"` or `"resamples"`),
#'   `dispatch_configs` (logical; use the furrr branch), `tune_allow_par`
#'   (logical; what the single-config functions pass to tune's control), and
#'   `tune_parallel_over` (`"resamples"` or `NULL`; the value *requested* of
#'   tune, which may rewrite it, see the note on `tune:::.update_parallel_over`
#'   in the spec).
#' @keywords internal
resolve_parallel_axis <- function(parallelize_over, allow_par, n_configs, cv_folds) {

  if (identical(parallelize_over, "both")) {

    cli::cli_abort(c(
      "{.arg parallelize_over} = {.val both} is not supported.",
      "i" = "Nested parallelism is the user's to register: set a nested {.fn future::plan} and use {.val configs}. See evaluate-design.md, Parallelism."
    ))

  }

  parallelize_over <- rlang::arg_match0(parallelize_over, PARALLELIZE_OVER_VALUES,
                                        arg_nm = "parallelize_over")

  if (!rlang::is_bool(allow_par)) {

    cli::cli_abort("{.arg allow_par} must be TRUE or FALSE, not {.val {allow_par}}.")

  }

  for (nm in c("n_configs", "cv_folds")) {

    v <- get(nm)

    if (!is.numeric(v) || length(v) != 1 || is.na(v) || v < 1) {

      cli::cli_abort("{.arg {nm}} must be a single positive number, not {.val {v}}.")

    }

  }

  if (!allow_par) {

    return(list(axis = "sequential", dispatch_configs = FALSE,
                tune_allow_par = FALSE, tune_parallel_over = NULL))

  }

  axis <- if (parallelize_over == "auto") {

    if (n_configs >= cv_folds) "configs" else "resamples"

  } else {

    parallelize_over

  }

  if (axis == "configs") {

    list(axis = "configs", dispatch_configs = TRUE,
         tune_allow_par = FALSE, tune_parallel_over = NULL)

  } else {

    list(axis = "resamples", dispatch_configs = FALSE,
         tune_allow_par = TRUE, tune_parallel_over = "resamples")

  }

}

## ---------------------------------------------------------------------------
## Plan inspection
## ---------------------------------------------------------------------------

#' Label the registered future plan
#'
#' @description
#' `future::plan()` returns the outer strategy as a function, never a list,
#' so nesting depth comes from `future::plan("list")`. Each level's class
#' stack is e.g. `tweaked, multisession, cluster, multiprocess, future,
#' function`; the first class after the wrapper classes names the strategy.
#'
#' @return A string such as `"sequential"`, `"multisession"`, or
#'   `"multisession > multisession"` for a nested plan.
#' @keywords internal
registered_plan_label <- function() {

  levels <- future::plan("list")

  labels <- vapply(levels, function(level) {

    cls <- setdiff(class(level), c("FutureStrategy", "tweaked", "future", "function"))
    if (length(cls) == 0) "unknown" else cls[[1]]

  }, character(1))

  paste(labels, collapse = " > ")

}

#' Workers the registered plan offers
#'
#' @description
#' `future::nbrOfWorkers()` for the level about to be used, which is the
#' number `tune::choose_framework()` reads. For a nested plan this is the
#' outer level; the inner count is visible only from inside a worker.
#'
#' @return Integer.
#' @keywords internal
registered_workers <- function() {

  as.integer(future::nbrOfWorkers())

}

#' Check that a parallel run has a backend to run on
#'
#' @description
#' Called only when the caller asked for parallelism. The gate is
#' `nbrOfWorkers() < 2`, not the plan's class: `plan(multisession,
#' workers = 1)` is not `sequential`-classed but yields nothing, and tune
#' returns `"sequential"` for it silently. That silence was issue #37.
#'
#' A fresh session and an explicit `plan(sequential)` are indistinguishable
#' (same class, same recorded call), so the message is written to be true of
#' both.
#'
#' @param where Character. The verb, for the message (`"evaluate()"`).
#' @return `TRUE` if parallel dispatch is possible, else `FALSE` after a
#'   warning.
#' @keywords internal
check_parallel_backend <- function(where = "evaluate()") {

  n     <- registered_workers()
  label <- registered_plan_label()

  if (n >= 2L) {

    ## tune's future path calls future.apply::future_lapply() and checks only
    ## that `future` is installed (tune 2.1.0, loop_call()). horizons declares
    ## future.apply in Imports for exactly this reason; the literal check here
    ## is what R CMD check's static "declared Imports should be used" test
    ## recognises, and it is a real guard for a broken library.
    if (!requireNamespace("future.apply", quietly = TRUE)) {

      cli::cli_abort(c(
        "{.pkg future.apply} is not installed, but tune's future backend calls it.",
        "i" = "Install it with {.code install.packages(\"future.apply\")}."
      ))

    }

    return(TRUE)

  }

  cli::cli_warn(c(
    "!" = "{where} was asked to run in parallel ({.code allow_par = TRUE}) but the registered future plan offers {n} worker{?s} ({.val {label}}).",
    "i" = "Running sequentially instead.",
    "i" = "Register a backend before calling, for example {.code future::plan(future::multisession, workers = 4)}."
  ))

  FALSE

}

#' Warn when tune would silently prefer a mirai daemon pool
#'
#' @description
#' `tune:::choose_framework()` defaults to mirai whenever both a future plan
#' and a mirai daemon pool have two or more workers, and the message that
#' would say so fires only under a verbose flag tune's internals never pass.
#' Detect the situation so the run, and the manifest, do not describe a plan
#' that was not used.
#'
#' @return Invisibly, `TRUE` if the warning fired.
#' @keywords internal
warn_if_mirai_preferred <- function() {

  if (!requireNamespace("mirai", quietly = TRUE)) return(invisible(FALSE))

  connections <- tryCatch(mirai::status()$connections, error = function(e) 0L)

  if (is.null(connections) || is.na(connections) || connections < 2L) {

    return(invisible(FALSE))

  }

  if (registered_workers() < 2L) return(invisible(FALSE))

  cli::cli_warn(c(
    "!" = "A mirai daemon pool ({connections} connections) and a future plan ({.val {registered_plan_label()}}) are both registered.",
    "i" = "tune prefers mirai when both are live and does not say so. Stop the daemons ({.code mirai::daemons(0)}) to run on the future plan."
  ))

  invisible(TRUE)

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
#' set before R starts, which this function says once.
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

  } else if (!nzchar(Sys.getenv("OPENBLAS_NUM_THREADS")) &&
             !nzchar(Sys.getenv("OMP_NUM_THREADS"))) {

    cli::cli_inform(c(
      "i" = "{.pkg RhpcBLASctl} is not installed, so BLAS threads cannot be pinned at runtime.",
      "i" = "Set {.envvar OPENBLAS_NUM_THREADS=1} and {.envvar OMP_NUM_THREADS=1} before starting R to avoid oversubscription."
    ))

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

## ---------------------------------------------------------------------------
## Deprecation of `workers`
## ---------------------------------------------------------------------------

#' Warn that `workers` is deprecated
#'
#' @description
#' `evaluate(workers = )` took a core count and split it into an outer and
#' inner level while managing its own nested plan. Deprecated outright on
#' 2026-09-14: the argument warns and is ignored. `rlang::warn()` with a
#' condition class so tests can match on the class rather than the text; it
#' fires every time, so a scripted loop cannot hide it.
#'
#' @param workers The value passed, or `NULL`.
#' @return Invisibly, `TRUE` if a warning fired.
#' @keywords internal
deprecate_workers_arg <- function(workers) {

  if (is.null(workers)) return(invisible(FALSE))

  rlang::warn(
    c(
      "`workers` is deprecated and ignored.",
      "i" = "Register a backend with `future::plan()` before calling, then use `allow_par = TRUE` and `parallelize_over`."
    ),
    class = "horizons_deprecated_workers"
  )

  invisible(TRUE)

}
