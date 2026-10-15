## ---------------------------------------------------------------------------
## The log: the warnings an object's verbs raised
## ---------------------------------------------------------------------------
## Every object carries one log of what its verbs reported (see
## ?horizons_conditions): each warning a verb raised, one row per topic per
## call, and each note, an event a verb reports without warning (samples
## dropped as the user asked, a cache rebuilt), one row each. A verb collects
## both in a per-call collector as it runs; at its end log_close() writes the
## rows, and only then does signal_log() raise each warning row, once. No site
## raises a warning itself, and signal_log() raises only what log_close()
## wrote, so "recorded before raised" and "once per topic per call" hold by
## construction.
##
## The collector is an environment, so the helpers a verb calls can add to it
## without threading it back. It is never sent to a worker: work that runs on a
## worker goes through run_unit(), which returns its records as data.

#' The topics a horizons warning can have
#'
#' @description
#' Every horizons warning carries the class `horizons_warning_<topic>` for one
#' of these (see [horizons_conditions]).
#'
#' @keywords internal
#' @noRd
LOG_TOPICS <- c("substitution", "quality", "skipped", "flagged",
                "partial_failure", "coverage", "applicability", "input")

#' An empty log
#'
#' @return Tibble with one row per thing a verb reported: `verb`, `type`
#'   (`"warning"`, raised once per topic per call, or `"note"`, recorded and
#'   shown in verbose output but not raised), `topic` (one of `LOG_TOPICS`
#'   for a warning, `NA` for a note), `message` (the text on one line), `ids`
#'   (a list of the configurations, members, samples, files or properties it
#'   is about) and `n` (how many times it arose).
#' @keywords internal
#' @noRd
new_log <- function() {

  tibble::tibble(
    verb    = character(0),
    type    = character(0),
    topic   = character(0),
    message = character(0),
    ids     = list(),
    n       = integer(0)
  )

}

## ---------------------------------------------------------------------------
## The collector
## ---------------------------------------------------------------------------

#' Open a verb call's collector
#'
#' @param verb Character(1). The verb, as the user calls it (`"evaluate"`).
#' @return An environment holding the verb and the events added so far.
#' @keywords internal
#' @noRd
log_open <- function(verb) {

  collector        <- new.env(parent = emptyenv())
  collector$verb   <- verb
  collector$events <- list()

  collector

}

#' Add a warning to a verb call's collector
#'
#' @description
#' Records one thing the verb will warn about at its end. The message is
#' rendered here, in the caller's environment, and kept as plain text, so it
#' can be raised later from another frame and its text can never be read as
#' a cli template again: a cause quoted from upstream that contains braces is
#' text, not an interpolation.
#'
#' @param collector From [log_open()].
#' @param topic Character(1). One of `LOG_TOPICS`.
#' @param message Character. A cli message: the problem statement, then
#'   bullets, interpolated in `.envir`.
#' @param ids Character. What the warning is about, by id.
#' @param n Integer(1). How many times it arose; `length(ids)` by default,
#'   or 1 when there are no ids.
#' @param .envir The environment the message is interpolated in.
#'
#' @return `collector`, invisibly.
#' @keywords internal
#' @noRd
log_warning <- function(collector,
                        topic,
                        message,
                        ids    = character(0),
                        n      = max(length(ids), 1L),
                        .envir = rlang::caller_env()) {

  if (!rlang::is_string(topic) || !topic %in% LOG_TOPICS) {

    abort_horizons(
      "{.arg topic} must be one of {.or {.val {LOG_TOPICS}}}, not {.obj_type_friendly {topic}}.",
      kind = "internal"
    )

  }

  log_event(collector, "warning", topic, message, ids, n, .envir)

}

#' Add a note to a verb call's collector
#'
#' @description
#' Records an event the verb reports without warning: something the user
#' asked for that changed the data (samples dropped under
#' `on_all_outliers = "drop"`), or a recovery that cost only time (a stale
#' cache rebuilt). The verb shows it in its verbose output; the log keeps it
#' with the warnings, so a pipeline's history is in one place. A note is
#' never raised.
#'
#' @inheritParams log_warning
#' @return `collector`, invisibly.
#' @keywords internal
#' @noRd
log_note <- function(collector,
                     message,
                     ids    = character(0),
                     n      = max(length(ids), 1L),
                     .envir = rlang::caller_env()) {

  log_event(collector, "note", NA_character_, message, ids, n, .envir)

}

#' Record one event in a collector
#'
#' @param type Character(1). `"warning"` or `"note"`.
#' @param topic Character(1). The warning's topic, or `NA` for a note.
#' @inheritParams log_warning
#' @return `collector`, invisibly.
#' @keywords internal
#' @noRd
log_event <- function(collector, type, topic, message, ids, n, .envir) {

  if (isTRUE(collector$closed)) {

    abort_horizons(
      "An event was added to {.fn {collector$verb}}'s collector after it was closed.",
      kind = "internal"
    )

  }

  ## Render now, as plain text -------------------------------------------------

  ### Pinned so the text is the same whatever the session's colours and width.
  old_opts <- options(cli.num_colors = 1L, cli.width = 100000L)
  on.exit(options(old_opts), add = TRUE)

  rendered <- vapply(message, cli::format_inline, character(1), .envir = .envir)
  names(rendered) <- names(message)

  collector$events[[length(collector$events) + 1L]] <- list(
    type    = type,
    topic   = topic,
    message = rendered,
    ids     = unique(as.character(ids)),
    n       = as.integer(n)
  )

  invisible(collector)

}

#' The log rows a verb call's collector holds
#'
#' @description
#' Warnings are one row per topic, in the order each topic first arose: a
#' topic added once keeps its message, and a topic added several times is
#' one warning whose message lists each, so the row and the warning say the
#' same thing. Notes are one row each. Rows keep the order in which their
#' first event arose.
#'
#' @param collector From [log_open()].
#' @return Rows shaped like [new_log()]'s, with the cli message of each row
#'   as the attribute `"messages"` (a list, one per row).
#' @keywords internal
#' @noRd
log_rows <- function(collector) {

  events <- collector$events

  if (length(events) == 0) {

    rows <- new_log()
    attr(rows, "messages") <- list()
    return(rows)

  }

  ### A warning's key is its topic, so a topic's events share a row; a note's
  ### key is its position, so each note is a row of its own.
  keys  <- vapply(seq_along(events), function(i) {
    e <- events[[i]]
    if (identical(e$type, "warning")) paste0("warning:", e$topic) else paste0("note:", i)
  }, character(1))
  order <- unique(keys)
  group <- lapply(order, function(k) events[keys == k])

  messages <- lapply(group, function(these) {

    if (length(these) == 1L) return(these[[1]]$message)

    ### Each event's statement becomes a bullet under one header; any bullets
    ### an event carried are folded into its line.
    lines <- vapply(these, function(e) clean_message_text(e$message), character(1))
    c(paste0(length(these), " ", gsub("_", " ", these[[1]]$topic), " warnings in `",
             collector$verb, "()`:"),
      stats::setNames(lines, rep("*", length(lines))))

  })

  rows <- tibble::tibble(
    verb    = collector$verb,
    type    = vapply(group, function(g) g[[1]]$type, character(1)),
    topic   = vapply(group, function(g) g[[1]]$topic, character(1)),
    message = vapply(messages, flatten_message, character(1)),
    ids     = lapply(group, function(g) unique(unlist(lapply(g, `[[`, "ids")))),
    n       = vapply(group, function(g) sum(vapply(g, `[[`, integer(1), "n")), integer(1))
  )

  attr(rows, "messages") <- messages
  rows

}

#' Close a verb call's collector onto a log
#'
#' @description
#' Writes this call's rows, one per topic, and closes the collector: nothing
#' more can be added, and [signal_log()] raises exactly these rows.
#'
#' @param collector From [log_open()].
#' @param log The object's log so far, shaped like [new_log()]'s.
#' @return `log` with this call's rows appended.
#' @keywords internal
#' @noRd
log_close <- function(collector, log = new_log()) {

  if (isTRUE(collector$closed)) {

    abort_horizons("{.fn {collector$verb}}'s collector was closed twice.",
                   kind = "internal")

  }

  collector$rows   <- log_rows(collector)
  collector$closed <- TRUE

  rows <- collector$rows
  attr(rows, "messages") <- NULL

  dplyr::bind_rows(log, rows)

}

#' Raise a verb call's warnings, once per topic
#'
#' @description
#' Run at the end of a verb, after [log_close()]. Raises each warning row
#' [log_close()] wrote as `horizons_warning_<topic>`, once, carrying the row
#' as the field `log`; notes are not raised. It refuses, as a bug in the verb, to run before the
#' collector is closed or a second time, so a warning is never raised that
#' the log does not hold.
#'
#' Under `options(warn = 2)`, R turns an unhandled warning into a plain error
#' that drops its fields and loses the object the verb was about to return.
#' So each warning is first offered to the caller's handlers, as R would
#' offer it: one that a handler muffles (`suppressWarnings()`,
#' `expect_warning()`) is done with, and one that a handler catches
#' (`tryCatch(warning = )`) unwinds as usual. The warnings no handler took
#' are then raised together as `horizons_error_promoted_warnings`, carrying
#' their rows as `log` and the finished result as `object`, so the call ends
#' in an error as the user asked and no work is lost.
#'
#' @param collector From [log_open()], closed by [log_close()].
#' @param object What the verb returns, carried on the error under
#'   `warn = 2`.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
signal_log <- function(collector, object = NULL) {

  if (!isTRUE(collector$closed)) {

    abort_horizons("{.fn signal_log} ran before {.fn {collector$verb}}'s collector was closed.",
                   kind = "internal")

  }

  if (isTRUE(collector$signalled)) {

    abort_horizons("{.fn {collector$verb}}'s warnings were raised twice.",
                   kind = "internal")

  }

  collector$signalled <- TRUE

  ### Notes are recorded, never raised.
  is_warning <- collector$rows$type == "warning"
  messages   <- attr(collector$rows, "messages")[is_warning]
  rows       <- collector$rows[is_warning, , drop = FALSE]
  attr(rows, "messages") <- NULL

  if (nrow(rows) == 0) return(invisible(NULL))

  warning_for <- function(i) {
    rlang::catch_cnd(
      cli::cli_warn(
        cli_escape(messages[[i]]),
        class = c(paste0("horizons_warning_", rows$topic[i]), "horizons_warning"),
        log   = rows[i, , drop = FALSE],
        call  = NULL
      ),
      classes = "warning"
    )
  }

  ## warn = 2: what no handler takes is one error that keeps the work ---------

  if (getOption("warn", 0L) >= 2L) {

    unhandled <- vapply(seq_len(nrow(rows)), function(i) {
      withRestarts(
        { signalCondition(warning_for(i)); TRUE },
        muffleWarning = function() FALSE
      )
    }, logical(1))

    if (!any(unhandled)) return(invisible(NULL))

    promoted <- rows[unhandled, , drop = FALSE]

    abort_horizons(
      c("{.fn {collector$verb}} raised {nrow(promoted)} warning{?s}, and {.code options(warn = 2)} turns warnings into errors.",
        stats::setNames(cli_escape(promoted$message), rep("*", nrow(promoted))),
        "i" = "The finished result is in this error's {.field object} field, and the warnings in its {.field log} field."),
      kind   = "run",
      class  = "horizons_error_promoted_warnings",
      log    = promoted,
      object = object,
      call   = NULL
    )

  }

  ## One warning per topic ------------------------------------------------------

  for (i in seq_len(nrow(rows))) {

    cli::cli_warn(
      cli_escape(messages[[i]]),
      class = c(paste0("horizons_warning_", rows$topic[i]), "horizons_warning"),
      log   = rows[i, , drop = FALSE],
      call  = NULL
    )

  }

  invisible(NULL)

}

#' A rendered cli message on one line
#'
#' @param message Character, possibly named with cli bullet names.
#' @return Character(1): the statement followed by its bullets, as
#'   [clean_message_text()] joins them.
#' @keywords internal
#' @noRd
flatten_message <- function(message) {

  if (length(message) <= 1L) return(clean_message_text(message))

  bullets <- message[-1]
  clean_message_text(c(message[1], paste0("• ", bullets)))

}

## ---------------------------------------------------------------------------
## run_unit and unit_step
## ---------------------------------------------------------------------------
## A unit is the smallest piece of work whose failure leaves the rest of the
## call usable: a configuration, a member (with its UQ and AD bundles as units
## of their own), a property, one configuration's predictions. run_unit()
## contains a unit's run failures and records its warnings; unit_step() labels
## the steps inside it, so a record says where it happened.

#' The error kinds a unit never contains
#'
#' @description
#' A bug, a door error and a missing package are not local to a unit: each
#' would hit every unit the same way, so it propagates with its class, chain
#' and backtrace intact.
#'
#' @keywords internal
#' @noRd
NEVER_CONTAINED <- paste0("horizons_error_", c("internal", "input", "dependency"))

#' The labels of the steps running now, innermost last
#'
#' @keywords internal
#' @noRd
unit_steps <- new.env(parent = emptyenv())
unit_steps$stack <- character(0)

#' Label the step a unit is in
#'
#' @description
#' Contains nothing. While `expr` runs, a failure or warning inside it is
#' recorded as having arisen in `step`, so the record says where it happened
#' without the step being wrapped on its own.
#'
#' @param step Character(1). The step, e.g. `"tune_grid"`.
#' @param expr The step's code.
#' @return The value of `expr`.
#' @keywords internal
#' @noRd
unit_step <- function(step, expr) {

  depth <- length(unit_steps$stack)
  unit_steps$stack <- c(unit_steps$stack, step)
  on.exit(unit_steps$stack <- utils::head(unit_steps$stack, depth), add = TRUE)

  expr

}

#' An empty set of unit records
#'
#' @return Tibble with one row per failure or warning in a unit: `unit` (what
#'   kind of unit, e.g. `"configuration"`), `id`, `step` (the innermost
#'   [unit_step()] label, or `NA`), `type` (`"failure"` or `"warning"`),
#'   `class` (the root cause's class), `message` (one line, from
#'   [condition_summary()]), `n` and `trace` (a list holding the backtrace as
#'   text, or `NULL`).
#' @keywords internal
#' @noRd
new_unit_records <- function() {

  tibble::tibble(
    unit    = character(0),
    id      = character(0),
    step    = character(0),
    type    = character(0),
    class   = character(0),
    message = character(0),
    n       = integer(0),
    trace   = list()
  )

}

#' Run one unit, containing its run failures and recording its warnings
#'
#' @description
#' Runs `expr` under one [rlang::try_fetch()]:
#'
#' * A warning is recorded and muffled, and the unit goes on. Nothing is
#'   printed or raised: the verb decides what to report at its end.
#' * An error of a kind in `NEVER_CONTAINED` propagates untouched, with its
#'   class, chain and backtrace, including when a recipe step has wrapped it.
#' * Any other error is a run failure: it is recorded with the step it arose
#'   in and its root cause's class, and the unit's value is `NULL`.
#'
#' The condition itself is never kept, since its backtrace can hold the data
#' (one failed fit on 500 by 1,700 data carried a 13.4 MB trace). With
#' `keep_traces = TRUE` the record keeps the backtrace as text.
#'
#' Recording must not fail: a record that cannot be built is a bug in
#' horizons and raises an internal error, which propagates, rather than
#' reading as this unit's failure.
#'
#' Records are plain data, so a unit run on a worker returns them with its
#' value.
#'
#' @param expr The unit's work.
#' @param unit Character(1). What kind of unit this is, e.g.
#'   `"configuration"` or `"member"`.
#' @param id Character(1). The unit's id.
#' @param debug Logical(1). When `TRUE`, no failure is contained, so the
#'   first one stops the call with its full backtrace. Warnings are still
#'   recorded: debug turns off containment, not the record.
#' @param keep_traces Logical(1). Keep a failure's backtrace, as text.
#'
#' @return List: `value` (the value of `expr`, or `NULL` when it failed),
#'   `failed` (logical(1)) and `records` (shaped like
#'   [new_unit_records()]'s).
#' @keywords internal
#' @noRd
run_unit <- function(expr, unit, id, debug = FALSE, keep_traces = FALSE) {

  ### A record is built from these, so they are checked before anything runs:
  ### a bad one would otherwise fail inside an outer unit and read as that
  ### unit's failure.
  if (!rlang::is_string(unit) || !rlang::is_string(id)) {

    abort_horizons("{.arg unit} and {.arg id} must each be a single string.",
                   kind = "internal")

  }

  depth   <- length(unit_steps$stack)
  records <- list()

  ### The innermost label pushed inside this unit; an outer unit's labels are
  ### not this unit's steps.
  current_step <- function() {
    stack <- unit_steps$stack
    if (length(stack) <= depth) NA_character_ else stack[length(stack)]
  }

  ### Everything that builds a record runs inside the guard, so a record that
  ### cannot be built is a horizons bug, which propagates, rather than this
  ### unit's failure.
  record <- function(cnd, type) {
    records[[length(records) + 1L]] <<- tryCatch(
      {
        trace <- if (identical(type, "failure") && isTRUE(keep_traces)) {
          traced <- rlang::cnd_entrace(cnd)
          if (!is.null(traced$trace)) format(traced$trace)
        }
        list(unit = unit, id = id, step = current_step(), type = type,
             class = root_class(cnd), message = condition_summary(cnd),
             n = 1L, trace = trace)
      },
      error = function(e) abort_horizons(
        "Recording a {type} in {unit} {.val {id}} failed.",
        kind = "internal", parent = e
      )
    )
  }

  ### unit_step() pops its own label; this restores the stack whatever the
  ### unit's code did to it.
  on.exit(unit_steps$stack <- utils::head(unit_steps$stack, depth), add = TRUE)

  ## Debug: record warnings, contain nothing --------------------------------------

  if (isTRUE(debug)) {

    value <- withCallingHandlers(
      expr,
      warning = function(w) {
        record(w, "warning")
        rlang::cnd_muffle(w)
      }
    )

    return(list(value = value, failed = FALSE, records = unit_records(records)))

  }

  ## Contain run failures, record warnings ---------------------------------------

  value <- rlang::try_fetch(
    expr,
    warning = function(w) {
      record(w, "warning")
      ### A warning signalled without a muffle restart cannot be muffled;
      ### declining lets it go on to the caller instead of becoming the
      ### unit's value.
      rlang::cnd_muffle(w)
      rlang::zap()
    },
    error = function(e) {
      ### Raised again as it is, class, chain and trace intact. Declining with
      ### zap() is not enough: try_fetch() then offers the condition's parent
      ### to the same handler, and an upstream parent would be contained.
      if (rlang::cnd_inherits(e, NEVER_CONTAINED)) stop(e)
      record(e, "failure")
      structure(list(), class = "horizons_unit_failed")
    }
  )

  failed <- inherits(value, "horizons_unit_failed")

  list(
    value   = if (!failed) value,
    failed  = failed,
    records = unit_records(records)
  )

}

#' Bind a unit's record lists into a table
#'
#' @param records List of records as `run_unit()` builds them.
#' @return Tibble shaped like [new_unit_records()]'s.
#' @keywords internal
#' @noRd
unit_records <- function(records) {

  if (length(records) == 0) return(new_unit_records())

  tibble::tibble(
    unit    = vapply(records, `[[`, character(1), "unit"),
    id      = vapply(records, `[[`, character(1), "id"),
    step    = vapply(records, `[[`, character(1), "step"),
    type    = vapply(records, `[[`, character(1), "type"),
    class   = vapply(records, `[[`, character(1), "class"),
    message = vapply(records, `[[`, character(1), "message"),
    n       = vapply(records, `[[`, integer(1), "n"),
    trace   = lapply(records, `[[`, "trace")
  )

}

#' The class of a condition's root cause
#'
#' @description
#' A failure often arrives wrapped (`recipes_error_step`,
#' `purrr_error_indexed`), so the outer class says how it travelled, not what
#' went wrong. This is the class of the deepest condition in the parent
#' chain.
#'
#' @param cnd A condition.
#' @return Character(1).
#' @keywords internal
#' @noRd
root_class <- function(cnd) {

  node <- cnd

  ### Bounded, as in condition_cause(), so a chain that loops cannot hang.
  for (i in seq_len(50L)) {

    parent <- tryCatch(node[["parent"]], error = function(e) NULL)
    if (!inherits(parent, "condition")) break
    node <- parent

  }

  class(node)[1]

}
