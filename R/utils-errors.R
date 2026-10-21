## ---------------------------------------------------------------------------
## abort_horizons
## ---------------------------------------------------------------------------

#' The kinds a horizons error can be
#'
#' @description
#' Every horizons error carries exactly one of these, as the class
#' `horizons_error_<kind>` (see [horizons_conditions]).
#'
#' @keywords internal
#' @noRd
CONDITION_KINDS <- c("input", "validation", "dependency", "run", "internal")

#' Raise a horizons error of one kind
#'
#' @description
#' The one constructor every horizons error goes through. The condition
#' carries the base class `horizons_error`, exactly one kind
#' (`horizons_error_<kind>`), and at most one specific subclass in front of
#' the kind, so a handler can catch all of horizons' errors, one kind of
#' them, or one refusal (see [horizons_conditions]).
#'
#' An `"internal"` error is a broken invariant, a bug in horizons. It is
#' raised with `.internal = TRUE`, which adds rlang's footer naming the
#' package and its issue tracker; the class is what handlers and tests
#' match on, since the flag adds none.
#'
#' @param message Character. A cli message: the problem statement, then
#'   bullets. Interpolated in `.envir`, so `{x}` reads the caller's `x`.
#' @param kind Character(1). One of `"input"`, `"validation"`,
#'   `"dependency"`, `"run"` or `"internal"`. Required: a horizons error
#'   always says which kind it is.
#' @param class Character(1) or `NULL`. A specific subclass, written in full
#'   (`"horizons_error_all_configs_failed"`), when the refusal earns one.
#' @param ... Passed to [cli::cli_abort()]: fields, `parent`, `body`.
#' @param call The frame the error reports, the caller's by default.
#' @param .envir The environment the message is interpolated in.
#'
#' @return Does not return.
#' @keywords internal
#' @noRd
abort_horizons <- function(message,
                           kind,
                           class  = NULL,
                           ...,
                           call   = rlang::caller_env(),
                           .envir = rlang::caller_env()) {

  ## Check the class arguments ------------------------------------------------

  ### Only horizons calls this, so a bad kind or subclass is a bug in the
  ### caller, reported as one rather than raised under a made-up class.
  if (!rlang::is_string(kind) || !kind %in% CONDITION_KINDS) {

    cli::cli_abort(
      "{.arg kind} must be one of {.or {.val {CONDITION_KINDS}}}, not {.obj_type_friendly {kind}}.",
      class = c("horizons_error_internal", "horizons_error"),
      .internal = TRUE
    )

  }

  if (!is.null(class) &&
      (!rlang::is_string(class) || !startsWith(class, "horizons_error_") ||
       class %in% paste0("horizons_error_", CONDITION_KINDS))) {

    cli::cli_abort(
      "{.arg class} must be one specific subclass named {.val horizons_error_<type>}, not a kind.",
      class = c("horizons_error_internal", "horizons_error"),
      .internal = TRUE
    )

  }

  ## Raise ---------------------------------------------------------------------

  cli::cli_abort(
    message,
    class     = c(class, paste0("horizons_error_", kind), "horizons_error"),
    ...,
    call      = call,
    .envir    = .envir,
    .internal = identical(kind, "internal")
  )

}

#' Check a user's argument with one of rlang's checkers
#'
#' @description
#' rlang's exported checkers ([rlang::check_bool()], [rlang::check_string()],
#' [rlang::check_number_whole()], [rlang::check_number_decimal()],
#' [rlang::check_data_frame()]) write the message the tidyverse uses for a
#' value of the wrong type. This runs one of them with horizons' input class,
#' so the refusal is a `horizons_error_input` like every other door check,
#' and passes on the argument's name and the verb's frame, so the message
#' names the user's argument and call rather than this helper.
#'
#' @param check A checker from rlang, e.g. `rlang::check_bool`.
#' @param x The value to check.
#' @param ... Passed to `check`, e.g. `allow_null = TRUE`, `min = 1`.
#' @param arg The argument's name, as the user wrote it.
#' @param call The frame the error reports, the caller's by default.
#'
#' @return `NULL` invisibly when `x` passes; otherwise an error of class
#'   `horizons_error_input`.
#' @keywords internal
#' @noRd
check_arg <- function(check,
                      x,
                      ...,
                      arg  = rlang::caller_arg(x),
                      call = rlang::caller_env()) {

  check(x, ..., arg = arg, call = call,
        class = c("horizons_error_input", "horizons_error"))

}

## ---------------------------------------------------------------------------
## horizons_conditions: the classes, documented in one place
## ---------------------------------------------------------------------------

#' Errors and warnings horizons raises
#'
#' @description
#' Every error horizons raises carries the base class `horizons_error`,
#' exactly one kind, and sometimes one more specific class in front of the
#' kind. Every warning carries the base class `horizons_warning` and one
#' topic. A handler can catch all of horizons' errors, one kind of them, or
#' one specific refusal, and filter warnings by what happened.
#'
#' @section Error kinds:
#' \describe{
#'   \item{`horizons_error_input`}{The call cannot run as written: an
#'     argument outside its domain, a required input missing, or data that
#'     breaks a stated requirement. Raised before any work starts.}
#'   \item{`horizons_error_validation`}{An object breaks its class contract,
#'     because it was edited by hand or written by an older version. Re-run
#'     the verb that wrote the field, or start again from [configure()].}
#'   \item{`horizons_error_dependency`}{A needed package is missing, or the
#'     environment cannot run the call (a parallel run under
#'     `devtools::load_all()`, whose workers load the installed build, or one
#'     whose workers load another version of horizons).}
#'   \item{`horizons_error_run`}{The work was attempted and failed: every
#'     configuration or member failed, a download failed, or the run stopped
#'     on a failure that would hit every unit the same way.}
#'   \item{`horizons_error_internal`}{A bug in horizons: something the code
#'     guarantees has not held. Please report it.}
#' }
#'
#' @section Specific errors:
#' \describe{
#'   \item{`horizons_error_all_configs_failed`}{(run) Every configuration
#'     failed in [evaluate()]. The per-configuration results travel on the
#'     condition as `results`.}
#'   \item{`horizons_error_all_members_failed`}{(run) Every member failed in
#'     `fit()`. The per-member results travel as `results`.}
#'   \item{`horizons_error_prediction_failed`}{(run) `predict()` had nothing
#'     to return: the configuration asked for, or every one of them, could
#'     not predict, or an ensemble's member or its combination could not.
#'     One row per failure travels as `results`: its `unit` (`"configuration"`, or
#'     `"combination"` for an ensemble's), `config_id` (`NA` for the
#'     combination), `step`, `class` and `error_message`.}
#'   \item{`horizons_error_read_failed`}{(run) [spectra()] could read none
#'     of the files it was given. One row per file travels as `results`: its
#'     `file`, the `step` that failed (`"file_read"`, or `"file_block"` for
#'     a file with no absorbance block), the `class` of the root cause, and
#'     the `cause`.}
#'   \item{`horizons_error_selection_failed`}{(run) [select_training()] had
#'     nothing to return: the twin subtraction left no training rows for any
#'     property it drew for. One row per property travels as `results`: its
#'     `property` and the `cause`.}
#'   \item{`horizons_error_ensemble_failed`}{(run) [ensemble()]'s
#'     meta-learner could not be built, so there is no ensemble to return: a
#'     step of its tuning, its fit, its out-of-fold pass or the members'
#'     predictions of the test rows failed. The failure travels as `results`,
#'     one row shaped as `horizons_error_prediction_failed`'s: `unit`
#'     (`"meta_learner"`), `config_id` (`NA`), `step`, `class` (the root
#'     cause's, e.g. `horizons_error_prediction_failed` for a member that
#'     cannot predict the test rows), `error_message` and `trace`.}
#'   \item{`horizons_error_systemic`}{(run) [evaluate()] stopped early,
#'     because the first configurations to run failed the same way across at
#'     least two models (the circuit breaker,
#'     `control_horizons(breaker = )`), or [evaluate()], [fit()] or
#'     [ensemble()] stopped because a parallel worker died. The rows finished
#'     so far travel as `results`; `NULL` from `ensemble()`, which has none
#'     before its meta-learner is built.}
#'   \item{`horizons_error_promoted_warnings`}{(run) A verb ended with
#'     warnings under `options(warn = 2)`. Instead of turning the first warning
#'     into an error that loses the result, the verb aborts with the finished
#'     result as `object` and its warnings as `log`.}
#'   \item{`horizons_error_download`}{(run) A spectral library could not be
#'     downloaded.}
#'   \item{`horizons_error_boruta`}{(run) [step_select_boruta()]'s Boruta run
#'     failed; Boruta's error is in its message.}
#'   \item{`horizons_error_qc`}{(run) [average()] had nothing to return:
#'     every sample failed quality control on every replicate and
#'     `on_all_outliers = "drop"` dropped it. One row per sample travels as
#'     `results`: its `sample_id` and `n_replicates`.}
#'   \item{`horizons_error_consent`}{(input) A library download was declined.}
#'   \item{`horizons_error_missing_predict_package`}{(dependency) `predict()`
#'     needs a model package that is not installed.}
#' }
#'
#' @section Warning topics:
#' \describe{
#'   \item{`horizons_warning_substitution`}{Something other than what was
#'     asked ran, under a documented fallback.}
#'   \item{`horizons_warning_quality`}{Everything ran as asked, but the result
#'     is weaker than its output shows (small folds, every member below the
#'     prune threshold). `predict()` raises each distinct warning another
#'     package raised while predicting under this topic, naming the
#'     configurations (or an ensemble's units) that raised it, and
#'     [spectra()] each one raised while reading, naming the files.
#'     [average()] names the samples it averaged though every replicate
#'     failed quality control.}
#'   \item{`horizons_warning_skipped`}{Something asked for did not run, such
#'     as a member's prediction intervals, a response [select_training()]
#'     could not draw for under `properties = NULL`, or a [validate()] check
#'     that could not run.}
#'   \item{`horizons_warning_flagged`}{A step kept samples it could not use;
#'     modelling leaves them out. From [standardize()], the spectra it
#'     flagged (`.flag`); from `step_transform_spectra()`, the spectra its
#'     bake returned `NA`, with the field `step`. From `predict()`, the
#'     rows it returned `NA` because they were flagged on arrival (`.flag`)
#'     or were not finite after
#'     a configuration's recipe; from [select_training()], the targets it drew
#'     no training set for because they were flagged on arrival.}
#'   \item{`horizons_warning_partial_failure`}{Some units failed and the rest
#'     returned.}
#'   \item{`horizons_warning_coverage`}{The data only partly covers what is
#'     asked of it, such as a wavenumber range, or [add_response()]'s source
#'     matching fewer than half the samples.}
#'   \item{`horizons_warning_applicability`}{Predictions outside the
#'     applicability domain.}
#'   \item{`horizons_warning_input`}{A request that is legal but risky enough
#'     to say so, such as [configure()] discarding an evaluation or a fit, or
#'     configuring an outcome a [select_training()] draw was not made for.}
#' }
#'
#' @section Where the record lives:
#' Each verb raises a topic at most once per call, at its end, and only after
#' writing it to the object's log, so a warning that is silenced or never
#' seen is still on the object. The log also keeps the notes a verb reports
#' without warning, such as samples dropped as the user asked, and the note
#' each of [spectra()], [parse_ids()], [standardize()], [average()] and
#' [add_response()] writes as the last row of every call, recording its
#' source and settings, so a pipeline's history is in one place. The one
#' call that writes none is `standardize()`'s early return on spectra
#' already standardized, whose `skipped` warning is its row. Notes are never
#' raised. Every row carries the `time` its call ended, so the first row's
#' is when the spectra were read (on a training set, the library's).
#' `predict()`, which changes no object,
#' carries each warning's record as the field `log`.
#'
#' Under `options(warn = 2)`, a warning that no handler takes ends the call
#' with `horizons_error_promoted_warnings`, which carries the finished result
#' as `object`; a warning the caller muffles or catches is handled as R
#' handles any warning.
#'
#' @section Catching errors raised inside a recipe step:
#' recipes wraps every error raised inside a step in `recipes_error_step`,
#' with the original as its parent. A base [tryCatch()] on a horizons class
#' does not see through the wrapper; [rlang::try_fetch()] and
#' [rlang::cnd_inherits()] do.
#'
#' @name horizons_conditions
NULL

## ---------------------------------------------------------------------------
## cli_escape
## ---------------------------------------------------------------------------

#' Escape text for use inside a cli template
#'
#' @description
#' Doubles every brace, which cli reads as a literal brace, so text that did
#' not come from the package (an upstream error message, a column name, a
#' path) can sit in a cli message without being evaluated as an
#' interpolation. A bare `"{wn_600}"` would otherwise make cli look up an
#' object called `wn_600` and crash the abort that was reporting it.
#'
#' @param x Character vector.
#' @return `x` with `{` and `}` doubled.
#' @keywords internal
#' @noRd
cli_escape <- function(x) {

  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)

}

## ---------------------------------------------------------------------------
## condition_summary
## ---------------------------------------------------------------------------

#' The classes of a condition horizons composed
#'
#' @description
#' A condition raised with [abort_horizons()], [warn_in_unit()] or
#' [note_in_unit()]. Its message is kept whole wherever it is recorded
#' ([condition_parts()]): any other package's text inside it was capped when
#' it was interpolated, through [condition_summary()]. Matched by exact
#' class, so a class from before the conditions contract
#' (`horizons_input_error`) is capped as another package's text until its
#' code raises through [abort_horizons()].
#'
#' @keywords internal
#' @noRd
HORIZONS_OWN_CLASSES <- c("horizons_error", "horizons_warning", "horizons_note")

#' Summarise a condition as one readable line
#'
#' @description
#' Turns a caught condition into the text a results-table cell or a cli
#' bullet should carry: the root cause of the condition's parent chain, on
#' one line, followed by where it happened when the cause arrived wrapped.
#' Every capture site that records why something failed goes through here.
#'
#' A recipe step error is the case this exists for. recipes wraps every step
#' error in a `recipes_error_step` condition whose `$message` is empty, with
#' the text in its parent chain, so capture sites that read `$message` stored
#' `"Parameter finalization failed: "` and nothing after it (#96). The same
#' error summarises as ``"boom from step (in `step_mutate()`)"``.
#'
#' Each condition's own text comes from `rlang::cnd_message(inherit = FALSE)`,
#' which renders base and rlang conditions alike, cli bodies included. The
#' deepest condition in the chain with any text is the cause; the outermost
#' condition above it whose call has a name says where. ANSI styling and cli
#' bullet glyphs are stripped, bullets are joined to the text before them, and
#' whitespace is collapsed so that lines cli wrapped rejoin.
#'
#' `x` may instead be text an upstream package already rendered from a
#' condition, such as a note in tune's `.notes`. Rendered text is read the
#' same way: the part after the last `"Caused by ..."` line is the cause, and
#' an opening ``"Error in `f()`:"`` line says where.
#'
#' The result is plain text, not a cli template. Interpolate it as a value
#' (`"{msg}"`), or pass it through [cli_escape()], before it meets cli.
#'
#' @param x A condition, or a character string holding a rendered message.
#' @param detail Character or `NULL`. Extra context for the closing
#'   parentheses, after the location, e.g. how many folds raised a note.
#' @param max_chars Integer. The longest cause kept before it is cut and
#'   marked with `"..."`. Only another package's text is cut: a condition
#'   horizons composed (`HORIZONS_OWN_CLASSES`) is kept whole.
#'
#' @return Character(1), never empty. A condition with no text anywhere in
#'   its chain is described by its class.
#' @keywords internal
#' @noRd
condition_summary <- function(x, detail = NULL, max_chars = 300L) {

  parts <- condition_parts(x, max_chars = max_chars)

  format_condition(parts$cause, parts$where, detail)

}

#' The cause and location of a condition, before they are joined
#'
#' @inheritParams condition_summary
#' @return List with `cause` (character(1), never empty, capped at
#'   `max_chars` unless horizons composed it) and `where` (a backticked
#'   call, or `NA`).
#' @keywords internal
#' @noRd
condition_parts <- function(x, max_chars = 300L) {

  ## This runs on the failure path, so it must not fail itself: text it
  ## cannot read (an invalid multibyte string, say) is reported as such
  ## rather than taking the config down with it.
  parts <- tryCatch(
    if (inherits(x, "condition")) condition_cause(x) else rendered_cause(x),
    error = function(e) list(
      cause    = "",
      where    = NULL,
      fallback = paste0(if (inherits(x, "condition")) class(x)[1] else "error",
                        " (message could not be read)")
    )
  )

  cause <- if (!is.na(parts$cause) && nzchar(parts$cause)) parts$cause else parts$fallback

  ## The cap keeps a results-table cell readable. The cause is at the front,
  ## so a cut only drops the tail of a long root message (in practice the
  ## trailing cli hint bullets), never the reason itself, and the location
  ## and detail are added after the cut so they always survive. Only text
  ## another package raised is cut: a message horizons composed is kept
  ## whole, since the other package's text inside it went through this cap
  ## when it was interpolated, and cutting it would drop what the package
  ## added after that text, a location or a fold count.
  if (!isTRUE(parts$own) && isTRUE(nchar(cause, allowNA = TRUE) > max_chars)) {

    cause <- paste0(substr(cause, 1L, max_chars - 3L), "...")

  }

  list(cause = cause, where = parts$where %||% NA_character_)

}

#' Join a cause to its location and detail
#'
#' @param cause Character(1).
#' @param where Character(1) or `NA`/`NULL`: a backticked call.
#' @param detail Character or `NULL`: further context, e.g. a fold count.
#' @return ``"cause (in `f()`; detail)"``, or `cause` alone when there is no
#'   context.
#' @keywords internal
#' @noRd
format_condition <- function(cause, where = NA_character_, detail = NULL) {

  context <- c(if (length(where) == 1 && !is.na(where)) paste0("in ", where), detail)
  context <- context[!is.na(context) & nzchar(context)]

  if (length(context) == 0) return(cause)

  paste0(cause, " (", paste(context, collapse = "; "), ")")

}

#' Root cause and location of a condition's parent chain
#'
#' @param cnd A condition.
#' @return List with `cause` (character, possibly empty), `where` (a
#'   backticked call such as ``"`step_mutate()`"``, or `NULL`), `fallback`
#'   (the text to use when `cause` is empty) and `own` (whether the condition
#'   the cause is read from is one horizons composed, `HORIZONS_OWN_CLASSES`).
#' @keywords internal
#' @noRd
condition_cause <- function(cnd) {

  ## Walk the parent chain, outermost first ----------------------------------

  ### Bounded, so a chain that loops back on itself cannot hang a capture site.
  chain <- list()
  node  <- cnd

  while (inherits(node, "condition") && length(chain) < 50L) {

    chain[[length(chain) + 1L]] <- node
    node <- tryCatch(node[["parent"]], error = function(e) NULL)

  }

  ## Render each condition's own text -----------------------------------------

  ### With the rendering pinned rather than left to the session: unicode
  ### bullet glyphs, which clean_message_text() can tell from text (under
  ### testthat, or in a non-UTF-8 locale, cli would otherwise use the letters
  ### "i", "x" and "v"), no colour, and no wrapping.
  old_opts <- options(cli.unicode = TRUE, cli.num_colors = 1L, cli.width = 100000L)
  on.exit(options(old_opts), add = TRUE)

  texts    <- vapply(chain, condition_own_text, character(1))
  has_text <- which(nzchar(texts))
  fallback <- paste0(class(cnd)[1], " (no message)")

  if (length(has_text) == 0) {

    return(list(cause = "", where = NULL, fallback = fallback))

  }

  ## The deepest condition with text is the reason ---------------------------

  root <- max(has_text)

  ## The outermost wrapper above it with a named call is where ---------------

  ### For a recipe step error that is the step, e.g. `step_select_boruta()`.
  where <- NULL

  for (outer in chain[seq_len(root - 1L)]) {

    where <- condition_call_label(outer)

    if (!is.null(where)) break

  }

  list(cause = texts[root], where = where, fallback = fallback,
       own = inherits(chain[[root]], HORIZONS_OWN_CLASSES))

}

#' One condition's own text, without its parents
#'
#' @param cnd A condition.
#' @return Character(1), cleaned by [clean_message_text()]; `""` when the
#'   condition carries no text of its own.
#' @keywords internal
#' @noRd
condition_own_text <- function(cnd) {

  txt <- tryCatch(rlang::cnd_message(cnd, inherit = FALSE),
                  error = function(e) NULL)

  ### A conditionMessage() method that errors, or returns something that is
  ### not text, leaves the raw field as the last resort.
  if (!is.character(txt) || length(txt) == 0) {

    txt <- tryCatch(cnd[["message"]], error = function(e) NULL)

  }

  if (!is.character(txt) || length(txt) == 0) return("")

  clean_message_text(txt)

}

#' Name a condition's call for a location label
#'
#' @param cnd A condition.
#' @return ``"`fn()`"`` or ``"`pkg::fn()`"``, or `NULL` when the condition has
#'   no call or its call has no plain function name: a handler frame such as
#'   `value[[3L]](cond)` or an operator such as `x[[i]]` says nothing useful.
#' @keywords internal
#' @noRd
condition_call_label <- function(cnd) {

  call <- tryCatch(conditionCall(cnd), error = function(e) NULL)

  if (!is.call(call)) return(NULL)

  fn <- tryCatch(rlang::call_name(call), error = function(e) NULL)

  if (is.null(fn) || !grepl("^[.A-Za-z][.A-Za-z0-9_]*$", fn)) return(NULL)

  ns <- tryCatch(rlang::call_ns(call), error = function(e) NULL)

  paste0("`", if (!is.null(ns)) paste0(ns, "::"), fn, "()`")

}

#' Root cause and location of an already rendered message
#'
#' @param x Character. Text rendered from a condition, e.g. a tune note, in
#'   rlang's layout: an optional ``"Error in `f()`:"`` line, then
#'   `"Caused by error ...:"` lines, each followed by that parent's message.
#' @return List shaped like [condition_cause()]'s.
#' @keywords internal
#' @noRd
rendered_cause <- function(x) {

  text  <- cli::ansi_strip(valid_utf8(paste(as.character(x), collapse = "\n")))
  lines <- strsplit(text, "\r?\n")[[1]]
  where <- NULL

  if (length(lines) == 0) {

    return(list(cause = "", where = NULL, fallback = "no message"))

  }

  caused <- grep("^Caused by (error|warning)( in .+)?:\\s*$", lines)

  if (length(caused) > 0) {

    header <- regmatches(
      lines[1],
      regexec("^(?:Error|Warning) in (.+):\\s*$", lines[1], perl = TRUE)
    )[[1]]

    if (length(header) == 2L) where <- header[2]

    lines <- lines[-seq_len(max(caused))]

  } else if (grepl("^(Error|Warning)( in .+)?:\\s*$", lines[1])) {

    lines <- lines[-1L]

  }

  ### Text rendered elsewhere may carry cli's ASCII bullets.
  list(cause    = clean_message_text(lines, ascii_bullets = TRUE),
       where    = where,
       fallback = "no message")

}

#' Flatten message text to one clean line
#'
#' @param x Character. Message text, possibly multi-line, styled, and
#'   bulleted by cli.
#' @param ascii_bullets Logical. Also read cli's ASCII bullets (`"i"`, `"x"`,
#'   `"v"`, `">"`) at the start of a line after the first. For text rendered
#'   outside [condition_cause()]'s pinned options, where they cannot be told
#'   apart from a line that starts with one of those letters as a word, so the
#'   first line, where cli puts a header rather than a bullet, is left alone.
#' @return Character(1): ANSI styling and leading bullet glyphs removed, each
#'   bullet joined to the text before it with `"; "` (or a space, when that
#'   text already ends in punctuation), every other line break and run of
#'   whitespace collapsed to one space.
#' @keywords internal
#' @noRd
clean_message_text <- function(x, ascii_bullets = FALSE) {

  text  <- cli::ansi_strip(valid_utf8(paste(x, collapse = "\n")))
  ### cli keeps some inline spans together with non-breaking spaces, which
  ### "\\s" does not match.
  text  <- gsub("\u00a0", " ", text, fixed = TRUE)
  lines <- strsplit(text, "\r?\n")[[1]]

  ### cli's bullet glyphs (cross, info, bullet, tick, arrow, warning sign) and
  ### rlang's "!" and "*". Only at the start of a line, where cli puts them.
  glyph  <- "^\\s*(\u2716|\u2715|\u2139|\u2022|\u2714|\u2192|\u26a0|!|\\*)\\s+"
  bullet <- grepl(glyph, lines, perl = TRUE)
  lines  <- sub(glyph, "", lines, perl = TRUE)

  if (ascii_bullets && length(lines) > 1) {

    ascii <- "^(i|x|v|>) (?=\\S)"
    later <- seq_along(lines) > 1 & !bullet & grepl(ascii, lines, perl = TRUE)

    bullet[later] <- TRUE
    lines[later]  <- sub(ascii, "", lines[later], perl = TRUE)

  }

  lines <- trimws(lines)

  keep   <- nzchar(lines)
  lines  <- lines[keep]
  bullet <- bullet[keep]

  if (length(lines) == 0) return("")

  ### A bullet after a line that already ends a sentence needs no "; ".
  ends <- grepl("[.!?:;]$", lines)
  sep  <- c("", ifelse(bullet[-1] & !ends[-length(lines)], "; ", " "))

  trimws(gsub("\\s+", " ", paste0(sep, lines, collapse = "")))

}

#' Make text safe for the string functions
#'
#' @param x Character.
#' @return `x` in UTF-8, with any byte that is not valid UTF-8 written out as
#'   `"<e9>"` rather than left to make `strsplit()` or `nchar()` fail. An
#'   upstream message in another encoding is the case.
#' @keywords internal
#' @noRd
valid_utf8 <- function(x) {

  out <- iconv(enc2utf8(x), from = "UTF-8", to = "UTF-8", sub = "byte")
  out[is.na(out)] <- ""
  out

}

## ---------------------------------------------------------------------------
## check_rows_aligned
## ---------------------------------------------------------------------------

#' Assert that values attached by position line up with their rows
#'
#' @description
#' Several steps attach values to samples by position: point predictions,
#' interval bounds and applicability-domain columns to the rows of
#' `new_data`, calibration predictions to their truth, provenance columns to
#' the rows `select_training()` drew. Position is only right while every step
#' in between keeps the rows' count and order, and nothing promises that: a
#' recipe step that filters rows at bake drops rows from `predict()` and
#' `bake()` alike, without an error. This checks the invariant just before
#' the values are attached, so a change upstream fails loudly instead of
#' labelling samples with another sample's values.
#'
#' Called with `n` and `n_expected`, it checks the count. Called with `ids`
#' and `expected_ids` (`sample_id`s), it checks the count and the order. When
#' the rows line up it does nothing; otherwise it aborts through
#' [abort_misaligned()].
#'
#' @param what Character(1). The values being attached, for the message,
#'   e.g. `"Interval columns for config 'cfg_001'"`.
#' @param to Character(1). The rows they are attached to, e.g.
#'   `"the rows of new_data"`.
#' @param n Integer(1). Rows (or length) of the values. Defaults to
#'   `length(ids)`.
#' @param n_expected Integer(1). Number of rows they are attached to.
#'   Defaults to `length(expected_ids)`.
#' @param ids,expected_ids Character or `NULL`. Row keys of the values and of
#'   the rows. When both are given they must agree position by position.
#' @param call The frame the abort reports, the caller's by default.
#'
#' @return Invisibly `NULL`. Aborts with class `horizons_error_internal`.
#' @keywords internal
#' @noRd
check_rows_aligned <- function(what,
                               to,
                               n            = length(ids),
                               n_expected   = length(expected_ids),
                               ids          = NULL,
                               expected_ids = NULL,
                               call         = rlang::caller_env()) {

  ## Count -----------------------------------------------------------------

  if (!identical(as.integer(n), as.integer(n_expected))) {

    abort_misaligned(what, to,
                     cli::format_inline("Got {n} row{?s} for {n_expected}."),
                     call = call)

  }

  ## Order, when both sides carry a key --------------------------------------

  if (is.null(ids) || is.null(expected_ids)) return(invisible(NULL))

  ids          <- as.character(ids)
  expected_ids <- as.character(expected_ids)

  if (identical(ids, expected_ids)) return(invisible(NULL))

  differs <- which(is.na(ids) != is.na(expected_ids) |
                   (!is.na(ids) & !is.na(expected_ids) & ids != expected_ids))
  first   <- differs[1]

  abort_misaligned(what, to, cli::format_inline(
    "The {.field sample_id}s differ at {length(differs)} of {n} position{?s}; the first is row {first}, {.val {ids[first]}} where {.val {expected_ids[first]}} was expected."
  ), call = call)

}

#' Abort because values attached by position do not line up with their rows
#'
#' @description
#' The one abort behind [check_rows_aligned()] and the out-of-fold row check
#' in [fit_uq()]. A misalignment is a broken internal invariant, a bug in
#' horizons rather than something the user did, so it is an `"internal"`
#' error, whose footer asks for a report.
#'
#' @param what,to Character(1). The values, and the rows they are attached
#'   to, as in [check_rows_aligned()].
#' @param detail Character(1). What does not line up, already formatted
#'   (with [cli::format_inline()]); it is interpolated as a value.
#' @param call The frame the abort reports.
#'
#' @return Does not return.
#' @keywords internal
#' @noRd
abort_misaligned <- function(what, to, detail, call = rlang::caller_env()) {

  abort_horizons(c(
    "{what} do not line up with {to}.",
    "x" = "{detail}",
    "i" = "Values are attached by position here, so a step upstream dropped, added or reordered rows."
  ), kind = "internal", call = call)

}
