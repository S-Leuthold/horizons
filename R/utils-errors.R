#' Safely Evaluate an Expression
#'
#' @description
#' Evaluates an expression using `purrr::safely()` to prevent errors from
#' halting execution. Returns a structured list with result, error, and
#' optionally captured warnings/messages. This is the workhorse for
#' evaluate()'s config loop — each model fit is wrapped in safely_execute()
#' so one failure doesn't kill the batch.
#'
#' @param expr An expression to evaluate (unquoted).
#' @param default_value Value returned if the expression errors. Default NULL.
#' @param error_message Optional character string logged on error. Supports
#'   glue-style interpolation from the caller's environment.
#' @param log_error Logical. If TRUE (default), logs error via `warning()`.
#' @param capture_conditions Logical. If TRUE, captures warnings and messages
#'   emitted during evaluation. Default FALSE.
#'
#' @return Named list:
#'   - `result`: Evaluated result, or `default_value` on error.
#'   - `error`: Error condition object, or NULL on success.
#'   - `warnings`: List of warning strings (if captured), else NULL.
#'   - `messages`: List of message strings (if captured), else NULL.
#'   - `n_warnings`: Integer count of captured warnings.
#'   - `n_messages`: Integer count of captured messages.
#'
#' @export
safely_execute <- function(expr,
                           default_value      = NULL,
                           error_message      = NULL,
                           log_error          = TRUE,
                           capture_conditions = FALSE) {

  ## Capture expression and caller environment ------------------------------

  expr_quo   <- rlang::enquo(expr)
  caller_env <- rlang::caller_env()

  ## Condition storage ------------------------------------------------------

  captured_warnings <- list()
  captured_messages <- list()

  ## Build safely-wrapped evaluator -----------------------------------------

  safe_eval <- purrr::safely(function() {

    if (capture_conditions) {

      withCallingHandlers(
        rlang::eval_tidy(expr_quo, env = caller_env),
        warning = function(w) {
          captured_warnings <<- append(captured_warnings, list(conditionMessage(w)))
          invokeRestart("muffleWarning")
        },
        message = function(m) {
          captured_messages <<- append(captured_messages, list(conditionMessage(m)))
          invokeRestart("muffleMessage")
        }
      )

    } else {

      rlang::eval_tidy(expr_quo, env = caller_env)

    }

  }, otherwise = default_value, quiet = TRUE)

  ## Execute ----------------------------------------------------------------

  result_list <- safe_eval()

  ## Log error if requested -------------------------------------------------

  if (!is.null(result_list$error) && log_error) {

    msg <- if (!is.null(error_message)) {

      tryCatch(
        glue::glue(error_message, .envir = caller_env),
        error = function(e) error_message
      )

    } else {

      "An error occurred"

    }

    warning(paste0(msg, ": ", condition_summary(result_list$error)), call. = FALSE)

  }

  ## Return structured result -----------------------------------------------

  list(
    result     = result_list$result,
    error      = result_list$error,
    warnings   = if (length(captured_warnings) > 0) captured_warnings else NULL,
    messages   = if (length(captured_messages) > 0) captured_messages else NULL,
    n_warnings = length(captured_warnings),
    n_messages = length(captured_messages)
  )

}

## ---------------------------------------------------------------------------
## handle_results
## ---------------------------------------------------------------------------

#' Handle Results from safely_execute
#'
#' @description
#' Processes the output from `safely_execute()`. On success, returns the
#' result (optionally re-emitting captured warnings). On failure, either
#' returns NULL or aborts with a structured error message, depending on
#' `abort_on_null`.
#'
#' @param safe_result List from `safely_execute()` with `result` and `error`.
#' @param error_title Character. Main error message for abort. Default
#'   "Operation failed".
#' @param error_hints Character vector. Troubleshooting suggestions appended
#'   to the error message.
#' @param abort_on_null Logical. Abort if result is NULL? Default TRUE.
#' @param silent Logical. Suppress re-emitted warnings/messages? Default FALSE.
#'
#' @return The result from `safe_result`, or NULL if `abort_on_null = FALSE`.
#' @export
handle_results <- function(safe_result,
                           error_title   = "Operation failed",
                           error_hints   = NULL,
                           abort_on_null = TRUE,
                           silent        = FALSE) {

  ## Validate input structure -----------------------------------------------

  if (!is.list(safe_result) || !all(c("result", "error") %in% names(safe_result))) {

    rlang::abort("Invalid safe_result structure. Expected output from safely_execute()")

  }

  result <- safe_result$result

  ## Success path -----------------------------------------------------------

  if (!is.null(result)) {

    if (!silent) {

      if (!is.null(safe_result$warnings) && length(safe_result$warnings) > 0) {

        for (w in safe_result$warnings) {
          warning(w, call. = FALSE)
        }

      }

      if (!is.null(safe_result$messages) && length(safe_result$messages) > 0) {

        for (m in safe_result$messages) {
          message(m)
        }

      }

    }

    return(result)

  }

  ## Failure path: return NULL or abort -------------------------------------

  if (!abort_on_null) return(NULL)

  ## Build abort message ----------------------------------------------------

  abort_msg <- error_title

  if (!is.null(error_hints) && length(error_hints) > 0) {

    abort_msg <- c(abort_msg, stats::setNames(error_hints, rep("i", length(error_hints))))

  }

  if (!is.null(safe_result$error)) {

    abort_msg <- c(abort_msg, x = condition_summary(safe_result$error))

  }

  rlang::abort(abort_msg)

}

## ---------------------------------------------------------------------------
## create_failed_result
## ---------------------------------------------------------------------------

#' Create a Standardized Failed-Config Result Row
#'
#' @description
#' When a config fails during evaluate()'s inner loop, this creates a
#' single-row tibble with the expected column structure so the results
#' tibble stays rectangular. All metric columns are NA, status is "failed".
#'
#' @param config_id Character. The config hash identifier.
#' @param error Error condition object, character string, or NULL.
#'
#' @return Single-row tibble with config_id, status, error_message, metric
#'   columns (all NA), and runtime_secs (NA).
#' @keywords internal
#' @export
create_failed_result <- function(config_id, error = NULL) {

  ## Extract error message --------------------------------------------------

  error_msg <- if (is.null(error)) {

    NA_character_

  } else if (inherits(error, "condition")) {

    condition_summary(error)

  } else if (is.character(error)) {

    error

  } else {

    as.character(error)

  }

  ## Build result row -------------------------------------------------------

  tibble::tibble(
    config_id     = config_id,
    status        = "failed",
    below_prune_threshold = NA,
    prune_threshold       = NA_real_,
    rmse          = NA_real_,
    rrmse         = NA_real_,
    rsq           = NA_real_,
    ccc           = NA_real_,
    rpd           = NA_real_,
    mae           = NA_real_,
    cv_rmse       = NA_real_,
    cv_rrmse      = NA_real_,
    cv_rsq        = NA_real_,
    cv_ccc        = NA_real_,
    cv_rpd        = NA_real_,
    cv_mae        = NA_real_,
    scoring_schema = SCORING_SCHEMA,
    best_params   = list(NULL),
    error_message = error_msg,
    warnings      = list(NULL),
    runtime_secs  = NA_real_
  )

}

## ---------------------------------------------------------------------------
## distinct_config_errors
## ---------------------------------------------------------------------------

#' Summarise the distinct error messages in a results table as cli bullets
#'
#' @description
#' Groups the configs of a results table by their `error_message` and returns
#' one `"x"` bullet per distinct message, in the order the messages first
#' appear, each followed by the configs that raised it, then an `"i"` bullet
#' counting the messages not shown. `evaluate()` and `fit()` put these
#' bullets in the abort they raise when every config (or member) fails, so
#' the cause is in the message rather than only in a table the caller may not
#' be able to reach.
#'
#' The bullets are finished: the upstream text in them has its braces escaped
#' with `cli_escape()`, so they can be passed to cli as they are. This is the
#' one place the brace-safety rule is enforced for these aborts; a caller
#' never builds the bullets itself.
#'
#' @param results Tibble with `config_id` and `error_message` columns.
#' @param n_show Integer. How many distinct messages to list. Default 3.
#' @param n_ids Integer. How many config ids to name per message before
#'   counting the rest. Default 3.
#'
#' @return Named character vector of cli bullets (names `"x"` and `"i"`),
#'   empty when no row has a message. Rows whose `error_message` is `NA` or
#'   empty are ignored.
#' @keywords internal
#' @noRd
distinct_config_errors <- function(results, n_show = 3L, n_ids = 3L) {

  ## Checked by name: `$` on a tibble without the column warns.
  if (!"error_message" %in% names(results)) {

    return(stats::setNames(character(0), character(0)))

  }

  msgs <- results$error_message

  has_msg  <- !is.na(msgs) & nzchar(msgs)
  distinct <- unique(msgs[has_msg])
  shown    <- utils::head(distinct, n_show)
  n_more   <- length(distinct) - length(shown)

  lines <- vapply(shown, function(m) {

    ids <- results$config_id[has_msg & msgs == m]

    id_label <- if (length(ids) > n_ids) {
      paste0(paste(ids[seq_len(n_ids)], collapse = ", "),
             " and ", length(ids) - n_ids, " more")
    } else {
      paste(ids, collapse = ", ")
    }

    paste0(m, " (", id_label, ")")

  }, character(1), USE.NAMES = FALSE)

  bullets <- stats::setNames(cli_escape(lines), rep("x", length(lines)))

  if (n_more > 0) {

    bullets <- c(bullets, "i" = paste0(
      n_more, " more distinct error message", if (n_more > 1) "s", " not shown."
    ))

  }

  bullets

}

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
#'   marked with `"..."`.
#'
#' @return Character(1), never empty. A condition with no text anywhere in
#'   its chain is described by its class.
#' @keywords internal
#' @noRd
condition_summary <- function(x, detail = NULL, max_chars = 300L) {

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
  ## and detail are added after the cut so they always survive.
  if (isTRUE(nchar(cause, allowNA = TRUE) > max_chars)) {

    cause <- paste0(substr(cause, 1L, max_chars - 3L), "...")

  }

  context <- c(if (!is.null(parts$where)) paste0("in ", parts$where), detail)
  context <- context[nzchar(context)]

  if (length(context) > 0) {

    cause <- paste0(cause, " (", paste(context, collapse = "; "), ")")

  }

  cause

}

#' Root cause and location of a condition's parent chain
#'
#' @param cnd A condition.
#' @return List with `cause` (character, possibly empty), `where` (a
#'   backticked call such as ``"`step_mutate()`"``, or `NULL`) and `fallback`
#'   (the text to use when `cause` is empty).
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

  list(cause = texts[root], where = where, fallback = fallback)

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
## tune_note_messages
## ---------------------------------------------------------------------------

#' Distinct notes of one type from a tune result, as readable lines
#'
#' @description
#' tune catches the errors and warnings raised inside `tune_grid()`,
#' `tune_bayes()`, `fit_resamples()` and `last_fit()` and stores them in the
#' result's `.notes` column instead of signalling them, so a capture site
#' that only watches for conditions never sees them. A recipe step that fails
#' or warns during prep inside tuning leaves its only trace there. This
#' reads the notes back through [condition_summary()] and returns one line
#' per distinct message, most frequent first, each saying how many folds
#' raised it (`"3 of 5 folds"`) when the result has more than one.
#'
#' @param tune_results A `tune_results` object (or anything
#'   `tune::collect_notes()` reads).
#' @param type Character. Note types to keep: `"error"`, `"warning"`, or both.
#'
#' @return Character vector, empty when there are no notes of that type or
#'   the notes cannot be read.
#' @keywords internal
#' @noRd
tune_note_messages <- function(tune_results, type = "warning") {

  notes <- tryCatch(tune::collect_notes(tune_results), error = function(e) NULL)

  if (!is.data.frame(notes) || !all(c("type", "note") %in% names(notes))) {

    return(character(0))

  }

  notes <- notes[notes$type %in% type, , drop = FALSE]

  if (nrow(notes) == 0) return(character(0))

  ## One key per note: the summary without any count --------------------------

  keys <- vapply(notes$note, condition_summary, character(1), USE.NAMES = FALSE)

  ## Which resample raised each note ------------------------------------------

  ### id, plus id2 for repeated CV. last_fit()'s notes carry no id, and one
  ### split has nothing to count, so no count is given there.
  id_cols  <- grep("^id", names(notes), value = TRUE)
  note_ids <- if (length(id_cols) > 0) {
    do.call(paste, c(unname(as.list(notes[id_cols])), sep = "/"))
  } else {
    NULL
  }

  n_resamples <- tryCatch({
    all_cols <- grep("^id", names(tune_results), value = TRUE)
    length(unique(do.call(paste, c(unname(as.list(tune_results[all_cols])), sep = "/"))))
  }, error = function(e) NA_integer_)

  ## Distinct messages, most frequent first ------------------------------------

  distinct <- unique(keys)
  counts   <- vapply(distinct, function(k) {
    if (is.null(note_ids)) sum(keys == k) else length(unique(note_ids[keys == k]))
  }, integer(1), USE.NAMES = FALSE)

  ord <- order(-counts, seq_along(distinct))

  vapply(ord, function(i) {

    first  <- notes$note[match(distinct[i], keys)]
    detail <- if (!is.null(note_ids) && isTRUE(n_resamples > 1)) {
      paste0(counts[i], " of ", n_resamples, " folds")
    }

    condition_summary(first, detail = detail)

  }, character(1))

}

#' The cause to append when every model in a tune result failed
#'
#' @param tune_results A `tune_results` object whose models all failed.
#' @return Character(1): the most frequent distinct error note, plus a count
#'   of the other distinct errors when there are any; `NULL` when the result
#'   carries no error notes.
#' @keywords internal
#' @noRd
tune_failure_cause <- function(tune_results) {

  causes <- tune_note_messages(tune_results, type = "error")

  if (length(causes) == 0) return(NULL)

  n_other <- length(causes) - 1L

  if (n_other == 0) return(causes[1])

  paste0(causes[1], ", plus ", n_other, " other distinct error",
         if (n_other > 1) "s")

}
