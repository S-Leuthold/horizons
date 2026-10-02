## ---------------------------------------------------------------------------
## Test helper: memoised fixtures
## ---------------------------------------------------------------------------
## memo_fixture() builds an expensive test object on its first use in an R
## process and serves the same value to every later caller. memo_dir() does
## the same for a directory and hands each caller a private copy of it. The
## cache key is the fixture's name plus a hash of the builder's body, its
## formals and the arguments passed through `...` (rlang::hash() ignores
## source references, so comments and layout do not count). Editing a
## builder or changing an argument therefore gives a new build. The cache
## lives in memory only; memo_reset() clears it, for interactive use. The
## formals are `.name` and `.build` so that a builder argument such as `n` is
## not partially matched to them.
##
## Rules for using it, checked in review:
##
##   1. Define each accessor as a top-level function, so that sourcing a test
##      file builds nothing:
##        fit60 <- function() memo_fixture("fit60", build_fit60)
##      A memo_dir() accessor must forward the caller's frame. The private
##      copy is deleted when the frame in `.env` exits, and without the
##      forwarding that frame is the accessor's own, so the copy would be
##      gone before the test saw it:
##        ck_a <- function(.env = parent.frame()) {
##          memo_dir("ck_a", build_ck_a, .env = .env)
##        }
##   2. Call accessors only inside it() or test_that(), after any skip_*(),
##      and before local_mocked_bindings(), local_options() or anything else
##      that would change what the builder does. A fixture built under a mock
##      is served to every later caller.
##   3. Builders are deterministic and self-contained: they set their own seed
##      and every option they depend on, and they take parameters only through
##      `...`. The key does not see a builder's enclosing variables, so two
##      closures with the same body share one entry.
##   4. Builders are quiet. A warning or message during a build aborts it and
##      names the fixture, so that output does not land on whichever test
##      happens to build first. Muffle expected noise inside the builder.
##      Package startup messages are let through, since they depend on which
##      test loaded the package first. The same goes for warnings a package
##      gives once per process: parallelly warns the first time it reads the
##      CPU set (on some Linux machines, about the cgroups CPU set), so a
##      builder that starts a parallel plan would abort only when it made
##      the first parallel call in its process, and the cached error would
##      then fail every user. The helper therefore calls
##      future::availableCores() once, muffled, before the first build in a
##      process. Muffle any other once-per-process warning in the builder.
##   5. Returned values are read-only. Lists and tibbles copy on modify, but
##      environments are modified in place: never modify an environment
##      inside a returned object. With HORIZONS_MEMO_VERIFY=true, every hit
##      checks the value (and a memo_dir() template's files) against a hash
##      taken at build time and aborts on any change. Verify mode turns R's
##      JIT compiler off for the rest of the process (memo_reset() turns it
##      back on), because compiling a stored closure when a test first calls
##      it changes how the closure serialises.
##   6. A memo_dir() builder writes into the directory it is given. Paths in
##      the returned value point at the shared template, so tests work in
##      `$dir`, their own copy, and never write through `$value`.
##
## On a miss, the RNG state after the build is stored and restored on every
## hit, so a hit leaves the RNG where a rebuild would (a builder that leaves
## the RNG untouched leaves the caller's stream alone). A build error is
## cached and re-signalled to each caller as `horizons_fixture_error`, with
## the original error as its parent. A skip during a build propagates and is
## not cached.
##
## Scope: one cache per R process. Under testthat's parallel workers each
## worker has its own cache, so a fixture is shared only among the files that
## worker runs. Keep a fixture's users in one file.

.horizons_memo           <- new.env(parent = emptyenv())
.horizons_memo$cache     <- new.env(parent = emptyenv())
.horizons_memo$root      <- NULL
.horizons_memo$jit_level <- NULL
.horizons_memo$primed    <- FALSE


## ---------------------------------------------------------------------------
## Public API
## ---------------------------------------------------------------------------

#' Build a fixture once per process and serve it to every caller
#'
#' @param .name [Character, length 1.] The fixture's name.
#' @param .build [Function.] Builds the value; called as `.build(...)` on a
#'   miss.
#' @param ... Arguments for `.build`. Part of the cache key.
#'
#' @return The value `.build(...)` returned when the entry was built.
#' @noRd
memo_fixture <- function(.name, .build, ...) {

  memo_check_args(.name, .build)

  args  <- list(...)
  key   <- memo_key("value", .name, .build, args)
  entry <- .horizons_memo$cache[[key]]

  if (is.null(entry)) {
    entry <- memo_build(.name, .build, function() .build(...))
    .horizons_memo$cache[[key]] <- entry
  } else {
    memo_verify(entry)
  }

  memo_serve(entry)

}

#' Build a template directory once and give each caller a private copy
#'
#' @param .name [Character, length 1.] The fixture's name.
#' @param .build [Function.] Called as `.build(dir, ...)` on a miss, where
#'   `dir` is an empty template directory the builder fills.
#' @param ... Arguments for `.build`. Part of the cache key.
#' @param .env [Environment.] Frame that owns the copy; it is deleted when
#'   that frame exits. Default: the caller's. An accessor wrapping memo_dir()
#'   must pass its own caller's frame here (header, rule 1).
#'
#' @return [List.] `dir`, a private copy of the template inside a
#'   `withr::local_tempdir()`, and `value`, what `.build()` returned.
#' @noRd
memo_dir <- function(.name, .build, ..., .env = parent.frame()) {

  memo_check_args(.name, .build)

  args  <- list(...)
  key   <- memo_key("dir", .name, .build, args)
  entry <- .horizons_memo$cache[[key]]

  if (is.null(entry)) {
    template <- memo_template_path(.name)
    entry    <- memo_build(.name, .build, function() .build(template, ...),
                           template = template)
    .horizons_memo$cache[[key]] <- entry
  } else {
    memo_verify(entry)
  }

  value <- memo_serve(entry)
  dir   <- withr::local_tempdir(pattern = "memo-copy-", .local_envir = .env)

  memo_copy_template(entry, dir)

  list(dir = dir, value = value)

}

#' Clear every memoised fixture and template directory in this process
#'
#' @description
#' Also turns the JIT compiler back on if verify mode turned it off.
#'
#' @return `NULL`, invisibly.
#' @noRd
memo_reset <- function() {

  cache <- .horizons_memo$cache
  rm(list = ls(cache, all.names = TRUE), envir = cache)

  if (!is.null(.horizons_memo$root)) {
    unlink(.horizons_memo$root, recursive = TRUE)
  }
  .horizons_memo$root <- NULL

  if (!is.null(.horizons_memo$jit_level)) {
    compiler::enableJIT(.horizons_memo$jit_level)
    .horizons_memo$jit_level <- NULL
  }

  invisible(NULL)

}


## ---------------------------------------------------------------------------
## Internals
## ---------------------------------------------------------------------------

memo_check_args <- function(name, build) {

  if (!rlang::is_string(name) || !nzchar(name)) {
    cli::cli_abort("{.arg name} must be a single non-empty string.",
                   call = NULL)
  }

  if (!is.function(build) || is.primitive(build)) {
    cli::cli_abort("{.arg build} must be a function.", call = NULL)
  }

}

memo_key <- function(kind, name, build, args) {

  paste(kind, name, rlang::hash(list(body(build), formals(build), args)),
        sep = ":")

}

## ---------------------------------------------------------------------------
## Building and serving an entry
## ---------------------------------------------------------------------------

memo_build <- function(name, build, run, template = NULL) {

  ### A skip or an interrupt leaves by a non-local exit and caches nothing;
  ### the template of a build that did not finish is removed.
  finished <- FALSE

  if (!is.null(template)) {
    dir.create(template, recursive = TRUE)
    on.exit(if (!finished) unlink(template, recursive = TRUE), add = TRUE)
  }

  memo_prime_process()

  if (memo_verify_on()) {
    memo_jit_off()
  }

  seed_before <- memo_get_seed()

  outcome <- tryCatch(
    withCallingHandlers(
      list(ok = TRUE, value = run()),
      warning = function(cnd) memo_abort_noisy(name, "a warning", cnd),
      message = function(cnd) {
        if (!inherits(cnd, "packageStartupMessage")) {
          memo_abort_noisy(name, "a message", cnd)
        }
      }
    ),
    error = function(cnd) list(ok = FALSE, error = cnd)
  )

  entry           <- new.env(parent = emptyenv())
  entry$name      <- name
  entry$ok        <- outcome$ok
  entry$value     <- outcome$value
  entry$error     <- outcome$error
  entry$build_env <- environment(build)
  entry$template  <- if (outcome$ok) template else NULL

  seed_after        <- memo_get_seed()
  entry$rng_changed <- !identical(seed_before, seed_after)
  entry$seed        <- seed_after

  if (outcome$ok && memo_verify_on()) {
    memo_take_digests(entry)
  }

  finished <- outcome$ok
  entry

}

memo_prime_process <- function() {

  if (.horizons_memo$primed) {
    return(invisible(NULL))
  }
  .horizons_memo$primed <- TRUE

  ### parallelly warns once per process when it first reads the CPU set
  ### (rule 4); take that warning here, outside any build.
  suppressWarnings(future::availableCores())

  invisible(NULL)

}

memo_abort_noisy <- function(name, what, cnd) {

  cli::cli_abort(
    c("Fixture {.val {name}} emitted {what} while building.",
      "i" = "Builders must be quiet; muffle expected output inside the builder."),
    class = "horizons_fixture_noisy", parent = cnd, call = NULL
  )

}

memo_serve <- function(entry) {

  if (!entry$ok) {
    cli::cli_abort(
      "Fixture {.val {entry$name}} failed to build.",
      class = "horizons_fixture_error", parent = entry$error,
      fixture = entry$name, call = NULL
    )
  }

  if (entry$rng_changed) {
    memo_set_seed(entry$seed)
  }

  entry$value

}

memo_get_seed <- function() {

  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  } else {
    NULL
  }

}

memo_set_seed <- function(seed) {

  if (is.null(seed)) {
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  } else {
    assign(".Random.seed", seed, envir = globalenv())
  }

}

memo_copy_template <- function(entry, dir) {

  if (!dir.exists(entry$template)) {
    cli::cli_abort(
      "The template directory of fixture {.val {entry$name}} has been removed.",
      class = "horizons_fixture_modified", call = NULL
    )
  }

  items <- list.files(entry$template, all.files = TRUE, no.. = TRUE,
                      full.names = TRUE)

  if (!all(file.copy(items, dir, recursive = TRUE, copy.date = TRUE))) {
    cli::cli_abort(
      "Could not copy the template directory of fixture {.val {entry$name}}.",
      call = NULL
    )
  }

}

memo_template_path <- function(name) {

  root <- .horizons_memo$root

  if (is.null(root) || !dir.exists(root)) {
    root <- tempfile("horizons-memo-")
    dir.create(root)
    .horizons_memo$root <- root
  }

  tempfile(paste0(gsub("[^A-Za-z0-9_.-]", "_", name), "-"), tmpdir = root)

}

## ---------------------------------------------------------------------------
## Verify mode (HORIZONS_MEMO_VERIFY=true)
## ---------------------------------------------------------------------------

memo_verify_on <- function() {

  isTRUE(as.logical(Sys.getenv("HORIZONS_MEMO_VERIFY", "false")))

}

memo_jit_off <- function() {

  ## The JIT compiles a closure in place on an early call, replacing its body
  ## with bytecode and setting flags that serialize() writes, so a stored
  ## closure a test merely calls would read as modified. With the JIT off a
  ## closure serialises the same before and after a call. The level before
  ## the first switch-off is kept for memo_reset().
  old <- compiler::enableJIT(0)

  if (is.null(.horizons_memo$jit_level)) {
    .horizons_memo$jit_level <- old
  }

}

memo_take_digests <- function(entry) {

  entry$digest <- memo_digest_value(entry$value, entry$build_env)

  if (!is.null(entry$template)) {
    entry$dir_digest <- memo_digest_dir(entry$template)
  }

}

memo_verify <- function(entry) {

  if (!entry$ok || !memo_verify_on()) {
    return(invisible(NULL))
  }

  memo_jit_off()

  ### Verify switched on after this entry was built: take the baseline now.
  if (is.null(entry$digest)) {
    memo_take_digests(entry)
    return(invisible(NULL))
  }

  if (!identical(memo_digest_value(entry$value, entry$build_env),
                 entry$digest)) {
    cli::cli_abort(
      c("Fixture {.val {entry$name}} was modified in place after it was built.",
        "i" = "A test that used it changed it; memoised values are read-only."),
      class = "horizons_fixture_modified", call = NULL
    )
  }

  if (!is.null(entry$template) &&
      !identical(memo_digest_dir(entry$template), entry$dir_digest)) {
    cli::cli_abort(
      c("The template directory of fixture {.val {entry$name}} changed after it was built.",
        "i" = "A test wrote into the template; use the copy in {.field dir}."),
      class = "horizons_fixture_modified", call = NULL
    )
  }

  invisible(NULL)

}

memo_digest_value <- function(value, build_env) {

  ## The builder's defining environment and its ancestors (the test file, the
  ## helpers, the namespace) are context, not part of the value. A closure or
  ## formula created during the build reaches them through its enclosure;
  ## without this hook their contents, which change as a file runs, would be
  ## hashed too. Serialization version 2 writes ALTREP vectors expanded, so a
  ## vector's internal representation changing does not count as a change.
  context <- list()
  env     <- build_env

  while (!identical(env, emptyenv())) {
    context[[length(context) + 1L]] <- env
    env <- parent.env(env)
  }

  hook <- function(e) {
    for (ctx in context) {
      if (identical(e, ctx)) return("memo-context")
    }
    NULL
  }

  rlang::hash(serialize(value, connection = NULL, version = 2, refhook = hook))

}

memo_digest_dir <- function(path) {

  ### Directories are listed by name only; md5sum() warns on them.
  entries <- sort(list.files(path, recursive = TRUE, all.files = TRUE,
                             no.. = TRUE, include.dirs = TRUE))
  is_file <- !dir.exists(file.path(path, entries))
  sums    <- rep(NA_character_, length(entries))
  sums[is_file] <- unname(tools::md5sum(file.path(path, entries[is_file])))

  rlang::hash(list(entries, sums))

}
