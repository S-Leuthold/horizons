## ---------------------------------------------------------------------------
## Randomness: one rule
## ---------------------------------------------------------------------------
##
## Every unit of work draws from its own seed, a hash of the run's seed (the
## root) and the unit's name, inside a scope that restores the caller's random
## state on exit. A unit's results then depend on the root and the unit alone:
## not on the order of work, the number of workers, the backend, what an
## engine or an earlier stage drew, or the session's RNGkind(). Each verb
## resolves the root once, at its door, and passes it down as `seed`.
##
## A name is `<verb>/<scope>/<stage>`: c("evaluate", config_id, "tune"),
## "fit/calibration", c("ensemble", "cvplus", "fold-3"). The parts are joined
## by "/" before hashing, so a name given as one string or as its parts gives
## the same seed.


#' Restore the caller's RNG state when a function exits
#'
#' @description
#' Records `.Random.seed` (or its absence) and registers its restore on
#' `frame`'s exit, after any exit handlers already there. Restoring
#' `.Random.seed` restores the RNG kind with it, and a stream that was absent
#' is removed again. A function that seeds or draws for its own purposes calls
#' this first, so the caller's stream is left as found.
#'
#' @param frame The frame whose exit restores the state: the caller's by
#'   default, or a `local()` block's when called inside one.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd

local_rng_state <- function(frame = parent.frame()) {

  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv()) else NULL

  restore <- function() {
    if (had_seed) assign(".Random.seed", old_seed, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  }

  ## Registered on the caller's frame, as withr's local_ functions do; written
  ## out because withr is only a Suggests.
  do.call(base::on.exit, list(as.call(list(restore)), add = TRUE), envir = frame)

  invisible(NULL)

}


#' A unit's seed, from the run's seed and the unit's name
#'
#' @description
#' The first seven hex digits of [digest::digest()] of `"<root>/<name>"`, as
#' an integer in `[0, 2^28)`: digest's defaults, as `generate_config_id()`
#' calls it, so the package has one hashing rule. The root is taken as an
#' integer, so `307` and `307L` give the same seed. A pure function of its
#' arguments: any unit can be re-run alone by recomputing its seed.
#'
#' Changing what this returns for a given root and name (the hash, the key, a
#' unit's name) changes every result, and bumps `SEED_RULE`.
#'
#' @param seed The run's root: a single whole number.
#' @param name The unit's name, as one string or as its parts.
#' @return An integer.
#' @keywords internal
#' @noRd

unit_seed <- function(seed, name) {

  ## A NULL or NA root would drop out of the key silently and give every run
  ## the same seeds
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || seed != trunc(seed)) {
    abort_horizons("A unit's seed needs the run's seed as one whole number.", kind = "internal")
  }

  key <- paste(c(as.integer(seed), name), collapse = "/")

  strtoi(substr(digest::digest(key), 1L, 7L), base = 16L)

}


#' Run code under a unit's seed
#'
#' @description
#' Seeds the generator with [unit_seed()] under Mersenne-Twister, Inversion
#' and Rejection (R's defaults since 3.6, pinned so a session's `RNGkind()` or
#' `RNGversion()` cannot move the draws), evaluates `code`, and restores the
#' caller's stream and kinds on exit, on an error too. Units nest: an inner
#' unit leaves the outer one's stream where it found it.
#'
#' @param seed The run's root.
#' @param name The unit's name, as one string or as its parts.
#' @param code The unit's work, evaluated lazily.
#' @return The value of `code`.
#' @keywords internal
#' @noRd

with_unit_seed <- function(seed, name, code) {

  local_rng_state()

  set.seed(unit_seed(seed, name), kind = "Mersenne-Twister",
           normal.kind = "Inversion", sample.kind = "Rejection")

  code

}


#' The run's root seed, resolved once at a verb's door
#'
#' @description
#' An explicit `seed`; else the object's recorded root ([recorded_seed()]);
#' else one integer drawn from the caller's stream with `sample.int(100000L,
#' 1L)`, under the caller's kinds, after which the stream is restored. So the
#' same `set.seed()` gives the same run however many verbs follow, and the
#' user's own draws are not moved; calls with nothing drawn between them get
#' the same root, and only a session that has never drawn gets a fresh one
#' each time. There is no fixed fallback.
#'
#' @param seed The verb's `seed`: `NULL` or a whole number, checked at the
#'   door.
#' @param x The object the verb was given.
#' @return A list: `seed`, the root as an integer, and `drawn`, `TRUE` when it
#'   came from the session's stream (the verb's tree then prints it).
#' @keywords internal
#' @noRd

resolve_seed <- function(seed, x) {

  seed <- seed %||% recorded_seed(x)

  if (!is.null(seed)) return(list(seed = as.integer(seed), drawn = FALSE))

  local_rng_state()

  list(seed = sample.int(100000L, 1L), drawn = TRUE)

}


#' The root an object records
#'
#' @description
#' The most recent of the roots the verbs record on their slots, read in
#' reverse lifecycle order: `ensemble$seed`, `models$seed`,
#' `evaluation$seed`, `selection$settings$seed`. So one number reproduces a
#' pipeline from the library draw to the ensemble, and an explicit seed at a
#' later verb is the one its successors take.
#'
#' @param x A horizons object.
#' @return The root, or `NULL` when the object records none.
#' @keywords internal
#' @noRd

recorded_seed <- function(x) {

  x$ensemble$seed %||% x$models$seed %||% x$evaluation$seed %||% x$selection$settings$seed

}


#' The tree's line for a root drawn from the session
#'
#' @param root From [resolve_seed()].
#' @return The line, or `NULL` when the root was given or recorded.
#' @keywords internal
#' @noRd

seed_tree_line <- function(root) {

  if (!root$drawn) return(NULL)

  paste0("Seed: ", root$seed, ", drawn from the session; `seed = ", root$seed, "` repeats this run")

}


#' Give a workflow's engine the stage's seed
#'
#' @description
#' Sets `seed` among the engine arguments of a workflow's model, for the
#' engines that take one (`SEEDED_ENGINES`), keeping every other engine
#' argument. parsnip and bonsai otherwise draw the engine's seed from the
#' stream (`sample.int(10^5, 1)`), which moves with whatever drew before and
#' is not recorded. Other engines' workflows are returned unchanged.
#'
#' @param workflow An untrained workflow with a model.
#' @param seed The stage's seed: [unit_seed()] of the stage's name.
#' @return The workflow.
#' @keywords internal
#' @noRd

seed_engine <- function(workflow, seed) {

  spec <- workflows::extract_spec_parsnip(workflow)

  if (!spec$engine %in% SEEDED_ENGINES) return(workflow)

  args      <- as.list(spec$eng_args)
  args$seed <- as.integer(seed)
  spec      <- parsnip::set_engine(spec, spec$engine, !!!args)

  workflows::update_model(workflow, spec)

}
