## ---------------------------------------------------------------------------
## Isolated engines: an engine's fits, each in a process of its own
## ---------------------------------------------------------------------------
## Some engines carry state from one fit to the next in the process that
## fits them, which no argument of theirs resets. A model whose MODEL_SPECS
## entry has `isolate = TRUE` is fitted through a twin of its engine, which
## horizons registers with parsnip at load: the engine's own registration,
## copied under the name "<engine>_isolated", whose fit function runs the
## engine's own registered fit function in a process started for that one fit
## (run_isolated(), R/utils-parallel.R). The fitted model comes back to the
## caller and predicts there through the engine's own code. Nothing here
## knows any engine: turning isolation on for another one is its row's field.
##
## Cubist 0.6.0 is the first. Its C code keeps two kinds of state across fits
## in a process: the stream of its own generator, KRandom(), which is never
## reset and from which the neighbour step's distance (`maxd`) is drawn, so a
## fit's predictions with neighbours depend on the fits before it; and two
## (p + 2) x (p + 2) matrices each fit leaves allocated, 44 MB a fit at 1,691
## predictors, which no collection returns. A process of its own starts each
## fit from the same state and gives the memory back when it exits.


#' The name of an engine's isolated twin
#'
#' @param engine Character(1). The engine, as parsnip names it.
#' @return Character(1).
#' @keywords internal
#' @noRd
isolated_engine_name <- function(engine) {

  paste0(engine, "_isolated")

}


#' The engine a model is fitted with
#'
#' @description
#' The engine `MODEL_SPECS` names, or its isolated twin when the model is
#' marked `isolate` and `isolate` is `TRUE`. The one reader of which engine a
#' model uses, so isolation on or off is decided here for every model.
#'
#' @param model Character(1). A short model name (`VALID_MODELS`).
#' @param isolate Logical(1). Whether isolated work is on
#'   (`control_horizons(isolate = )`).
#' @return Character(1).
#' @keywords internal
#' @noRd
model_engine <- function(model, isolate = TRUE) {

  spec <- MODEL_SPECS[[model]]

  if (isTRUE(isolate) && isTRUE(spec$isolate)) isolated_engine_name(spec$engine) else spec$engine

}


#' The models among `models` whose fits are isolated when isolation is on
#'
#' @param models Character. Short model names, possibly repeated.
#' @return Character: the distinct models marked `isolate` in `MODEL_SPECS`,
#'   in the order they first appear.
#' @keywords internal
#' @noRd
isolated_models <- function(models) {

  models <- unique(as.character(models))

  models[vapply(models, function(m) isTRUE(MODEL_SPECS[[m]]$isolate), logical(1))]

}


#' Register the isolated twin of every engine `MODEL_SPECS` marks
#'
#' @description
#' Called from `.onLoad()`, after the packages that register the engines
#' have loaded. An engine whose own registration is absent (its package is
#' not installed) gets no twin; the verbs' door check refuses its models
#' first. Registering twice in one session is a no-op.
#'
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
register_isolated_engines <- function() {

  for (spec in MODEL_SPECS) {

    if (isTRUE(spec$isolate)) register_isolated_engine(spec$fn, spec$engine)

  }

  invisible(NULL)

}


#' Register one engine's isolated twin with parsnip
#'
#' @description
#' Copies the engine's registration for `mode` from parsnip's model
#' environment under [isolated_engine_name()]: its arguments, with any that
#' tune treats as a submodel; its prediction types, so prediction and the
#' submodel path stay the engine's own code; its encoding; and its packages,
#' with horizons added, so a session or daemon that fits or predicts with it
#' loads horizons, which registers the twin. Only the fit changes: its
#' function is [isolated_fit()], given the engine's own fit function and
#' name among its defaults.
#'
#' @param model Character(1). The parsnip model, e.g. `"cubist_rules"`.
#' @param engine Character(1). The engine, e.g. `"Cubist"`.
#' @param mode Character(1). The mode horizons fits.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
register_isolated_engine <- function(model, engine, mode = "regression") {

  twin       <- isolated_engine_name(engine)
  registered <- parsnip::get_from_env(model)

  if (is.null(registered) || !any(registered$engine == engine & registered$mode == mode)) {
    return(invisible(NULL))
  }

  if (any(registered$engine == twin & registered$mode == mode)) return(invisible(NULL))

  ### The arguments table has no mode: an argument is the engine's in every
  ### mode
  of_engine <- function(table) {
    rows <- parsnip::get_from_env(paste0(model, "_", table))
    keep <- rows$engine == engine
    if ("mode" %in% names(rows)) keep <- keep & rows$mode == mode
    rows[keep, , drop = FALSE]
  }

  args     <- of_engine("args")
  fit      <- of_engine("fit")
  pred     <- of_engine("predict")
  pkgs     <- of_engine("pkgs")
  encoding <- parsnip::get_encoding(model)
  encoding <- encoding[encoding$engine == engine & encoding$mode == mode, , drop = FALSE]

  parsnip::set_model_engine(model, mode = mode, eng = twin)

  for (pkg in unique(c(unlist(pkgs$pkg), "horizons"))) {
    parsnip::set_dependency(model, eng = twin, pkg = pkg, mode = mode)
  }

  for (i in seq_len(nrow(args))) {
    parsnip::set_model_arg(model, eng = twin, parsnip = args$parsnip[i], original = args$original[i],
                           func = args$func[[i]], has_submodel = args$has_submodel[i])
  }

  value          <- fit$value[[1]]
  value$defaults <- c(value$defaults, list(.isolated_fit = value$func, .isolated_engine = engine))
  value$func     <- c(pkg = "horizons", fun = "isolated_fit")

  parsnip::set_fit(model, eng = twin, mode = mode, value = value)

  parsnip::set_encoding(model, eng = twin, mode = mode, options = list(
    predictor_indicators = encoding$predictor_indicators[1],
    compute_intercept    = encoding$compute_intercept[1],
    remove_intercept     = encoding$remove_intercept[1],
    allow_sparse_x       = encoding$allow_sparse_x[1]
  ))

  for (i in seq_len(nrow(pred))) {
    parsnip::set_pred(model, eng = twin, mode = mode, type = pred$type[i], value = pred$value[[i]])
  }

  invisible(NULL)

}


#' Fit an isolated engine's model in a process of its own
#'
#' @description
#' The fit function of every isolated twin ([register_isolated_engine()]).
#' parsnip calls it with what it would have given the engine's own fit
#' function, every argument by name, and the twin's defaults: that function,
#' and the engine's name. It calls the function with those arguments in a
#' process started for this one fit ([run_isolated()]), whose random stream
#' is seeded from one draw of the caller's, and returns the fitted model,
#' which predicts here as the engine's would. A warning or error the fit
#' raises is raised here with its class; a process that ends without its
#' result is `horizons_error_process_died`.
#'
#' @param ... The engine's fit arguments, by name: the data, the model's
#'   arguments and any engine arguments.
#' @param .isolated_fit Named character: the engine's own fit function, its
#'   `pkg` and `fun`, as parsnip registers it.
#' @param .isolated_engine Character(1). The engine's name, for a death's
#'   message.
#'
#' @return The engine's fitted model.
#' @keywords internal
#' @export
isolated_fit <- function(..., .isolated_fit, .isolated_engine) {

  args <- list(...)
  pkg  <- .isolated_fit["pkg"]

  if (length(args) == 0 || is.null(names(args)) || any(!nzchar(names(args)))) {
    abort_horizons("An isolated fit takes its arguments by name, as parsnip passes them.", kind = "internal")
  }

  fn <- if (is.na(pkg)) as.name(.isolated_fit[["fun"]]) else call("::", as.name(pkg), as.name(.isolated_fit[["fun"]]))

  ### The arguments by name, as symbols the process finds among the values
  ### it is sent, so the call holds none of the data
  fit_call <- as.call(c(list(fn), stats::setNames(lapply(names(args), as.name), names(args))))

  run_isolated(fit_call, args, what = paste0("a ", .isolated_engine, " fit"))

}


#' The progress tree's line on isolated fits
#'
#' @param models Character. The models the run fits.
#' @param isolate Logical(1). `control_horizons(isolate = )`.
#' @return Character(1), or `NULL` when the run fits no model whose fits are
#'   isolated.
#' @keywords internal
#' @noRd
isolation_tree_line <- function(models, isolate) {

  iso <- isolated_models(models)

  if (length(iso) == 0) return(NULL)

  if (isTRUE(isolate)) {
    paste0("Isolated fits: ", paste(iso, collapse = ", "),
           ", each fit in a process of its own (control_horizons(isolate = FALSE) turns it off)")
  } else {
    paste0("Isolated fits: off, as asked; ", paste(iso, collapse = ", "), " fitted in this session")
  }

}


#' Note on the log that fits isolation would have run here ran in the session
#'
#' @description
#' With `control_horizons(isolate = FALSE)` the models `MODEL_SPECS` marks
#' `isolate` are fitted in the process that runs them, as asked, which is a
#' note: their results can then depend on the fits made before them there.
#'
#' @param collector The verb's collector.
#' @param models Character. The models the run fits.
#' @param isolate Logical(1).
#' @return `collector`, invisibly.
#' @keywords internal
#' @noRd
log_isolation_off <- function(collector, models, isolate) {

  iso <- isolated_models(models)

  if (isTRUE(isolate) || length(iso) == 0) return(invisible(collector))

  log_note(collector, paste0(
    "{.val {iso}} {?was/were} fitted in the process that ran {?it/them}, as {.code control_horizons(isolate = FALSE)} asked, ",
    "rather than each fit in a process of its own, so {?its/their} results can depend on the fits made before them there."
  ), ids = iso)

}
