#' Define a Parsnip Model Specification
#'
#' @description
#' Builds a model's tunable parsnip specification from its row of
#' `MODEL_SPECS`: parsnip's model function `fn`, called with the row's
#' `fixed` arguments and each of its `tuned` arguments as `tune::tune()`; the
#' engine `model_engine()` names, with the row's `engine_args` and its
#' thread argument (`threads$argument`) at `ENGINE_THREADS`; and the
#' regression mode. Each argument is captured as parsnip captures one written
#' in the call (`500`, `tune::tune()`, `"impurity"`). Used by
#' `evaluate_single_config()` and `fit_single_config()` to build the workflow
#' for each config.
#'
#' @param model Character. One of the short names in `VALID_MODELS`:
#'   "rf", "cubist", "xgboost", "plsr", "elastic_net", "svm_rbf",
#'   "mars", "lightgbm", "mlp".
#' @param isolate Logical. Whether a model `MODEL_SPECS` marks `isolate`
#'   takes its engine's isolated twin, which fits each model in a process of
#'   its own (`model_engine()`); `control_horizons(isolate = )`.
#'
#' @return A `parsnip` model specification with `mode = "regression"`.
#' @keywords internal
define_model_spec <- function(model, isolate = TRUE) {

  row <- MODEL_SPECS[[model]]

  ### The verbs' doors refuse a model outside VALID_MODELS first
  if (is.null(row)) {
    abort_horizons("{.fn define_model_spec} was given {.val {model}}, which {.code MODEL_SPECS} does not hold.",
                   kind = "internal")
  }

  tuned <- rep(list(quote(tune::tune())), length(row$tuned))
  names(tuned) <- row$tuned

  ## The engine's thread argument at the package's count, which an engine
  ## left to its own default may set to every core (bonsai's lightgbm does)
  threads <- if (!is.null(row$threads$argument)) stats::setNames(list(ENGINE_THREADS), row$threads$argument)

  ## The model's call is put together from the row and evaluated here, so
  ## parsnip captures every argument as the expression the row holds
  spec <- eval(rlang::call2(row$fn, !!!row$fixed, !!!tuned, .ns = "parsnip"))
  spec <- parsnip::set_engine(spec, model_engine(model, isolate), !!!row$engine_args, !!!threads)

  parsnip::set_mode(spec, "regression")

}

## ---------------------------------------------------------------------------
## The model table's readers
## ---------------------------------------------------------------------------

#' The `MODEL_SPECS` row of an engine
#'
#' @description
#' The one reader of an engine's traits (whether it takes a seed, its
#' threads), keyed by the engine's name as a fitted spec carries it. An
#' isolated twin finds its engine's row ([isolated_engine_name()]), so
#' isolating an engine keeps every trait it has. The xgb meta-learner's
#' `"xgboost"` and `"glmnet"` find the xgboost and elastic_net rows.
#'
#' @param engine Character(1). The engine, as parsnip names it.
#' @return The row (a list), or `NULL` for an engine no row names.
#' @keywords internal
#' @noRd
engine_spec <- function(engine) {

  for (row in MODEL_SPECS) {

    if (identical(engine, row$engine) || identical(engine, isolated_engine_name(row$engine))) return(row)

  }

  NULL

}

#' The engines' thread options, at the package's thread count
#'
#' @description
#' Each `option` the rows of `MODEL_SPECS` declare under `threads`, as a
#' named list for [options()]. The one reader of them, for every place that
#' pins threads: [pin_parent_threads()], the configs-axis worker,
#' `.onLoad()` and [isolated_fit()].
#'
#' @return Named list: option name -> `ENGINE_THREADS`.
#' @keywords internal
#' @noRd
engine_thread_options <- function() {

  option <- unique(unlist(lapply(MODEL_SPECS, function(row) row$threads$option), use.names = FALSE))

  stats::setNames(rep(list(ENGINE_THREADS), length(option)), option)

}

#' A model's name in the progress trees and reports
#'
#' @param model Character(1). A short model name.
#' @return Character(1): its `label` in `MODEL_SPECS`, or `model` itself for
#'   a name the table does not hold.
#' @keywords internal
#' @noRd
model_label <- function(model) {

  MODEL_SPECS[[model]]$label %||% model

}

## ---------------------------------------------------------------------------
## The packages a set of configurations needs, checked at the door
## ---------------------------------------------------------------------------
## Several engines and one selection step live in Suggests packages. Every
## verb that fits or predicts checks them before any work starts (the razor,
## rule 1), so a missing one is refused once, naming it, rather than failing
## every configuration that needs it. A package missing later, mid-run, can
## then only be missing where a unit ran, on a parallel worker, which
## run_unit() recognises (missing_package_signal()).

#' The packages a set of configurations needs, and what needs each
#'
#' @description
#' Pure: no `requireNamespace()` calls and no side effects, so "which
#' packages, and why" is testable without touching the search path. Reads
#' each model's `packages` in `MODEL_SPECS` and `SELECTION_PACKAGES`
#' (`R/constants.R`).
#'
#' @param models Character. The `model` of each configuration (may repeat).
#' @param feature_selection Character. The `feature_selection` of each
#'   configuration, when the verb trains their recipes; empty when it only
#'   bakes trained ones (`predict()`), whose selection steps call no other
#'   package at bake time.
#' @param needs_ranger_for_uq Logical(1). Add `ranger`, for prediction
#'   intervals: [fit_uq()]'s quantile forest is a bare `ranger::ranger`
#'   object whatever the configuration's own model.
#' @return Named list: package name -> character vector of what needs it
#'   (model names, a feature selection, or prediction intervals).
#' @keywords internal
#' @noRd
needed_packages <- function(models,
                            feature_selection   = character(0),
                            needs_ranger_for_uq = FALSE) {

  needed <- list()

  add <- function(pkgs, reason) {
    for (pkg in pkgs) needed[[pkg]] <<- unique(c(needed[[pkg]], reason))
  }

  for (m in unique(models)) add(MODEL_SPECS[[m]]$packages, m)

  for (fs in unique(feature_selection)) {
    add(SELECTION_PACKAGES[[fs]], paste(fs, "feature selection"))
  }

  if (isTRUE(needs_ranger_for_uq)) add("ranger", "prediction intervals")

  needed

}

#' Refuse a call whose packages are not all installed
#'
#' @description
#' Checks every needed package and refuses once, naming each missing one
#' and what needs it, rather than on the first miss, which would hide the
#' rest behind a fix-one-rerun-find-the-next cycle. Loading a package is the
#' check, so the ones present are loaded on the way, which `predict()`
#' relies on to dispatch on a stored workflow.
#'
#' @param needed Named list: package name -> character vector of what needs
#'   it, as [needed_packages()] returns it.
#' @param verb Character(1). The verb that refuses, for the message.
#' @param configure_remedy Logical(1). Add the remedy of leaving out, in
#'   `configure()`, what needs the packages: for the verbs that fit what
#'   `configure()` set up.
#' @param call The frame the refusal reports: the verb's.
#' @return Invisibly `NULL`; aborts with class
#'   `horizons_error_missing_package` (kind `dependency`).
#' @keywords internal
#' @noRd
check_packages <- function(needed, verb, configure_remedy = FALSE,
                           call = rlang::caller_env()) {

  missing <- names(needed)[
    !vapply(names(needed), requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing) == 0) return(invisible(NULL))

  n_missing <- length(missing)

  ## Built with paste0(), not glue interpolation: the names and reasons are
  ## the package's own strings (never an upstream error message), so there is
  ## no brace-injection risk; the {.pkg ...} and {.code ...} spans are cli
  ## markup, rendered when the abort parses the finished vector below.
  bullets <- vapply(missing, function(pkg) {

    reasons <- paste(needed[[pkg]], collapse = ", ")
    ## Every package checked here is on CRAN
    hint    <- sprintf('install.packages("%s")', pkg)

    paste0("{.pkg ", pkg, "}, needed for ", reasons, ": {.code ", hint, "}")

  }, character(1))

  names(bullets) <- rep("x", length(bullets))

  abort_horizons(c(
    "{.fn {verb}} needs {n_missing} package{?s} that {?is/are} not installed.",
    bullets,
    if (configure_remedy) c("i" = "{cli::qty(n_missing)}Install {?it/them}, or leave out the models or the feature selection that need {?it/them} in {.fn configure}.")
  ), kind = "dependency", class = "horizons_error_missing_package", call = call)

}

#' Refuse configurations whose packages are not installed, before they run
#'
#' @description
#' The door check of `evaluate()` and `fit()`: the engines of the
#' configurations' models and the packages of the selection steps their
#' recipes train.
#'
#' @param configs Rows of `config$configs`: the configurations the verb runs.
#' @param verb Character(1). `"evaluate"` or `"fit"`.
#' @param call The frame the refusal reports: the verb's.
#' @return Invisibly `NULL`; aborts as [check_packages()] does.
#' @keywords internal
#' @noRd
check_config_packages <- function(configs, verb, call = rlang::caller_env()) {

  check_packages(needed_packages(configs$model, configs$feature_selection),
                 verb, configure_remedy = TRUE, call = call)

}
