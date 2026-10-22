#' Define a Parsnip Model Specification
#'
#' @description
#' Maps a short model name (from `VALID_MODELS`) to a tunable parsnip model
#' specification. Each spec has `tune()` placeholders for hyperparameters
#' and the correct engine set. Used by `evaluate_single_config()` to build
#' the workflow for each config.
#'
#' @param model Character. One of the short names in `VALID_MODELS`:
#'   "rf", "cubist", "xgboost", "plsr", "elastic_net", "svm_rbf",
#'   "mars", "lightgbm", "mlp".
#'
#' @return A `parsnip` model specification with `mode = "regression"`.
#' @keywords internal
define_model_spec <- function(model) {

  switch(model,

    "rf" = parsnip::rand_forest(
      trees = 500,
      mtry  = tune::tune(),
      min_n = tune::tune()
    ) |>
      parsnip::set_engine("ranger", importance = "impurity") |>
      parsnip::set_mode("regression"),

    "cubist" = parsnip::cubist_rules(
      committees = tune::tune(),
      neighbors  = tune::tune(),
      max_rules  = tune::tune()
    ) |>
      parsnip::set_engine("Cubist") |>
      parsnip::set_mode("regression"),

    "xgboost" = parsnip::boost_tree(
      trees          = 500,
      tree_depth     = tune::tune(),
      learn_rate     = tune::tune(),
      mtry           = tune::tune(),
      min_n          = tune::tune(),
      loss_reduction = tune::tune(),
      sample_size    = tune::tune()
    ) |>
      parsnip::set_engine("xgboost") |>
      parsnip::set_mode("regression"),

    "lightgbm" = parsnip::boost_tree(
      trees      = 500,
      tree_depth = tune::tune(),
      learn_rate = tune::tune(),
      mtry       = tune::tune(),
      min_n      = tune::tune()
    ) |>
      parsnip::set_engine("lightgbm") |>
      parsnip::set_mode("regression"),

    "elastic_net" = parsnip::linear_reg(
      penalty = tune::tune(),
      mixture = tune::tune()
    ) |>
      parsnip::set_engine("glmnet") |>
      parsnip::set_mode("regression"),

    "svm_rbf" = parsnip::svm_rbf(
      cost      = tune::tune(),
      rbf_sigma = tune::tune()
    ) |>
      parsnip::set_engine("kernlab") |>
      parsnip::set_mode("regression"),

    "mars" = parsnip::mars(
      prod_degree = tune::tune(),
      num_terms   = tune::tune()
    ) |>
      parsnip::set_engine("earth") |>
      parsnip::set_mode("regression"),

    "plsr" = parsnip::pls(
      num_comp = tune::tune()
    ) |>
      parsnip::set_engine("pls") |>
      parsnip::set_mode("regression"),

    "mlp" = parsnip::mlp(
      hidden_units = tune::tune(),
      penalty      = tune::tune(),
      epochs       = tune::tune()
    ) |>
      parsnip::set_engine("nnet", MaxNWts = 10000) |>
      parsnip::set_mode("regression")

    ### No default: the door validators refuse a model outside VALID_MODELS,
    ### and a NULL spec fails loudly in add_model().

  )

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
#' `MODEL_PACKAGES` and `SELECTION_PACKAGES` (`R/constants.R`).
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

  for (m in unique(models)) add(MODEL_PACKAGES[[m]], m)

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
