#' Package-level imports and NSE globals
#'
#' @description
#' Package-wide `@importFrom` declarations and the global-variable registration
#' that quiets R CMD check for non-standard-evaluation column names. Not a
#' user-facing object.
#'
#' @importFrom rlang :=
#' @importFrom stats coef dist median var
#' @importFrom utils head object.size
#'
#' @keywords internal
#' @name horizons-package-imports
NULL

## Quiet the R CMD check "no visible binding for global variable" NOTEs for the
## non-standard-evaluation names used inside dplyr/tidyr pipelines. These are
## pipeline references (.metric/.estimate = yardstick output columns), not real
## globals. The magrittr dot is gone along with magrittr itself — the package
## uses the base pipe throughout, per .claude/rules/r-analysis.md.
utils::globalVariables(c(".metric", ".estimate"))


#' Package startup configuration
#'
#' @description
#' Minimal package initialization. Thread control is the user's responsibility.
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {

  ## ---------------------------------------------------------------------------
  ## Ensure recipes selectors resolve
  ## ---------------------------------------------------------------------------

  ## all_outcomes() / all_predictors() quosures resolve against recipes, which
  ## is a declared Import. Loading it here keeps those selectors available.

  requireNamespace("recipes", quietly = TRUE)

  ## ---------------------------------------------------------------------------
  ## Ensure parsnip engine-extension packages are available
  ## ---------------------------------------------------------------------------

  ## These register parsnip engines the pipeline selects by string
  ## (`set_engine("glmnet")`, the `pls` model via plsmod, `cubist_rules()` via
  ## rules). They are never called through `::`, so they must be loaded here for
  ## the engines to resolve at fit time. Soft-loaded: a missing extension means
  ## that engine is unavailable, not a package load failure.
  ##
  ## Written out literally rather than looped. R CMD check's "unused Imports"
  ## test is static and only recognises a *literal* string argument, so the
  ## previous for-loop was invisible to it and the
  ## "Namespaces in Imports field not imported from" NOTE kept firing even
  ## though the packages are genuinely used.

  requireNamespace("glmnet", quietly = TRUE)
  requireNamespace("plsmod", quietly = TRUE)
  requireNamespace("rules",  quietly = TRUE)

  ## ---------------------------------------------------------------------------
  ## Trust the user - no automatic thread control
  ## ---------------------------------------------------------------------------

  # The user is responsible for setting their own parallel configuration
  # We don't enforce any thread limits or parallel settings

  # Optional: Check if user explicitly wants thread control help
  if (Sys.getenv("HORIZONS_THREAD_CONTROL", "FALSE") == "TRUE") {

    # Only if explicitly requested, set conservative defaults
    Sys.setenv(
      OMP_NUM_THREADS        = "1",
      OPENBLAS_NUM_THREADS   = "1",
      MKL_NUM_THREADS        = "1"
    )

    options(
      ranger.num.threads = 1,
      xgboost.nthread    = 1
    )

  }

  invisible()
}

.onAttach <- function(libname, pkgname) {

  ## Display version and thread control status ----

  packageStartupMessage(
    "horizons v", utils::packageVersion("horizons"), " loaded. ",
    "Please flag bugs on Github (www.github.com/S-Leuthold/horizons)"
  )

  ## Reported here rather than in .onLoad, where R CMD check flags
  ## packageStartupMessage() as a NOTE. The thread pinning itself stays in
  ## .onLoad, because it has to happen before any engine loads.

  if (Sys.getenv("HORIZONS_THREAD_CONTROL", "FALSE") == "TRUE") {

    packageStartupMessage(
      "horizons: thread control enabled via HORIZONS_THREAD_CONTROL; ",
      "unset it to leave threading to you."
    )

  }

  invisible()
}
