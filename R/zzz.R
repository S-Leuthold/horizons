#' Package-level imports and NSE globals
#'
#' @description
#' Package-wide `@importFrom` declarations and the global-variable registration
#' that quiets R CMD check for non-standard-evaluation column names. Not a
#' user-facing object.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @importFrom stats coef dist median var
#' @importFrom utils head object.size
#'
#' @keywords internal
#' @name horizons-package-imports
NULL

## Quiet the R CMD check "no visible binding for global variable" NOTEs for the
## non-standard-evaluation names used inside dplyr/tidyr/magrittr pipelines.
## These are pipeline references (. = the magrittr dot; .metric/.estimate =
## yardstick output columns), not real globals.
utils::globalVariables(c(".", ".metric", ".estimate"))


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
  ## the engines to resolve at fit time — and this keeps them honestly "used"
  ## Imports rather than R CMD check NOTEs. Soft-loaded: a missing extension
  ## means that engine is unavailable, not a package load failure.

  for (pkg in c("glmnet", "plsmod", "rules")) {

    requireNamespace(pkg, quietly = TRUE)

  }

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

    packageStartupMessage("horizons: Thread control enabled (set HORIZONS_THREAD_CONTROL=FALSE to disable)")
  }

  invisible()
}

.onAttach <- function(libname, pkgname) {

  ## Display version and thread control status ----

  packageStartupMessage(
    "horizons v", utils::packageVersion("horizons"), " loaded. ",
    "Please flag bugs on Github (www.github.com/S-Leuthold/horizons)"
  )


  invisible()
}
