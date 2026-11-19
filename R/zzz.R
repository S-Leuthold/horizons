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
## uses the base pipe throughout.
utils::globalVariables(c(".metric", ".estimate"))


#' The build of horizons this session loaded
#'
#' @description
#' `built` is the `Built` field of the installed DESCRIPTION this session
#' loaded horizons from, read once as it loads ([read_built()]): the R
#' version, platform and time of the install, so two installs of one version
#' differ. `NA` for a source tree loaded with `devtools::load_all()`, which
#' has none.
#'
#' @keywords internal
#' @noRd
loaded_build <- new.env(parent = emptyenv())
loaded_build$built <- NA_character_

#' The `Built` field of an installed package's DESCRIPTION
#'
#' @param path The package's directory.
#' @return Character(1), or `NA` when there is none or it cannot be read.
#' @keywords internal
#' @noRd
read_built <- function(path) {

  tryCatch(unname(read.dcf(file.path(path, "DESCRIPTION"), "Built")[1, 1]),
           error = function(e) NA_character_)

}


#' Package startup configuration
#'
#' @description
#' Minimal package initialization. Thread control is the user's responsibility.
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {

  ## ---------------------------------------------------------------------------
  ## Ensure parsnip engine-extension packages are available
  ## ---------------------------------------------------------------------------

  ## These register parsnip engines the pipeline selects by string
  ## (`set_engine("glmnet")`, `cubist_rules()` via rules). They are never
  ## called through `::`, so they must be loaded here for the engines to
  ## resolve at fit time. Soft-loaded: a missing extension means
  ## that engine is unavailable, not a package load failure.
  ##
  ## Written out literally rather than looped. R CMD check's "unused Imports"
  ## test is static and only recognises a *literal* string argument, so the
  ## previous for-loop was invisible to it and the
  ## "Namespaces in Imports field not imported from" NOTE kept firing even
  ## though the packages are genuinely used.

  requireNamespace("glmnet", quietly = TRUE)
  requireNamespace("rules",  quietly = TRUE)

  ## The plsr model's engine, which horizons provides (R/pls-engine.R)
  register_pls_engine()

  ## The isolated twins of the engines MODEL_SPECS marks, copied from the
  ## registrations the packages above made (R/isolated-engines.R)
  register_isolated_engines()

  ## The build this session loads, so a horizons installed over it later can
  ## be told from it (draw_in_session())
  loaded_build$built <- read_built(getNamespaceInfo(pkgname, "path"))

  ## ---------------------------------------------------------------------------
  ## Trust the user - no automatic thread control
  ## ---------------------------------------------------------------------------

  # The user is responsible for setting their own parallel configuration
  # We don't enforce any thread limits or parallel settings

  # Optional: Check if user explicitly wants thread control help.
  # The environment variables are effective here because .onLoad runs
  # before any engine loads OpenBLAS. During a parallel run the mechanism
  # that matters is pin_parent_threads() (R/utils-parallel.R), which
  # evaluate() and fit() call before dispatch and which works at runtime
  # via RhpcBLASctl.
  if (Sys.getenv("HORIZONS_THREAD_CONTROL", "FALSE") == "TRUE") {

    # Only if explicitly requested, set conservative defaults
    Sys.setenv(
      OMP_NUM_THREADS        = "1",
      OPENBLAS_NUM_THREADS   = "1",
      MKL_NUM_THREADS        = "1"
    )

    # The thread options the model table declares for its engines
    # (MODEL_SPECS, R/constants.R)
    options(engine_thread_options())

  }

  invisible()
}

.onAttach <- function(libname, pkgname) {

  ## Display version and thread control status ----

  packageStartupMessage(startup_message(utils::packageVersion("horizons")))

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

## ---------------------------------------------------------------------------
## startup_message() — which build is attached, and where the release lives
## ---------------------------------------------------------------------------

#' The message printed when horizons is attached
#'
#' Development builds carry a fourth version component of 9000 or more
#' (`0.10.0.9000`), the R convention for an unreleased version. A development
#' build says so, says it can be part way through a release's changes, and
#' names the command that installs the latest release, which is what analysis
#' should run on; `install_github("S-Leuthold/horizons")` installs the
#' development branch.
#'
#' @param version A `package_version`, normally
#'   `utils::packageVersion("horizons")`.
#' @return A single string.
#' @keywords internal
#' @noRd
startup_message <- function(version) {

  version <- package_version(version)
  parts   <- unlist(version)
  is_dev  <- length(parts) >= 4 && parts[4] >= 9000

  bugs <- "Please flag bugs at https://github.com/S-Leuthold/horizons/issues"

  if (!is_dev) {
    return(paste0("horizons ", version, ". ", bugs))
  }

  paste0(
    "horizons ", version, " (development version). This build can be part way ",
    "through the next release's changes, so for analysis install the latest ",
    "release: remotes::install_github(\"S-Leuthold/horizons@main\"). ", bugs
  )

}
