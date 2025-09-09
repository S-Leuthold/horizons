#' Package Startup Configuration
#'
#' @description
#' Minimal package initialization. Thread control is the user's responsibility.
#'
#' @keywords internal

.onLoad <- function(libname, pkgname) {

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
    
    message("horizons: Thread control enabled (set HORIZONS_THREAD_CONTROL=FALSE to disable)")
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
