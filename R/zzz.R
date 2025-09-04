#' Package Startup Configuration
#'
#' @description
#' Sets thread control and package options on package load to prevent
#' uncontrolled threading in HPC environments.
#'
#' @keywords internal

.onLoad <- function(libname, pkgname) {

  ## -----------------------------------------------------------------------------
  ## Thread Control on Package Load
  ## -----------------------------------------------------------------------------

  # Check if user wants to skip thread limiting (for parallel processing)
  skip_thread_control <- Sys.getenv("HORIZONS_SKIP_THREAD_CONTROL", "FALSE")
  
  if (toupper(skip_thread_control) != "TRUE") {
    ## Set comprehensive thread control environment variables ----

    Sys.setenv(
      ## Standard threading controls ----
      OMP_NUM_THREADS        = "1",
      OPENBLAS_NUM_THREADS   = "1",
      MKL_NUM_THREADS        = "1",
      VECLIB_MAXIMUM_THREADS = "1",
      NUMEXPR_NUM_THREADS    = "1",

      ## Additional BLAS variants ----
      GOTO_NUM_THREADS       = "1",
      BLIS_NUM_THREADS       = "1"
    )
  }

  ## Set package-specific options ----
  
  # Set mc.cores to available cores unless thread control is active
  if (toupper(skip_thread_control) == "TRUE") {
    options(mc.cores = parallel::detectCores())
  } else {
    options(mc.cores = 1L)
  }

  options(
    ## Ranger threading ----
    ranger.num.threads = if (toupper(skip_thread_control) == "TRUE") parallel::detectCores() else 1,
    ranger.num.cores   = if (toupper(skip_thread_control) == "TRUE") parallel::detectCores() else 1,

    ## XGBoost threading ----
    xgboost.nthread    = if (toupper(skip_thread_control) == "TRUE") parallel::detectCores() else 1

  )

  ## Use RhpcBLASctl if available ----

  if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    suppressMessages({
      RhpcBLASctl::blas_set_num_threads(1)
      RhpcBLASctl::omp_set_num_threads(1)
    })
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
