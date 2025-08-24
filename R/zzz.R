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
  
  ## Set package-specific options ----
  
  options(
    ## Ranger threading ----
    ranger.num.threads = 1,
    ranger.num.cores   = 1,
    
    ## XGBoost threading ----
    xgboost.nthread    = 1,
    
    ## General R parallelization ----
    mc.cores           = 1,
    cores              = 1
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
  
  ## Check thread control in interactive sessions ----
  
  if (interactive()) {
    omp_threads <- Sys.getenv("OMP_NUM_THREADS")
    blas_threads <- Sys.getenv("OPENBLAS_NUM_THREADS")
    
    if (omp_threads == "1" && blas_threads == "1") {
      packageStartupMessage("Thread control: ACTIVE (single-threaded mode)")
    } else {
      packageStartupMessage(
        "Thread control: Check settings for HPC use\n",
        "  OMP_NUM_THREADS = ", omp_threads, "\n",
        "  OPENBLAS_NUM_THREADS = ", blas_threads
      )
    }
  }
  
  invisible()
}