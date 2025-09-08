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
  
  # Detect execution context for smart defaults
  # Source utils-parallel.R functions if available
  context <- tryCatch({
    if (exists("detect_parallel_context", mode = "function")) {
      detect_parallel_context(verbose = FALSE)
    } else {
      NULL
    }
  }, error = function(e) NULL)
  
  # Determine if we're in HPC environment
  in_hpc <- !is.null(context) && (context$on_cluster || context$in_hpc_eval)
  
  if (toupper(skip_thread_control) != "TRUE") {
    ## Set comprehensive thread control environment variables ----
    # Always control BLAS/OpenMP to prevent thread explosion

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

  ## Set package-specific options with context awareness ----
  
  # For mc.cores, be smart about context
  # In HPC with forking, we can allow more cores
  # In local/multisession, keep it limited
  if (toupper(skip_thread_control) == "TRUE") {
    options(mc.cores = parallel::detectCores())
  } else if (in_hpc && !is.null(context) && context$use_forking) {
    # In HPC forking context, allow parallel within workers
    options(mc.cores = max(2, parallel::detectCores() / 4))
  } else {
    # Default safe setting
    options(mc.cores = 1L)
  }

  # Package-specific threading - always control these
  thread_limit <- if (toupper(skip_thread_control) == "TRUE") {
    parallel::detectCores()
  } else if (in_hpc) {
    2  # Allow limited threading in HPC
  } else {
    1  # Safe default
  }
  
  options(
    ## Ranger threading ----
    ranger.num.threads = thread_limit,
    ranger.num.cores   = thread_limit,

    ## XGBoost threading ----
    xgboost.nthread    = thread_limit

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
