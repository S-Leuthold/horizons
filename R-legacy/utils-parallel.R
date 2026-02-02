#' Parallel Processing Utilities for Context-Aware Backend Selection
#'
#' @description
#' Provides intelligent parallel backend selection based on execution context,
#' with optimizations for HPC environments, memory management, and reproducibility.
#'
#' @keywords internal
#' @name utils-parallel

## ---------------------------------------------------------------------------
## Context Detection Functions
## ---------------------------------------------------------------------------

#' Detect Parallel Execution Context
#'
#' Determines the execution environment to select optimal parallel backend.
#' Checks for HPC schedulers, RStudio, operating system, and nested contexts.
#'
#' @param min_cores Minimum cores to consider HPC context (default: 8)
#' @param verbose Logical. Print detection details (default: FALSE)
#'
#' @return List with context information:
#' \describe{
#'   \item{context}{One of "hpc", "local", or "nested"}
#'   \item{is_unix}{Logical, TRUE if Unix-like system}
#'   \item{is_windows}{Logical, TRUE if Windows}
#'   \item{cores_available}{Number of detected cores}
#'   \item{in_hpc_eval}{Logical, TRUE if called from evaluate_models_hpc}
#'   \item{on_cluster}{Logical, TRUE if HPC scheduler detected}
#'   \item{in_rstudio}{Logical, TRUE if RStudio detected}
#'   \item{in_container}{Logical, TRUE if Docker/Singularity detected}
#'   \item{use_forking}{Logical, TRUE if forking should be used}
#'   \item{recommended_backend}{Either "multicore" or "multisession"}
#'   \item{memory_available_gb}{Available memory in GB}
#' }
#'
#' @importFrom future nbrOfWorkers availableCores
#' @importFrom parallel detectCores
#' @noRd

detect_parallel_context <- function(min_cores = 8, verbose = FALSE) {
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Detect System Type
  ## ---------------------------------------------------------------------------
  
  is_unix    <- .Platform$OS.type == "unix"
  is_windows <- .Platform$OS.type == "windows"
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Detect Execution Environment
  ## ---------------------------------------------------------------------------
  
  # Check if we're in HPC evaluation (called from evaluate_models_hpc)
  call_stack <- sys.calls()
  in_hpc_eval <- any(vapply(call_stack, function(x) {
    if (length(x) > 0) {
      fun_name <- as.character(x[[1]])
      "evaluate_models_hpc" %in% fun_name || "evaluate_single_model_hpc" %in% fun_name
    } else {
      FALSE
    }
  }, logical(1)))
  
  # Check for HPC schedulers
  on_slurm <- Sys.getenv("SLURM_JOB_ID") != ""
  on_pbs   <- Sys.getenv("PBS_JOBID") != ""
  on_sge   <- Sys.getenv("SGE_TASK_ID") != ""
  on_cluster <- on_slurm || on_pbs || on_sge
  
  # Check if in RStudio
  in_rstudio <- Sys.getenv("RSTUDIO") == "1" || 
                Sys.getenv("RSTUDIO_SESSION_PORT") != ""
  
  # Check if in container
  in_docker <- file.exists("/.dockerenv")
  in_singularity <- Sys.getenv("SINGULARITY_CONTAINER") != ""
  in_container <- in_docker || in_singularity
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Detect Available Resources
  ## ---------------------------------------------------------------------------
  
  cores_available <- parallel::detectCores(logical = TRUE)
  
  # Try to detect memory (platform-specific)
  memory_available_gb <- tryCatch({
    if (is_unix) {
      # Check cgroups first (containers/SLURM)
      if (file.exists("/sys/fs/cgroup/memory/memory.limit_in_bytes")) {
        mem_bytes <- as.numeric(readLines("/sys/fs/cgroup/memory/memory.limit_in_bytes", n = 1))
        mem_bytes / (1024^3)
      } else if (on_slurm && Sys.getenv("SLURM_MEM_PER_NODE") != "") {
        # SLURM memory allocation in MB
        as.numeric(Sys.getenv("SLURM_MEM_PER_NODE")) / 1024
      } else if (file.exists("/proc/meminfo")) {
        # Try to get system memory on Linux
        mem_info <- tryCatch(
          readLines("/proc/meminfo", n = 2),
          error = function(e) character(0)
        )
        mem_line <- grep("MemAvailable", mem_info, value = TRUE)
        if (length(mem_line) > 0) {
          as.numeric(gsub("[^0-9]", "", mem_line)) / (1024^2)
        } else {
          NA_real_
        }
      } else {
        NA_real_
      }
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_)
  
  ## ---------------------------------------------------------------------------
  ## Step 4: Check for Nested Parallelization
  ## ---------------------------------------------------------------------------
  
  current_workers <- future::nbrOfWorkers()
  in_nested <- current_workers > 1
  
  ## ---------------------------------------------------------------------------
  ## Step 5: Determine Recommended Backend
  ## ---------------------------------------------------------------------------
  
  # Multicore (forking) is preferred when:
  # - Unix-like system (required for forking)
  # - Either on HPC or has sufficient cores
  # - Not in RStudio (can crash RStudio)
  # - Not in nested context (avoid fork bombs)
  
  use_forking <- is_unix && 
                 !is_windows &&
                 !in_rstudio &&
                 !in_nested &&
                 (in_hpc_eval || on_cluster || cores_available >= min_cores)
  
  recommended_backend <- if (use_forking) "multicore" else "multisession"
  
  # Determine context label
  context <- if (in_nested) {
    "nested"
  } else if (in_hpc_eval || on_cluster) {
    "hpc"
  } else {
    "local"
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 6: Print Detection Results if Verbose
  ## ---------------------------------------------------------------------------
  
  if (verbose) {
    cli::cli_h3("Parallel Context Detection")
    cli::cli_text("System: {.val {Sys.info()['sysname']}} ({cores_available} cores)")
    cli::cli_text("Context: {.strong {context}}")
    if (on_cluster) {
      scheduler <- if (on_slurm) "SLURM" else if (on_pbs) "PBS" else "SGE"
      cli::cli_text("HPC Scheduler: {.val {scheduler}}")
    }
    if (in_container) {
      container <- if (in_docker) "Docker" else "Singularity"
      cli::cli_text("Container: {.val {container}}")
    }
    if (in_rstudio) cli::cli_text("RStudio: {.val Detected}")
    if (in_nested) cli::cli_text("Nested workers: {.val {current_workers}}")
    if (!is.na(memory_available_gb)) {
      cli::cli_text("Memory: {.val {round(memory_available_gb, 1)} GB}")
    }
    cli::cli_text("Recommended backend: {.strong {recommended_backend}}")
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 7: Return Context Information
  ## ---------------------------------------------------------------------------
  
  list(
    context             = context,
    is_unix            = is_unix,
    is_windows         = is_windows,
    cores_available    = cores_available,
    in_hpc_eval        = in_hpc_eval,
    on_cluster         = on_cluster,
    in_rstudio         = in_rstudio,
    in_container       = in_container,
    in_nested          = in_nested,
    use_forking        = use_forking,
    recommended_backend = recommended_backend,
    memory_available_gb = memory_available_gb
  )
}

## ---------------------------------------------------------------------------
## Backend Setup Functions
## ---------------------------------------------------------------------------

#' Setup Appropriate Parallel Backend
#'
#' Configures future backend based on execution context with optimizations
#' for work stealing, memory management, and reproducibility.
#'
#' @param n_workers Number of parallel workers
#' @param force_backend Override auto-detection ("multicore" or "multisession")
#' @param memory_limit_gb Memory limit per worker in GB (default: 2)
#' @param enable_work_stealing Enable dynamic scheduling for load balancing
#' @param verbose Print backend configuration details
#'
#' @return Previous future plan (for restoration)
#'
#' @importFrom future plan multicore multisession sequential
#' @importFrom cli cli_alert_success cli_alert_info cli_alert_warning
#' @noRd

setup_parallel_backend <- function(n_workers,
                                  force_backend = NULL,
                                  memory_limit_gb = 2,
                                  enable_work_stealing = TRUE,
                                  verbose = FALSE) {
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Store Current Plan for Restoration
  ## ---------------------------------------------------------------------------
  
  old_plan <- future::plan()
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Determine Backend
  ## ---------------------------------------------------------------------------
  
  if (!is.null(force_backend)) {
    backend <- match.arg(force_backend, c("multicore", "multisession", "sequential"))
    reason <- "forced by user"
  } else {
    context <- detect_parallel_context(verbose = FALSE)
    backend <- context$recommended_backend
    reason <- paste0(context$context, " context")
    
    # Override if not enough workers
    if (n_workers <= 1) {
      backend <- "sequential"
      reason <- "single worker"
    }
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Configure Memory Limits
  ## ---------------------------------------------------------------------------
  
  # Set global size limit for objects passed to workers
  old_globals_size <- getOption("future.globals.maxSize", default = 500 * 1024^2)
  options(future.globals.maxSize = memory_limit_gb * 1024^3)
  
  # Store old value for restoration
  attr(old_plan, "old_globals_size") <- old_globals_size
  
  ## ---------------------------------------------------------------------------
  ## Step 4: Setup Backend with Appropriate Configuration
  ## ---------------------------------------------------------------------------
  
  if (backend == "multicore") {
    
    ## Forking backend - efficient for Unix/Linux ------------------------
    
    # Configure scheduling for work stealing via options
    if (enable_work_stealing) {
      options(future.scheduling = 2L)  # Dynamic scheduling with work stealing
    } else {
      options(future.scheduling = 1L)  # Static scheduling
    }
    
    future::plan(
      future::multicore,
      workers = n_workers
    )
    
    if (verbose) {
      work_steal_msg <- if (enable_work_stealing) "with work stealing" else "static scheduling"
      cli::cli_alert_success(
        "Using {.strong multicore} backend ({reason}): {n_workers} workers {work_steal_msg}"
      )
    }
    
  } else if (backend == "multisession") {
    
    ## New process backend - safe for all platforms ----------------------
    
    # Set environment to bypass parallelly limits
    old_max_workers <- Sys.getenv("R_PARALLELLY_MAXWORKERS_LOCALHOST")
    Sys.setenv(R_PARALLELLY_MAXWORKERS_LOCALHOST = "999999")
    
    # Store for restoration
    attr(old_plan, "old_max_workers") <- old_max_workers
    
    # Configure multisession
    future::plan(
      future::multisession,
      workers = n_workers
    )
    
    if (verbose) {
      cli::cli_alert_info(
        "Using {.strong multisession} backend ({reason}): {n_workers} workers"
      )
    }
    
  } else {
    
    ## Sequential - no parallelization -----------------------------------
    
    future::plan(future::sequential)
    
    if (verbose) {
      cli::cli_alert_warning("Using {.strong sequential} backend ({reason})")
    }
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 5: Configure Additional Future Options
  ## ---------------------------------------------------------------------------
  
  options(
    future.scheduling = if (enable_work_stealing && backend == "multicore") 2L else 1L,
    future.chunk.size = NULL,  # Auto chunk sizing
    future.rng.onMisuse = "ignore"  # We'll handle RNG explicitly
  )
  
  return(old_plan)
}

## ---------------------------------------------------------------------------
## Thread Control Functions
## ---------------------------------------------------------------------------

#' Set Package-Specific Thread Controls
#'
#' Configures thread settings for packages like ranger, xgboost, data.table
#' based on the parallel context to prevent thread explosion.
#'
#' @param context_aware Logical. Use context detection for smart defaults
#' @param max_threads Maximum threads per package (default: 1 for safety)
#' @param verbose Print thread configuration
#'
#' @importFrom data.table setDTthreads
#' @noRd

set_thread_controls <- function(context_aware = TRUE,
                                max_threads = 1,
                                verbose = FALSE) {
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Determine Thread Limit Based on Context
  ## ---------------------------------------------------------------------------
  
  if (context_aware) {
    context <- detect_parallel_context(verbose = FALSE)
    
    # In multicore (forking) context, we can allow more threads
    # since memory is shared
    if (context$use_forking && context$context == "hpc") {
      thread_limit <- min(max_threads, 2)  # Allow 2 threads in HPC forking
    } else if (context$in_nested) {
      thread_limit <- 1  # Always 1 in nested context
    } else {
      thread_limit <- max_threads
    }
  } else {
    thread_limit <- max_threads
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Set Package-Specific Options
  ## ---------------------------------------------------------------------------
  
  # data.table threading
  data.table::setDTthreads(thread_limit)
  
  # ranger (random forest) threading
  options(
    ranger.num.threads = thread_limit,
    ranger.num.cores   = thread_limit
  )
  
  # xgboost threading
  options(
    xgboost.nthread = thread_limit
  )
  
  # Set mc.cores for base R parallel
  # This is context-dependent
  if (context_aware && context$use_forking) {
    # In forking context, we can use more cores
    options(mc.cores = context$cores_available)
  } else {
    # In multisession or nested, keep it limited
    options(mc.cores = thread_limit)
  }
  
  if (verbose) {
    cli::cli_alert_info("Thread controls set to {thread_limit} thread(s) per package")
  }
  
  invisible(thread_limit)
}

## ---------------------------------------------------------------------------
## RNG Management Functions
## ---------------------------------------------------------------------------

#' Setup Reproducible Parallel RNG
#'
#' Initializes L'Ecuyer-CMRG random number streams for parallel workers
#' to ensure reproducibility across parallel operations.
#'
#' @param seed Master seed for RNG
#' @param n_workers Number of parallel workers
#' @param verbose Print RNG configuration
#'
#' @return List of RNG seeds for workers
#' @noRd

setup_parallel_rng <- function(seed, n_workers, verbose = FALSE) {
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Set RNG Kind for Parallel Streams
  ## ---------------------------------------------------------------------------
  
  # Store current RNG kind
  old_kind <- RNGkind()[1]
  
  # Use L'Ecuyer-CMRG for parallel-safe streams
  RNGkind("L'Ecuyer-CMRG")
  set.seed(seed)
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Generate Independent Seeds for Workers
  ## ---------------------------------------------------------------------------
  
  # Create seed sequence
  seeds <- vector("list", n_workers)
  s <- .Random.seed
  
  for (i in seq_len(n_workers)) {
    seeds[[i]] <- s
    # Advance to next independent stream
    s <- parallel::nextRNGStream(s)
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Restore Original RNG Kind
  ## ---------------------------------------------------------------------------
  
  RNGkind(old_kind)
  
  if (verbose) {
    cli::cli_alert_info(
      "RNG configured: {n_workers} independent L'Ecuyer streams from seed {seed}"
    )
  }
  
  return(seeds)
}

## ---------------------------------------------------------------------------
## Memory Management Functions
## ---------------------------------------------------------------------------

#' Optimize Memory for Parallel Processing
#'
#' Configures memory settings and performs garbage collection
#' optimized for parallel processing of large spectral datasets.
#'
#' @param force_gc Logical. Force garbage collection (default: TRUE)
#' @param verbose Print memory status
#'
#' @noRd

optimize_parallel_memory <- function(force_gc = TRUE, verbose = FALSE) {
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Aggressive Garbage Collection
  ## ---------------------------------------------------------------------------
  
  if (force_gc) {
    gc(verbose = FALSE, full = TRUE, reset = TRUE)
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Get Memory Status
  ## ---------------------------------------------------------------------------
  
  if (verbose) {
    mem_used <- sum(gc()[, 2])
    cli::cli_alert_info("Memory in use: {round(mem_used, 1)} MB")
  }
  
  invisible(TRUE)
}

## ---------------------------------------------------------------------------
## Helper Functions
## ---------------------------------------------------------------------------

#' Get Display Name for Current Backend
#'
#' Returns a clean display name for the current future backend.
#'
#' @return Character string with backend name (e.g., "multicore", "multisession")
#' @noRd

get_backend_display <- function() {
  current_plan <- future::plan()
  if (inherits(current_plan, "FutureStrategy")) {
    backend_class <- class(current_plan)[1]
  } else if (is.list(current_plan) && length(current_plan) > 0) {
    backend_class <- class(current_plan[[1]])[1]
  } else {
    return("unknown")
  }
  
  # Clean up the name
  backend_display <- gsub("^Future", "", backend_class)
  backend_display <- tolower(backend_display)
  
  return(backend_display)
}

## ---------------------------------------------------------------------------
## Restoration Functions
## ---------------------------------------------------------------------------

#' Restore Parallel Settings
#'
#' Restores previous parallel backend and settings after parallel operations.
#'
#' @param old_plan Previous future plan object from setup_parallel_backend
#' @param verbose Print restoration details
#'
#' @importFrom future plan
#' @noRd

restore_parallel_settings <- function(old_plan, verbose = FALSE) {
  
  ## ---------------------------------------------------------------------------
  ## Step 1: Restore Future Plan
  ## ---------------------------------------------------------------------------
  
  future::plan(old_plan)
  
  ## ---------------------------------------------------------------------------
  ## Step 2: Restore Environment Variables
  ## ---------------------------------------------------------------------------
  
  # Restore parallelly max workers if it was changed
  if (!is.null(attr(old_plan, "old_max_workers"))) {
    if (attr(old_plan, "old_max_workers") == "") {
      Sys.unsetenv("R_PARALLELLY_MAXWORKERS_LOCALHOST")
    } else {
      Sys.setenv(R_PARALLELLY_MAXWORKERS_LOCALHOST = attr(old_plan, "old_max_workers"))
    }
  }
  
  ## ---------------------------------------------------------------------------
  ## Step 3: Restore Options
  ## ---------------------------------------------------------------------------
  
  # Restore globals size limit
  if (!is.null(attr(old_plan, "old_globals_size"))) {
    options(future.globals.maxSize = attr(old_plan, "old_globals_size"))
  }
  
  if (verbose) {
    cli::cli_alert_success("Parallel settings restored")
  }
  
  invisible(TRUE)
}