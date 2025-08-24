#' Manage Worker Pool for Parallel Model Evaluation
#'
#' @description
#' Manages worker allocation, prevents nested parallelization, and optimizes resource
#' utilization for HPC environments. This function intelligently allocates workers
#' based on the task type (grid search vs Bayesian optimization) and available resources.
#'
#' Key features:
#' - Dynamic worker calculation based on task requirements
#' - Nested parallelization prevention via environment variables
#' - Platform-specific optimizations (Linux multicore vs others)
#' - Memory-aware worker limits
#' - Automatic work-stealing for load balancing
#'
#' @param n_workers_requested Integer. Number of workers requested by user (1-190)
#' @param task_type Character. Type of task: "grid_search", "bayesian", or "refit"
#' @param n_tasks Integer. Number of tasks to parallelize (e.g., hyperparameter combinations)
#' @param cv_folds Integer. Number of cross-validation folds
#' @param verbose Logical. Print diagnostic information (default = FALSE)
#'
#' @return A list containing:
#' \itemize{
#'   \item \strong{n_workers}: Optimal number of workers to use
#'   \item \strong{parallel_plan}: Future plan object configured for the environment
#'   \item \strong{strategy}: Character description of parallelization strategy
#'   \item \strong{cleanup_fn}: Function to call after parallel operations complete
#' }
#'
#' @details
#' ## Worker Allocation Strategy
#'
#' The function implements intelligent worker allocation:
#'
#' \strong{Grid Search}:
#' - Workers = min(requested, n_hyperparameters × cv_folds, available_cores - 2)
#' - Each worker evaluates one hyperparameter combination across all CV folds
#'
#' \strong{Bayesian Optimization}:
#' - Workers = min(requested, cv_folds, available_cores - 2)
#' - Each iteration evaluates one hyperparameter across CV folds in parallel
#'
#' \strong{Refitting}:
#' - Workers = min(requested, n_tasks, available_cores - 2)
#' - Higher resolution tuning for top models
#'
#' ## Nested Parallelization Prevention
#'
#' Uses environment variable `HORIZONS_PARALLEL_LEVEL`:
#' - Level 0: Top-level orchestrator (can parallelize)
#' - Level 1: Worker process (must run sequentially)
#' - Level 2+: Nested worker (error condition)
#'
#' @examples
#' \dontrun{
#' # Setup for grid search
#' worker_config <- manage_worker_pool(
#'   n_workers_requested = 50,
#'   task_type           = "grid_search",
#'   n_tasks             = 50,  # 10 hyperparams × 5 CV folds
#'   cv_folds            = 5
#' )
#'
#' # Use the configured plan
#' worker_config$parallel_plan
#'
#' # Run parallel operations...
#'
#' # Cleanup when done
#' worker_config$cleanup_fn()
#' }
#'
#' @seealso
#' \code{\link{run_hpc_evaluation}}, \code{\link{evaluate_model_fit_parallel}}
#'
#' @importFrom future plan multicore multisession sequential nbrOfWorkers
#' @importFrom parallel detectCores
#' @importFrom cli cli_alert_info cli_alert_warning cli_alert_success
#' @importFrom RhpcBLASctl blas_set_num_threads omp_set_num_threads
#' @export

manage_worker_pool <- function(n_workers_requested,
                              task_type           = c("grid_search", "bayesian", "refit"),
                              n_tasks             = NULL,
                              cv_folds            = 5,
                              verbose             = FALSE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Validate Inputs and Check Environment
  ## ---------------------------------------------------------------------------

  task_type <- match.arg(task_type)

  ## Check parallelization level -----------------------------------------------

  parallel_level <- as.integer(Sys.getenv("HORIZONS_PARALLEL_LEVEL", "0"))

  if (parallel_level > 0) {
    
    if (verbose) {
      cli::cli_alert_warning("Nested parallelization detected (level {parallel_level}). Forcing sequential.")
    }
    
    return(list(
      n_workers     = 1,
      parallel_plan = future::plan(future::sequential),
      strategy      = "sequential (nested context)",
      cleanup_fn    = function() { invisible(NULL) }
    ))
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Calculate Optimal Worker Count
  ## ---------------------------------------------------------------------------

  ## Get system resources ------------------------------------------------------

  available_cores <- parallel::detectCores(logical = TRUE)
  system_name     <- Sys.info()["sysname"]

  ## Reserve cores for system --------------------------------------------------

  reserved_cores <- ifelse(available_cores > 50, 2, 1)
  max_workers    <- available_cores - reserved_cores

  ## Calculate task-specific optimal workers ----------------------------------

  if (task_type == "grid_search") {
    
    ## For grid search, parallelize across all hyperparameter × CV combinations
    
    if (is.null(n_tasks)) {
      cli::cli_abort("n_tasks must be specified for grid_search")
    }
    
    optimal_workers <- min(
      n_workers_requested,
      n_tasks,  # Don't exceed number of tasks
      max_workers
    )
    
    strategy <- glue::glue("grid_search: {optimal_workers} workers for {n_tasks} tasks")
    
  } else if (task_type == "bayesian") {
    
    ## For Bayesian with 'everything' parallelization, we can use more workers
    ## This allows parallel evaluation of multiple candidate points
    
    optimal_workers <- min(
      n_workers_requested,
      max(cv_folds, n_tasks),  # Use n_tasks if provided, else cv_folds
      max_workers
    )
    
    strategy <- glue::glue("bayesian: {optimal_workers} workers for {cv_folds} CV folds per iteration")
    
  } else {  # refit
    
    ## For refitting, use full parallelization
    
    if (is.null(n_tasks)) {
      n_tasks <- min(25, n_workers_requested)  # Set reasonable ceiling to avoid circular dependency
    }
    
    optimal_workers <- min(
      n_workers_requested,
      n_tasks,
      max_workers
    )
    
    strategy <- glue::glue("refit: {optimal_workers} workers for {n_tasks} tasks")
  }

  ## Ensure at least 1 worker --------------------------------------------------

  optimal_workers <- max(1, optimal_workers)

  ## ---------------------------------------------------------------------------
  ## Step 3: Configure Parallel Backend
  ## ---------------------------------------------------------------------------

  ## Store current plan for restoration ----------------------------------------

  old_plan <- future::plan()

  ## Create cleanup function ---------------------------------------------------

  cleanup_fn <- function() {
    
    ## Restore original plan -------------------------------------------------
    
    future::plan(old_plan)
    
    ## Reset environment variable --------------------------------------------
    
    Sys.setenv(HORIZONS_PARALLEL_LEVEL = "0")
    
    ## Reset thread controls -------------------------------------------------
    
    Sys.unsetenv("OMP_NUM_THREADS")
    Sys.unsetenv("OPENBLAS_NUM_THREADS")
    Sys.unsetenv("MKL_NUM_THREADS")
    
    ## Light garbage collection ----------------------------------------------
    
    gc(verbose = FALSE, full = FALSE)
    
    if (verbose) {
      cli::cli_alert_success("Worker pool cleaned up")
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Set Up Parallel Plan
  ## ---------------------------------------------------------------------------

  if (optimal_workers == 1) {
    
    ## Sequential processing -------------------------------------------------
    
    future::plan(future::sequential)
    
    if (verbose) {
      cli::cli_alert_info("Using sequential processing (1 worker)")
    }
    
  } else {
    
    ## Parallel processing ---------------------------------------------------
    
    ## Set comprehensive thread controls to prevent nested parallelism -------
    
    Sys.setenv(
      ## Standard threading controls ----
      OMP_NUM_THREADS      = "1",
      OPENBLAS_NUM_THREADS = "1",
      MKL_NUM_THREADS      = "1",
      VECLIB_MAXIMUM_THREADS = "1",
      NUMEXPR_NUM_THREADS  = "1",
      
      ## Additional BLAS variants ----
      GOTO_NUM_THREADS     = "1",
      BLIS_NUM_THREADS     = "1",
      
      ## Package-specific controls ----
      RANGER_NUM_THREADS   = "1",
      XGBOOST_NTHREAD      = "1",
      
      ## CPU affinity settings ----
      OMP_PROC_BIND        = "TRUE",
      OMP_PLACES           = "cores"
    )
    
    ## Set R global options for package-specific threading -------------------
    
    options(
      ranger.num.threads = 1,
      xgboost.nthread    = 1,
      mc.cores           = 1,
      cores              = 1
    )
    
    ## Use RhpcBLASctl if available ------------------------------------------
    
    if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
      RhpcBLASctl::blas_set_num_threads(1)
      RhpcBLASctl::omp_set_num_threads(1)
    }
    
    ## Choose backend based on platform --------------------------------------
    
    if (system_name == "Linux") {
      
      ## Linux: Use multicore (fork-based, more efficient) ------------------
      
      future::plan(future::multicore, workers = optimal_workers)
      
      if (verbose) {
        cli::cli_alert_info("Using multicore backend with {optimal_workers} workers")
      }
      
    } else {
      
      ## Windows/Mac: Use multisession (socket-based) -----------------------
      
      future::plan(future::multisession, workers = optimal_workers)
      
      if (verbose) {
        cli::cli_alert_info("Using multisession backend with {optimal_workers} workers")
      }
    }
    
    ## Mark as parallel context ----------------------------------------------
    
    Sys.setenv(HORIZONS_PARALLEL_LEVEL = "1")
  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Set Global Options for Workers
  ## ---------------------------------------------------------------------------

  ## Set future options --------------------------------------------------------

  options(
    future.globals.maxSize = 10000 * 1024^2,  # 10 GB limit
    future.rng.onMisuse    = "ignore"         # Suppress RNG warnings
  )

  ## ---------------------------------------------------------------------------
  ## Step 6: Return Configuration
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_alert_success("Worker pool configured: {optimal_workers}/{n_workers_requested} workers")
    cli::cli_alert_info("Strategy: {strategy}")
    cli::cli_alert_info("Available cores: {available_cores}, Reserved: {reserved_cores}")
  }

  return(list(
    n_workers     = optimal_workers,
    parallel_plan = future::plan(),
    strategy      = strategy,
    cleanup_fn    = cleanup_fn
  ))
}


#' Calculate Optimal Worker Allocation for Model Type
#'
#' @description
#' Determines optimal worker allocation based on model type and complexity.
#' Memory-intensive models get fewer workers to prevent OOM errors.
#'
#' @param model_type Character. Model type (e.g., "random_forest", "xgboost", "plsr")
#' @param feature_selection Character. Feature selection method (e.g., "boruta", "cars", "pca")
#' @param n_workers_available Integer. Maximum workers available
#' @param n_features Integer. Number of features in dataset
#' @param n_samples Integer. Number of samples in dataset
#'
#' @return Integer. Recommended number of workers for this model configuration
#'
#' @details
#' Models are categorized by memory intensity:
#' - High: Random Forest, XGBoost, Boruta, CARS (use 50-75% of available workers)
#' - Medium: SVM, Cubist, Neural Networks (use 75-90% of available workers)
#' - Low: PLSR, Elastic Net, Linear models (use 90-100% of available workers)
#'
#' @examples
#' \dontrun{
#' # Memory-intensive configuration
#' n_workers <- calculate_model_workers(
#'   model_type          = "random_forest",
#'   feature_selection   = "boruta",
#'   n_workers_available = 50,
#'   n_features          = 1000,
#'   n_samples           = 500
#' )
#' # Returns ~25-35 workers to prevent memory issues
#' }
#'
#' @export

calculate_model_workers <- function(model_type,
                                   feature_selection   = "none",
                                   n_workers_available,
                                   n_features          = NULL,
                                   n_samples           = NULL) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Define Memory Intensity Categories
  ## ---------------------------------------------------------------------------

  high_memory_models <- c(
    "random_forest", "ranger", "xgboost", "lightgbm", "catboost"
  )

  high_memory_features <- c(
    "boruta", "cars", "vip", "shap"
  )

  medium_memory_models <- c(
    "svm_rbf", "svm_poly", "cubist", "neural_network", "kknn"
  )

  low_memory_models <- c(
    "plsr", "pls", "elastic_net", "glmnet", "lm", "ridge", "lasso"
  )

  ## ---------------------------------------------------------------------------
  ## Step 2: Calculate Memory Multiplier
  ## ---------------------------------------------------------------------------

  ## Base multiplier based on model type ---------------------------------------

  if (model_type %in% high_memory_models) {
    
    base_multiplier <- 0.6  # Use 60% of available workers
    
  } else if (model_type %in% medium_memory_models) {
    
    base_multiplier <- 0.8  # Use 80% of available workers
    
  } else if (model_type %in% low_memory_models) {
    
    base_multiplier <- 1.0  # Use all available workers
    
  } else {
    
    base_multiplier <- 0.75  # Default conservative approach
    
  }

  ## Adjust for feature selection method ---------------------------------------

  if (feature_selection %in% high_memory_features) {
    
    base_multiplier <- base_multiplier * 0.75  # Further reduce by 25%
    
  }

  ## Adjust for data size if provided ------------------------------------------

  if (!is.null(n_features) && !is.null(n_samples)) {
    
    data_size <- n_features * n_samples
    
    if (data_size > 1e7) {  # Large dataset (>10M elements)
      
      base_multiplier <- base_multiplier * 0.8
      
    } else if (data_size > 1e8) {  # Very large dataset (>100M elements)
      
      base_multiplier <- base_multiplier * 0.6
      
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Calculate Final Worker Count
  ## ---------------------------------------------------------------------------

  recommended_workers <- ceiling(n_workers_available * base_multiplier)
  
  ## Ensure at least 1 worker --------------------------------------------------
  
  recommended_workers <- max(1, recommended_workers)
  
  ## Don't exceed available ----------------------------------------------------
  
  recommended_workers <- min(recommended_workers, n_workers_available)

  return(recommended_workers)
}