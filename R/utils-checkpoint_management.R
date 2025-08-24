#' Checkpoint Management for HPC Model Evaluation
#'
#' @description
#' Efficient checkpoint system using batched files to avoid creating thousands
#' of individual files while maintaining fast access for monitoring.
#'
#' Uses a two-tier system:
#' - Batched result files (e.g., 100 models per file)
#' - Index file for fast lookups
#'
#' @keywords checkpoint batch
#' @importFrom fs path file_exists dir_create
#' @importFrom qs qsave qread
#' @name checkpoint_management
NULL


#' Save Model Result to Batched Checkpoint
#'
#' @description
#' Saves a model result to a batched checkpoint file, reducing filesystem overhead
#' from thousands of individual files to dozens of batch files.
#'
#' @param result Model evaluation result object
#' @param model_index Integer. Index of the model (1 to n_models)
#' @param checkpoint_dir Character. Directory for checkpoint files
#' @param batch_size Integer. Number of models per batch file (default = 100)
#'
#' @return Invisible NULL
#'
#' @export

save_checkpoint_batch <- function(result,
                                 model_index,
                                 checkpoint_dir,
                                 batch_size = 100) {

  ## Calculate batch number ----------------------------------------------------
  
  batch_num  <- ceiling(model_index / batch_size)
  batch_file <- fs::path(checkpoint_dir, sprintf("batch_%04d.qs", batch_num))
  
  ## Calculate position within batch -------------------------------------------
  
  batch_position <- ((model_index - 1) %% batch_size) + 1
  
  ## Read existing batch or create new -----------------------------------------
  
  if (fs::file_exists(batch_file)) {
    batch_data <- tryCatch(
      qs::qread(batch_file),
      error = function(e) list()
    )
  } else {
    batch_data <- list()
  }
  
  ## Add result to batch -------------------------------------------------------
  
  batch_data[[as.character(model_index)]] <- result
  
  ## Save batch atomically -----------------------------------------------------
  
  temp_file <- paste0(batch_file, ".tmp")
  qs::qsave(batch_data, temp_file)
  file.rename(temp_file, batch_file)
  
  ## Update index file ---------------------------------------------------------
  
  update_checkpoint_index(
    checkpoint_dir = checkpoint_dir,
    model_index    = model_index,
    batch_num      = batch_num,
    status         = result$status_summary$status[1]
  )
  
  invisible(NULL)
}


#' Update Checkpoint Index
#'
#' @description
#' Maintains an index file that tracks which models are in which batch files
#' for efficient lookups by the monitor.
#'
#' @param checkpoint_dir Character. Directory for checkpoint files
#' @param model_index Integer. Model index
#' @param batch_num Integer. Batch number containing this model
#' @param status Character. Model status (success/error/pruned)
#'
#' @return Invisible NULL
#'
#' @keywords internal

update_checkpoint_index <- function(checkpoint_dir,
                                   model_index,
                                   batch_num,
                                   status) {

  index_file <- fs::path(checkpoint_dir, "checkpoint_index.qs")
  
  ## Read existing index or create new ----------------------------------------
  
  if (fs::file_exists(index_file)) {
    index <- tryCatch(
      qs::qread(index_file),
      error = function(e) list(
        completed = integer(),
        batch_map = list(),
        status_counts = list(success = 0, error = 0, pruned = 0)
      )
    )
  } else {
    index <- list(
      completed     = integer(),
      batch_map     = list(),
      status_counts = list(success = 0, error = 0, pruned = 0)
    )
  }
  
  ## Update index --------------------------------------------------------------
  
  index$completed <- unique(c(index$completed, model_index))
  index$batch_map[[as.character(model_index)]] <- batch_num
  
  ## Update status counts ------------------------------------------------------
  
  if (!is.null(status) && status %in% names(index$status_counts)) {
    index$status_counts[[status]] <- index$status_counts[[status]] + 1
  }
  
  ## Save index atomically -----------------------------------------------------
  
  temp_file <- paste0(index_file, ".tmp")
  qs::qsave(index, temp_file)
  file.rename(temp_file, index_file)
  
  invisible(NULL)
}


#' Read Model Results from Batched Checkpoints
#'
#' @description
#' Efficiently reads model results from batched checkpoint files using the index.
#' Optimized for the monitor to read all results quickly.
#'
#' @param checkpoint_dir Character. Directory containing checkpoint files
#' @param model_indices Integer vector. Specific models to read (NULL for all)
#'
#' @return List of model results
#'
#' @export

read_checkpoint_results <- function(checkpoint_dir,
                                   model_indices = NULL) {

  index_file <- fs::path(checkpoint_dir, "checkpoint_index.qs")
  
  ## Read index ----------------------------------------------------------------
  
  if (!fs::file_exists(index_file)) {
    return(list())
  }
  
  index <- tryCatch(
    qs::qread(index_file),
    error = function(e) list(completed = integer(), batch_map = list())
  )
  
  ## Determine which models to read --------------------------------------------
  
  if (is.null(model_indices)) {
    model_indices <- index$completed
  } else {
    model_indices <- intersect(model_indices, index$completed)
  }
  
  if (length(model_indices) == 0) {
    return(list())
  }
  
  ## Group models by batch for efficient reading -------------------------------
  
  models_by_batch <- list()
  
  for (idx in model_indices) {
    batch_num <- index$batch_map[[as.character(idx)]]
    if (!is.null(batch_num)) {
      if (is.null(models_by_batch[[as.character(batch_num)]])) {
        models_by_batch[[as.character(batch_num)]] <- integer()
      }
      models_by_batch[[as.character(batch_num)]] <- c(
        models_by_batch[[as.character(batch_num)]],
        idx
      )
    }
  }
  
  ## Read results from batches -------------------------------------------------
  
  all_results <- list()
  
  for (batch_str in names(models_by_batch)) {
    batch_file <- fs::path(checkpoint_dir, sprintf("batch_%04d.qs", as.integer(batch_str)))
    
    if (fs::file_exists(batch_file)) {
      batch_data <- tryCatch(
        qs::qread(batch_file),
        error = function(e) list()
      )
      
      ## Extract requested models from this batch -----------------------------
      
      for (idx in models_by_batch[[batch_str]]) {
        if (!is.null(batch_data[[as.character(idx)]])) {
          all_results[[as.character(idx)]] <- batch_data[[as.character(idx)]]
        }
      }
    }
  }
  
  return(all_results)
}


#' Get Checkpoint Summary Statistics
#'
#' @description
#' Quick function to get summary statistics without reading all results.
#' Uses the index file for fast lookups.
#'
#' @param checkpoint_dir Character. Directory containing checkpoint files
#'
#' @return List with counts and statistics
#'
#' @export

get_checkpoint_summary <- function(checkpoint_dir) {

  index_file <- fs::path(checkpoint_dir, "checkpoint_index.qs")
  
  if (!fs::file_exists(index_file)) {
    return(list(
      n_completed   = 0,
      n_success     = 0,
      n_error       = 0,
      n_pruned      = 0,
      completed_ids = integer()
    ))
  }
  
  index <- tryCatch(
    qs::qread(index_file),
    error = function(e) list(
      completed = integer(),
      status_counts = list(success = 0, error = 0, pruned = 0)
    )
  )
  
  return(list(
    n_completed   = length(index$completed),
    n_success     = index$status_counts$success %||% 0,
    n_error       = index$status_counts$error %||% 0,
    n_pruned      = index$status_counts$pruned %||% 0,
    completed_ids = sort(index$completed)
  ))
}


#' Clean Up Old Checkpoint Files
#'
#' @description
#' Removes old individual checkpoint files if migrating from the old system
#' to the new batched system.
#'
#' @param checkpoint_dir Character. Directory containing checkpoint files
#' @param dry_run Logical. If TRUE, only reports what would be deleted
#'
#' @return Number of files removed
#'
#' @export

cleanup_old_checkpoints <- function(checkpoint_dir,
                                   dry_run = TRUE) {

  ## Find old individual model files ------------------------------------------
  
  old_files <- list.files(
    checkpoint_dir,
    pattern = "^model_\\d+_result\\.qs$",
    full.names = TRUE
  )
  
  n_files <- length(old_files)
  
  if (n_files == 0) {
    cli::cli_alert_success("No old checkpoint files to clean up")
    return(0)
  }
  
  if (dry_run) {
    cli::cli_alert_info("Would remove {n_files} old checkpoint files")
    cli::cli_alert_info("Run with dry_run = FALSE to actually delete")
  } else {
    cli::cli_alert_warning("Removing {n_files} old checkpoint files...")
    file.remove(old_files)
    cli::cli_alert_success("Cleaned up {n_files} old checkpoint files")
  }
  
  return(n_files)
}


#' Consolidate Existing Checkpoints into Batches
#'
#' @description
#' Converts existing individual checkpoint files into the new batched format.
#' Useful for migrating ongoing runs to the new system.
#'
#' @param checkpoint_dir Character. Directory containing checkpoint files
#' @param batch_size Integer. Number of models per batch (default = 100)
#' @param remove_old Logical. Remove old files after consolidation
#'
#' @return Number of models consolidated
#'
#' @export

consolidate_checkpoints <- function(checkpoint_dir,
                                   batch_size = 100,
                                   remove_old = FALSE) {

  ## Find existing individual files -------------------------------------------
  
  old_files <- list.files(
    checkpoint_dir,
    pattern = "^model_(\\d+)_result\\.qs$",
    full.names = TRUE
  )
  
  if (length(old_files) == 0) {
    cli::cli_alert_info("No individual checkpoint files to consolidate")
    return(0)
  }
  
  cli::cli_alert_info("Found {length(old_files)} individual checkpoint files to consolidate")
  
  ## Extract model indices -----------------------------------------------------
  
  model_indices <- as.integer(
    gsub(".*model_(\\d+)_result\\.qs$", "\\1", basename(old_files))
  )
  
  ## Group by batch ------------------------------------------------------------
  
  batches <- split(
    data.frame(
      file = old_files,
      index = model_indices,
      stringsAsFactors = FALSE
    ),
    ceiling(model_indices / batch_size)
  )
  
  ## Process each batch --------------------------------------------------------
  
  n_consolidated <- 0
  
  for (batch_num in names(batches)) {
    batch_data  <- list()
    batch_files <- batches[[batch_num]]
    
    cli::cli_alert_info("Processing batch {batch_num} ({nrow(batch_files)} models)")
    
    for (i in seq_len(nrow(batch_files))) {
      result <- tryCatch(
        qs::qread(batch_files$file[i]),
        error = function(e) NULL
      )
      
      if (!is.null(result)) {
        batch_data[[as.character(batch_files$index[i])]] <- result
        n_consolidated <- n_consolidated + 1
      }
    }
    
    ## Save batch --------------------------------------------------------------
    
    if (length(batch_data) > 0) {
      batch_file <- fs::path(checkpoint_dir, sprintf("batch_%04d.qs", as.integer(batch_num)))
      qs::qsave(batch_data, batch_file)
      cli::cli_alert_success("Saved batch {batch_num} with {length(batch_data)} models")
    }
  }
  
  ## Create index --------------------------------------------------------------
  
  cli::cli_alert_info("Creating checkpoint index...")
  
  index <- list(
    completed     = sort(model_indices),
    batch_map     = list(),
    status_counts = list(success = 0, error = 0, pruned = 0)
  )
  
  for (idx in model_indices) {
    index$batch_map[[as.character(idx)]] <- ceiling(idx / batch_size)
  }
  
  index_file <- fs::path(checkpoint_dir, "checkpoint_index.qs")
  qs::qsave(index, index_file)
  
  ## Clean up old files if requested -------------------------------------------
  
  if (remove_old) {
    cli::cli_alert_warning("Removing old individual checkpoint files...")
    file.remove(old_files)
    cli::cli_alert_success("Removed {length(old_files)} old files")
  }
  
  cli::cli_alert_success("Consolidated {n_consolidated} models into {length(batches)} batch files")
  
  return(n_consolidated)
}