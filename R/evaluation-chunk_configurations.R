#' Configuration Chunking and Checkpointing Utilities
#'
#' Helper functions for processing large model configuration grids in manageable chunks
#' with checkpoint/resume capabilities for HPC environments. Designed for use with
#' `run_model_evaluation()` when using `parallel_strategy = "models"`.
#'
#' @name chunk-configurations
#' @keywords internal

#' Split Configuration Grid into Chunks
#'
#' Divides a large configuration data frame into smaller chunks for batch processing.
#' Preserves row order and adds chunk metadata for tracking.
#'
#' @param config A data frame of model configurations to split.
#' @param chunk_size Integer. Maximum number of configurations per chunk (default = 50).
#'
#' @return A list of data frames, each containing up to `chunk_size` configurations.
#'   Each chunk has attributes for tracking: `chunk_index`, `total_chunks`.
#'
#' @examples
#' \dontrun{
#' chunks <- split_into_chunks(my_configs, chunk_size = 25)
#' length(chunks)  # Number of chunks created
#' nrow(chunks[[1]])  # Size of first chunk
#' }
#'
#' @importFrom dplyr slice
#' @export
split_into_chunks <- function(config, chunk_size = 50) {
  
  n_configs <- nrow(config)
  n_chunks <- ceiling(n_configs / chunk_size)
  
  chunks <- vector("list", n_chunks)
  
  for (i in seq_len(n_chunks)) {
    start_idx <- ((i - 1) * chunk_size) + 1
    end_idx <- min(i * chunk_size, n_configs)
    
    chunk_data <- config[start_idx:end_idx, , drop = FALSE]
    
    # Add metadata attributes
    attr(chunk_data, "chunk_index") <- i
    attr(chunk_data, "total_chunks") <- n_chunks
    attr(chunk_data, "start_config") <- start_idx
    attr(chunk_data, "end_config") <- end_idx
    
    chunks[[i]] <- chunk_data
  }
  
  return(chunks)
}

#' Save Chunk Results to Checkpoint File
#'
#' Saves the results from processing a configuration chunk to a checkpoint file
#' for resuming interrupted runs.
#'
#' @param chunk_results List of model results from processing a chunk.
#' @param chunk_index Integer. Index of the chunk being saved.
#' @param checkpoint_dir Character. Directory path for checkpoint files.
#' @param chunk_config Data frame. Configuration data for this chunk (optional metadata).
#'
#' @return Invisibly returns the checkpoint file path.
#'
#' @importFrom qs qsave
#' @importFrom fs path
#' @importFrom glue glue
#' @export
save_checkpoint <- function(chunk_results, chunk_index, checkpoint_dir, chunk_config = NULL) {
  
  checkpoint_file <- fs::path(checkpoint_dir, glue::glue("chunk_{sprintf('%03d', chunk_index)}.qs"))
  
  checkpoint_data <- list(
    chunk_index = chunk_index,
    timestamp = Sys.time(),
    results = chunk_results,
    n_configs = length(chunk_results),
    config_subset = chunk_config
  )
  
  qs::qsave(checkpoint_data, checkpoint_file)
  
  invisible(checkpoint_file)
}

#' Load Existing Checkpoint Files
#'
#' Scans checkpoint directory for existing results and determines which chunks
#' have been completed. Used for resuming interrupted runs.
#'
#' @param checkpoint_dir Character. Directory path containing checkpoint files.
#' @param total_chunks Integer. Expected total number of chunks.
#'
#' @return A list with:
#'   \itemize{
#'     \item \strong{completed_results}: List of results from completed chunks.
#'     \item \strong{completed_indices}: Integer vector of completed chunk indices.
#'     \item \strong{next_chunk}: Integer index of next chunk to process.
#'     \item \strong{n_completed}: Number of chunks already completed.
#'   }
#'
#' @importFrom fs dir_exists dir_ls
#' @importFrom qs qread
#' @importFrom cli cli_alert_info cli_alert_warning
#' @export
load_existing_checkpoints <- function(checkpoint_dir, total_chunks) {
  
  if (!fs::dir_exists(checkpoint_dir)) {
    return(list(
      completed_results = list(),
      completed_indices = integer(0),
      next_chunk = 1L,
      n_completed = 0L
    ))
  }
  
  checkpoint_files <- fs::dir_ls(checkpoint_dir, glob = "*.qs")
  
  if (length(checkpoint_files) == 0) {
    return(list(
      completed_results = list(),
      completed_indices = integer(0),
      next_chunk = 1L,
      n_completed = 0L
    ))
  }
  
  # Load and validate checkpoints
  completed_results <- list()
  completed_indices <- integer(0)
  
  for (file in checkpoint_files) {
    tryCatch({
      checkpoint_data <- qs::qread(file)
      chunk_idx <- checkpoint_data$chunk_index
      
      completed_results[[chunk_idx]] <- checkpoint_data$results
      completed_indices <- c(completed_indices, chunk_idx)
      
    }, error = function(e) {
      cli::cli_alert_warning("Corrupted checkpoint file: {basename(file)}")
    })
  }
  
  n_completed <- length(completed_indices)
  next_chunk <- if (n_completed == 0) 1L else max(completed_indices) + 1L
  
  if (n_completed > 0) {
    cli::cli_alert_info("Found {n_completed} completed chunks. Resuming from chunk {next_chunk}")
  }
  
  return(list(
    completed_results = completed_results,
    completed_indices = sort(completed_indices),
    next_chunk = next_chunk,
    n_completed = n_completed
  ))
}

#' Calculate ETA Based on Completed Chunks
#'
#' Estimates time to completion based on the duration of completed chunks.
#'
#' @param completed_chunks Integer. Number of chunks completed.
#' @param total_chunks Integer. Total number of chunks to process.
#' @param chunk_durations Numeric vector. Duration of each completed chunk in minutes.
#'
#' @return A list with ETA information:
#'   \itemize{
#'     \item \strong{eta_mins}: Estimated minutes remaining.
#'     \item \strong{eta_time}: Estimated completion time (POSIXct).
#'     \item \strong{eta_formatted}: Human-readable completion time.
#'     \item \strong{mean_duration}: Average chunk duration in minutes.
#'   }
#'
#' @export
calculate_chunk_eta <- function(completed_chunks, total_chunks, chunk_durations) {
  
  if (completed_chunks == 0 || length(chunk_durations) == 0) {
    return(list(
      eta_mins = NA_real_,
      eta_time = NA,
      eta_formatted = "Unknown",
      mean_duration = NA_real_
    ))
  }
  
  mean_duration <- mean(chunk_durations, na.rm = TRUE)
  remaining_chunks <- total_chunks - completed_chunks
  eta_mins <- remaining_chunks * mean_duration
  eta_time <- Sys.time() + (eta_mins * 60)
  eta_formatted <- format(eta_time, "%Y-%m-%d at %I:%M %p")
  
  return(list(
    eta_mins = eta_mins,
    eta_time = eta_time,
    eta_formatted = eta_formatted,
    mean_duration = mean_duration
  ))
}

#' Enhanced Memory Cleanup Between Chunks
#'
#' Performs aggressive memory cleanup between chunk processing to prevent
#' memory accumulation in long-running batch jobs.
#'
#' @return Current memory usage in GB.
#'
#' @importFrom pryr mem_used
#' @export
chunk_memory_cleanup <- function() {
  
  # Force garbage collection
  invisible(gc(verbose = FALSE, full = TRUE))
  
  # Clear any temporary variables that might linger
  rm(list = ls(pattern = "^temp_|^tmp_|^chunk_temp", envir = parent.frame()), 
     envir = parent.frame())
  
  # Additional cleanup for future workers
  if (exists(".future", envir = .GlobalEnv)) {
    try({
      rm(.future, envir = .GlobalEnv)
    }, silent = TRUE)
  }
  
  # Calculate current memory usage
  mem_bytes <- pryr::mem_used()
  mem_gb <- round(as.numeric(mem_bytes) / 1073741824, 2)
  
  return(mem_gb)
}

#' Setup Checkpoint Directory Structure
#'
#' Creates the checkpoint directory and any necessary subdirectories.
#' Ensures proper permissions and validates write access.
#'
#' @param checkpoint_dir Character. Path to checkpoint directory.
#'
#' @return Invisibly returns TRUE if successful.
#'
#' @importFrom fs dir_create dir_exists
#' @importFrom cli cli_alert_success cli_alert_danger
#' @export
setup_checkpoint_directory <- function(checkpoint_dir) {
  
  if (is.null(checkpoint_dir)) {
    return(invisible(FALSE))
  }
  
  tryCatch({
    if (!fs::dir_exists(checkpoint_dir)) {
      fs::dir_create(checkpoint_dir, recurse = TRUE)
      cli::cli_alert_success("Created checkpoint directory: {checkpoint_dir}")
    }
    
    # Test write permissions
    test_file <- fs::path(checkpoint_dir, ".write_test")
    writeLines("test", test_file)
    file.remove(test_file)
    
    return(invisible(TRUE))
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to setup checkpoint directory: {checkpoint_dir}")
    cli::cli_alert_danger("Error: {conditionMessage(e)}")
    return(invisible(FALSE))
  })
}