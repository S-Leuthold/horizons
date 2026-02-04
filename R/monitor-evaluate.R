#' Monitor a Running Parallel Evaluation
#'
#' @description
#' Displays progress for a parallel `evaluate()` run by reading checkpoint
#' files from the output directory. Run this from a separate R session while
#' `evaluate()` is running.
#'
#' @param output_dir Character. Path to the output directory passed to
#'   `evaluate()`.
#' @param watch Logical. If TRUE, continuously poll for updates. Default FALSE
#'   (single snapshot).
#' @param interval Numeric. Seconds between polls when `watch = TRUE`.
#'   Default 10.
#'
#' @return Invisibly returns a list with completion stats (n_complete,
#'   n_total, rate, eta, best_config, best_metric).
#'
#' @export
monitor_evaluate <- function(output_dir, watch = FALSE, interval = 10) {

  if (!dir.exists(output_dir)) {

    rlang::abort(paste0("Output directory not found: ", output_dir))

  }

  manifest_path <- file.path(output_dir, "eval_manifest.rds")

  if (!file.exists(manifest_path)) {

    rlang::abort(paste0(
      "No eval_manifest.rds found in '", output_dir, "'. ",
      "Is evaluate() running with workers > 1?"
    ))

  }

  manifest      <- readRDS(manifest_path)
  checkpoint_dir <- file.path(output_dir, "checkpoints")

  if (watch) {

    on.exit(cat("\nMonitor stopped.\n"))

    repeat {

      stats <- .monitor_snapshot(manifest, checkpoint_dir)
      .render_monitor(stats, manifest)

      if (stats$n_complete >= manifest$n_total) {

        cat("\nAll configs complete!\n")
        return(invisible(stats))

      }

      Sys.sleep(interval)

    }

  } else {

    stats <- .monitor_snapshot(manifest, checkpoint_dir)
    .render_monitor(stats, manifest)
    invisible(stats)

  }

}


## -------------------------------------------------------------------------
## Internal helpers
## -------------------------------------------------------------------------


#' Read checkpoint directory and compute stats
#' @noRd
.monitor_snapshot <- function(manifest, checkpoint_dir) {

  if (!dir.exists(checkpoint_dir)) {

    return(list(
      n_complete  = 0L,
      n_total     = manifest$n_total,
      pct         = 0,
      rate        = NA_real_,
      eta         = NA_character_,
      best_config = NA_character_,
      best_metric = NA_real_,
      recent      = character(0)
    ))

  }

  checkpoint_files <- list.files(checkpoint_dir, pattern = "\\.rds$",
                                  full.names = TRUE)

  n_complete <- length(checkpoint_files)
  elapsed    <- as.numeric(difftime(Sys.time(), manifest$start_time,
                                     units = "hours"))
  rate       <- if (elapsed > 0) n_complete / elapsed else NA_real_

  n_remaining <- manifest$n_total - n_complete
  eta <- if (!is.na(rate) && rate > 0) {
    hrs <- n_remaining / rate
    if (hrs < 1) paste0(round(hrs * 60), " min") else paste0(round(hrs, 1), " hr")
  } else {
    NA_character_
  }

  pct <- round(100 * n_complete / manifest$n_total, 1)

  ## Find best so far
  best_config <- NA_character_
  best_metric <- NA_real_
  metric_name <- manifest$metric
  higher_better <- metric_name %in% c("rpd", "rsq", "ccc")

  recent_files <- checkpoint_files[order(file.mtime(checkpoint_files),
                                          decreasing = TRUE)]

  for (f in recent_files) {

    row <- tryCatch(readRDS(f), error = function(e) NULL)

    if (!is.null(row) && row$status %in% c("success", "pruned") &&
        !is.na(row[[metric_name]])) {

      val <- row[[metric_name]]

      if (is.na(best_metric) ||
          (higher_better && val > best_metric) ||
          (!higher_better && val < best_metric)) {

        best_metric <- val
        best_config <- row$config_id

      }

    }

  }

  ## Recent completions (last 5)
  recent <- character(0)

  for (f in utils::head(recent_files, 5)) {

    row <- tryCatch(readRDS(f), error = function(e) NULL)

    if (!is.null(row)) {

      model_name <- MODEL_DISPLAY_NAMES[row$model] %||% row$model
      metric_val <- if (!is.na(row[[metric_name]])) {
        paste0(toupper(metric_name), " = ", round(row[[metric_name]], 3))
      } else {
        row$status
      }
      recent <- c(recent, paste0(row$config_id, " (", model_name, ") ",
                                  metric_val))

    }

  }

  list(
    n_complete  = n_complete,
    n_total     = manifest$n_total,
    pct         = pct,
    rate        = rate,
    eta         = eta,
    best_config = best_config,
    best_metric = best_metric,
    recent      = recent
  )

}


#' Render monitor output to console
#' @noRd
.render_monitor <- function(stats, manifest) {

  cat("\014")
  cat(paste0(paste(rep("\u2500", 50), collapse = ""), "\n"))
  cat(paste0("  evaluate() monitor \u2014 ", format(Sys.time(), "%H:%M:%S"), "\n"))
  cat(paste0(paste(rep("\u2500", 50), collapse = ""), "\n\n"))

  cat(paste0("  Progress:  ", stats$n_complete, " / ", stats$n_total,
             " (", stats$pct, "%)\n"))

  if (!is.na(stats$rate)) {

    cat(paste0("  Rate:      ", round(stats$rate, 1), " models/hr\n"))

  }

  if (!is.na(stats$eta)) {

    cat(paste0("  ETA:       ", stats$eta, "\n"))

  }

  if (!is.na(stats$best_config)) {

    cat(paste0("  Best:      ", stats$best_config, " \u2014 ",
               toupper(manifest$metric), " = ",
               round(stats$best_metric, 3), "\n"))

  }

  if (length(stats$recent) > 0) {

    cat(paste0("\n  Recent completions:\n"))

    for (r in stats$recent) {
      cat(paste0("    \u2022 ", r, "\n"))
    }

  }

  cat(paste0("\n", paste(rep("\u2500", 50), collapse = ""), "\n"))

}
