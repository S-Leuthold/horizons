#' Monitor a Running Parallel Evaluation
#'
#' @description
#' Displays progress for a parallel `evaluate()` run by reading checkpoint
#' files from the output directory. Run this from a separate R session while
#' `evaluate()` is running.
#'
#' The monitor reads the checkpoints through the same helpers and gates as
#' `evaluate()`: the per-config files under `checkpoints/`, plus the rows of
#' a legacy `eval_checkpoint.rds` for configs with no per-config file. A row
#' is neither counted nor ranked unless `evaluate()` would resume it: it must
#' match the training-data fingerprint and the tuning settings recorded in
#' `eval_manifest.rds` (which `evaluate()` writes at the start of each run),
#' be scored under the current scoring schema, and belong to a config in the
#' manifest's grid. A manifest written before a fingerprint or the settings
#' were recorded cannot be checked against them, so rows are not refused on
#' that count. Refused rows and unreadable files are shown, not hidden.
#'
#' @param output_dir Character. Path to the output directory passed to
#'   `evaluate()`.
#' @param watch Logical. If TRUE, continuously poll for updates. Default FALSE
#'   (single snapshot).
#' @param interval Numeric. Seconds between polls when `watch = TRUE`.
#'   Default 10.
#'
#' @return Invisibly returns a list with completion stats (n_complete,
#'   n_total, rate, eta, best_config, best_metric), `ignored` (the number of
#'   rows refused, by reason: `other_data`, `other_settings`,
#'   `earlier_schema`, `not_in_grid`) and `unreadable` (checkpoint files that
#'   could not be read, relative to `output_dir`).
#'
#' @export
monitor_evaluate <- function(output_dir, watch = FALSE, interval = 10) {

  if (!dir.exists(output_dir)) {

    rlang::abort(paste0("Output directory not found: ", output_dir))

  }

  manifest <- .read_monitor_manifest(output_dir)

  if (watch) {

    on.exit(cat("\nMonitor stopped.\n"))

    repeat {

      ## Re-read on every poll: evaluate() rewrites the manifest at the start
      ## of each run, and a re-run with other settings or data would
      ## otherwise be gated against the first run's, showing 0 complete
      ## forever.
      manifest <- .read_monitor_manifest(output_dir)
      stats    <- .monitor_snapshot(manifest, output_dir)
      .render_monitor(stats, manifest)

      if (stats$n_complete >= manifest$n_total) {

        cat("\nAll configs complete!\n")
        return(invisible(stats))

      }

      Sys.sleep(interval)

    }

  } else {

    stats <- .monitor_snapshot(manifest, output_dir)
    .render_monitor(stats, manifest)
    invisible(stats)

  }

}


## -------------------------------------------------------------------------
## Internal helpers
## -------------------------------------------------------------------------


#' Read the run manifest evaluate() writes
#' @noRd
.read_monitor_manifest <- function(output_dir) {

  manifest_path <- file.path(output_dir, "eval_manifest.rds")

  if (!file.exists(manifest_path)) {

    rlang::abort(paste0(
      "No eval_manifest.rds found in '", output_dir, "'. ",
      "evaluate() writes it when called with output_dir; has the run started?"
    ))

  }

  manifest <- readRDS(manifest_path)

  ## Schema 1 (pre-2026-09-15) manifests carry workers/outer/inner from the
  ## auto-split design; schema 2 carries the axis and the user's plan; schema
  ## 3 (2026-09-21) adds the training-data fingerprint; schema 4 (#42) the
  ## data fields and tuning settings. All are read: the monitor needs only
  ## n_total, metric and start_time to work, so a run started before M2 can
  ## still be watched.
  manifest$schema_version <- manifest$schema_version %||% 1L

  manifest

}


#' Read checkpoint directory and compute stats
#' @noRd
.monitor_snapshot <- function(manifest, output_dir) {

  ## -------------------------------------------------------------------------
  ## Read and gate the store as evaluate() does
  ## -------------------------------------------------------------------------
  ## The expected fingerprint and settings come from the manifest, which
  ## evaluate() writes after its own gate has passed. What an older manifest
  ## does not record is NA or NULL, which the gate treats as uncheckable.

  data_fp <- list(
    data_hash   = manifest$data_hash %||% NA_character_,
    data_n_rows = manifest$data_n_rows %||% NA_integer_,
    data_fields = manifest$data_fields
  )

  store <- read_checkpoint_store(output_dir)
  gated <- gate_checkpoint_rows(store$rows, data_fp, manifest$settings,
                                manifest$config_ids)

  verdicts <- vapply(gated$refused, `[[`, character(1), "verdict")

  ignored <- c(
    other_data     = sum(verdicts == "data_mismatch"),
    other_settings = sum(verdicts == "settings_mismatch"),
    earlier_schema = gated$n_foreign,
    not_in_grid    = gated$n_stale
  )

  n_complete <- length(gated$kept)
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

  ## Find best so far, by the SAME rule evaluate() will use: successes only
  ## (pruned rows only if there is no success at all), ranked on cv_<metric>
  ## through rank_configs_by_cv() with its config_id tie-break, falling back
  ## to the test-set column for checkpoint rows written before cv_* existed.
  best_config   <- NA_character_
  best_metric   <- NA_real_
  metric_name   <- manifest$metric
  higher_better <- metric_name %in% HIGHER_BETTER_METRICS

  rows <- unname(lapply(gated$kept, `[[`, "row"))

  if (length(rows) > 0) {

    all_rows   <- dplyr::bind_rows(rows)
    candidates <- all_rows[all_rows$status == "success", , drop = FALSE]

    if (nrow(candidates) == 0) {

      candidates <- all_rows[all_rows$status == "pruned", , drop = FALSE]

    }

    cv_col <- paste0("cv_", metric_name)

    if (nrow(candidates) > 0 && cv_col %in% names(candidates) &&
        any(!is.na(candidates[[cv_col]]))) {

      ranked      <- suppressWarnings(rank_configs_by_cv(candidates, metric_name))
      best_config <- ranked$config_id[1]
      best_metric <- ranked[[cv_col]][1]

    } else if (nrow(candidates) > 0 && metric_name %in% names(candidates) &&
               any(!is.na(candidates[[metric_name]]))) {

      ## Legacy rows: same ordering rule on the test-set column
      vals        <- candidates[[metric_name]]
      keep        <- !is.na(vals)
      candidates  <- candidates[keep, , drop = FALSE]
      vals        <- vals[keep]
      key         <- if (higher_better) -vals else vals
      ord         <- order(key, candidates$config_id)
      best_config <- candidates$config_id[ord[1]]
      best_metric <- vals[ord[1]]

    }

  }

  ## Recent completions (last 5), among the per-config files that passed the
  ## gate; rows adopted from a legacy single file have no completion time.
  recent <- character(0)

  on_disk <- Filter(function(k) !is.na(k$path), gated$kept)
  mtimes  <- file.mtime(vapply(on_disk, `[[`, character(1), "path"))

  for (k in utils::head(on_disk[order(mtimes, decreasing = TRUE)], 5)) {

    row <- k$row

    ## Checkpoint rows are evaluate_single_config() result rows, which carry
    ## no `model` column; only show the model when something wrote one.
    model_label <- if ("model" %in% names(row) && !is.na(row$model)) {
      paste0(" (", MODEL_DISPLAY_NAMES[row$model] %||% row$model, ")")
    } else {
      ""
    }
    metric_val <- if (!is.na(.monitor_metric_value(row, metric_name))) {
      paste0(toupper(metric_name), " = ",
             round(.monitor_metric_value(row, metric_name), 3))
    } else {
      row$status
    }
    recent <- c(recent, paste0(row$config_id, model_label, " ", metric_val))

  }

  list(
    n_complete  = n_complete,
    n_total     = manifest$n_total,
    pct         = pct,
    rate        = rate,
    eta         = eta,
    best_config = best_config,
    best_metric = best_metric,
    recent      = recent,
    ignored     = ignored,
    unreadable  = names(store$unreadable)
  )

}


#' The value the monitor ranks a checkpoint row on
#'
#' evaluate() ranks on the cross-validated metric (`cv_<metric>`, #50), so
#' the monitor's "best so far" reads the same column. Checkpoint rows written
#' before that column existed fall back to the test-set metric, so an old run
#' can still be monitored.
#' @noRd
.monitor_metric_value <- function(row, metric_name) {

  cv_col <- paste0("cv_", metric_name)

  if (cv_col %in% names(row) && !is.na(row[[cv_col]])) {

    return(row[[cv_col]])

  }

  if (metric_name %in% names(row)) row[[metric_name]] else NA_real_

}


#' Render monitor output to console
#' @noRd
.render_monitor <- function(stats, manifest) {

  cat("\014")
  cat(paste0(paste(rep("\u2500", 50), collapse = ""), "\n"))
  cat(paste0("  evaluate() monitor \u2014 ", format(Sys.time(), "%H:%M:%S"), "\n"))

  if ((manifest$schema_version %||% 1L) >= 2L) {

    cat(paste0("  Parallel:  over ", manifest$axis, " on ", manifest$plan,
               " (", manifest$workers, " worker",
               if (!identical(manifest$workers, 1L)) "s" else "", ")\n"))

  } else {

    cat(paste0("  Parallel:  legacy manifest (workers = ",
               manifest$workers %||% "?", ")\n"))

  }

  ## Which rows this directory is scoring. Without it, two runs pointed at
  ## one output_dir are indistinguishable in the monitor.
  if (!is.null(manifest$data_hash) && !is.na(manifest$data_hash)) {

    outcome <- manifest$data_fields$outcome

    cat(paste0("  Data:      ", manifest$data_n_rows %||% "?",
               " training rows",
               if (!is.null(outcome)) paste0(" of ", outcome) else "",
               ", hash ", substr(manifest$data_hash, 1, 12), "\n"))

  }

  if (!is.null(manifest$settings)) {

    cat(paste0("  Settings:  ",
               paste0(names(manifest$settings), " = ",
                      vapply(manifest$settings, format_setting_value,
                             character(1)),
                      collapse = ", "),
               "\n"))

  }
  cat(paste0(paste(rep("\u2500", 50), collapse = ""), "\n\n"))

  cat(paste0("  Progress:  ", stats$n_complete, " / ", stats$n_total,
             " (", stats$pct, "%)\n"))

  ## Rows evaluate() would not resume, so they are not in the progress count.
  if (sum(stats$ignored) > 0) {

    reasons <- c(other_data     = "other training data",
                 other_settings = "other tuning settings",
                 earlier_schema = "earlier scoring schema",
                 not_in_grid    = "config not in this grid")
    shown   <- stats$ignored[stats$ignored > 0]

    cat(paste0("  Ignored:   ", sum(stats$ignored), " checkpoint",
               if (sum(stats$ignored) != 1) "s" else "", " (",
               paste0(shown, " ", reasons[names(shown)], collapse = ", "),
               ")\n"))

  }

  if (length(stats$unreadable) > 0) {

    cat(paste0("  Unreadable: ", paste(stats$unreadable, collapse = ", "),
               "\n"))

  }

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
