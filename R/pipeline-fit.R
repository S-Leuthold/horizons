#' Fit Top Model Configurations
#'
#' @description
#' Re-tunes the top N configurations from `evaluate()` with warm-start
#' Bayesian optimization, generates out-of-fold (OOF) CV predictions for
#' downstream stacking, fits final deployable models, evaluates on a new
#' held-out test set, and optionally trains uncertainty quantification (UQ)
#' components.
#'
#' This is the render layer — it owns all console output. The capture layer
#' (`fit_single_config()`) runs silently and returns structured results.
#'
#' @param x A `horizons_eval` object (output of `evaluate()`).
#' @param n_best Integer. Number of top configurations to re-tune. Default 5.
#' @param metric Character or NULL. Metric for ranking. If NULL, uses the
#'   rank_metric from `evaluate()`. Default NULL.
#' @param compute_uq Logical. Train UQ components (quantile model +
#'   conformal calibration). Default TRUE.
#' @param allow_par Logical. Enable parallel CV folds. Default FALSE.
#' @param seed Integer. Random seed for Split F and CV folds. Default 307L.
#' @param verbose Logical. Print progress tree to console. Default TRUE.
#'
#' @return A `horizons_fit` object (inherits from `horizons_eval`,
#'   `horizons_data`) with `models$` slot populated.
#'
#' @export
fit <- function(x,
                n_best     = 5L,
                metric     = NULL,
                compute_uq = TRUE,
                allow_par  = FALSE,
                seed       = 307L,
                verbose    = TRUE) {

  start_time <- Sys.time()

  ## -----------------------------------------------------------------------
  ## Step 0: Preflight validation
  ## -----------------------------------------------------------------------

  if (!inherits(x, "horizons_eval")) {

    rlang::abort(
      "Input must be a `horizons_eval` object (output of `evaluate()`)."
    )

  }

  eval_results <- x$evaluation$results

  if (is.null(eval_results) || nrow(eval_results) == 0) {

    rlang::abort(
      "No evaluation results found. Run `evaluate()` before `fit()`."
    )

  }

  ## Determine ranking metric
  rank_metric <- metric %||% x$evaluation$rank_metric %||% "rpd"
  higher_better <- c("rpd", "rsq", "ccc")

  ## Extract successful configs and rank
  successes <- eval_results[eval_results$status == "success", ]

  if (nrow(successes) == 0) {

    rlang::abort(
      "No successful configurations in evaluation results. Cannot fit models."
    )

  }

  ## Rank successes by metric
  if (!rank_metric %in% names(successes)) {

    rlang::abort(paste0(
      "Rank metric '", rank_metric,
      "' not found in evaluation results. ",
      "Available: ", paste(names(successes), collapse = ", ")
    ))

  }

  metric_vals <- successes[[rank_metric]]

  if (all(is.na(metric_vals))) {

    rlang::abort(paste0(
      "All values for rank metric '", rank_metric, "' are NA. Cannot rank."
    ))

  }

  if (rank_metric %in% higher_better) {

    rank_order <- order(metric_vals, decreasing = TRUE)

  } else {

    rank_order <- order(metric_vals, decreasing = FALSE)

  }

  successes <- successes[rank_order, ]

  ## Cap n_best at available successes
  n_available <- nrow(successes)
  n_best      <- as.integer(n_best)

  if (n_best > n_available) {

    if (verbose) {

      cat(paste0(
        "\u2502  ", cli::col_yellow(
          "Requested n_best = ", n_best,
          " but only ", n_available,
          " successful configs available. Using ", n_available, "."
        ), "\n"
      ))

    }

    n_best <- n_available

  }

  top_configs <- successes[seq_len(n_best), ]

  ## Extract references
  role_map     <- x$data$role_map
  outcome_col  <- role_map$variable[role_map$role == "outcome"]
  analysis     <- x$data$analysis
  all_configs  <- x$config$configs
  tuning       <- x$config$tuning
  cv_folds     <- tuning$cv_folds

  ## -----------------------------------------------------------------------
  ## Step 1: Data partitioning — Split F (new, independent from evaluate)
  ## -----------------------------------------------------------------------

  set.seed(seed)

  split_F <- tryCatch(
    rsample::initial_split(analysis, prop = 0.8, strata = outcome_col),
    error = function(e) {

      if (verbose) {

        cat(paste0(
          "\u2502  ", cli::col_yellow(
            "Stratified split failed, retrying without strata"
          ), "\n"
        ))

      }

      rsample::initial_split(analysis, prop = 0.8)

    }
  )

  train_F <- rsample::training(split_F)
  test_F  <- rsample::testing(split_F)
  n_train <- nrow(train_F)
  n_test  <- nrow(test_F)

  ## UQ partitioning: split train_F into train_Fit / calib_Fit
  calib_data <- NULL

  if (compute_uq) {

    split_C <- tryCatch(
      rsample::initial_split(train_F, prop = 0.8, strata = outcome_col),
      error = function(e) {

        rsample::initial_split(train_F, prop = 0.8)

      }
    )

    train_Fit <- rsample::training(split_C)
    calib_data <- rsample::testing(split_C)

    ## Guard: minimum calibration size
    if (nrow(calib_data) < N_CALIB_MIN) {

      if (verbose) {

        cat(paste0(
          "\u2502  ", cli::col_yellow(
            "Calibration set too small (", nrow(calib_data),
            " < ", N_CALIB_MIN, "). Disabling UQ."
          ), "\n"
        ))

      }

      compute_uq <- FALSE
      train_Fit  <- train_F
      calib_data <- NULL

    }

  } else {

    train_Fit <- train_F

  }

  ## -----------------------------------------------------------------------
  ## Step 2: Create CV resamples from train_Fit
  ## -----------------------------------------------------------------------

  cv_resamples <- tryCatch(
    rsample::vfold_cv(train_Fit, v = cv_folds, strata = outcome_col),
    error = function(e) {

      if (verbose) {

        cat(paste0(
          "\u2502  ", cli::col_yellow(
            "Stratified CV failed, retrying without strata"
          ), "\n"
        ))

      }

      rsample::vfold_cv(train_Fit, v = cv_folds)

    }
  )

  ## -----------------------------------------------------------------------
  ## Step 3: Tree header
  ## -----------------------------------------------------------------------

  if (verbose) {

    cat("\n")
    cat(paste0("\u250C fit ",
               paste(rep("\u2500", 57), collapse = ""), "\n"))
    cat("\u2502\n")
    cat(paste0(
      "\u2502  Re-tuning top ", n_best, " of ",
      nrow(eval_results), " configurations\n"
    ))
    cat(paste0(
      "\u2502  Split: ", n_train, " train / ", n_test,
      " test (new, independent)\n"
    ))

    if (compute_uq) {

      cat(paste0(
        "\u2502  UQ calibration: ", nrow(train_Fit), " fit / ",
        nrow(calib_data), " calibration\n"
      ))

    }

    cat(paste0(
      "\u2502  CV: ", cv_folds, "-fold stratified on ", outcome_col, "\n"
    ))
    cat(paste0(
      "\u2502  Bayesian: ", tuning$bayesian_iter,
      " iterations with warm-start\n"
    ))
    cat("\u2502\n")

  }

  ## -----------------------------------------------------------------------
  ## Step 4: Config loop
  ## -----------------------------------------------------------------------

  results_list <- list()

  for (i in seq_len(n_best)) {

    top_row   <- top_configs[i, ]
    config_id <- top_row$config_id

    ## Look up full config row from original configs table
    cfg <- all_configs[all_configs$config_id == config_id, ]

    ## Pretty config description
    model_name <- MODEL_DISPLAY_NAMES[cfg$model] %||% cfg$model
    desc_parts <- c(model_name, cfg$transformation, cfg$preprocessing,
                    cfg$feature_selection)

    if (!is.na(cfg$covariates)) {

      desc_parts <- c(desc_parts, paste0("+", cfg$covariates))

    }

    config_desc <- paste(desc_parts, collapse = " + ")
    is_last     <- i == n_best
    branch      <- if (is_last) "\u2514\u2500" else "\u251C\u2500"
    cont        <- if (is_last) "   " else "\u2502  "

    ## Render config start
    if (verbose) {

      cat(paste0(
        "\u2502  ", branch, " [", i, "/", n_best, "] ", config_desc, "\n"
      ))

    }

    ## Extract best_params from evaluate
    best_params_eval <- top_row$best_params[[1]]

    ## Call capture layer
    config_result <- fit_single_config(
      config_row          = cfg,
      split_F             = split_F,
      cv_resamples        = cv_resamples,
      calib_data          = calib_data,
      train_data          = train_Fit,
      role_map            = role_map,
      best_params_eval    = best_params_eval,
      final_bayesian_iter = tuning$bayesian_iter,
      grid_size           = tuning$grid_size,
      compute_uq          = compute_uq,
      allow_par           = allow_par,
      seed                = seed
    )

    results_list[[i]] <- config_result

    ## Render result
    if (verbose) {

      if (config_result$status == "success") {

        ## Test metrics
        tm <- config_result$test_metrics
        cat(paste0(
          "\u2502  ", cont, "\u251C\u2500 Test: ",
          "RPD = ", round(tm$rpd, 2),
          ", R\u00B2 = ", round(tm$rsq, 2),
          ", RMSE = ", round(tm$rmse, 3), "\n"
        ))

        ## CV metrics comparison
        cv_met <- config_result$cv_metrics
        cv_rpd <- cv_met[cv_met$.metric == "rpd", ]
        cv_rmse <- cv_met[cv_met$.metric == "rmse", ]

        if (nrow(cv_rpd) == 1 && nrow(cv_rmse) == 1) {

          cat(paste0(
            "\u2502  ", cont, "\u251C\u2500 CV:   ",
            "RPD = ", round(cv_rpd$mean, 2),
            " \u00B1 ", round(cv_rpd$std_err, 2),
            ", RMSE = ", round(cv_rmse$mean, 3),
            " \u00B1 ", round(cv_rmse$std_err, 3), "\n"
          ))

        }

        ## Degradation flag
        if (config_result$degraded) {

          cat(paste0(
            "\u2502  ", cont, "\u251C\u2500 ",
            cli::col_yellow("\u26A0 DEGRADED: ",
                            config_result$degraded_reason), "\n"
          ))

        }

        ## UQ coverage
        if (!is.null(config_result$uq)) {

          cat(paste0(
            "\u2502  ", cont, "\u251C\u2500 UQ coverage: ",
            round(config_result$uq$oof_coverage * 100, 1), "% ",
            "(target ", round(config_result$uq$level_default * 100, 0),
            "%, width = ", round(config_result$uq$mean_width, 3), ")\n"
          ))

        }

      } else {

        cat(paste0(
          "\u2502  ", cont, "\u251C\u2500 ",
          cli::col_red("FAILED: ", config_result$error_message), "\n"
        ))

      }

      ## Warnings
      if (!is.null(config_result$warnings)) {

        for (w in config_result$warnings) {

          cat(paste0(
            "\u2502  ", cont, "\u251C\u2500 ",
            cli::col_yellow(w), "\n"
          ))

        }

      }

      ## Runtime
      cat(paste0(
        "\u2502  ", cont, "\u2514\u2500 ",
        if (config_result$status == "failed") {
          cli::col_red("\u2717")
        } else {
          cli::col_green("\u2713")
        },
        " ", round(config_result$runtime_secs, 1), "s\n"
      ))

    }

    ## Memory cleanup after each config
    gc(verbose = FALSE)

  }

  ## -----------------------------------------------------------------------
  ## Step 5: Assemble results
  ## -----------------------------------------------------------------------

  ## Collect workflows (named by config_id)
  workflows_list <- list()

  for (res in results_list) {

    if (res$status == "success" && !is.null(res$fitted_workflow)) {

      workflows_list[[ res$config_id ]] <- res$fitted_workflow

    }

  }

  ## Row-bind CV predictions across configs
  cv_pred_parts <- purrr::compact(purrr::map(results_list, "cv_predictions"))
  all_cv_predictions <- if (length(cv_pred_parts) > 0) {

    dplyr::bind_rows(cv_pred_parts)

  } else {

    NULL

  }

  ## Build results tibble
  results_tibble <- purrr::map_dfr(results_list, function(res) {

    tm <- res$test_metrics
    cv <- res$cv_metrics

    ## Extract CV summary metrics
    cv_rmse <- if (!is.null(cv)) cv[cv$.metric == "rmse", ] else NULL
    cv_rpd  <- if (!is.null(cv)) cv[cv$.metric == "rpd", ]  else NULL

    tibble::tibble(
      config_id       = res$config_id,
      status          = res$status,
      degraded        = res$degraded %||% NA,
      degraded_reason = res$degraded_reason %||% NA_character_,
      rmse            = if (!is.null(tm)) tm$rmse  else NA_real_,
      rrmse           = if (!is.null(tm)) tm$rrmse else NA_real_,
      rsq             = if (!is.null(tm)) tm$rsq   else NA_real_,
      ccc             = if (!is.null(tm)) tm$ccc   else NA_real_,
      rpd             = if (!is.null(tm)) tm$rpd   else NA_real_,
      mae             = if (!is.null(tm)) tm$mae   else NA_real_,
      cv_rmse_mean    = if (!is.null(cv_rmse) && nrow(cv_rmse) == 1) cv_rmse$mean    else NA_real_,
      cv_rmse_se      = if (!is.null(cv_rmse) && nrow(cv_rmse) == 1) cv_rmse$std_err else NA_real_,
      cv_rpd_mean     = if (!is.null(cv_rpd)  && nrow(cv_rpd)  == 1) cv_rpd$mean     else NA_real_,
      cv_rpd_se       = if (!is.null(cv_rpd)  && nrow(cv_rpd)  == 1) cv_rpd$std_err  else NA_real_,
      best_params     = list(res$best_params),
      runtime_secs    = res$runtime_secs
    )

  })

  ## Build row_index: .row → id mapping from train_Fit
  id_col <- role_map$variable[role_map$role == "id"]

  if (length(id_col) == 0) {

    id_col <- "sample_id"

  } else {

    id_col <- id_col[1]

  }

  row_index <- tibble::tibble(
    .row      = seq_len(nrow(train_Fit)),
    sample_id = train_Fit[[id_col]]
  )

  ## Collect UQ bundles (named by config_id)
  uq_list <- NULL

  if (compute_uq) {

    uq_list <- list()

    for (res in results_list) {

      if (res$status == "success" && !is.null(res$uq)) {

        uq_list[[ res$config_id ]] <- res$uq

      }

    }

    if (length(uq_list) == 0) uq_list <- NULL

  }

  ## -----------------------------------------------------------------------
  ## Step 6: Populate models$ slot and promote class
  ## -----------------------------------------------------------------------

  total_runtime <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  x$models <- list(
    workflows      = workflows_list,
    n_models       = length(workflows_list),
    cv_predictions = all_cv_predictions,
    results        = results_tibble,
    split          = split_F,
    row_index      = row_index,
    uq             = uq_list,
    timestamp      = Sys.time(),
    runtime_secs   = total_runtime
  )

  class(x) <- c("horizons_fit", "horizons_eval", "horizons_data", "list")

  ## -----------------------------------------------------------------------
  ## Step 7: Tree footer / summary
  ## -----------------------------------------------------------------------

  if (verbose) {

    n_success  <- sum(results_tibble$status == "success")
    n_failed   <- sum(results_tibble$status == "failed")
    n_degraded <- sum(results_tibble$degraded == TRUE, na.rm = TRUE)

    cat("\u2502\n")
    cat(paste0("\u2502  Summary\n"))
    cat(paste0(
      "\u2502  \u251C\u2500 Successful: ", n_success, " / ", n_best, "\n"
    ))

    if (n_degraded > 0) {

      degraded_ids <- results_tibble$config_id[results_tibble$degraded == TRUE &
                                                  !is.na(results_tibble$degraded)]
      cat(paste0(
        "\u2502  \u251C\u2500 ",
        cli::col_yellow("Degraded: ", n_degraded, " / ", n_best,
                         " (", paste(degraded_ids, collapse = ", "), ")"),
        "\n"
      ))

    }

    if (n_failed > 0) {

      cat(paste0(
        "\u2502  \u251C\u2500 ",
        cli::col_red("Failed: ", n_failed, " / ", n_best), "\n"
      ))

    }

    ## Best test RPD
    success_rows <- results_tibble[results_tibble$status == "success", ]

    if (nrow(success_rows) > 0) {

      best_idx <- which.max(success_rows$rpd)
      best_rpd <- success_rows$rpd[best_idx]
      best_id  <- success_rows$config_id[best_idx]

      cat(paste0(
        "\u2502  \u251C\u2500 Best test RPD: ", best_id,
        " (", round(best_rpd, 2), ")\n"
      ))

    }

    ## Runtime
    if (total_runtime < 60) {

      time_str <- paste0(round(total_runtime, 1), "s")

    } else {

      time_str <- paste0(round(total_runtime / 60, 1), " min")

    }

    cat(paste0("\u2502  \u2514\u2500 Runtime: ", time_str, "\n"))
    cat("\u2502\n")
    cat(paste0(
      "\u2514\u2500 Class: horizons_eval \u2192 horizons_fit\n"
    ))
    cat(paste0(
      paste(rep("\u2500", 62), collapse = ""), "\n"
    ))

  }

  x

}
