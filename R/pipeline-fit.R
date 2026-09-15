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
#' @param metric Character or NULL. Metric for ranking the candidate
#'   configs, by bare name (`"rpd"`, `"rmse"`, ...). If NULL, uses the
#'   rank_metric from `evaluate()`. Ranking reads the cross-validated value
#'   at each config's selected hyperparameters (`evaluation$results$cv_<metric>`),
#'   never the test-set column, so the test sets stay held out from
#'   selection. Default NULL.
#' @param compute_uq Logical. Train UQ components (quantile model +
#'   conformal calibration). Default TRUE.
#' @param compute_ad Logical. Compute applicability-domain metadata (centroid +
#'   shrinkage covariance + held-out OOD thresholds) per config, so `predict()`
#'   can emit `.ad_distance` / `.ad_flag`. Shares the UQ calibration split.
#'   Default TRUE.
#' @param allow_par Logical. If `TRUE`, tune parallelises the CV folds
#'   inside each member's re-tune and OOF pass on whatever `future::plan()`
#'   the caller has registered; `fit()` never registers a plan. If the plan
#'   offers fewer than two workers, warns naming it and runs sequentially.
#'   `fit()` parallelises folds only in this version; a configs axis across
#'   the `n_best` members is gated on the fitted-object memory contract (a
#'   fitted object is hundreds of MB and would cross to workers). Default
#'   FALSE.
#' @param seed Integer. Random seed for CV folds and, through
#'   `fit_split_seed()` (`seed + 1L`), for Split F. The offset keeps Split F
#'   independent of `evaluate()`'s split when both are called with the same
#'   `seed`, which is the default. Default 307L.
#' @param verbose Logical. Print progress tree to console. Default TRUE.
#'
#' @return A `horizons_fit` object (inherits from `horizons_eval`,
#'   `horizons_data`) with `models$` slot populated. The slot includes
#'   `response_bound` (max training outcome times `RESPONSE_BOUND_MARGIN`),
#'   the deploy-time winsorization guardrail `predict()` applies to
#'   back-transformed point predictions.
#'
#' @export
fit <- function(x,
                n_best     = 5L,
                metric     = NULL,
                compute_uq = TRUE,
                compute_ad = TRUE,
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

  ## -----------------------------------------------------------------------
  ## Step 0b: Parallel backend
  ## -----------------------------------------------------------------------
  ## The user owns the backend. Confirm there is one to run on, pin threads
  ## in this process before tune spawns anything, and never touch the plan.

  if (!rlang::is_bool(allow_par)) {

    rlang::abort("`allow_par` must be TRUE or FALSE.")

  }

  if (allow_par) {

    allow_par <- check_parallel_backend("fit()")

    if (allow_par) {

      warn_if_mirai_preferred()
      unpin_threads <- pin_parent_threads()
      on.exit(unpin_threads(), add = TRUE)

    }

  }

  ## Determine ranking metric. The name stays bare ("rpd"); ranking reads the
  ## cross-validated column cv_rpd, so members are chosen without touching
  ## either test set (#50). The test-set metrics on the leaderboard remain
  ## reported, and are honest precisely because they are not used here.
  rank_metric <- metric %||% x$evaluation$rank_metric %||% "rpd"

  ## Extract successful configs and rank
  successes <- eval_results[eval_results$status == "success", ]

  if (nrow(successes) == 0) {

    rlang::abort(
      "No successful configurations in evaluation results. Cannot fit models."
    )

  }

  successes <- rank_configs_by_cv(successes, rank_metric)

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
  ## evaluate() seeds its split with `seed` and this call shape on the same
  ## frame, so seeding Split F with `seed` too made the two partitions
  ## bit-identical at the shared default (#50). Derive F's seed instead.

  set.seed(fit_split_seed(seed))

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

  ## Visible guard: a caller can still make the partitions coincide (for
  ## example fit(seed = evaluate_seed - 1L)). Warn rather than abort; the
  ## test metrics are then post-selection and the user should know.
  eval_split <- x$evaluation$split

  if (!is.null(eval_split) && !is.null(eval_split$in_id) &&
      identical(sort(as.integer(split_F$in_id)),
                sort(as.integer(eval_split$in_id)))) {

    cli::cli_warn(c(
      "!" = "fit()'s train/test partition is identical to evaluate()'s.",
      "i" = "Reported test metrics are then measured on the rows the configs were selected on.",
      "i" = "Pass a different {.arg seed} to fit() to get an independent partition."
    ))

  }

  ## Calibration partitioning: split train_F into train_Fit / calib_Fit.
  ## UQ and AD share this one held-out split (D7) \u2014 both calibrate on calib_Fit
  ## and never train on it. Built whenever either capability is requested.
  calib_data <- NULL

  if (compute_uq || compute_ad) {

    split_C <- tryCatch(
      rsample::initial_split(train_F, prop = 0.8, strata = outcome_col),
      error = function(e) {

        rsample::initial_split(train_F, prop = 0.8)

      }
    )

    train_Fit <- rsample::training(split_C)
    calib_data <- rsample::testing(split_C)

    ## Guard: minimum calibration size. Too small disables BOTH capabilities
    ## that needed it \u2014 neither UQ nor AD can calibrate on an undersized set.
    if (nrow(calib_data) < N_CALIB_MIN) {

      if (verbose) {

        disabled <- paste(c(if (compute_uq) "UQ", if (compute_ad) "AD"),
                          collapse = " and ")

        cat(paste0(
          "\u2502  ", cli::col_yellow(
            "Calibration set too small (", nrow(calib_data),
            " < ", N_CALIB_MIN, "). Disabling ", disabled, "."
          ), "\n"
        ))

      }

      compute_uq <- FALSE
      compute_ad <- FALSE
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
      ## configure() stores the screening and the final re-tune budgets
      ## separately; fit() used to pass the screening one here, so the
      ## user-facing `final_bayesian_iter` did nothing (#46). Objects
      ## configured before that field existed fall back to the constant.
      final_bayesian_iter = tuning$final_bayesian_iter %||% DEFAULT_FINAL_BAYES_ITER,
      grid_size           = tuning$grid_size,
      compute_uq          = compute_uq,
      compute_ad          = compute_ad,
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

  ## Collect AD bundles (named by config_id) — same shape as uq_list
  ad_list <- NULL

  if (compute_ad) {

    ad_list <- list()

    for (res in results_list) {

      if (res$status == "success" && !is.null(res$ad)) {

        ad_list[[ res$config_id ]] <- res$ad

      }

    }

    if (length(ad_list) == 0) ad_list <- NULL

  }

  ## -----------------------------------------------------------------------
  ## Step 6: Populate models$ slot and promote class
  ## -----------------------------------------------------------------------

  total_runtime <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  ## Persist the facts fit() computed so predict() reads them rather than
  ## re-deriving. workflows_list is named in best-first order, so its first name
  ## is the top fitted config. predictor_schema is the training-axis predictor
  ## column set predict() validates new_data against (no need to re-introspect a
  ## butchered recipe at predict time).
  best_config      <- if (length(workflows_list) > 0) names(workflows_list)[1] else NA_character_
  predictor_schema <- role_map$variable[role_map$role == "predictor"]

  ## Deploy-time guardrail bound: predictions are winsorized to this value in
  ## predict_one_config(). max-times-margin (not a quantile) — the bound should
  ## permit modest extrapolation and catch only the physically absurd.
  response_bound <- max(analysis[[outcome_col]], na.rm = TRUE) * RESPONSE_BOUND_MARGIN

  x$models <- list(
    workflows        = workflows_list,
    n_models         = length(workflows_list),
    best_config      = best_config,
    rank_metric      = rank_metric,
    predictor_schema = predictor_schema,
    response_bound   = response_bound,
    cv_predictions   = all_cv_predictions,
    results          = results_tibble,
    split            = split_F,
    row_index        = row_index,
    uq               = uq_list,
    ad               = ad_list,
    timestamp        = Sys.time(),
    runtime_secs     = total_runtime
  )

  class(x) <- c("horizons_fit", "horizons_eval", "horizons_data", "list")

  ## Certify the contract before returning: structural checks only (see
  ## validate_horizons_fit), so every fit() return matches invariants I6/I7
  ## (workflow keys subset config ids; uq keys subset workflow keys) and the
  ## response_bound guardrail contract.
  x <- validate_horizons_fit(x)

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

## ---------------------------------------------------------------------------
## fit_split_seed \u2014 Split F's seed, derived from the user's seed
## ---------------------------------------------------------------------------

#' Seed for fit()'s train/test partition
#'
#' @description
#' `evaluate()` seeds its train/test split with `seed`. `fit()` builds its own
#' split with the same `rsample::initial_split()` call on the same frame, so
#' seeding it with `seed` too reproduced `evaluate()`'s partition exactly
#' (#50). Split F is seeded with `seed + 1L` instead. The offset is documented
#' rather than hidden so a caller who needs to reproduce the partition can.
#'
#' @param seed Integer seed passed to `fit()`.
#' @return Integer seed for Split F.
#' @keywords internal
fit_split_seed <- function(seed) {

  as.integer(seed) + 1L

}
