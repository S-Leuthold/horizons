## ---------------------------------------------------------------------------
## Tests: fit() and the members it takes
## ---------------------------------------------------------------------------
## Which members fit() takes from an evaluation when none passed the prune
## gate or all fell below it (#38), and what it does when members fail. Each
## block builds its own objects; make_fit_object() is in helper-fixtures.R.

## The fit objects the member tests read, built once per process
## (helper-memo.R). Each test reads its object before anything is mocked, so
## no build runs under a mock.
fit_obj <- function(n_configs) {
  memo_fixture("members_fit_obj", make_fit_object, n = 60, n_configs = n_configs)
}


## =========================================================================
## No config passed the prune gate: fit() takes evaluate()'s fallback (#38)
## =========================================================================
## evaluate() takes best_config from the pruned configs when none succeeded.
## fit() kept successes only, so it refused the object evaluate() had just
## returned ("No successful configurations").

describe("fit() - an evaluation with only pruned configs (#38)", {

  ## A threshold no model clears, with a Bayesian stage for the gate to skip,
  ## so evaluate() prunes both configs.
  obj <- make_eval_object(n = 60, n_configs = 2)
  obj$config$tuning$bayesian_iter       <- 1L
  obj$config$tuning$final_bayesian_iter <- 0L

  pruned <- suppressWarnings(
    evaluate(obj, prune = TRUE, prune_threshold = 9999,
             seed = 42L)
  )

  it("fits the pruned configs in evaluate()'s order, and warns that it fell back", {

    skip_unless_slow_tier()

    ## Precondition: nothing succeeded, and evaluate() still named a winner
    expect_true(all(pruned$evaluation$results$status == "pruned"))

    w <- expect_warning(
      r <- keep_only_warning(
        fit(pruned, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L),
        "horizons_warning_quality"
      ),
      class = "horizons_warning_quality"
    )

    ## One quality warning, on the log too: pruned members are below the
    ## threshold by definition. It names the threshold and each member's cv RPD.
    msg <- gsub("\\s+", " ", conditionMessage(w))   # undo cli line wrapping

    expect_true("quality" %in% r$log$topic[r$log$verb == "fit"])
    expect_match(msg, "prune gate", fixed = TRUE)
    expect_match(msg, "prune threshold of 9999", fixed = TRUE)

    res <- pruned$evaluation$results

    for (i in seq_len(nrow(res))) {
      expect_match(msg, paste0(res$config_id[i], " ", formatC(res$cv_rpd[i], digits = 2, format = "f")),
                   fixed = TRUE)
    }

    expected <- rank_configs_by_cv(pruned$evaluation$results,
                                   pruned$evaluation$rank_metric)$config_id

    expect_s3_class(r, "horizons_fit")
    expect_identical(r$models$results$config_id, expected)
    expect_identical(r$models$best_config, pruned$evaluation$best_config)

  })

  it("refuses when no pruned config carries the ranking metric", {

    unranked <- pruned
    unranked$evaluation$results$cv_rpd <- NA_real_

    expect_error(
      fit(unranked, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE),
      paste0("None succeeded, and none of the ", nrow(unranked$evaluation$results),
             " pruned configurations has a cv_rpd value."), fixed = TRUE,
      class = "horizons_error_input"
    )

  })

  ## rank_configs_by_cv() used to refuse this unclassed.
  it("refuses, classed, when configs succeeded but none has the ranking metric", {

    unranked <- pruned
    unranked$evaluation$results$status <- "success"
    unranked$evaluation$results$cv_rpd <- NA_real_

    err <- expect_error(
      fit(unranked, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE),
      "succeeded, but none has",
      class = "horizons_error_input"
    )

    ## evaluate() returns such an evaluation, so the remedy is another metric
    msg <- gsub("\\s+", " ", conditionMessage(err))   # undo cli line wrapping
    expect_match(msg, "fit(x, metric = )", fixed = TRUE)
    expect_no_match(msg, "Re-run", fixed = TRUE)

  })

  it("leaves a configuration without the ranking metric out, and says so once", {

    one_unranked <- pruned
    one_unranked$evaluation$results$status    <- "success"
    one_unranked$evaluation$results$cv_rpd[1] <- NA_real_
    left_out <- one_unranked$evaluation$results$config_id[1]

    got <- collect_warnings(
      fit(one_unranked, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L),
      "left out of the ranking"
    )

    expect_length(got$warnings, 1L)
    expect_s3_class(got$warnings[[1]], "horizons_warning_skipped")
    expect_match(conditionMessage(got$warnings[[1]]), left_out, fixed = TRUE)

    ## The skipped ranking is one row of the log; the members also fell below
    ## the prune threshold, a quality row of its own
    log <- got$value$log[got$value$log$verb == "fit" & got$value$log$topic %in% "skipped", ]
    expect_identical(nrow(log), 1L)
    expect_identical(log$ids[[1]], left_out)
    expect_false(left_out %in% got$value$models$results$config_id)

  })

})


## =========================================================================
## Every member fell below the prune threshold at bayesian_iter = 0 (#38)
## =========================================================================
## With no Bayesian stage the gate skips nothing, so nothing is pruned and
## the fallback warning cannot fire. The gate's reading is recorded apart from
## the status (below_prune_threshold), and fit() warns from it.

describe("fit() - members below the prune threshold at bayesian_iter = 0 (#38)", {

  obj <- make_eval_object(n = 60, n_configs = 2)
  obj$config$tuning$bayesian_iter       <- 0L
  obj$config$tuning$final_bayesian_iter <- 0L

  below <- suppressWarnings(
    evaluate(obj, prune = TRUE, prune_threshold = 9999,
             seed = 42L)
  )

  it("warns, naming the threshold and the members' cv RPD", {

    skip_unless_slow_tier()

    ## Precondition: both are successes, and both fell below the threshold
    expect_true(all(below$evaluation$results$status == "success"))
    expect_true(all(below$evaluation$results$below_prune_threshold))

    w <- expect_warning(
      r <- keep_only_warning(
        fit(below, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L),
        "horizons_warning_quality"
      ),
      class = "horizons_warning_quality"
    )

    msg <- gsub("\\s+", " ", conditionMessage(w))   # undo cli line wrapping

    ## Not the fallback: these configs succeeded
    expect_no_match(msg, "prune gate", fixed = TRUE)
    res <- below$evaluation$results

    expect_match(msg, "prune threshold of 9999", fixed = TRUE)

    for (i in seq_len(nrow(res))) {
      expect_match(msg, paste0(res$config_id[i], " ", formatC(res$cv_rpd[i], digits = 2, format = "f")),
                   fixed = TRUE)
    }

    expect_s3_class(r, "horizons_fit")
    expect_equal(r$models$n_models, 2L)

  })

  it("is quiet when a member cleared the threshold", {

    skip_unless_slow_tier()

    cleared <- below
    cleared$evaluation$results$below_prune_threshold[1] <- FALSE

    r <- suppressWarnings(
      fit(cleared, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L)
    )

    expect_false(any(grepl("prune threshold", r$log$message[r$log$verb == "fit"], fixed = TRUE)))
    expect_s3_class(r, "horizons_fit")

  })

  ## Unpruned below-threshold members under a configured bayesian_iter > 0
  ## came from rows scored with no Bayesian stage (resumed checkpoints of a
  ## bayesian_iter = 0 run); naming bayesian_iter = 0 as the setting would be
  ## a false cause.
  it("words its explanation from the configured bayesian_iter", {

    members <- tibble::tibble(
      config_id             = c("cfg_001", "cfg_002"),
      status                = "success",
      below_prune_threshold = TRUE,
      prune_threshold       = 1,
      cv_rpd                = c(0.93, 0.88)
    )

    warn_text <- function(bayesian_iter) {
      col <- log_open("fit")
      log_members_below_threshold(col, members, fallback = FALSE,
                                  bayesian_iter = bayesian_iter)
      log_close(col)$message
    }

    expect_match(warn_text(0L), "no Bayesian stage to skip (`bayesian_iter = 0`)",
                 fixed = TRUE)

    at_five <- warn_text(5L)
    expect_match(at_five, "configured `bayesian_iter = 5`", fixed = TRUE)
    expect_no_match(at_five, "no Bayesian stage to skip", fixed = TRUE)

    expect_match(warn_text(NULL), "None was pruned, so they ranked as successes.",
                 fixed = TRUE)

  })

  ## Rows written without the gate's reading: a pruned row is below the
  ## threshold all the same, since the gate pruned it, and a success is not
  it("counts a pruned member without a reading as below the threshold, and a success without one as not", {

    unread <- tibble::tibble(config_id = c("cfg_001", "cfg_002"), cv_rpd = c(0.93, 0.88))

    pruned <- unread
    pruned$status <- "pruned"

    logged <- function(members, fallback) {
      col <- log_open("fit")
      log_members_below_threshold(col, members, fallback = fallback)
      log_close(col)
    }

    log <- logged(pruned, fallback = TRUE)
    expect_identical(log$topic, "quality")
    expect_match(log$message, "prune threshold (not recorded on these rows)", fixed = TRUE)
    expect_identical(log$ids[[1]], c("cfg_001", "cfg_002"))

    succeeded <- unread
    succeeded$status <- "success"

    expect_identical(nrow(logged(succeeded, fallback = FALSE)), 0L)

  })

})


## =========================================================================
## Every member fails
## =========================================================================
## fit() used to carry on to validate_horizons_fit(), which refused the empty
## workflows slot with a structural message that said nothing about why the
## members failed; and models$results dropped error_message.

describe("fit() - member failures", {

  obj <- fit_obj(2L)
  obj$config$tuning$final_bayesian_iter <- 0L

  ## A member failure as fit_single_config() reports one. The message carries
  ## braces, which must reach the abort as text, not as a cli template.
  failed_member <- function(...) {

    cfg <- list(...)$config_row

    list(config_id = cfg$config_id, status = "failed",
         degraded = NA, degraded_reason = NA_character_,
         fitted_workflow = NULL, best_params = NULL,
         cv_predictions = NULL, test_metrics = NULL, cv_metrics = NULL,
         uq = NULL, ad = NULL, warnings = NULL,
         error_message = paste0("Warm-start tuning failed: {", cfg$model, "} diverged"),
         runtime_secs = 0)

  }

  it("aborts with horizons_error_all_members_failed when every member fails, naming the errors", {

    err <- testthat::with_mocked_bindings(
      tryCatch(
        suppressWarnings(
          fit(obj, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE)
        ),
        horizons_error_all_members_failed = function(e) e
      ),
      fit_single_config = failed_member,
      .package = "horizons"
    )

    expect_s3_class(err, "horizons_error_all_members_failed")

    msg <- gsub("\\s+", " ", conditionMessage(err))   # undo cli line wrapping
    expect_match(msg, "{rf} diverged", fixed = TRUE)
    expect_match(msg, "{cubist} diverged", fixed = TRUE)

    expect_s3_class(err$results, "tbl_df")
    expect_setequal(err$results$config_id, c("cfg_001", "cfg_002"))
    expect_true(all(err$results$status == "failed"))

    ## The metric and CV columns are present and double, though no member
    ## filled them (the fit validator holds the full column table, #203)
    metric_cols <- c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae",
                     "cv_rmse_mean", "cv_rmse_se", "cv_rpd_mean", "cv_rpd_se")
    expect_true(all(metric_cols %in% names(err$results)))
    expect_true(all(vapply(err$results[metric_cols], is.double, logical(1))))

  })

  it("keeps each member's error_message in models$results", {

    skip_unless_slow_tier()

    real_fit_single_config <- fit_single_config

    r <- testthat::with_mocked_bindings(
      suppressWarnings(
        fit(obj, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L)
      ),
      fit_single_config = function(...) {
        if (list(...)$config_row$config_id == "cfg_002") {
          failed_member(...)
        } else {
          real_fit_single_config(...)
        }
      },
      .package = "horizons"
    )

    res <- r$models$results

    expect_identical(res$error_message[res$config_id == "cfg_002"],
                     "Warm-start tuning failed: {cubist} diverged")
    expect_true(all(is.na(res$error_message[res$status == "success"])))

  })

})


## =========================================================================
## Each member is one unit
## =========================================================================
## The members are mocked (the fitted workflow is a stand-in, which the
## validator does not read), so each test pays only for fit()'s own draws.

describe("fit() - each member is one unit", {

  ## make_fit_object() with four evaluated configurations on two models, the
  ## copies tied with their originals, so ranking orders them rf, rf, cubist,
  ## cubist
  four_members <- function(models = c("rf", "cubist")) {

    obj  <- fit_obj(2L)
    more <- obj$config$configs
    more$config_id <- c("cfg_003", "cfg_004")

    res <- obj$evaluation$results
    add <- res
    add$config_id <- more$config_id

    obj$config$configs    <- dplyr::bind_rows(obj$config$configs, more)
    obj$config$configs$model <- rep(models, length.out = 4L)[order(c(1, 3, 2, 4))]
    obj$config$n_configs  <- 4L
    obj$evaluation$results <- dplyr::bind_rows(res, add)

    obj

  }

  fitted_member <- function(config_row, ...) {

    list(
      config_id = config_row$config_id, status = "success", degraded = FALSE,
      degraded_reason = NA_character_, fitted_workflow = "fitted workflow",
      best_params = tibble::tibble(mtry = 2L), warm_start = TRUE, start_grid_size = 2L,
      cv_predictions = NULL,
      test_metrics = tibble::tibble(rmse = 0.5, rrmse = 20, rsq = 0.6, ccc = 0.7, rpd = 1.5, mae = 0.4),
      cv_metrics = tibble::tibble(.metric = c("rmse", "rpd"), mean = c(0.45, 1.6), std_err = c(0.03, 0.1)),
      uq = NULL, ad = NULL, warnings = "The member's engine warned", error_message = NA_character_,
      class = NA_character_, trace = NULL, runtime_secs = 1
    )

  }

  failed_member <- function(config_row, ...) {

    list(
      config_id = config_row$config_id, status = "failed", degraded = NA,
      degraded_reason = NA_character_, fitted_workflow = NULL, best_params = NULL,
      warm_start = TRUE, start_grid_size = 2L, cv_predictions = NULL, test_metrics = NULL,
      cv_metrics = NULL, uq = NULL, ad = NULL, warnings = NULL,
      error_message = "Final model fit failed: the engine broke on 12 rows",
      class = "simpleError", trace = "the backtrace, as text", runtime_secs = 1
    )

  }

  it("records each member's failure class, backtrace and warnings, and warns partial_failure once", {

    obj <- fit_obj(2L)

    got <- collect_warnings(
      testthat::with_mocked_bindings(
        fit(obj, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L),
        fit_single_config = function(config_row, ...) {
          if (config_row$config_id == "cfg_002") failed_member(config_row) else fitted_member(config_row)
        },
        .package = "horizons"
      ),
      "."
    )

    res <- got$value$models$results

    expect_identical(res$class, c(NA_character_, "simpleError"))
    expect_identical(res$trace, list(NULL, "the backtrace, as text"))
    expect_identical(res$warnings[[1]], "The member's engine warned")

    partial <- Filter(function(w) inherits(w, "horizons_warning_partial_failure"), got$warnings)
    expect_length(partial, 1L)
    expect_match(conditionMessage(partial[[1]]), "1 of 2 members failed, so the fit holds the rest",
                 fixed = TRUE)
    expect_match(conditionMessage(partial[[1]]), "Final model fit failed: the engine broke", fixed = TRUE)

    log <- got$value$log[got$value$log$verb == "fit" & got$value$log$topic %in% "partial_failure", ]
    expect_identical(log$ids[[1]], "cfg_002")

  })

  it("passes debug and keep_traces to every member, and with debug runs them in this session", {

    obj  <- fit_obj(1L)
    seen <- list()
    out  <- utils::capture.output(
      ## rsample's own stratification notice is not under test
      fitted <- testthat::with_mocked_bindings(
        suppressWarnings(fit(obj, n_best = 1L,
                             compute_uq = FALSE, compute_ad = FALSE, seed = 42L,
                             control = control_horizons(allow_par = TRUE, debug = TRUE,
                                                        keep_traces = TRUE, verbose = TRUE))),
        fit_single_config = function(config_row, ..., allow_par, debug, keep_traces) {
          seen <<- list(allow_par = allow_par, debug = debug, keep_traces = keep_traces)
          fitted_member(config_row)
        },
        check_parallel_backend = function(...) stop("the backend was checked"),
        .package = "horizons"
      )
    )

    expect_identical(seen, list(allow_par = FALSE, debug = TRUE, keep_traces = TRUE))
    expect_true("│  Parallel: off, since debug runs in this session" %in% out)
    expect_s3_class(fitted, "horizons_fit")

  })

  it("stops early, with the members so far, when the first ones fail the same way across two models", {

    obj   <- four_members()
    calls <- 0L

    err <- expect_error(
      testthat::with_mocked_bindings(
        suppressWarnings(fit(obj, n_best = 4L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L)),
        fit_single_config = function(config_row, ...) {
          calls <<- calls + 1L
          failed_member(config_row)
        },
        .package = "horizons"
      ),
      class = "horizons_error_systemic"
    )

    expect_identical(calls, 3L)
    expect_identical(nrow(err$results), 3L)
    expect_match(conditionMessage(err), "The first 3 members to run all failed the same way, across 2 models",
                 fixed = TRUE)
    expect_match(conditionMessage(err), "`fit()` stopped rather than run the other 1", fixed = TRUE)

  })

  it("does not stop for failures on one model, nor when the breaker is off", {

    one_model <- four_members(models = "rf")

    expect_error(
      testthat::with_mocked_bindings(
        suppressWarnings(fit(one_model, n_best = 4L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L)),
        fit_single_config = failed_member, .package = "horizons"
      ),
      class = "horizons_error_all_members_failed"
    )

    expect_error(
      testthat::with_mocked_bindings(
        suppressWarnings(fit(four_members(), n_best = 4L, compute_uq = FALSE, compute_ad = FALSE,
                             seed = 42L, control = control_horizons(breaker = FALSE))),
        fit_single_config = failed_member, .package = "horizons"
      ),
      class = "horizons_error_all_members_failed"
    )

  })

  it("ends as all-failed rather than systemic when the breaker's run fills the last members", {

    ## Three members, all failing the same way across two models: nothing is
    ## left to save by stopping
    obj <- four_members()

    expect_error(
      testthat::with_mocked_bindings(
        suppressWarnings(fit(obj, n_best = 3L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L)),
        fit_single_config = failed_member, .package = "horizons"
      ),
      class = "horizons_error_all_members_failed"
    )

  })

  it("stops with the members so far when a parallel worker dies", {

    obj   <- fit_obj(2L)
    calls <- 0L

    err <- expect_error(
      testthat::with_mocked_bindings(
        suppressWarnings(fit(obj, n_best = 2L,
                             compute_uq = FALSE, compute_ad = FALSE, seed = 42L)),
        fit_single_config = function(config_row, ...) {
          calls <<- calls + 1L
          if (calls == 2L) {
            stop(structure(class = c("FutureInterruptError", "FutureError", "error", "condition"),
                           list(message = "worker died", call = NULL)))
          }
          fitted_member(config_row)
        },
        .package = "horizons"
      ),
      class = "horizons_error_systemic"
    )

    expect_match(conditionMessage(err), "A parallel worker stopped while `fit()` was running",
                 fixed = TRUE)
    expect_identical(err$results$config_id, "cfg_001")
    expect_s3_class(err$parent, "FutureError")

  })

  it("fails the member whose every out-of-fold fit failed and returns the other, rather than stopping fit() (#102)", {

    ## Real fits of the members, several seconds even without the search
    skip_unless_slow_tier()

    ## The cubist member's engine fails on every fold, but only while its
    ## out-of-fold predictions are made; the rf member is untouched. No
    ## Bayesian iterations: the search is not what is tested.
    obj <- fit_obj(2L)
    obj$config$tuning$final_bayesian_iter <- 0L

    in_oof <- FALSE
    real_cubist    <- Cubist::cubist
    real_resamples <- tune::fit_resamples
    local_mocked_bindings(cubist = function(...) {
      if (in_oof) stop("the engine broke on every fold")
      real_cubist(...)
    }, .package = "Cubist")
    local_mocked_bindings(fit_resamples = function(...) {
      in_oof <<- TRUE
      on.exit(in_oof <<- FALSE)
      real_resamples(...)
    }, .package = "tune")

    got <- collect_warnings(
      fit(obj, n_best = 2L, seed = 42L, compute_uq = FALSE, compute_ad = FALSE),
      "."
    )
    res <- got$value$models$results

    expect_identical(res$status, c("success", "failed"))
    expect_match(res$error_message[2],
                 "^OOF predictions failed: every out-of-fold fit failed \u2014 the engine broke on every fold")
    expect_identical(names(got$value$models$workflows), res$config_id[1])
    expect_true(any(vapply(got$warnings, inherits, logical(1), "horizons_warning_partial_failure")))

  })

  it("stops with the members so far when a worker dies during a warm-start Bayesian search, reported as a message or a warning", {

    ## Real fits of the members, several seconds even without the search
    skip_unless_slow_tier()

    ## tune catches future's error inside its search; future.apply reports it
    ## first, as a warning before 1.21.0 and as a message since. The first
    ## member's search returns without an iteration, the second one's worker
    ## dies.
    obj <- fit_obj(2L)

    for (report in list(message, warning)) {

      calls <- 0L
      local_mocked_bindings(tune_bayes = function(...) {
        calls <<- calls + 1L
        if (calls == 2L) report("Caught FutureInterruptError. Canceling all iterations ...")
        list(...)$initial
      }, .package = "tune")

      err <- expect_error(
        suppressWarnings(fit(obj, n_best = 2L,
                             compute_uq = FALSE, compute_ad = FALSE, seed = 42L)),
        class = "horizons_error_systemic"
      )

      expect_match(conditionMessage(err), "A parallel worker stopped while `fit()` was running",
                   fixed = TRUE)
      expect_identical(err$results$config_id, "cfg_001")
      expect_s3_class(err$parent, "horizons_error_systemic")
      expect_match(conditionMessage(err$parent),
                   "A parallel worker stopped during the warm-start Bayesian search.", fixed = TRUE)

    }

  })

  it("stops at a Ctrl-C during a warm-start Bayesian search, rather than keeping the iterations before it", {

    ## Real fits of the members, several seconds even without the search
    skip_unless_slow_tier()

    ## tune's on.exit() return cancels the interrupt's unwind, so without the
    ## record the search's first iteration came back as the member's result
    obj          <- fit_obj(1L)
    real_pred_gp <- tune:::pred_gp
    calls        <- 0L
    local_mocked_bindings(pred_gp = function(...) {
      calls <<- calls + 1L
      if (calls == 2L) rlang::interrupt()
      real_pred_gp(...)
    }, .package = "tune")

    got <- tryCatch(
      suppressWarnings(fit(obj, n_best = 1L,
                           compute_uq = FALSE, compute_ad = FALSE, seed = 42L)),
      interrupt = function(i) "interrupted"
    )

    expect_identical(got, "interrupted")

  })

  it("carries a members table with every column and no rows when the first member's worker dies", {

    obj <- fit_obj(2L)

    err <- expect_error(
      testthat::with_mocked_bindings(
        suppressWarnings(fit(obj, n_best = 2L,
                             compute_uq = FALSE, compute_ad = FALSE, seed = 42L)),
        fit_single_config = function(config_row, ...) {
          stop(structure(class = c("FutureInterruptError", "FutureError", "error", "condition"),
                         list(message = "worker died", call = NULL)))
        },
        .package = "horizons"
      ),
      class = "horizons_error_systemic"
    )

    one <- member_results(list(fitted_member(list(config_id = "cfg_001"))))

    expect_identical(nrow(err$results), 0L)
    expect_identical(names(err$results), names(one))
    expect_identical(vapply(err$results, typeof, character(1)), vapply(one, typeof, character(1)))

  })

  it("refuses a parallel run its workers cannot match, before anything is drawn", {

    obj <- fit_obj(1L)

    run <- function(...) {
      testthat::with_mocked_bindings(
        fit(obj, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L,
            control = control_horizons(allow_par = TRUE)),
        check_parallel_backend = function(...) TRUE,
        draw_stratified        = function(...) stop("a draw was made"),
        fit_single_config      = function(...) stop("a member was fitted"),
        ...,
        .package = "horizons"
      )
    }

    expect_error(run(running_under_load_all = function() TRUE),
                 "Parallel `fit()` cannot run under `devtools::load_all()`", fixed = TRUE,
                 class = "horizons_error_dependency")

    expect_error(run(running_under_load_all = function() FALSE,
                     worker_horizons_version = function() "0.0.1"),
                 "A parallel worker loads horizons 0.0.1", fixed = TRUE,
                 class = "horizons_error_dependency")

  })

})
