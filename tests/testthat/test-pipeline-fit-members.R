## ---------------------------------------------------------------------------
## Tests: fit() and the members it takes
## ---------------------------------------------------------------------------
## Which members fit() takes from an evaluation when none passed the prune
## gate or all fell below it (#38), and what it does when members fail. The
## blocks read their fit objects from fit_obj(), below, or build their own;
## make_fit_object() is in helper-fixtures.R.

## The fit objects the member tests read, built once per process
## (helper-memo.R). Each test reads its object inside its own it() block,
## before anything is mocked, so no build runs under a mock.
fit_obj <- function(n_configs) {
  memo_fixture("members_fit_obj", make_fit_object, n = 60, n_configs = n_configs)
}

## The configuration of `obj` that fits `model`
member_of <- function(obj, model) {
  obj$config$configs$config_id[obj$config$configs$model == model]
}

## fit_obj(2L) with `n` evaluated configurations, its two repeated, ranked in
## config_id order, whose models are `models` repeated in that order: rf,
## cubist, rf, cubist, ... by default. Read inside an it() block, before
## anything is mocked.
ranked_members <- function(n, models = c("rf", "cubist")) {

  obj  <- fit_obj(2L)
  ids  <- sprintf("cfg_%03d", seq_len(n))
  from <- rep(obj$config$configs$config_id, length.out = n)

  configs <- obj$config$configs[match(from, obj$config$configs$config_id), ]
  results <- obj$evaluation$results[match(from, obj$evaluation$results$config_id), ]

  configs$config_id <- results$config_id <- ids
  configs$model     <- rep(models, length.out = n)

  obj$config$configs     <- configs
  obj$config$n_configs   <- as.integer(n)
  obj$evaluation$results <- results

  rank_configs_as(obj, ids)

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

  ## Rows with no reading (NA, as prune = FALSE writes them): a pruned row is
  ## below the threshold all the same, since the gate pruned it, and a
  ## success is not. A member with no cross-validated RPD is listed as NA.
  it("counts a pruned member without a reading as below the threshold, and a success without one as not", {

    unread <- tibble::tibble(config_id = c("cfg_001", "cfg_002"), cv_rpd = c(0.93, NA_real_),
                             below_prune_threshold = NA, prune_threshold = NA_real_)

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
    expect_match(log$message, "Cross-validated RPD: cfg_001 0.93, cfg_002 NA.", fixed = TRUE)
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

  ## Two members, no Bayesian iterations; read inside each it() block
  member_obj <- function() {
    obj <- fit_obj(2L)
    obj$config$tuning$final_bayesian_iter <- 0L
    obj
  }

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

    obj <- member_obj()

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

    obj <- member_obj()

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

  ## Four members, ranked in config_id order: rf, cubist, rf, cubist by
  ## default (ranked_members())
  four_members <- function(models = c("rf", "cubist")) ranked_members(4L, models)

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

  it("passes no_improve to every member's re-tune, keeps each member's search record, and says how many stopped early", {

    obj  <- fit_obj(2L)
    obj$config$tuning$final_bayesian_iter <- 25L
    seen <- list()
    out  <- utils::capture.output(
      fitted <- testthat::with_mocked_bindings(
        suppressWarnings(fit(obj, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L,
                             control = control_horizons(no_improve = 3L, verbose = TRUE))),
        fit_single_config = function(config_row, ..., no_improve) {
          seen[[config_row$config_id]] <<- no_improve
          member <- fitted_member(config_row)
          member$bayes_iter <- 3L
          member$bayes_stop <- if (config_row$config_id == "cfg_001") "no_improve" else "budget"
          member
        },
        .package = "horizons"
      )
    )

    expect_identical(unname(unlist(seen)), c(3L, 3L))

    res <- fitted$models$results
    expect_identical(res$bayes_iter, c(3L, 3L))
    expect_identical(res$bayes_stop[res$config_id == "cfg_001"], "no_improve")
    expect_identical(res$bayes_stop[res$config_id == "cfg_002"], "budget")

    line <- paste0("The re-tune's Bayesian search stopped early in 1 of the 2 members it ran in, after 3 ",
                   "iterations without improvement; `control_horizons(no_improve = FALSE)` runs every ",
                   "iteration, and a larger number waits longer.")
    expect_true(paste0("\u2502  \u251C\u2500 ", line) %in% out)
    expect_true(line %in% fitted$log$message[fitted$log$verb == "fit" & fitted$log$type == "note"])

  })

  it("says per member whether its re-tune kept evaluate()'s hyperparameters, and counts those that did among the members with them to keep", {

    ## Three members: one kept evaluate()'s parameters, one moved off them,
    ## and one had none to keep (its re-tune chose with no incumbent)
    obj   <- four_members()
    obj$config$tuning$final_bayesian_iter <- 25L
    calls <- 0L
    out   <- utils::capture.output(
      fitted <- testthat::with_mocked_bindings(
        suppressWarnings(fit(obj, n_best = 3L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L,
                             control = control_horizons(verbose = TRUE))),
        fit_single_config = function(config_row, ...) {
          calls  <<- calls + 1L
          member <- fitted_member(config_row)
          member$retune_moved <- c(FALSE, TRUE, NA)[calls]
          member$retune_gain  <- c(0.4, 1.8, NA)[calls]
          member
        },
        .package = "horizons"
      )
    )

    expect_identical(fitted$models$results$retune_moved, c(FALSE, TRUE, NA))

    expect_identical(sum(grepl("Re-tune: kept evaluate()'s hyperparameters; the best candidate was better by 0.4 of its standard errors",
                               out, fixed = TRUE)), 1L)
    expect_identical(sum(grepl("Re-tune: moved to a candidate better by 1.8 of its standard errors",
                               out, fixed = TRUE)), 1L)
    expect_identical(sum(grepl("\u251C\u2500 Re-tune:", out, fixed = TRUE)), 2L)

    line <- paste0("The re-tune kept evaluate()'s hyperparameters for 1 of 2 members: the best candidate replaces ",
                   "them only when its CV RMSE is lower by more than its standard error, or when they were not ",
                   "scored on every fold.")
    expect_true(paste0("\u2502  \u251C\u2500 ", line) %in% out)
    expect_true(line %in% fitted$log$message[fitted$log$verb == "fit" & fitted$log$type == "note"])

  })

  it("names the members gridded whole in its header, in place of a search they never run", {

    skip_if_not_installed("earth")

    ## rf searches; plsr's components are gridded whole, one fit a fold, and
    ## so is mars's space, two fits. At one iteration the floor raises rf's
    ## budget and would raise mars's, which never searches.
    obj <- four_members(c("rf", "plsr"))
    obj$config$configs$model[obj$config$configs$config_id == "cfg_004"] <- "mars"
    obj$config$tuning$final_bayesian_iter <- 1L
    out <- utils::capture.output(
      testthat::with_mocked_bindings(
        suppressWarnings(fit(obj, n_best = 4L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L,
                             control = control_horizons(verbose = TRUE))),
        fit_single_config = function(config_row, ...) fitted_member(config_row),
        .package = "horizons"
      )
    )

    expect_identical(
      grep("Re-tune: ", out, value = TRUE, fixed = TRUE)[1],
      paste0("\u2502  Re-tune: from evaluate()'s hyperparameters and d points drawn around them for d tuned ",
             "parameters, 1 iterations (raised to 17 for rf: at least 10 points a tuned parameter); ",
             "plsr and mars gridded whole; ",
             "evaluate()'s are kept unless the best candidate is better by more than its standard error")
    )

    ## Every member gridded whole: no search's words at all
    expect_identical(retune_header_line(25L, NULL, character(0), "plsr"),
                     paste0("Re-tune: plsr gridded whole; evaluate()'s are kept unless the best candidate is ",
                            "better by more than its standard error"))

  })

  it("never prints a moved member's lead at or below the margin, nor a kept member's above it", {

    ## A lead just past one standard error rounds to 1 at two digits, which
    ## would read as a lead the margin keeps
    expect_identical(retune_member_line(TRUE, 1.004),
                     "Re-tune: moved to a candidate better by more than 1 of its standard errors")
    expect_identical(retune_member_line(TRUE, 1.8),
                     "Re-tune: moved to a candidate better by 1.8 of its standard errors")
    expect_identical(retune_member_line(FALSE, 0.996),
                     "Re-tune: kept evaluate()'s hyperparameters; the best candidate was better by 1 of its standard errors")
    expect_identical(retune_member_line(FALSE, 1),
                     "Re-tune: kept evaluate()'s hyperparameters; the best candidate was better by 1 of its standard errors")

  })

  it("says once, in its header and on its log, that the re-tune is off at final_bayesian_iter = 0, the default, and counts no member as kept", {

    obj <- fit_obj(2L)
    obj$config$tuning$final_bayesian_iter <- 0L

    out <- utils::capture.output(
      fitted <- testthat::with_mocked_bindings(
        suppressWarnings(fit(obj, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L,
                             control = control_horizons(verbose = TRUE))),
        fit_single_config = function(config_row, ...) {
          member <- fitted_member(config_row)
          member$retune_moved <- FALSE
          member
        },
        .package = "horizons"
      )
    )

    off <- paste0("off (`final_bayesian_iter = 0`, the default), so each member keeps evaluate()'s ",
                  "hyperparameters; `configure(final_bayesian_iter = )` above 0 turns it on")

    expect_true(paste0("\u2502  Re-tune: ", off) %in% out)
    expect_false(any(grepl("\u251C\u2500 Re-tune:", out, fixed = TRUE)))
    expect_false(any(grepl("The re-tune kept", out, fixed = TRUE)))

    notes <- fitted$log$message[fitted$log$verb == "fit" & fitted$log$type == "note"]
    expect_identical(sum(notes == paste0("The re-tune is ", off, ".")), 1L)
    expect_false(any(grepl("The re-tune kept", notes, fixed = TRUE)))

  })

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

    ## Each member read by the configuration it is, whichever ranked first
    res    <- got$value$models$results
    failed <- res[res$config_id == "cfg_002", ]
    fitted <- res[res$config_id == "cfg_001", ]

    expect_identical(c(fitted$class, failed$class), c(NA_character_, "simpleError"))
    expect_identical(c(fitted$trace, failed$trace), list(NULL, "the backtrace, as text"))
    expect_identical(fitted$warnings[[1]], "The member's engine warned")

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

  it("does not stop for failures on one of the members' models, nor when the breaker is off", {

    ## Ranked rf, rf, rf, cubist: the first three fail on one model, and
    ## the one on another is the last
    one_model <- four_members(models = c("rf", "rf", "rf", "cubist"))

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

  it("stops with the members so far when the daemons fall across a member", {

    obj    <- fit_obj(2L)
    ran    <- character(0)
    fallen <- FALSE

    err <- expect_error(
      testthat::with_mocked_bindings(
        suppressWarnings(fit(obj, n_best = 2L,
                             compute_uq = FALSE, compute_ad = FALSE, seed = 42L,
                             control = control_horizons(allow_par = TRUE))),
        check_parallel_backend = function(...) TRUE,
        check_parallel_workers = function(...) invisible(NULL),
        connected_daemons      = function() if (fallen) 1L else 2L,
        fit_single_config      = function(config_row, ...) {
          ran <<- c(ran, config_row$config_id)
          if (length(ran) == 2L) fallen <<- TRUE
          fitted_member(config_row)
        },
        .package = "horizons"
      ),
      class = "horizons_error_systemic"
    )

    msg <- gsub("\\s+", " ", conditionMessage(err))
    expect_match(msg, "A mirai daemon stopped while `fit()` was running", fixed = TRUE)
    expect_match(msg, "Fewer mirai daemons were connected after the work than before it", fixed = TRUE)

    ## The rows so far are the member that finished before the daemons fell
    expect_identical(err$results$config_id, ran[1])
    expect_null(err$parent)

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

    ## Fitted in this session, where the mock is: an isolated fit's process
    ## would fit with the real engine
    got <- collect_warnings(
      fit(obj, n_best = 2L, seed = 42L, compute_uq = FALSE, compute_ad = FALSE,
          control = control_horizons(isolate = FALSE)),
      "."
    )
    ## Each member read by its model, whichever ranked first
    res    <- got$value$models$results
    rf     <- res[res$config_id == member_of(obj, "rf"), ]
    cubist <- res[res$config_id == member_of(obj, "cubist"), ]

    expect_identical(c(rf$status, cubist$status), c("success", "failed"))
    expect_match(cubist$error_message,
                 "^OOF predictions failed: every out-of-fold fit failed \u2014 the engine broke on every fold")
    expect_identical(names(got$value$models$workflows), rf$config_id)
    expect_true(any(vapply(got$warnings, inherits, logical(1), "horizons_warning_partial_failure")))

  })

  it("fails the member whose isolated final fit's process dies, outside tune, and returns the other", {

    ## Real fits of the members
    skip_unless_slow_tier()
    skip_on_os("windows")

    obj <- fit_obj(2L)
    obj$config$tuning$final_bayesian_iter <- 0L

    ## The cubist member's process dies on its final fit, the one fit made
    ## outside tune's calls
    in_tune     <- 0L
    real_grid   <- tune::tune_grid
    real_resamp <- tune::fit_resamples
    inside <- function(f) function(...) {
      in_tune <<- in_tune + 1L
      on.exit(in_tune <<- in_tune - 1L)
      f(...)
    }
    local_mocked_bindings(tune_grid = inside(real_grid), fit_resamples = inside(real_resamp),
                          .package = "tune")
    local_dying_fits(function(i) in_tune == 0L)

    before <- connected_daemons()
    got    <- with_deadline(collect_warnings(
      fit(obj, n_best = 2L, seed = 42L, compute_uq = FALSE, compute_ad = FALSE),
      "."
    ), 300)
    ## Each member read by its model, whichever ranked first
    res    <- got$value$models$results
    rf     <- res[res$config_id == member_of(obj, "rf"), ]
    cubist <- res[res$config_id == member_of(obj, "cubist"), ]

    expect_identical(c(rf$status, cubist$status), c("success", "failed"))
    expect_identical(cubist$class, "horizons_error_process_died")
    expect_match(cubist$error_message, paste("^Final model fit failed:", PROCESS_DIED, "a Cubist fit"))
    expect_identical(names(got$value$models$workflows), rf$config_id)
    expect_identical(connected_daemons(), before)

  })

  it("stops with the members so far when a daemon dies during a warm-start Bayesian search, which tune cuts short quietly", {

    ## Real fits of the members, several seconds even without the search
    skip_unless_slow_tier()

    ## A daemon that dies inside an iteration makes tune stop the search and
    ## return what it had ("Optimization stopped prematurely"), no error: so
    ## the second member's search returns its initial grid as the daemons
    ## fall, and the member is never kept.
    obj    <- fit_obj(2L)
    obj$config$tuning$final_bayesian_iter <- 25L
    calls  <- 0L
    fallen <- FALSE

    local_mocked_bindings(tune_bayes = function(...) {
      calls <<- calls + 1L
      if (calls == 2L) fallen <<- TRUE
      list(...)$initial
    }, .package = "tune")
    local_mocked_bindings(
      check_parallel_backend = function(...) TRUE,
      check_parallel_workers = function(...) invisible(NULL),
      connected_daemons      = function() if (fallen) 1L else 2L
    )

    err <- expect_error(
      suppressWarnings(fit(obj, n_best = 2L,
                           compute_uq = FALSE, compute_ad = FALSE, seed = 42L,
                           control = control_horizons(allow_par = TRUE))),
      class = "horizons_error_systemic"
    )

    msg <- gsub("\\s+", " ", conditionMessage(err))
    expect_match(msg, "A mirai daemon stopped while `fit()` was running", fixed = TRUE)
    expect_match(msg, "Fewer mirai daemons were connected", fixed = TRUE)

    ## The rows so far are the first member fit() ran, the best ranked
    expect_identical(err$results$config_id,
                     rank_configs_by_cv(obj$evaluation$results, obj$evaluation$rank_metric)$config_id[1])

  })

  it("stops at a Ctrl-C during a warm-start Bayesian search, rather than keeping the iterations before it", {

    ## One member: its grid and one Bayesian iteration run before the
    ## interrupt, about two seconds. tune's on.exit() return cancels the
    ## interrupt's unwind, so without the record the search's first iteration
    ## came back as the member's result.
    ##
    ## The warm start is centred on a min_n the 32 rows a fold trains on can
    ## split: around evaluate()'s min_n of 40, every starting candidate
    ## predicts a constant, their scores tie, and tune's search stops before
    ## its first iteration.
    obj          <- fit_obj(1L)
    obj$config$tuning$final_bayesian_iter <- 25L
    obj$evaluation$results$best_params[[1]]$min_n <- 5L
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

  it("carries a members table with every column and no rows when the daemons fall across the first member", {

    obj    <- fit_obj(2L)
    fallen <- FALSE

    err <- expect_error(
      testthat::with_mocked_bindings(
        suppressWarnings(fit(obj, n_best = 2L,
                             compute_uq = FALSE, compute_ad = FALSE, seed = 42L,
                             control = control_horizons(allow_par = TRUE))),
        check_parallel_backend = function(...) TRUE,
        check_parallel_workers = function(...) invisible(NULL),
        connected_daemons      = function() if (fallen) 1L else 2L,
        fit_single_config      = function(config_row, ...) {
          fallen <<- TRUE
          stop("Argument 1 must be a data frame or a named atomic vector.")
        },
        .package = "horizons"
      ),
      class = "horizons_error_systemic"
    )

    ## Chained to the member's error
    expect_match(conditionMessage(err$parent), "Argument 1 must be a data frame", fixed = TRUE)

    one <- member_results(list(fitted_member(list(config_id = "cfg_001"))))

    expect_identical(nrow(err$results), 0L)
    expect_identical(names(err$results), names(one))
    expect_identical(vapply(err$results, typeof, character(1)), vapply(one, typeof, character(1)))

  })

  it("carries a members table with every column and no rows when no daemon is left during the check at the door", {

    skip_on_cran()

    ## The check's task is one no daemon takes, and none is connected
    obj  <- fit_obj(1L)
    task <- local_unresolved_task()
    local_mocked_bindings(everywhere = function(...) structure(list(task), class = "mirai_map"),
                          .package = "mirai")

    err <- expect_error(
      testthat::with_mocked_bindings(
        with_deadline(suppressWarnings(fit(obj, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L,
                                           control = control_horizons(allow_par = TRUE))), 30),
        running_under_load_all = function() FALSE,
        check_parallel_backend = function(...) TRUE,
        draw_stratified        = function(...) stop("a draw was made"),
        .package = "horizons"
      ),
      class = "horizons_error_no_daemons"
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
                     daemon_reports          = function(...) daemons_on("0.0.1")),
                 "A mirai daemon loads horizons 0.0.1", fixed = TRUE,
                 class = "horizons_error_dependency")

  })

})


## =========================================================================
## The packages the members need, checked at the door
## =========================================================================
## A missing engine used to fail every member that needed it, one by one, as
## run failures; it is refused before any member is fitted, for the members
## alone.

describe("fit() - the members' packages", {

  it("refuses, before any member is fitted, a member whose engine is not installed, and checks only the members", {

    ## Read the fixture before anything is mocked (helper-memo.R)
    x <- fit_obj(2)
    x$evaluation$results$cv_rpd <- c(2, 1)   # cfg_001 (rf) ranks first

    local_mocked_bindings(
      fit_single_config = function(...) rlang::abort("reached", class = "member_reached")
    )

    ## cubist's engine is installed nowhere
    local_model_specs(list(cubist = list(packages = c("rules", "horizonsFakePkgXYZ123"))))

    err <- expect_error(fit(x, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE),
                        class = "horizons_error_missing_package")

    expect_s3_class(err, "horizons_error_dependency")
    expect_identical(rlang::call_name(err$call), "fit")
    msg <- gsub("\\s+", " ", conditionMessage(err))
    expect_match(msg, "`fit()` needs 1 package that is not installed.", fixed = TRUE)
    expect_match(msg, "horizonsFakePkgXYZ123, needed for cubist", fixed = TRUE)

    ## The one member, rf, needs nothing missing, so it is fitted
    expect_error(fit(x, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE),
                 class = "member_reached")

  })

  it("refuses, before any member is fitted, a recorded range a member's parameter cannot take, typing only the members", {

    skip_if_not_installed("Cubist")

    ## Read the fixture before anything is mocked (helper-memo.R)
    x <- fit_obj(2)
    x$evaluation$results$cv_rpd <- c(2, 1)   # cfg_001 (rf) ranks first

    ## A range configure() could not type had rules not been installed then
    x$config$tuning$ranges$committees <- c(1.5, 25)

    local_mocked_bindings(
      fit_single_config = function(...) rlang::abort("reached", class = "member_reached")
    )

    err <- expect_error(fit(x, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE),
                        class = "horizons_error_validation")

    expect_identical(rlang::call_name(err$call), "fit")
    expect_match(err$findings, "committees.* is not a range of cubist's .*committees")

    ## The one member, rf, does not tune committees, so it is fitted
    expect_error(fit(x, n_best = 1L, compute_uq = FALSE, compute_ad = FALSE),
                 class = "member_reached")

  })

  it("stops as systemic, with the members so far, when a member's package is missing where it ran", {

    x <- fit_obj(2)
    x$evaluation$results$cv_rpd <- c(2, 1)   # cfg_001 (rf) is fitted first

    ## cubist's engine cannot be loaded where its member is fitted, though
    ## the door found it in this session, and typed the ranges on its
    ## parameters there (check_range_types())
    real_spec <- define_model_spec
    local_mocked_bindings(define_model_spec = function(model, ...) {
      at_door <- any(vapply(sys.calls(), function(cl) identical(cl[[1]], quote(range_type_findings)), logical(1)))
      if (model == "cubist" && !at_door) loadNamespace("horizonsFakePkgXYZ123")
      real_spec(model, ...)
    })

    err <- expect_error(
      suppressWarnings(fit(x, n_best = 2L, compute_uq = FALSE, compute_ad = FALSE, seed = 42L)),
      class = "horizons_error_systemic"
    )

    expect_identical(class(err)[1], "horizons_error_systemic")
    expect_s3_class(err$parent, "horizons_error_missing_package")
    expect_identical(err$results$config_id, "cfg_001")
    expect_identical(err$results$status, "success")
    expect_match(gsub("\\s+", " ", conditionMessage(err)),
                 "A package `fit()` needs is not installed where its work ran", fixed = TRUE)

  })

})


## =========================================================================
## A member's fit as a task: fit_member_worker()
## =========================================================================
## What the members axis sends a daemon for each member (#60), called here in
## this session: it needs no daemon, so these run under load_all() too, but
## for the split member, whose inner daemons load the installed horizons. The
## task carries its member's data, as fit()'s fold preparations' tasks do,
## since fit() has no output_dir to write a payload file to.

## One member, rf on PCA, so the task carries fold preparations and the
## training part's that bounds mtry, with UQ and AD on, so it carries the
## calibration split's positions too: what fit() hands it and what it
## returns, recorded as it is fitted in this session, its calibration split,
## and the task fit() would send for it, through a serialization round trip
## as mirai sends it
build_member_task <- function() {

  obj <- make_fit_object(n = 100, n_configs = 1, feature_selection = "pca")

  seen       <- NULL
  split_C    <- NULL
  real_fit   <- fit_member
  real_parts <- calibration_parts

  fitted <- testthat::with_mocked_bindings(
    suppressWarnings(fit(obj, n_best = 1L, seed = 42L, control = control_horizons(min_calib_rows = 10))),
    fit_member = function(config_row, best_params_eval, fold_entries, inputs, settings, ...) {
      value <- real_fit(config_row, best_params_eval, fold_entries, inputs, settings, ...)
      seen  <<- list(config_row = config_row, best_params = best_params_eval, fold_entries = fold_entries,
                     inputs = inputs, settings = settings, value = value)
      value
    },
    calibration_parts = function(split, ...) {
      split_C <<- split
      real_parts(split, ...)
    }
  )

  shared  <- member_payload(seen$inputs$split_F, NULL, seen$inputs$cv_resamples, split_C, seen$inputs$role_map,
                            seen$settings)
  payload <- unserialize(serialize(member_task_payload(shared, seen$config_row, seen$best_params,
                                                       seen$fold_entries), NULL))

  list(fitted = fitted, seen = seen, split_C = split_C, payload = payload)

}

member_task <- function() memo_fixture("member_task", build_member_task)

describe("fit_member_worker() - a member's fit as a task", {

  it("keeps its body out of fit()'s frame and takes its inputs as arguments", {

    expect_true(isNamespace(environment(fit_member_worker)))
    expect_named(formals(fit_member_worker), c("payload", "inner"))

  })

  it("fits its member as this session fits it, its intervals and its domain among them, and keeps a warning raised outside the member's unit", {

    member <- member_task()
    seen   <- member$seen

    expect_fits_nontrivial(member$fitted)
    expect_false(is.null(seen$fold_entries))
    expect_false(is.null(member$split_C))
    expect_setequal(names(member$payload), FIT_ARG_NAMES)

    ## A package's warning as the daemon loads the member's packages, which
    ## mirai would drop
    real_prepare <- prepare_worker
    local_mocked_bindings(prepare_worker = function(...) {
      warning("a package warned as it loaded")
      real_prepare(...)
    })

    task <- fit_member_worker(member$payload)

    expect_identical(attr(task, "inner_daemons"), list(daemons = 1L, started = FALSE, completed = 0L))
    expect_true(any(grepl("a package warned as it loaded", task$warnings, fixed = TRUE)))
    expect_true(all(seen$value$warnings %in% task$warnings))

    same <- setdiff(names(seen$value), c("fitted_workflow", "warnings", "uq", "ad", "runtime_secs"))

    expect_identical(task[same], seen$value[same])
    expect_equal(task$uq, seen$value$uq)
    expect_equal(task$ad, seen$value$ad)

    test_rows <- rsample::testing(seen$inputs$split_F)
    expect_identical(predict(task$fitted_workflow, test_rows), predict(seen$value$fitted_workflow, test_rows))

  })

  it("fits a split member's folds on its inner daemons as an unsplit one's on its own, and counts their tasks (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    ## The worker in this session, which stands in for its daemon
    mirai::daemons(0)
    member <- member_task()

    split   <- with_deadline(fit_member_worker(member$payload, inner = 3L), 120)
    unsplit <- with_deadline(fit_member_worker(member$payload, inner = 1L), 120)

    ## The member's folds went to its inner daemons, tune sending a call's
    ## three folds together, so the count is a multiple of three, which the
    ## inner daemons' three set-up tasks would not leave it
    expect_true(attr(split, "inner_daemons")$started)
    expect_gte(attr(split, "inner_daemons")$completed, 3L)
    expect_identical(attr(split, "inner_daemons")$completed %% 3L, 0L)
    expect_identical(attr(unsplit, "inner_daemons"), list(daemons = 1L, started = FALSE, completed = 0L))
    expect_false(mirai::daemons_set())

    same <- setdiff(names(unsplit), c("fitted_workflow", "uq", "ad", "runtime_secs"))
    attr(split, "inner_daemons") <- attr(unsplit, "inner_daemons") <- NULL

    expect_identical(split[same], unsplit[same])
    expect_equal(split$uq, unsplit$uq)
    expect_equal(split$ad, unsplit$ad)

    test_rows <- rsample::testing(member$seen$inputs$split_F)
    expect_identical(predict(split$fitted_workflow, test_rows), predict(unsplit$fitted_workflow, test_rows))

  })

  it("refuses a payload with a key missing, and a worker on another version, as the configuration worker does", {

    good <- stats::setNames(vector("list", length(FIT_ARG_NAMES)), FIT_ARG_NAMES)
    good$pkg_version <- as.character(utils::packageVersion("horizons"))

    err <- expect_error(fit_member_worker(good[setdiff(names(good), "settings")]),
                        "Malformed worker payload", class = "horizons_error_internal")
    expect_match(conditionMessage(err), "Missing: settings; unexpected: -", fixed = TRUE)

    ## The version first: another version's keys may differ, and that is
    ## not a bug
    other <- good[setdiff(names(good), "settings")]
    other$pkg_version <- "0.0.0"

    expect_error(fit_member_worker(other), "but this session runs 0.0.0.", fixed = TRUE,
                 class = "horizons_error_dependency")

  })

})


## =========================================================================
## The members axis: each member a task of its own (#60)
## =========================================================================
## fit()'s members on the daemons, through the window evaluate()'s configs
## axis runs through. The tests in this session run the real window with
## each task computed as it is sent (local_tasks_in_session(),
## helper-parallel.R); those on daemons need an installed build.

## Three members of three kinds: rf on PCA (a seeded engine, with fold
## preparations and the training part's prep that bounds mtry), plsr (a
## submodel engine) and cubist (each fit in a process of its own). Read
## inside each it() block, before anything is mocked.
members_axis_obj <- function() {
  memo_fixture("members_axis_obj", make_fit_object, n = 100, n_configs = 3,
               models = c("rf", "plsr", "cubist"), feature_selection = c("pca", "none", "none"))
}

## UQ and AD on, at a calibration minimum these rows meet
members_control <- function(...) control_horizons(min_calib_rows = 10, ...)

## The parts of two fits of the same members that must be identical
## whatever axis they ran on: the members table without its run times, the
## out-of-fold predictions, each member's intervals and domain, the chosen
## member, and what predict() serves from them
expect_same_members <- function(got, want) {

  keep <- setdiff(names(want$models$results), "runtime_secs")

  expect_identical(got$models$results[keep], want$models$results[keep])
  expect_identical(got$models$best_config, want$models$best_config)
  expect_identical(got$models$cv_predictions, want$models$cv_predictions)
  expect_equal(got$models$uq, want$models$uq)
  expect_equal(got$models$ad, want$models$ad)

  new_rows <- rsample::testing(rebuild_split(want)$split)
  expect_identical(suppressWarnings(predict(got, new_rows)), suppressWarnings(predict(want, new_rows)))

}

describe("fit() - the members axis (#60)", {

  it("fits every member as this session does, each a task of its own, and keeps them in rank order when they finish out of it", {

    skip_if_not_installed("Cubist")

    obj     <- members_axis_obj()
    in_turn <- suppressWarnings(fit(obj, n_best = 3L, seed = 42L, control = members_control()))

    ## The members compared learned something, so a difference would show
    expect_fits_nontrivial(in_turn)

    ## Costs rising with rank, so the window sends the last-ranked member
    ## first and the best last: collected out of rank
    local_tasks_in_session(2L)
    local_mocked_bindings(config_costs = function(configs, n_rows) as.numeric(seq_len(nrow(configs))))

    sent <- character(0)
    real_worker <- fit_member_worker
    local_mocked_bindings(fit_member_worker = function(payload, inner = 1L) {
      sent <<- c(sent, payload$config_row$config_id)
      real_worker(payload, inner)
    })

    as_tasks <- suppressWarnings(fit(obj, n_best = 3L, seed = 42L,
                                     control = members_control(allow_par = TRUE, parallelize_over = "configs")))

    expect_identical(sent, rev(in_turn$models$results$config_id))
    expect_same_members(as_tasks, in_turn)

    ## The run's record: the axis and the daemons it began with
    expect_identical(as_tasks$models[c("parallelize_over", "workers")], list(parallelize_over = "configs", workers = 2L))
    expect_identical(in_turn$models[c("parallelize_over", "workers")], list(parallelize_over = "sequential", workers = 1L))

    ## Nothing the members say is lost on the way
    expect_identical(without_time(log_rows_added(obj, as_tasks)), without_time(log_rows_added(obj, in_turn)))

  })

  ## A member as fit_single_config() returns one, fitted or failed, for the
  ## tests of what fit() does with them; the fitted workflow is a stand-in,
  ## which nothing past the validator reads
  stand_in <- function(config_row, failed = FALSE) {
    list(
      config_id = config_row$config_id, status = if (failed) "failed" else "success",
      degraded = if (failed) NA else FALSE, degraded_reason = NA_character_,
      fitted_workflow = if (!failed) "fitted workflow", best_params = if (!failed) tibble::tibble(mtry = 2L),
      warm_start = TRUE, start_grid_size = NA_integer_, cv_predictions = NULL,
      test_metrics = if (!failed) tibble::tibble(rmse = 0.5, rrmse = 20, rsq = 0.6, ccc = 0.7, rpd = 1.5, mae = 0.4),
      cv_metrics = if (!failed) tibble::tibble(.metric = c("rmse", "rpd"), mean = c(0.45, 1.6), std_err = c(0.03, 0.1)),
      uq = NULL, ad = NULL, warnings = NULL,
      error_message = if (failed) "Final model fit failed: the engine broke on 12 rows" else NA_character_,
      class = if (failed) "simpleError" else NA_character_, trace = NULL, runtime_secs = 1
    )
  }

  it("takes the members axis on auto by evaluate()'s rule: once there are as many members as folds", {

    obj <- members_axis_obj()

    local_tasks_in_session(2L)
    local_mocked_bindings(fit_single_config = function(config_row, ...) stand_in(config_row))

    axis_at <- function(n_best) {
      suppressWarnings(fit(obj, n_best = n_best, seed = 42L, compute_uq = FALSE, compute_ad = FALSE,
                           control = control_horizons(allow_par = TRUE)))$models$parallelize_over
    }

    ## Three folds: two members stay on the folds axis, three go out as tasks
    expect_identical(obj$config$tuning$cv_folds, 3L)
    expect_identical(axis_at(2L), "resamples")
    expect_identical(axis_at(3L), "configs")

  })

  it("takes the members axis at the defaults: fit()'s n_best members on configure()'s cv_folds", {

    ## Read from the two functions, not restated, so a change to either
    ## default that moves the axis fails here, and ?fit's "Parallelism" with it
    n_best   <- eval(formals(fit)$n_best)
    cv_folds <- eval(formals(configure)$cv_folds)

    obj <- ranked_members(n_best)
    obj$config$tuning$cv_folds <- cv_folds

    local_tasks_in_session(2L)
    local_mocked_bindings(fit_single_config = function(config_row, ...) stand_in(config_row))

    fitted <- suppressWarnings(fit(obj, seed = 42L, compute_uq = FALSE, compute_ad = FALSE,
                                   control = control_horizons(allow_par = TRUE)))

    expect_identical(nrow(fitted$models$results), as.integer(n_best))
    expect_identical(fitted$models$parallelize_over, "configs")

  })

  it("stops when the first members to come in fail the same way across two models, and not when a success comes in with them", {

    ## Five members ranked rf, cubist, rf, cubist, rf: the first three fail
    ## alike, the last two succeed
    obj   <- ranked_members(5L)
    fails <- c("cfg_001", "cfg_002", "cfg_003")

    ## Equal costs, so the window starts them in rank order; and each member
    ## on one daemon, since split_plan() would give each rf member inner
    ## daemons, which would bring them in over more passes and start real
    ## inner daemons in this session
    local_mocked_bindings(
      fit_single_config = function(config_row, ...) stand_in(config_row, config_row$config_id %in% fails),
      config_costs      = function(configs, n_rows) rep(1, nrow(configs))
    )

    run <- function(n) {
      local_tasks_in_session(n)
      suppressWarnings(fit(obj, n_best = 5L, seed = 42L, compute_uq = FALSE, compute_ad = FALSE,
                           control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                      inner_daemons = FALSE)))
    }

    ## On one daemon they come in one a pass, in rank order: the third
    ## failure trips it with two members still to fit
    err <- expect_error(run(1L), class = "horizons_error_systemic")

    expect_identical(err$results$config_id, fails)
    expect_match(conditionMessage(err), "The first 3 members to run all failed the same way, across 2 models",
                 fixed = TRUE)
    expect_match(conditionMessage(err), "`fit()` stopped rather than run the other 2", fixed = TRUE)

    ## On four the first four come in in one pass, the three failures and a
    ## success, while the fifth waits: the success is read first, so the
    ## failures are ordinary ones and the fit returns. Read failures first,
    ## they would trip it with the fifth still to fit.
    fitted <- run(4L)

    expect_identical(fitted$models$results$status, c("failed", "failed", "failed", "success", "success"))
    expect_identical(fitted$models$best_config, "cfg_004")

  })

  it("runs a debug run in this session, as one asked for in parallel on the members axis", {

    obj <- members_axis_obj()

    local_mocked_bindings(
      fit_single_config      = function(config_row, ...) stand_in(config_row),
      check_parallel_backend = function(...) stop("the backend was checked"),
      fit_member_worker      = function(...) stop("a member was sent out")
    )

    fitted <- suppressWarnings(fit(obj, n_best = 3L, seed = 42L, compute_uq = FALSE, compute_ad = FALSE,
                                   control = control_horizons(allow_par = TRUE, parallelize_over = "configs",
                                                              debug = TRUE)))

    expect_identical(fitted$models[c("parallelize_over", "workers")], list(parallelize_over = "sequential", workers = 1L))

  })

})


## The members fitted in this session, read once per process: what every
## members-axis run is compared with
members_in_turn <- function() {
  memo_fixture("members_axis_in_turn", function() {
    suppressWarnings(fit(members_axis_obj(), n_best = 3L, seed = 42L, control = members_control()))
  })
}

describe("fit() - the members axis on daemons (#60)", {

  it("fits every member on two daemons as this session does, and leaves the daemons as it found them (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()
    skip_if_not_installed("Cubist")

    obj     <- members_axis_obj()
    in_turn <- members_in_turn()
    expect_fits_nontrivial(in_turn)

    local_test_daemons(2)
    before <- daemons_record()

    on_two <- with_deadline(suppressWarnings(
      fit(obj, n_best = 3L, seed = 42L, control = members_control(allow_par = TRUE, parallelize_over = "configs"))
    ), 300)

    expect_identical(daemons_record(), before)
    ## The members ran on the daemons: more tasks completed than the one each
    ## the check at the door spends and rf's fold preparations
    expect_gt(mirai::info()[["completed"]], 2L + 4L)

    expect_same_members(on_two, in_turn)
    expect_identical(on_two$models[c("parallelize_over", "workers")], list(parallelize_over = "configs", workers = 2L))

  })

  it("fits a member whose folds run on inner daemons as this session does, and says so on the log (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()
    skip_if_not_installed("Cubist")

    obj     <- members_axis_obj()
    in_turn <- members_in_turn()
    first   <- in_turn$models$results$config_id[1]

    ## The plan splits the best-ranked member's three folds over three inner
    ## daemons, which take the whole window while it runs
    local_mocked_bindings(split_plan = function(costs, n, folds) c(3L, rep(1L, length(costs) - 1L)))
    local_test_daemons(3)

    split <- with_deadline(suppressWarnings(
      fit(obj, n_best = 3L, seed = 42L, control = members_control(allow_par = TRUE, parallelize_over = "configs"))
    ), 300)

    expect_same_members(split, in_turn)

    note <- split$log[split$log$type == "note" & grepl("on inner daemons it started", split$log$message, fixed = TRUE), ]

    expect_identical(nrow(note), 1L)
    expect_match(note$message, paste0("`fit()` ran the folds of 1 long configuration on inner daemons it started on its daemon, within the daemons set: ",
                                      first, " on 3."), fixed = TRUE)

  })

  it("stops as systemic, with the members so far, when a member's daemon dies, and cancels the member in flight (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    ## Ranked, and started, in config order: the first two together, the
    ## third once a daemon is free
    obj <- rank_configs_as(members_axis_obj(), c("cfg_001", "cfg_002", "cfg_003"))
    local_mocked_bindings(config_costs = function(configs, n_rows) as.numeric(rev(seq_len(nrow(configs)))))

    local_test_daemons(2)
    marks <- withr::local_tempdir()

    ## On the daemons, through the file system, every wait with a deadline:
    ## the first member finishes; the second holds its daemon until the run
    ## has stopped; the third, started on the first's daemon, kills it once
    ## the first is in, a poll later
    with_deadline(mirai::everywhere({
      ns   <- asNamespace("horizons")
      real <- get("fit_single_config", envir = ns)
      utils::assignInNamespace("fit_single_config", function(config_row, ...) {
        id    <- config_row$config_id
        until <- Sys.time() + 100
        file.create(file.path(marks, paste0(id, "-started")))
        if (id == "cfg_002") {
          while (Sys.time() < until) Sys.sleep(0.05)
        }
        if (id == "cfg_003") {
          while (!file.exists(file.path(marks, "cfg_001-done")) && Sys.time() < until) Sys.sleep(0.05)
          Sys.sleep(1)
          tools::pskill(Sys.getpid(), tools::SIGKILL)
        }
        value <- real(config_row, ...)
        file.create(file.path(marks, paste0(id, "-done")))
        value
      }, ns = "horizons")
    }, marks = marks)[], 30)

    err <- expect_error(
      with_deadline(suppressWarnings(fit(obj, n_best = 3L, seed = 42L, compute_uq = FALSE, compute_ad = FALSE,
                                         control = control_horizons(allow_par = TRUE, parallelize_over = "configs"))), 120),
      class = "horizons_error_systemic"
    )

    ## Raised once, by the window, naming the member and the transport's
    ## error value
    msg <- gsub("\\s+", " ", conditionMessage(err))
    expect_match(msg, paste("The mirai daemon running member \"cfg_003\" stopped while it ran",
                            "(`errorValue` 19, Connection reset), so `fit()` cannot go on."),
                 fixed = TRUE)
    expect_match(msg, "in this error's results field", fixed = TRUE)
    expect_null(err$parent)
    expect_identical(err$results$config_id, "cfg_001")
    expect_identical(err$results$status, "success")

    ## The second member was cancelled, not run on: once the daemon left is
    ## idle, it has not finished
    with_deadline(while (mirai::info()[["executing"]] > 0L) Sys.sleep(0.1), 60)
    expect_setequal(list.files(marks), c("cfg_001-started", "cfg_001-done", "cfg_002-started", "cfg_003-started"))

  })

})
