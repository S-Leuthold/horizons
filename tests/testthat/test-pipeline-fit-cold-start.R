## ---------------------------------------------------------------------------
## Tests: fit() started cold
## ---------------------------------------------------------------------------
## fit() on a configured object with one configuration and no evaluation
## (#45): it draws the split evaluate() would draw and records an unscreened
## evaluation. Each block builds its own objects.


## =========================================================================
## Cold start: a configured object with one configuration (#45)
## =========================================================================
## With one configuration there is nothing for evaluate() to screen, so fit()
## takes the configured object, draws the split evaluate() would draw at the
## same seed, and re-tunes from a space-filling grid where evaluate()'s
## parameters would otherwise seed it.

describe("fit() - cold start from one configuration (#45)", {

  ## make_eval_object() is configured, not evaluated (helper-fixtures.R)
  obj <- make_eval_object(n = 60, n_configs = 1)
  obj$config$tuning$final_bayesian_iter <- 0L

  cold_out <- utils::capture.output(
    cold <- suppressWarnings(
      fit(obj, compute_uq = FALSE, compute_ad = FALSE, verbose = TRUE, seed = 42L)
    )
  )

  ## The same configuration through evaluate(), at the same seed
  ev <- suppressWarnings(evaluate(obj, prune = FALSE, verbose = FALSE, seed = 42L))

  warm_out <- utils::capture.output(
    warm <- suppressWarnings(
      fit(ev, compute_uq = FALSE, compute_ad = FALSE, verbose = TRUE, seed = 42L)
    )
  )

  it("fits a configured object without evaluate(), and predict() works on it", {

    expect_identical(class(cold), c("horizons_fit", "horizons_eval", "horizons_data", "list"))
    expect_identical(validate_horizons_fit(cold), cold)
    expect_identical(names(cold$models$workflows), "cfg_001")

    new_data <- obj$data$analysis[1:5, setdiff(names(obj$data$analysis), "SOC")]
    p        <- predict(cold, new_data, interval = FALSE)

    expect_identical(p$sample_id, new_data$sample_id)
    expect_true(all(is.finite(p$.pred)))

  })

  it("records an unscreened evaluation: one not_evaluated row, no CV, no parameters", {

    rec <- cold$evaluation

    expect_false(rec$screened)
    expect_identical(rec$best_config, "cfg_001")
    expect_identical(rec$rank_metric, "rpd")
    expect_identical(rec$results$config_id, "cfg_001")
    expect_identical(rec$results$status, "not_evaluated")
    expect_true(all(is.na(unlist(rec$results[paste0("cv_", c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae"))]))))
    expect_null(rec$results$best_params[[1]])
    expect_equal(rec$n_train + rec$n_test, 60)

  })

  it("holds out the rows evaluate() holds out at the same seed", {

    expect_identical(cold$evaluation$split$in_id, ev$evaluation$split$in_id)
    expect_identical(rsample::testing(cold$models$split)$sample_id,
                     rsample::testing(ev$evaluation$split)$sample_id)
    expect_identical(rsample::testing(cold$models$split)$sample_id,
                     rsample::testing(warm$models$split)$sample_id)

  })

  it("holds out evaluate()'s rows when some outcomes are NA", {

    skip_unless_slow_tier()

    with_na <- obj
    with_na$data$analysis$SOC[c(2, 9, 30)] <- NA_real_

    cold_na <- suppressWarnings(
      fit(with_na, compute_uq = FALSE, compute_ad = FALSE, verbose = FALSE, seed = 7L)
    )
    ev_na <- suppressWarnings(
      evaluate(with_na, prune = FALSE, verbose = FALSE, seed = 7L)
    )

    expect_identical(cold_na$models$split$data, ev_na$evaluation$split$data)
    expect_identical(cold_na$models$split$in_id, ev_na$evaluation$split$in_id)

  })

  it("prints and records the space-filling start", {

    expect_true(any(grepl("Cold start: no warm-start parameters; space-filling grid of 2 points",
                          cold_out, fixed = TRUE)))
    expect_false(cold$models$results$warm_start)
    expect_identical(cold$models$results$start_grid_size, 2L)

  })

  it("records a warm start on the evaluate() path and prints no space-filling note", {

    expect_true(warm$models$results$warm_start)
    expect_false(any(grepl("space-filling", warm_out, fixed = TRUE)))

  })

  it("says in the tree that it started cold from a configured object", {

    expect_true(any(grepl("Cold start: 1 configuration, not screened by evaluate()",
                          cold_out, fixed = TRUE)))
    expect_true(any(grepl("horizons_data → horizons_fit", cold_out, fixed = TRUE)))
    expect_false(any(grepl("Cold start", warm_out, fixed = TRUE)))

  })

  ## The Evaluation section of print() or summary(): the lines after its
  ## heading, up to the blank line that ends it.
  evaluation_section <- function(out) {
    start <- which(out == "Evaluation")[1]
    end   <- start + which(out[-seq_len(start)] == "")[1]
    out[(start + 1):(end - 1)]
  }

  it("prints and summarises the record as unevaluated, and nothing else", {

    only_line <- "   └─ Configs evaluated: none (fit() started cold from 1)"

    ## No success count, results, rank metric or runtime to contradict it
    expect_identical(evaluation_section(utils::capture.output(print(cold))), only_line)
    expect_identical(evaluation_section(utils::capture.output(summary(cold))), only_line)
    expect_false(any(grepl("none (fit() started cold", utils::capture.output(print(warm)),
                           fixed = TRUE)))

  })

  it("closes print()'s evaluation branch on its last line, cold or not", {

    ## A fitted object's Best line used to stay open (├─)
    warm_section <- evaluation_section(utils::capture.output(print(warm)))
    expect_match(warm_section[length(warm_section)], "^   └─ Best: ")
    expect_false(any(grepl("└─", warm_section[-length(warm_section)], fixed = TRUE)))

    ## Nothing succeeded: Successful is the last line, and used to stay open
    none_succeeded <- ev
    none_succeeded$evaluation$results$status <- "pruned"

    none_section <- evaluation_section(utils::capture.output(print(none_succeeded)))
    expect_identical(none_section[length(none_section)], "   └─ Successful: 0")

  })

  it("prints the Bayesian budget the re-tune runs, not the screening one", {

    budget <- obj
    budget$config$tuning$bayesian_iter       <- 0L
    budget$config$tuning$final_bayesian_iter <- 1L

    ## Stop at the first member: only the header is under test
    out <- utils::capture.output(
      testthat::with_mocked_bindings(
        tryCatch(
          suppressWarnings(
            fit(budget, compute_uq = FALSE, compute_ad = FALSE, verbose = TRUE)
          ),
          horizons_all_members_failed = function(e) NULL
        ),
        fit_single_config = function(...) {
          list(config_id = "cfg_001", status = "failed", error_message = "mocked",
               runtime_secs = 0)
        },
        .package = "horizons"
      )
    )

    expect_true(any(grepl("Bayesian: 1 iterations", out, fixed = TRUE)))
    expect_false(any(grepl("Bayesian: 0 iterations", out, fixed = TRUE)))

  })

  ## Degradation compares the test RPD with fit()'s own out-of-fold CV on the
  ## fit rows, not with evaluate()'s, so a cold start has what the check needs.
  it("checks degradation against its own cross-validation, which a cold start has", {

    res <- cold$models$results

    expect_true(is.finite(res$cv_rpd_mean))
    expect_true(is.finite(res$cv_rpd_se))
    expect_type(res$degraded, "logical")
    expect_false(is.na(res$degraded))

  })

  it("re-fits a cold-started fit as a cold start, keeping its recorded metric", {

    skip_unless_slow_tier()

    cold_rmse <- suppressWarnings(
      fit(obj, metric = "rmse", compute_uq = FALSE, compute_ad = FALSE,
          verbose = FALSE, seed = 42L)
    )

    again_out <- utils::capture.output(
      again <- suppressWarnings(
        fit(cold_rmse, compute_uq = FALSE, compute_ad = FALSE, verbose = TRUE, seed = 42L)
      )
    )

    expect_s3_class(again, "horizons_fit")
    expect_false(again$evaluation$screened)
    expect_identical(again$models$split$in_id, cold$models$split$in_id)

    ## metric = NULL carries the recorded one, as it carries evaluate()'s
    expect_identical(again$evaluation$rank_metric, "rmse")
    expect_identical(again$models$rank_metric, "rmse")

    expect_true(any(grepl("horizons_fit → horizons_fit", again_out, fixed = TRUE)))

  })

  ## evaluate() and the cold start both write `screened`, so an evaluation
  ## without it is not a current object (#130). It is refused before any
  ## member is re-tuned, not by the fit validator after all of them are.
  it("refuses an evaluation without `screened` before fitting anything", {

    legacy <- ev
    legacy$evaluation$screened <- NULL

    expect_false("screened" %in% names(legacy$evaluation))

    local_mocked_bindings(
      fit_single_config = function(...) stop("fit_single_config() was called")
    )

    err <- expect_error(
      fit(legacy, compute_uq = FALSE, compute_ad = FALSE, verbose = FALSE, seed = 42L),
      class = "horizons_validation_error"
    )

    msg <- gsub("\\s+", " ", conditionMessage(err))

    expect_match(msg, "missing", fixed = TRUE)
    expect_match(msg, "screened", fixed = TRUE)
    expect_match(msg, "Re-run `evaluate()`", fixed = TRUE)

  })

  it("names every missing evaluation key in the refusal", {

    legacy <- ev
    legacy$evaluation$recipe        <- NULL
    legacy$evaluation$response_trim <- NULL

    local_mocked_bindings(
      fit_single_config = function(...) stop("fit_single_config() was called")
    )

    err <- expect_error(
      fit(legacy, compute_uq = FALSE, compute_ad = FALSE, verbose = FALSE, seed = 42L),
      class = "horizons_validation_error"
    )

    msg <- gsub("\\s+", " ", conditionMessage(err))

    expect_match(msg, "response_trim", fixed = TRUE)
    expect_match(msg, "recipe", fixed = TRUE)

  })

  it("is refused by ensemble(), as any single-member fit is", {

    expect_error(ensemble(cold, verbose = FALSE), "at least 2 members")

  })

  it("refuses more than one configuration, naming evaluate()", {

    two <- make_eval_object(n = 60, n_configs = 2)

    err <- expect_error(fit(two, verbose = FALSE), class = "horizons_input_error")

    msg <- gsub("\\s+", " ", conditionMessage(err))   # undo cli line wrapping
    expect_match(msg, "can start without `evaluate()` only from a single configuration", fixed = TRUE)
    expect_match(msg, "2 configurations", fixed = TRUE)

  })

  it("names an outcome column the analysis table lacks", {

    ## The cold start reads the outcome before fit() validates the object, so
    ## this is the refusal a user meets, not the validator's or "All outcome
    ## values are NA".
    gone <- make_eval_object(n = 60, n_configs = 1)
    outcome <- gone$data$role_map$variable[gone$data$role_map$role == "outcome"]
    gone$data$analysis[[outcome]] <- NULL

    err <- expect_error(fit(gone, verbose = FALSE), class = "horizons_input_error")

    msg <- gsub("\\s+", " ", conditionMessage(err))
    expect_match(msg, "The analysis table has no outcome column to model.", fixed = TRUE)
    expect_match(msg, paste0("names ", outcome, " as the outcome"), fixed = TRUE)

  })

  it("records evaluation$recipe as evaluate() does, and builds its recipe from it (#62)", {

    expect_identical(cold$evaluation$recipe, ev$evaluation$recipe)

    ## A non-default record reaches both the evaluation record and the re-tune
    tuned <- obj
    tuned$config$recipe <- list(sg_window = 7L, pca_threshold = 0.9)

    seen <- NULL

    testthat::with_mocked_bindings(
      tryCatch(
        suppressWarnings(
          fit(tuned, compute_uq = FALSE, compute_ad = FALSE, verbose = FALSE)
        ),
        horizons_all_members_failed = function(e) NULL
      ),
      fit_single_config = function(...) {
        seen <<- list(...)[c("sg_window", "pca_threshold")]
        list(config_id = "cfg_001", status = "failed", error_message = "mocked",
             runtime_secs = 0)
      },
      .package = "horizons"
    )

    expect_identical(seen, list(sg_window = 7L, pca_threshold = 0.9))
    ## The helper draws the split, and rsample warns about thin quantiles
    record <- suppressWarnings(cold_start_evaluation(tuned, NULL, 42L))

    expect_identical(record$evaluation$recipe,
                     list(sg_window = 7L, sg_window_cm = 14, pca_threshold = 0.9))

  })

  it("refuses a Savitzky-Golay window as wide as the spectrum, as evaluate() does (#62)", {

    ## make_eval_object() has 10 spectral columns
    wide <- obj
    wide$config$recipe <- list(sg_window = 11L, pca_threshold = 0.995)

    ran <- 0L

    err <- testthat::with_mocked_bindings(
      tryCatch(fit(wide, verbose = FALSE, seed = 42L), error = function(e) e),
      fit_single_config = function(...) { ran <<- ran + 1L; NULL },
      .package = "horizons"
    )

    expect_s3_class(err, "horizons_input_error")
    expect_match(conditionMessage(err), "11 grid points (22 cm", fixed = TRUE)
    expect_match(conditionMessage(err), "10 spectral columns", fixed = TRUE)
    expect_identical(ran, 0L)

  })

  it("refuses a configured object with no configuration", {

    none <- obj
    none$config$configs <- obj$config$configs[0, ]

    expect_error(fit(none, verbose = FALSE), "configure()", class = "horizons_input_error")

  })

  it("refuses a column added after configure() with no role_map entry on the cold-start path too (#24)", {

    ## The entry-stage validate_horizons_data() call runs after cold_start_evaluation()
    ## populates $evaluation (Step 0), on the configured-object path exactly as it
    ## does on the horizons_eval path — a column with no role_map entry must not
    ## reach build_recipe()'s outcome ~ . undetected just because there was no
    ## evaluate() call to certify the object first.
    stray <- obj
    stray$data$analysis$stray_column <- seq_len(nrow(stray$data$analysis))

    expect_error(
      fit(stray, compute_uq = FALSE, compute_ad = FALSE, verbose = FALSE, seed = 42L),
      "[Mm]issing from.*role_map"
    )

  })

  ## On the evaluate() path an unknown metric used to surface as a missing
  ## cv_<metric> column, with the advice to re-run evaluate().
  it("refuses an unknown metric on either path, naming it and the valid ones", {

    for (x in list(obj, ev)) {

      err <- expect_error(fit(x, metric = "accuracy", verbose = FALSE),
                          class = "horizons_input_error")

      msg <- gsub("\\s+", " ", conditionMessage(err))   # undo cli line wrapping
      expect_match(msg, "accuracy", fixed = TRUE)
      expect_match(msg, "rrmse", fixed = TRUE)
      expect_no_match(msg, "Re-run", fixed = TRUE)

    }

  })

})


## =========================================================================
## Cold start with UQ and AD on (#45)
## =========================================================================
## The calibration set is carved from the cold start's own training part, as
## it is from evaluate()'s, and predict() serves intervals and AD from it.

describe("fit() - cold start with UQ and AD (#45)", {

  ## n = 250 with 10 NA outcomes: 240 modelled rows, enough for the
  ## calibration split to clear N_CALIB_MIN
  obj <- make_eval_object(n = 250, n_configs = 1)
  obj$config$tuning$final_bayesian_iter <- 0L
  obj$data$analysis$SOC[seq(5, 50, by = 5)] <- NA_real_

  cold <- suppressWarnings(
    fit(obj, compute_uq = TRUE, compute_ad = TRUE, verbose = FALSE, seed = 42L)
  )

  modelled_ids <- obj$data$analysis$sample_id[!is.na(obj$data$analysis$SOC)]
  test_ids     <- rsample::testing(cold$models$split)$sample_id
  fit_ids      <- unique(cold$models$cv_predictions$sample_id)

  ## Split C, reproduced from calib_split_seed() and the training part alone
  set.seed(calib_split_seed(42L))
  split_C   <- rsample::initial_split(rsample::training(cold$models$split),
                                      prop = CALIB_PROP,
                                      strata = dplyr::all_of("SOC"))
  calib_ids <- rsample::testing(split_C)$sample_id

  it("partitions the modelled frame into disjoint test, calibration and fit rows", {

    expect_length(modelled_ids, 240L)
    expect_setequal(rsample::training(split_C)$sample_id, fit_ids)

    expect_length(intersect(test_ids, calib_ids), 0L)
    expect_length(intersect(test_ids, fit_ids), 0L)
    expect_length(intersect(calib_ids, fit_ids), 0L)
    expect_setequal(c(test_ids, calib_ids, fit_ids), modelled_ids)
    expect_identical(length(c(test_ids, calib_ids, fit_ids)), 240L)

    expect_equal(cold$models$uq[[1]]$n_calib, length(calib_ids))

  })

  it("serves intervals and AD flags from predict()", {

    expect_true(has_uq(cold))
    expect_true(has_ad(cold))

    new_data <- obj$data$analysis[1:6, setdiff(names(obj$data$analysis), "SOC")]
    p        <- predict(cold, new_data, interval = TRUE)

    expect_true(all(c(".pred_lower", ".pred_upper", ".ad_distance", ".ad_flag") %in% names(p)))
    expect_true(all(p$.pred_lower <= p$.pred_upper))
    expect_false(anyNA(p$.ad_flag))

  })

})
