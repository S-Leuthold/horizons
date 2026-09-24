## ---------------------------------------------------------------------------
## Tests: evaluate()
## ---------------------------------------------------------------------------
## make_eval_object() is defined in helper-fixtures.R

## Expected columns in evaluation$results
EXPECTED_EVAL_COLS <- c(
  "config_id", "status", "rmse", "rrmse", "rsq", "ccc", "rpd", "mae",
  "best_params", "error_message", "warnings", "runtime_secs"
)

## =========================================================================
## Gate checks
## =========================================================================

describe("evaluate() - gate checks", {

  it("aborts when config$configs is NULL", {

    obj <- make_eval_object()
    obj$config$configs <- NULL

    expect_error(evaluate(obj, verbose = FALSE), "No configurations found")

  })

  it("aborts when config$configs has zero rows", {

    obj <- make_eval_object()
    obj$config$configs <- obj$config$configs[0, ]

    expect_error(evaluate(obj, verbose = FALSE), "No configurations found")

  })

  it("aborts when outcome column has no non-NA values", {

    obj <- make_eval_object()
    outcome <- obj$data$role_map$variable[obj$data$role_map$role == "outcome"]
    obj$data$analysis[[outcome]] <- NA_real_

    expect_error(evaluate(obj, verbose = FALSE), "outcome.*NA")

  })

  it("aborts when sample size is too small for CV", {

    obj <- make_eval_object(n = 4)

    expect_error(evaluate(obj, verbose = FALSE), "sample size")

  })

  it("aborts with invalid metric name", {

    obj <- make_eval_object()

    expect_error(evaluate(obj, metric = "accuracy", verbose = FALSE), "metric")

  })

  it("refuses a column added after configure() with no role_map entry (#24)", {

    ## validate_horizons_eval() (at return) certifies the evaluation slot,
    ## not the base data contract, so a column landing in $data$analysis
    ## after configure() — by a later parse_ids(), or a direct assignment —
    ## used to reach build_recipe()'s `outcome ~ .` as an unregistered
    ## predictor, undetected. evaluate()'s entry-stage validate_horizons_data()
    ## call closes that gap.

    obj <- make_eval_object()
    obj$data$analysis$stray_column <- seq_len(nrow(obj$data$analysis))

    expect_error(
      evaluate(obj, verbose = FALSE),
      "[Mm]issing from.*role_map"
    )

  })

})

## =========================================================================
## Success path
## =========================================================================

describe("evaluate() - success path", {

  obj <- make_eval_object(n = 60, n_configs = 2)

  result <- suppressWarnings(evaluate(obj, verbose = FALSE, seed = 42L))

  it("returns a horizons_eval object", {

    expect_s3_class(result, "horizons_eval")
    expect_s3_class(result, "horizons_data")

  })

  it("has evaluation$results with one row per config", {

    expect_equal(nrow(result$evaluation$results), 2)

  })

  it("has all expected columns in results", {

    expect_true(all(EXPECTED_EVAL_COLS %in% names(result$evaluation$results)))

  })

  it("sets best_config to a valid config_id", {

    expect_true(result$evaluation$best_config %in%
                  result$config$configs$config_id)

  })

  it("stores rank_metric", {

    expect_equal(result$evaluation$rank_metric, "rpd")

  })

  it("stores the train/test split", {

    expect_s3_class(result$evaluation$split, "rsplit")

  })

  it("records n_train and n_test", {

    expect_true(result$evaluation$n_train > 0)
    expect_true(result$evaluation$n_test > 0)
    expect_equal(result$evaluation$n_train + result$evaluation$n_test,
                 nrow(obj$data$analysis))

  })

  it("records positive runtime", {

    expect_true(result$evaluation$runtime_secs > 0)

  })

  it("records a timestamp", {

    expect_s3_class(result$evaluation$timestamp, "POSIXct")

  })

  it("writes exactly the evaluation keys new_horizons_data() declares (#71)", {

    ## The constructor's empty slot is what configure() resets to, so it has
    ## to name what evaluate() actually writes.
    expect_identical(names(result$evaluation), names(new_horizons_data()$evaluation))

  })

  it("records that it screened the configurations (#45)", {

    ## fit()'s cold start writes FALSE; evaluate() ranked, so TRUE.
    expect_true(result$evaluation$screened)

  })

  it("has non-NA metrics for successful configs", {

    success_rows <- result$evaluation$results$status == "success"
    expect_true(any(success_rows))

    for (m in c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae")) {
      expect_false(any(is.na(result$evaluation$results[[m]][success_rows])),
                   info = paste("NA found in", m))
    }

  })

})

## =========================================================================
## Metric ranking
## =========================================================================

describe("evaluate() - metric ranking", {

  it("selects best config by specified metric", {

    obj <- make_eval_object(n_configs = 2)
    result <- suppressWarnings(evaluate(obj, metric = "rsq", verbose = FALSE, seed = 42L))

    expect_equal(result$evaluation$rank_metric, "rsq")

    ## Best config is the highest CROSS-VALIDATED rsq among successes (#50).
    ## rank_metric keeps the bare name; the ranking column is cv_rsq.
    successes <- result$evaluation$results %>%
      dplyr::filter(status == "success")
    best_row <- successes[which.max(successes$cv_rsq), ]

    expect_equal(result$evaluation$best_config, best_row$config_id)

  })

  it("records the cv_* columns on every result row", {

    obj <- make_eval_object(n_configs = 2)
    result <- suppressWarnings(evaluate(obj, verbose = FALSE, seed = 42L))

    cv_cols <- paste0("cv_", c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae"))
    res     <- result$evaluation$results

    expect_true(all(cv_cols %in% names(res)))

    ## rmse and rpd are always defined; rsq / ccc can be NA when a fold's
    ## predictions are near-constant, and tune's mean carries that NA.
    ok <- res[res$status == "success", ]
    expect_true(all(is.finite(ok$cv_rmse)))
    expect_true(all(is.finite(ok$cv_rpd)))

  })

})

## =========================================================================
## rank_configs_by_cv() — the shared ranking rule
## =========================================================================

describe("rank_configs_by_cv()", {

  rows <- tibble::tibble(
    config_id = c("a", "b", "c"),
    status    = "success",
    rpd       = c(3.0, 1.0, 2.0),    # test-set: a would win
    rmse      = c(0.5, 2.0, 1.0),
    cv_rpd    = c(1.5, 2.5, 2.0),    # CV: b wins
    cv_rmse   = c(1.2, 0.6, 0.9)
  )

  it("orders best-first by cv_<metric>, never by the test-set column", {

    expect_equal(rank_configs_by_cv(rows, "rpd")$config_id,  c("b", "c", "a"))
    expect_equal(rank_configs_by_cv(rows, "rmse")$config_id, c("b", "c", "a"))

  })

  it("drops rows with NA cv_<metric> with a warning naming them", {

    legacy <- rows
    legacy$cv_rpd[1] <- NA_real_

    expect_warning(ranked <- rank_configs_by_cv(legacy, "rpd"), "skipped")
    expect_equal(ranked$config_id, c("b", "c"))

  })

  it("aborts with a re-run remedy when nothing can be ranked", {

    all_na <- rows
    all_na$cv_rpd <- NA_real_

    expect_error(rank_configs_by_cv(all_na, "rpd"), "[Rr]e-run evaluate")
    expect_error(rank_configs_by_cv(rows[, c("config_id", "status", "rpd")], "rpd"),
                 "[Rr]e-run evaluate")

  })

})

## =========================================================================
## All configs fail
## =========================================================================

describe("evaluate() - all configs fail", {

  it("aborts when every config fails", {

    obj <- make_eval_object(n_configs = 2)
    obj$config$configs$model <- c("nope_1", "nope_2")

    expect_error(
      suppressWarnings(evaluate(obj, verbose = FALSE)),
      "All configurations failed"
    )

  })

  ## evaluate() aborts before it assigns x$evaluation, so the message's old
  ## hint ("Check evaluation$results") could not be followed. The results now
  ## ride on the condition, so a loop over subsets can recover them (#41).
  it("signals horizons_all_configs_failed, carrying the results and naming the errors", {

    obj <- make_eval_object(n_configs = 5)

    ## Four distinct messages; one carries braces, which must reach the
    ## message as text rather than be read as a cli template.
    obj$config$configs$model <- c("{wn_600}", "nope_2", "nope_2", "nope_4", "nope_5")

    err <- tryCatch(
      suppressWarnings(evaluate(obj, verbose = FALSE)),
      horizons_all_configs_failed = function(e) e
    )

    expect_s3_class(err, "horizons_all_configs_failed")

    ## The per-config results, error messages included
    expect_s3_class(err$results, "tbl_df")
    expect_identical(err$results$config_id, obj$config$configs$config_id)
    expect_true(all(err$results$status == "failed"))
    expect_true(all(grepl("Unknown model type", err$results$error_message)))

    ## The first three distinct messages, each with the configs that raised
    ## it; the fourth is counted, not listed.
    msg <- gsub("\\s+", " ", conditionMessage(err))   # undo cli line wrapping
    expect_match(msg, "'{wn_600}'", fixed = TRUE)
    expect_match(msg, "'nope_2'", fixed = TRUE)
    expect_match(msg, "cfg_002, cfg_003", fixed = TRUE)
    expect_match(msg, "'nope_4'", fixed = TRUE)
    expect_no_match(msg, "'nope_5'", fixed = TRUE)
    expect_match(msg, "1 more distinct error message")

    ## How to recover the table after an uncaught abort; nothing here came
    ## from a checkpoint, so no checkpoint note.
    expect_match(msg, "rlang::last_error()$results", fixed = TRUE)
    expect_no_match(msg, "loaded from checkpoints", fixed = TRUE)

  })

})

## =========================================================================
## NA outcome handling
## =========================================================================

describe("evaluate() - NA outcome rows", {

  it("drops rows with NA outcome and still succeeds", {

    obj <- make_eval_object(n = 40)
    outcome <- obj$data$role_map$variable[obj$data$role_map$role == "outcome"]

    ## Set 5 rows to NA
    obj$data$analysis[[outcome]][1:5] <- NA_real_

    result <- suppressWarnings(evaluate(obj, verbose = FALSE, seed = 42L))

    ## Should succeed with remaining rows
    expect_s3_class(result, "horizons_eval")
    expect_equal(result$evaluation$n_train + result$evaluation$n_test, 35)

  })

  it("outcome_complete_rows(), the rule fit() shares, drops and counts NA outcomes (#67)", {

    df <- tibble::tibble(sample_id = c("A", "B", "C", "D"),
                         y         = c(1, NA, 3, NA))

    kept <- outcome_complete_rows(df, "y")

    expect_identical(kept$data$sample_id, c("A", "C"))
    expect_identical(kept$n_dropped, 2L)

    ## Nothing to drop: the table comes back untouched
    complete <- df[c(1, 3), ]
    expect_identical(outcome_complete_rows(complete, "y")$data, complete)
    expect_identical(outcome_complete_rows(complete, "y")$n_dropped, 0L)

    expect_error(outcome_complete_rows(df[c(2, 4), ], "y"),
                 class = "horizons_input_error")

  })

  it("names an outcome column the analysis table lacks, rather than calling it all NA", {

    ## evaluate()'s own outcome_complete_rows() check has a dedicated "no such
    ## column" message for exactly this case, distinct from "all NA" — but
    ## the entry-stage validate_horizons_data() call (#24) now certifies the
    ## base data contract first, and a role_map row with no matching analysis
    ## column is a structural defect that check catches before
    ## outcome_complete_rows() ever runs. Either way the message names SOC
    ## and never claims the values are all NA.

    obj <- make_eval_object()
    obj$data$analysis$SOC <- NULL

    err <- expect_error(evaluate(obj, verbose = FALSE),
                        class = "horizons_validation_error")

    expect_match(conditionMessage(err), "SOC", fixed = TRUE)
    expect_match(conditionMessage(err), "missing from", fixed = TRUE)
    expect_no_match(conditionMessage(err), "All outcome values are NA", fixed = TRUE)

  })

})

## =========================================================================
## draw_eval_split(): the one split draw evaluate() and fit() share (#45)
## =========================================================================
## fit() cold-starts a configured object with one configuration by drawing
## the split itself, so the draw moved out of evaluate() into a helper both
## verbs call. It has to give the split evaluate() drew before the move.

describe("draw_eval_split()", {

  obj <- make_eval_object(n = 60, n_configs = 1)
  obj$data$analysis$SOC[c(3, 17, 41)] <- NA_real_

  ## evaluate()'s draw before #45, as it was written inline: the rows with an
  ## observed outcome, then the seed, then a stratified split, falling back to
  ## an unstratified one.
  draw_before <- function(analysis, seed) {

    modelled <- outcome_complete_rows(analysis, "SOC")$data

    set.seed(seed)

    tryCatch(
      rsample::initial_split(modelled, prop = SPLIT_PROP, strata = dplyr::all_of("SOC")),
      error = function(e) rsample::initial_split(modelled, prop = SPLIT_PROP)
    )

  }

  it("gives the split evaluate() drew before it was factored out", {

    for (seed in c(1L, 42L, 307L)) {

      drawn  <- suppressWarnings(draw_eval_split(obj$data$analysis, "SOC", seed))
      before <- suppressWarnings(draw_before(obj$data$analysis, seed))

      expect_identical(drawn$split$in_id, before$in_id, info = paste("seed", seed))
      expect_identical(drawn$split$data, before$data, info = paste("seed", seed))
      expect_true(drawn$stratified)
      expect_identical(drawn$n_dropped, 3L)

    }

  })

  it("falls back to an unstratified split where evaluate() did", {

    real_initial_split <- rsample::initial_split

    local_mocked_bindings(
      initial_split = function(data, prop = 3 / 4, strata = NULL, ...) {
        if (!missing(strata)) stop("stratification refused")
        real_initial_split(data, prop = prop, ...)
      },
      .package = "rsample"
    )

    drawn  <- draw_eval_split(obj$data$analysis, "SOC", 42L)
    before <- draw_before(obj$data$analysis, 42L)

    expect_false(drawn$stratified)
    expect_true(drawn$strata_failed)
    expect_identical(drawn$split$in_id, before$in_id)

  })

  it("reports an unstratified split when rsample drops the strata without failing (#91)", {

    ## 30 rows: too few for rsample to bin the outcome, so it warns and draws
    ## unstratified, and the draw used to be reported stratified
    small <- make_eval_object(n = 30, n_configs = 1)

    drawn <- suppressWarnings(draw_eval_split(small$data$analysis, "SOC", 42L))

    expect_false(drawn$stratified)
    expect_false(drawn$strata_failed)

  })

  it("is the split evaluate() stores", {

    ev    <- suppressWarnings(evaluate(obj, prune = FALSE, verbose = FALSE, seed = 42L))
    drawn <- suppressWarnings(draw_eval_split(obj$data$analysis, "SOC", 42L))

    expect_identical(ev$evaluation$split$in_id, drawn$split$in_id)
    expect_identical(ev$evaluation$split$data, drawn$split$data)

  })

})

## =========================================================================
## evaluate()'s tree header says what the draws did (#91)
## =========================================================================
## The split line printed "stratified" whatever the draw did, and the notes
## for a failed stratified draw printed above the tree's header.

## evaluate()'s console output down to its first configuration, which is
## mocked to stop the run: only the header is under test.
evaluate_header <- function(obj) {

  utils::capture.output(
    testthat::with_mocked_bindings(
      tryCatch(
        suppressWarnings(evaluate(obj, prune = FALSE, seed = 42L)),
        header_rendered = function(e) NULL
      ),
      evaluate_single_config = function(...) rlang::abort("stop", class = "header_rendered"),
      .package = "horizons"
    )
  )

}

describe("evaluate() - the split line and the fallback notes (#91)", {

  it("says unstratified when rsample drew the split without strata", {

    ## 30 rows are too few for rsample to bin the outcome
    out <- evaluate_header(make_eval_object(n = 30, n_configs = 1))

    expect_true("│  Split: 24 train / 6 test (80/20, unstratified)" %in% out)
    expect_false(any(grepl(", stratified)", out, fixed = TRUE)))

  })

  it("says stratified when the strata held", {

    out <- evaluate_header(make_eval_object(n = 60, n_configs = 1))

    expect_true("│  Split: 48 train / 12 test (80/20, stratified)" %in% out)

  })

  it("prints the notes for a failed stratified draw inside the tree, under the lines they qualify", {

    real_initial_split <- rsample::initial_split
    real_vfold_cv      <- rsample::vfold_cv

    local_mocked_bindings(
      initial_split = function(data, prop = 3 / 4, strata = NULL, ...) {
        if (!missing(strata)) stop("stratification refused")
        real_initial_split(data, prop = prop, ...)
      },
      vfold_cv = function(data, v = 10, repeats = 1, strata = NULL, ...) {
        if (!missing(strata)) stop("stratification refused")
        real_vfold_cv(data, v = v, repeats = repeats, ...)
      },
      .package = "rsample"
    )

    out <- evaluate_header(make_eval_object(n = 60, n_configs = 1))

    header     <- grep("┌ Evaluation", out, fixed = TRUE)
    split_line <- grep("│  Split: ", out, fixed = TRUE)
    split_note <- grep("Stratified split failed, retrying without strata", out, fixed = TRUE)
    cv_line    <- grep("│  Tuning: ", out, fixed = TRUE)
    cv_note    <- grep("Stratified CV failed, retrying without strata", out, fixed = TRUE)

    expect_length(header, 1L)
    expect_identical(out[split_line], "│  Split: 48 train / 12 test (80/20, unstratified)")
    expect_identical(split_note, split_line + 1L)
    expect_identical(cv_note, cv_line + 1L)
    expect_true(all(c(split_note, cv_note) > header))

  })

})

## =========================================================================
## Checkpointing
## =========================================================================

describe("evaluate() - checkpointing", {

  it("writes one checkpoint file per config, and no single-file checkpoint", {

    obj <- make_eval_object(n_configs = 2)
    tmpdir <- tempfile("eval_ckpt_write_")
    dir.create(tmpdir)
    on.exit(unlink(tmpdir, recursive = TRUE))

    checkpoint_dir <- file.path(tmpdir, "checkpoints")

    result <- suppressWarnings(evaluate(obj, output_dir = tmpdir, verbose = FALSE, seed = 42L))

    ## The per-config files are the only store (#42)
    expect_setequal(list.files(checkpoint_dir),
                    paste0(obj$config$configs$config_id, ".rds"))
    expect_false(file.exists(file.path(tmpdir, "eval_checkpoint.rds")))

  })

  it("resumes from checkpoint on re-run", {

    obj <- make_eval_object(n_configs = 2)
    tmpdir <- tempfile("eval_ckpt_resume_")
    dir.create(tmpdir)
    on.exit(unlink(tmpdir, recursive = TRUE))

    ## First run
    result1 <- suppressWarnings(evaluate(obj, output_dir = tmpdir, verbose = FALSE, seed = 42L))

    ## Second run should load from checkpoint (same results)
    result2 <- suppressWarnings(evaluate(obj, output_dir = tmpdir, verbose = FALSE, seed = 42L))

    expect_equal(result1$evaluation$results$config_id,
                 result2$evaluation$results$config_id)
    expect_equal(result1$evaluation$best_config,
                 result2$evaluation$best_config)

  })

  ## Rows checkpointed before the prune gate went inert at bayesian_iter = 0
  ## carry "pruned" where a fresh run now says "success", so without the
  ## relabel the candidate pool would depend on whether the run was resumed.
  it("relabels resumed 'pruned' rows as successes when bayesian_iter = 0 (#38)", {

    obj <- make_eval_object(n_configs = 2)
    obj$config$tuning$bayesian_iter <- 0L

    tmpdir <- tempfile("eval_ckpt_relabel_")
    dir.create(tmpdir)
    on.exit(unlink(tmpdir, recursive = TRUE))

    ## A first run at bayesian_iter = 0, whose rows are then given the label
    ## the old code wrote there. Resuming it under another bayesian_iter would
    ## be refused by the settings fingerprint (#42), so the old rows are
    ## written by hand rather than by a run with a Bayesian stage.
    first <- suppressWarnings(evaluate(obj, prune_threshold = 9999,
                                       output_dir = tmpdir, verbose = FALSE,
                                       seed = 42L))

    ## ...and rows written before the gate's reading was recorded do not
    ## carry it
    old_code <- function(rows) {
      rows$status                <- "pruned"
      rows$below_prune_threshold <- NULL
      rows$prune_threshold       <- NULL
      rows
    }

    for (f in list.files(file.path(tmpdir, "checkpoints"), full.names = TRUE)) {
      saveRDS(old_code(readRDS(f)), f)
    }

    resumed <- suppressWarnings(evaluate(obj, prune_threshold = 9999,
                                         output_dir = tmpdir, verbose = FALSE,
                                         seed = 42L))
    res <- resumed$evaluation$results

    ## Resumed, not re-run
    expect_equal(res$runtime_secs, first$evaluation$results$runtime_secs)

    expect_true(all(res$status == "success"))
    expect_true(all(res$below_prune_threshold))

  })

  ## Rows checkpointed before the cv_* columns existed can succeed with no
  ## value to rank on. rank_configs_by_cv() refused them unclassed and without
  ## the results, and re-running evaluate() resumes them rather than re-running.
  it("signals horizons_all_configs_failed when no success has a cv_<metric>, naming the checkpoints", {

    obj <- make_eval_object(n_configs = 2)

    tmpdir <- tempfile("eval_ckpt_nocv_")
    dir.create(tmpdir)
    on.exit(unlink(tmpdir, recursive = TRUE))

    suppressWarnings(evaluate(obj, output_dir = tmpdir, verbose = FALSE, seed = 42L))

    cv_cols <- paste0("cv_", c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae"))

    for (f in list.files(file.path(tmpdir, "checkpoints"), full.names = TRUE)) {
      row <- readRDS(f)
      row[cv_cols] <- NULL
      saveRDS(row, f)
    }

    err <- tryCatch(
      suppressWarnings(evaluate(obj, output_dir = tmpdir, verbose = FALSE,
                                seed = 42L)),
      horizons_all_configs_failed = function(e) e
    )

    expect_s3_class(err, "horizons_all_configs_failed")
    expect_setequal(err$results$config_id, obj$config$configs$config_id)

    msg <- gsub("\\s+", " ", conditionMessage(err))   # undo cli line wrapping
    expect_match(msg, "2 succeeded, but none has a cv_rpd value", fixed = TRUE)
    expect_match(msg, "2 configurations were loaded from checkpoints", fixed = TRUE)
    expect_match(msg, "cfg_001, cfg_002", fixed = TRUE)

    ## One store (#42): the per-config files, and the legacy file only when a
    ## resumed row came from it.
    expect_match(msg, "delete their files, checkpoints/<config_id>.rds.", fixed = TRUE)
    expect_no_match(msg, "eval_checkpoint.rds", fixed = TRUE)

    ## cfg_002 now comes from a legacy single file, which the loader copies
    ## into checkpoints/ but which would be read again once the copy is gone.
    legacy <- readRDS(file.path(tmpdir, "checkpoints", "cfg_002.rds"))
    saveRDS(legacy, file.path(tmpdir, "eval_checkpoint.rds"))
    unlink(file.path(tmpdir, "checkpoints", "cfg_002.rds"))

    err <- tryCatch(
      suppressMessages(suppressWarnings(
        evaluate(obj, output_dir = tmpdir, verbose = FALSE, seed = 42L)
      )),
      horizons_all_configs_failed = function(e) e
    )

    msg <- gsub("\\s+", " ", conditionMessage(err))
    expect_match(msg, "and eval_checkpoint.rds, which 1 of them was read from", fixed = TRUE)

  })

})

## =========================================================================
## Pruning passthrough
## =========================================================================

describe("evaluate() - pruning", {

  it("passes prune settings to evaluate_single_config", {

    obj <- make_eval_object(n_configs = 1)

    ## The gate only runs when there is a Bayesian stage to skip (#38)
    obj$config$tuning$bayesian_iter <- 1L

    ## Set a very high prune threshold — RPD must be above 9999
    result <- suppressWarnings(evaluate(obj, prune = TRUE, prune_threshold = 9999,
                                        verbose = FALSE, seed = 42L))

    ## The single config should be pruned
    expect_equal(result$evaluation$results$status, "pruned")

  })

})

## =========================================================================
## Recipe settings passthrough (#62)
## =========================================================================

describe("evaluate() - recipe settings", {

  ## Capture what evaluate() hands to evaluate_single_config() without tuning.
  capture_recipe_args <- function(obj) {

    seen <- NULL

    testthat::with_mocked_bindings(
      evaluate_single_config = function(...) {
        seen <<- list(...)[c("sg_window", "pca_threshold")]
        tibble::tibble(config_id = list(...)$config_row$config_id,
                       status = "failed", error_message = "mocked")
      },
      tryCatch(suppressWarnings(evaluate(obj, verbose = FALSE, seed = 42L)),
               error = function(e) NULL),
      .package = "horizons"
    )

    seen

  }

  ## Twenty predictors, 2 cm-1 apart, so a window of 13 fits the spectrum.
  obj <- make_eval_object(n_wn = 20, n_configs = 1)

  it("passes configure()'s sg_window and pca_threshold to every config", {

    obj$config$recipe <- list(sg_window = 13L, pca_threshold = 0.9)

    expect_identical(capture_recipe_args(obj),
                     list(sg_window = 13L, pca_threshold = 0.9))

  })

  it("falls back to the values the recipe always ran when the record is absent (older objects)", {

    obj$config$recipe <- NULL

    expect_identical(capture_recipe_args(obj),
                     list(sg_window = 9L, pca_threshold = 0.995))

  })

  ## A window as wide as the spectrum used to pass prep() and fail every
  ## config inside tune ("Grid search failed"); a wider one failed them all
  ## ("All configurations failed"). Neither named the window. evaluate() now
  ## refuses both before a single config runs.

  refused_without_running <- function(obj) {

    ran <- 0L

    err <- testthat::with_mocked_bindings(
      evaluate_single_config = function(...) {
        ran <<- ran + 1L
        tibble::tibble(config_id = "cfg_001", status = "failed")
      },
      tryCatch(evaluate(obj, verbose = FALSE, seed = 42L),
               error = function(e) e),
      .package = "horizons"
    )

    list(error = err, ran = ran)

  }

  it("refuses a window as wide as the spectrum, naming both numbers and the width", {

    narrow <- make_eval_object(n_wn = 9, n_configs = 1)
    narrow$config$recipe <- list(sg_window = 9L, pca_threshold = 0.995)

    out <- refused_without_running(narrow)

    expect_s3_class(out$error, "horizons_input_error")
    expect_match(conditionMessage(out$error), "9 grid points (18 cm", fixed = TRUE)
    expect_match(conditionMessage(out$error), "9 spectral columns", fixed = TRUE)
    expect_identical(out$ran, 0L)

  })

  it("refuses a window wider than the spectrum the same way", {

    obj$config$recipe <- list(sg_window = 21L, pca_threshold = 0.995)

    out <- refused_without_running(obj)

    expect_s3_class(out$error, "horizons_input_error")
    expect_match(conditionMessage(out$error), "21 grid points (42 cm", fixed = TRUE)
    expect_match(conditionMessage(out$error), "20 spectral columns", fixed = TRUE)
    expect_identical(out$ran, 0L)

  })

  it("records the settings it ran with, the width measured on the axis it ran on", {

    ## The axis here is 4 cm-1, as if standardize() had coarsened the object
    ## after configure(): the record follows the axis the recipe saw, which is
    ## why configure() stores no width of its own.
    coarse <- obj
    wn_old <- coarse$data$role_map$variable[coarse$data$role_map$role == "predictor"]
    wn_new <- paste0("wn_", seq(4000, by = -4, length.out = length(wn_old)))
    names(coarse$data$analysis)[match(wn_old, names(coarse$data$analysis))] <- wn_new
    coarse$data$role_map$variable[match(wn_old, coarse$data$role_map$variable)] <- wn_new
    coarse$config$recipe <- list(sg_window = 7L, pca_threshold = 0.9)

    result <- suppressWarnings(evaluate(coarse, verbose = FALSE, seed = 42L))

    expect_identical(result$evaluation$recipe,
                     list(sg_window = 7L, sg_window_cm = 28, pca_threshold = 0.9))

  })

})

## =========================================================================
## Seed reproducibility
## =========================================================================

describe("evaluate() - reproducibility", {

  it("produces identical results with the same seed", {

    obj <- make_eval_object(n_configs = 1)

    r1 <- suppressWarnings(evaluate(obj, verbose = FALSE, seed = 123L))
    r2 <- suppressWarnings(evaluate(obj, verbose = FALSE, seed = 123L))

    expect_equal(r1$evaluation$results$rmse, r2$evaluation$results$rmse)
    expect_equal(r1$evaluation$results$rsq, r2$evaluation$results$rsq)

  })

})


## =========================================================================
## Ranking tie-break and checkpoint scoring schema (review, 2026-09-15)
## =========================================================================

describe("rank_configs_by_cv() - tie-break", {

  it("breaks ties on config_id so row order cannot change the winner", {

    rows <- tibble::tibble(
      config_id = c("cfg_003", "cfg_001", "cfg_002"),
      status    = "success",
      cv_rpd    = c(1.029, 1.029, 0.955),
      cv_rmse   = c(0.50, 0.50, 0.70)
    )

    expect_equal(rank_configs_by_cv(rows, "rpd")$config_id[1],  "cfg_001")
    expect_equal(rank_configs_by_cv(rows, "rmse")$config_id[1], "cfg_001")

    ## same rows, different order -> same winner
    expect_equal(rank_configs_by_cv(rows[c(2, 3, 1), ], "rpd")$config_id[1], "cfg_001")

  })

})

describe("checkpoint scoring schema", {

  it("treats rows without the column as schema 1", {

    expect_identical(checkpoint_row_schema(tibble::tibble(config_id = "a")), 1L)
    expect_identical(checkpoint_row_schema(tibble::tibble(config_id = "a", scoring_schema = NA)), 1L)
    expect_identical(checkpoint_row_schema(tibble::tibble(config_id = "a", scoring_schema = 2L)), 2L)

  })

  it("drops rows scored under a different schema and keeps the current ones", {

    candidates <- lapply(
      list(
        tibble::tibble(config_id = "a", scoring_schema = 1L),
        tibble::tibble(config_id = "b", scoring_schema = SCORING_SCHEMA),
        tibble::tibble(config_id = "c", scoring_schema = NA_integer_),
        tibble::tibble(config_id = "d")
      ),
      function(r) list(row = r, source = "x", path = NA_character_)
    )

    gated <- gate_checkpoint_rows(candidates, list(data_hash = NA_character_),
                                  settings = NULL)

    expect_named(gated$kept, "b")
    expect_equal(gated$n_foreign, 3L)

  })

  it("checks the fingerprint before the schema, whichever store a row came from", {

    ## Written on other rows AND under an earlier schema: refused, not
    ## quietly dropped.
    row <- tibble::tibble(config_id = "a", scoring_schema = 1L,
                          data_hash = "other", data_n_rows = 10L)

    v <- checkpoint_row_verdict(row, list(data_hash = "this", data_n_rows = 10L),
                                settings = NULL)

    expect_identical(v$verdict, "data_mismatch")

  })

  it("stamps every result row with the current schema", {

    obj <- make_eval_object(n_configs = 2)
    res <- suppressWarnings(evaluate(obj, verbose = FALSE, seed = 42L))

    expect_true(all(res$evaluation$results$scoring_schema == SCORING_SCHEMA))

  })

  it("on resume, re-evaluates configs whose checkpoint was written under schema 1", {

    obj    <- make_eval_object(n_configs = 2)
    tmpdir <- withr::local_tempdir()

    first <- suppressWarnings(evaluate(obj, output_dir = tmpdir, verbose = FALSE, seed = 42L))

    ## Rewrite one per-config checkpoint as a legacy (schema-1) row
    f   <- file.path(tmpdir, "checkpoints", "cfg_001.rds")
    row <- readRDS(f)
    row$scoring_schema <- NULL
    row$cv_rpd <- 999          # a value that would win if it were trusted
    saveRDS(row, f)

    out <- capture.output(
      second <- suppressWarnings(evaluate(obj, output_dir = tmpdir, verbose = TRUE, seed = 42L))
    )

    expect_true(any(grepl("earlier scoring schema", out)))
    expect_false(any(second$evaluation$results$cv_rpd == 999))
    expect_equal(second$evaluation$best_config, first$evaluation$best_config)

  })

})

## =========================================================================
## Checkpoint data provenance (2026-09-21)
## =========================================================================
## A dry run and a real run sharing one output_dir used to resume each
## other's results, because checkpoints are keyed by config_id alone.

describe("eval_data_fingerprint()", {

  it("hashes the id column, invariant to row order", {

    df <- tibble::tibble(sample_id = c("c", "a", "b"), x = 1:3)
    rm <- tibble::tibble(variable = c("sample_id", "x"),
                         role     = c("id", "predictor"))

    fp1 <- eval_data_fingerprint(df, rm)
    fp2 <- eval_data_fingerprint(df[c(3, 1, 2), ], rm)

    expect_identical(fp1$data_hash, fp2$data_hash)
    expect_identical(fp1$data_n_rows, 3L)

  })

  it("changes when the row set changes", {

    rm <- tibble::tibble(variable = "sample_id", role = "id")

    a <- eval_data_fingerprint(tibble::tibble(sample_id = c("a", "b")), rm)
    b <- eval_data_fingerprint(tibble::tibble(sample_id = c("a", "c")), rm)

    expect_false(identical(a$data_hash, b$data_hash))

  })

  it("changes when the outcome changes on the same rows", {

    df <- tibble::tibble(sample_id = c("a", "b"), clay = c(1, 2), oc = c(3, 4))

    clay <- eval_data_fingerprint(
      df,
      tibble::tibble(variable = c("sample_id", "clay", "oc"),
                     role     = c("id", "outcome", "response"))
    )

    oc <- eval_data_fingerprint(
      df,
      tibble::tibble(variable = c("sample_id", "clay", "oc"),
                     role     = c("id", "response", "outcome"))
    )

    ## generate_config_id() does not hash the outcome, so without this the two
    ## runs share config ids AND a fingerprint.
    expect_false(identical(clay$data_hash, oc$data_hash))

  })

  it("returns NA when no identifier column is available", {

    fp <- eval_data_fingerprint(tibble::tibble(x = 1:3), NULL)

    expect_true(is.na(fp$data_hash))
    expect_identical(fp$data_n_rows, 3L)
    expect_null(fp$data_fields)

  })

  ## The hash covers the ids and the outcome name only; the fields cover the
  ## values on those rows (#42).
  roles_xy <- tibble::tibble(variable = c("sample_id", "wn_1", "wn_2", "y"),
                             role     = c("id", "predictor", "predictor", "outcome"))
  df_xy    <- tibble::tibble(sample_id = c("c", "a", "b"), wn_1 = c(1, 2, 3),
                             wn_2 = c(4, 5, 6), y = c(7, 8, 9))

  it("records the value fields, invariant to row order", {

    fp1 <- eval_data_fingerprint(df_xy, roles_xy)
    fp2 <- eval_data_fingerprint(df_xy[c(2, 3, 1), ], roles_xy)

    expect_named(fp1$data_fields, c("outcome", "ids", "roles", "outcome_values",
                                    "predictors", "covariates"))
    expect_identical(fp1$data_fields$outcome, "y")
    expect_identical(fp1$data_fields, fp2$data_fields)

  })

  it("changes only the predictor field when the spectra change on the same ids", {

    spectra <- df_xy
    spectra$wn_1 <- spectra$wn_1 + 0.001

    cmp <- compare_record(eval_data_fingerprint(spectra, roles_xy)$data_fields,
                          eval_data_fingerprint(df_xy, roles_xy)$data_fields)

    expect_identical(cmp$differ, "predictors")
    expect_identical(eval_data_fingerprint(spectra, roles_xy)$data_hash,
                     eval_data_fingerprint(df_xy, roles_xy)$data_hash)

  })

  it("changes only the outcome-values field when the outcome is rescaled under its name", {

    rescaled   <- df_xy
    rescaled$y <- rescaled$y * 10

    cmp <- compare_record(eval_data_fingerprint(rescaled, roles_xy)$data_fields,
                          eval_data_fingerprint(df_xy, roles_xy)$data_fields)

    expect_identical(cmp$differ, "outcome_values")

  })

  it("changes the roles field when a column changes role", {

    roles_meta <- roles_xy
    roles_meta$role[roles_meta$variable == "wn_2"] <- "meta"

    cmp <- compare_record(eval_data_fingerprint(df_xy, roles_meta)$data_fields,
                          eval_data_fingerprint(df_xy, roles_xy)$data_fields)

    expect_setequal(cmp$differ, c("roles", "predictors"))

  })

  it("ignores roles that never reach the model: a sibling response or a meta column", {

    wider <- df_xy
    wider$clay <- c(1, 2, 3)
    wider$note <- c("p", "q", "r")

    roles_wider <- rbind(roles_xy,
                         tibble::tibble(variable = c("clay", "note"),
                                        role     = c("response", "meta")))

    expect_identical(eval_data_fingerprint(wider, roles_wider)$data_fields,
                     eval_data_fingerprint(df_xy, roles_xy)$data_fields)

  })

})

## A condition's message on one line: cli wraps at the console width, so a
## phrase can straddle a line break.
flat_message <- function(err) {
  gsub("\\s+", " ", paste(conditionMessage(err), collapse = " "))
}

describe("evaluate() - checkpoint data provenance", {

  it("stamps the fingerprint and settings on results, checkpoints and the manifest", {

    obj    <- make_eval_object(n_configs = 2)
    tmpdir <- withr::local_tempdir()

    res <- suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                                     verbose = FALSE, seed = 42L))

    expect_true(all(!is.na(res$evaluation$results$data_hash)))
    expect_true(all(res$evaluation$results$data_n_rows ==
                      res$evaluation$n_train))

    manifest <- readRDS(file.path(tmpdir, "eval_manifest.rds"))
    expect_identical(manifest$schema_version, 4L)
    expect_identical(manifest$data_hash, res$evaluation$results$data_hash[1])
    expect_identical(manifest$data_n_rows, res$evaluation$n_train)
    expect_identical(manifest$settings,
                     eval_settings(cv_folds = 3L, grid_size = 2L,
                                   bayesian_iter = 0L, prune = FALSE,
                                   prune_threshold = NA_real_, seed = 42L,
                                   ## configure()'s recipe settings (#62), at
                                   ## the defaults an unconfigured record runs
                                   sg_window = 9L, pca_threshold = 0.995))

    row <- readRDS(file.path(tmpdir, "checkpoints", "cfg_001.rds"))
    expect_identical(row$data_hash, manifest$data_hash)
    expect_identical(row$data_fields[[1]], manifest$data_fields)
    expect_identical(manifest$data_fields$outcome, "SOC")
    expect_identical(row$settings[[1]], manifest$settings)
    expect_identical(res$evaluation$results$settings[[1]], manifest$settings)

  })

  it("resumes silently when the training rows are unchanged", {

    obj    <- make_eval_object(n_configs = 2)
    tmpdir <- withr::local_tempdir()

    first  <- suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                                        verbose = FALSE, seed = 42L))
    second <- suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                                        verbose = FALSE, seed = 42L))

    expect_equal(first$evaluation$best_config, second$evaluation$best_config)
    expect_equal(sort(first$evaluation$results$config_id),
                 sort(second$evaluation$results$config_id))

  })

  it("aborts when the same output_dir is resumed on a different row set", {

    tmpdir <- withr::local_tempdir()

    suppressWarnings(evaluate(make_eval_object(n = 40, n_configs = 2),
                              output_dir = tmpdir, prune = FALSE,
                              verbose = FALSE, seed = 42L))

    expect_error(
      suppressWarnings(evaluate(make_eval_object(n = 60, n_configs = 2),
                                output_dir = tmpdir, prune = FALSE,
                                verbose = FALSE, seed = 42L)),
      class = "horizons_input_error"
    )

    err <- tryCatch(
      suppressWarnings(evaluate(make_eval_object(n = 60, n_configs = 2),
                                output_dir = tmpdir, prune = FALSE,
                                verbose = FALSE, seed = 42L)),
      horizons_input_error = function(e) e
    )

    msg <- flat_message(err)

    expect_match(msg, "different training data")
    expect_match(msg, basename(tmpdir), fixed = TRUE)
    expect_match(msg, "different set of training samples")
    expect_match(msg, "output_dir")

    ## Reported from the verb the user called, not from a helper.
    expect_identical(as.character(err$call[[1]]), "evaluate")

  })

  it("aborts when the same rows are re-run under a different outcome", {

    tmpdir <- withr::local_tempdir()

    ## Same rows, same config ids, different response. Only the outcome name
    ## differs, which is the collision generate_config_id() cannot see.
    obj_soc <- make_eval_object(n_configs = 2)

    obj_clay <- obj_soc
    names(obj_clay$data$analysis)[names(obj_clay$data$analysis) == "SOC"] <- "clay"
    obj_clay$data$role_map$variable[obj_clay$data$role_map$variable == "SOC"] <- "clay"

    suppressWarnings(evaluate(obj_soc, output_dir = tmpdir, prune = FALSE,
                              verbose = FALSE, seed = 42L))

    err <- tryCatch(
      suppressWarnings(evaluate(obj_clay, output_dir = tmpdir, prune = FALSE,
                                verbose = FALSE, seed = 42L)),
      horizons_input_error = function(e) e
    )

    expect_s3_class(err, "horizons_input_error")

    ## Named, not two opaque hashes.
    msg <- flat_message(err)
    expect_match(msg, "computed for outcome SOC; this run models clay")
    expect_match(msg, "one `output_dir` per outcome")

  })

  ## The same samples with other values used to resume, because the hash
  ## covers the ids and the outcome name only (#42).
  refuses_on <- function(edit) {

    ## Twelve predictors, so demoting one still leaves the spectrum wider than
    ## the default window of 9 and evaluate()'s window check (#62) does not
    ## refuse ahead of the checkpoint gate under test.
    obj    <- make_eval_object(n_wn = 12, n_configs = 2)
    tmpdir <- withr::local_tempdir(.local_envir = parent.frame())

    suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                              verbose = FALSE, seed = 42L))

    tryCatch(
      suppressWarnings(evaluate(edit(obj), output_dir = tmpdir, prune = FALSE,
                                verbose = FALSE, seed = 42L)),
      horizons_input_error = function(e) e
    )

  }

  it("aborts when the spectra change on the same samples", {

    err <- refuses_on(function(o) {
      o$data$analysis$wn_4000 <- o$data$analysis$wn_4000 + 0.01
      o
    })

    expect_s3_class(err, "horizons_input_error")
    msg <- flat_message(err)
    expect_match(msg, "different predictor values")
    expect_no_match(msg, "different outcome values")

  })

  it("aborts when the outcome is rescaled under the same name", {

    err <- refuses_on(function(o) {
      o$data$analysis$SOC <- o$data$analysis$SOC * 10
      o
    })

    expect_s3_class(err, "horizons_input_error")
    expect_match(flat_message(err),
                 "different outcome values")

  })

  it("aborts when a predictor changes role", {

    err <- refuses_on(function(o) {
      o$data$role_map$role[o$data$role_map$variable == "wn_3982"] <- "meta"
      ## Keep the stored count honest so the only thing wrong with this
      ## object is what the test means to exercise — evaluate()'s own
      ## fingerprint catching the role change — rather than also tripping
      ## the entry-stage validate_horizons_data() call (#24) on a stale
      ## n_predictors.
      o$data$n_predictors <- sum(o$data$role_map$role == "predictor")
      o
    })

    expect_s3_class(err, "horizons_input_error")
    expect_match(flat_message(err), "role_map")

  })

  it("resumes after add_response() joins another property", {

    ## A sibling response is held out of the model (response_hold), so it
    ## cannot change what a row holds, and must not refuse the resume.
    obj    <- make_eval_object(n_configs = 2)
    tmpdir <- withr::local_tempdir()

    first <- suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                                       verbose = FALSE, seed = 42L))

    lab <- tibble::tibble(sample_id = obj$data$analysis$sample_id,
                          clay      = seq_len(nrow(obj$data$analysis)))
    invisible(capture.output(wider <- add_response(obj, lab, variable = "clay")))

    warns <- testthat::capture_warnings(
      second <- evaluate(wider, output_dir = tmpdir, prune = FALSE,
                         verbose = FALSE, seed = 42L)
    )

    expect_false(any(grepl("fingerprint", warns)))
    expect_equal(second$evaluation$results$runtime_secs,
                 first$evaluation$results$runtime_secs)

  })

  it("warns once and proceeds for legacy checkpoints with no fingerprint", {

    obj    <- make_eval_object(n_configs = 2)
    tmpdir <- withr::local_tempdir()

    first <- suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                                       verbose = FALSE, seed = 42L))

    ## Hand-write the pre-provenance shape: no fingerprint columns.
    for (f in list.files(file.path(tmpdir, "checkpoints"), full.names = TRUE)) {

      row <- readRDS(f)
      row$data_hash   <- NULL
      row$data_n_rows <- NULL
      row$data_fields <- NULL
      saveRDS(row, f)

    }

    warns <- testthat::capture_warnings(
      second <- evaluate(obj, output_dir = tmpdir, prune = FALSE,
                         verbose = FALSE, seed = 42L)
    )

    ## Once per run, not once per unverifiable file.
    expect_equal(sum(grepl("no training-data fingerprint", gsub("\\s+", " ", warns))), 1L)

    expect_s3_class(second, "horizons_eval")
    expect_equal(first$evaluation$best_config, second$evaluation$best_config)

  })

  it("warns once and resumes rows with the id hash but no value fields", {

    obj    <- make_eval_object(n_configs = 2)
    tmpdir <- withr::local_tempdir()

    first <- suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                                       verbose = FALSE, seed = 42L))

    ## The shape every row written before the value fields has: unverified,
    ## not refused, even though the values cannot be checked.
    for (f in list.files(file.path(tmpdir, "checkpoints"), full.names = TRUE)) {

      row <- readRDS(f)
      row$data_fields <- NULL
      saveRDS(row, f)

    }

    warns <- testthat::capture_warnings(
      second <- evaluate(obj, output_dir = tmpdir, prune = FALSE,
                         verbose = FALSE, seed = 42L)
    )

    expect_equal(sum(grepl("no training-data fingerprint covering", gsub("\\s+", " ", warns))), 1L)
    expect_equal(second$evaluation$results$runtime_secs,
                 first$evaluation$results$runtime_secs)

  })

})

## =========================================================================
## One checkpoint store (#42)
## =========================================================================
## evaluate() used to keep two stores: a whole-table eval_checkpoint.rds,
## written only by the sequential path, and one file per config under
## checkpoints/. The single file was read first and shadowed the per-config
## rows, so repairing a per-config file changed nothing, and the two stores
## ran their gates in different orders. The per-config files are now the only
## store; a legacy single file is read only for configs with no per-config
## file.

## The per-config rows of a finished run, named by config id.
read_per_config_rows <- function(output_dir) {

  files <- list.files(file.path(output_dir, "checkpoints"),
                      pattern = "\\.rds$", full.names = TRUE)

  stats::setNames(lapply(files, readRDS), sub("\\.rds$", "", basename(files)))

}

## A single-file checkpoint in the shape the old sequential path wrote: the
## whole table, with the fingerprint also carried in attributes.
write_legacy_checkpoint <- function(output_dir, rows) {

  tbl <- dplyr::bind_rows(rows)
  attr(tbl, "data_hash")   <- rows[[1]]$data_hash
  attr(tbl, "data_n_rows") <- rows[[1]]$data_n_rows
  saveRDS(tbl, file.path(output_dir, "eval_checkpoint.rds"))

}

describe("evaluate() - one checkpoint store (#42)", {

  it("reads a repaired per-config file over a legacy single file", {

    obj    <- make_eval_object(n_configs = 2)
    tmpdir <- withr::local_tempdir()

    suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                              verbose = FALSE, seed = 42L))

    rows <- read_per_config_rows(tmpdir)

    ## The legacy file carries a bad cfg_001; the per-config file, as if it
    ## had been repaired, carries the good one.
    stale <- rows
    stale$cfg_001$cv_rpd <- 999
    write_legacy_checkpoint(tmpdir, stale)

    second <- suppressMessages(suppressWarnings(
      evaluate(obj, output_dir = tmpdir, prune = FALSE, verbose = FALSE,
               seed = 42L)
    ))

    got <- second$evaluation$results
    expect_equal(got$cv_rpd[got$config_id == "cfg_001"], rows$cfg_001$cv_rpd)

  })

  it("reads a legacy single file only for configs with no per-config file, and says so", {

    obj    <- make_eval_object(n_configs = 2)
    tmpdir <- withr::local_tempdir()

    suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                              verbose = FALSE, seed = 42L))

    ## Mark every legacy row so its origin shows in the results.
    legacy <- lapply(read_per_config_rows(tmpdir), function(r) {
      r$runtime_secs <- -1
      r
    })
    write_legacy_checkpoint(tmpdir, legacy)
    unlink(file.path(tmpdir, "checkpoints", "cfg_002.rds"))

    expect_message(
      second <- suppressWarnings(
        evaluate(obj, output_dir = tmpdir, prune = FALSE, verbose = FALSE,
                 seed = 42L)
      ),
      "eval_checkpoint.rds",
      class = "horizons_checkpoint_message"
    )

    got <- second$evaluation$results
    expect_equal(got$runtime_secs[got$config_id == "cfg_002"], -1)
    expect_false(got$runtime_secs[got$config_id == "cfg_001"] == -1)

    ## The legacy row is copied into the per-config store, so deleting the
    ## legacy file afterwards loses nothing.
    expect_true(file.exists(file.path(tmpdir, "checkpoints", "cfg_002.rds")))

  })

  it("warns naming a per-config file it cannot read, and re-evaluates that config", {

    obj    <- make_eval_object(n_configs = 2)
    tmpdir <- withr::local_tempdir()

    suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                              verbose = FALSE, seed = 42L))

    ## Only the per-config store, so nothing else can stand in for the file.
    unlink(file.path(tmpdir, "eval_checkpoint.rds"))
    writeLines("not an rds file", file.path(tmpdir, "checkpoints", "cfg_001.rds"))

    warns <- testthat::capture_warnings(
      second <- evaluate(obj, output_dir = tmpdir, prune = FALSE,
                         verbose = FALSE, seed = 42L)
    )

    expect_true(any(grepl("cfg_001.rds", warns, fixed = TRUE)))
    expect_setequal(second$evaluation$results$config_id, c("cfg_001", "cfg_002"))

    ## Re-evaluated and rewritten, so the next resume can read it.
    expect_identical(readRDS(file.path(tmpdir, "checkpoints", "cfg_001.rds"))$config_id,
                     "cfg_001")

  })

  it("never lets a leftover temp file stand in for a config's own file", {

    ## Older versions wrote each row to file*.rds before renaming it; a
    ## leftover one sorted ahead of most model prefixes and shadowed the real
    ## file (review finding, #42). Config ids with a model prefix reproduce
    ## the order.
    obj <- make_eval_object(n_configs = 2)
    obj$config$configs$config_id <- c("rf_001", "rf_002")
    tmpdir <- withr::local_tempdir()

    suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                              verbose = FALSE, seed = 42L))

    real     <- readRDS(file.path(tmpdir, "checkpoints", "rf_001.rds"))
    leftover <- real
    leftover$data_hash <- NA_character_      # unverified, and made to win
    leftover$cv_rpd    <- 999
    saveRDS(leftover, file.path(tmpdir, "checkpoints", "file1a2b3c.rds"))

    expect_identical(
      list.files(file.path(tmpdir, "checkpoints"))[1], "file1a2b3c.rds"
    )

    warns <- testthat::capture_warnings(
      second <- evaluate(obj, output_dir = tmpdir, prune = FALSE,
                         verbose = FALSE, seed = 42L)
    )

    got <- second$evaluation$results
    expect_equal(got$cv_rpd[got$config_id == "rf_001"], real$cv_rpd)

    flat <- gsub("\\s+", " ", warns)
    expect_true(any(grepl("file1a2b3c.rds: holds config rf_001; not a checkpoint name",
                          flat, fixed = TRUE)))

  })

  it("refuses a foreign-schema row written on other data from either store", {

    obj    <- make_eval_object(n_configs = 2)
    tmpdir <- withr::local_tempdir()

    suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                              verbose = FALSE, seed = 42L))
    unlink(file.path(tmpdir, "eval_checkpoint.rds"))

    f       <- file.path(tmpdir, "checkpoints", "cfg_001.rds")
    foreign <- readRDS(f)
    foreign$scoring_schema <- 1L
    foreign$data_hash      <- "0000deadbeef"

    refusal <- function() {
      tryCatch(
        suppressMessages(suppressWarnings(
          evaluate(obj, output_dir = tmpdir, prune = FALSE, verbose = FALSE,
                   seed = 42L)
        )),
        horizons_input_error = function(e) e
      )
    }

    ## In the per-config store: the fingerprint is checked before the
    ## schema, so the row aborts rather than being dropped silently.
    saveRDS(foreign, f)
    err <- refusal()
    expect_s3_class(err, "horizons_input_error")
    expect_match(flat_message(err), "cfg_001.rds", fixed = TRUE)

    ## In a legacy single file, for a config with no per-config file: the
    ## same verdict.
    unlink(f)
    write_legacy_checkpoint(tmpdir, list(cfg_001 = foreign))
    err <- refusal()
    expect_s3_class(err, "horizons_input_error")
    expect_match(flat_message(err), "eval_checkpoint.rds", fixed = TRUE)

  })

})

## =========================================================================
## Tuning-settings provenance (#42)
## =========================================================================
## The data fingerprint says which rows a checkpoint was scored on, not how it
## was tuned. Reconfiguring cv_folds or grid_size and re-running into the same
## output_dir used to resume results tuned under the old settings, silently.

describe("eval_settings()", {

  it("is invariant to integer versus double spelling", {

    expect_identical(eval_settings(grid_size = 10L, seed = 42L),
                     eval_settings(grid_size = 10, seed = 42))

  })

  it("names the settings that differ, and separates the ones a row lacks", {

    current <- eval_settings(cv_folds = 5L, grid_size = 10L, seed = 42L)
    stored  <- eval_settings(cv_folds = 5L, grid_size = 20L)

    cmp <- compare_record(stored, current)

    expect_identical(cmp$differ, "grid_size")
    expect_identical(cmp$missing, "seed")

    none <- compare_record(NULL, current)
    expect_length(none$differ, 0)
    expect_setequal(none$missing, names(current))

  })

})

describe("evaluate() - tuning-settings provenance", {

  it("refuses to resume checkpoints tuned under a different grid_size, naming it", {

    obj    <- make_eval_object(n_configs = 2)
    tmpdir <- withr::local_tempdir()

    suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                              verbose = FALSE, seed = 42L))

    changed <- obj
    changed$config$tuning$grid_size <- obj$config$tuning$grid_size + 1L

    err <- tryCatch(
      suppressWarnings(evaluate(changed, output_dir = tmpdir, prune = FALSE,
                                verbose = FALSE, seed = 42L)),
      horizons_input_error = function(e) e
    )

    expect_s3_class(err, "horizons_input_error")

    msg <- flat_message(err)
    expect_match(msg, "grid_size")
    expect_match(msg, "settings")
    expect_match(msg, "output_dir")
    expect_identical(as.character(err$call[[1]]), "evaluate")

  })

  it("refuses a changed prune threshold while pruning, and ignores it when not", {

    obj    <- make_eval_object(n_configs = 2)
    tmpdir <- withr::local_tempdir()

    suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = TRUE,
                              prune_threshold = 1, verbose = FALSE, seed = 42L))

    expect_error(
      suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = TRUE,
                                prune_threshold = 2, verbose = FALSE, seed = 42L)),
      "prune_threshold",
      class = "horizons_input_error"
    )

    ## With pruning off the threshold is never read, so it cannot have
    ## changed what a row holds.
    off <- withr::local_tempdir()

    first  <- suppressWarnings(evaluate(obj, output_dir = off, prune = FALSE,
                                        prune_threshold = 1, verbose = FALSE, seed = 42L))
    second <- suppressWarnings(evaluate(obj, output_dir = off, prune = FALSE,
                                        prune_threshold = 2, verbose = FALSE, seed = 42L))

    expect_equal(second$evaluation$results$runtime_secs,
                 first$evaluation$results$runtime_secs)

  })

  it("resumes when only the ranking metric changes", {

    obj    <- make_eval_object(n_configs = 2)
    tmpdir <- withr::local_tempdir()

    first  <- suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                                        metric = "rpd", verbose = FALSE, seed = 42L))
    second <- suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                                        metric = "rmse", verbose = FALSE, seed = 42L))

    ## Every row carries all six cv_ columns and the ranking is recomputed on
    ## each run, so the metric is not part of what a row holds.
    expect_equal(second$evaluation$results$runtime_secs,
                 first$evaluation$results$runtime_secs)
    expect_equal(second$evaluation$rank_metric, "rmse")

  })

  it("counts rows with no settings stamp as unverified, warns once, and resumes", {

    obj    <- make_eval_object(n_configs = 2)
    tmpdir <- withr::local_tempdir()

    first <- suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                                       verbose = FALSE, seed = 42L))
    unlink(file.path(tmpdir, "eval_checkpoint.rds"))

    ## The shape every row written before the settings stamp has.
    for (f in list.files(file.path(tmpdir, "checkpoints"), full.names = TRUE)) {

      row <- readRDS(f)
      row$settings <- NULL
      saveRDS(row, f)

    }

    warns <- testthat::capture_warnings(
      second <- evaluate(obj, output_dir = tmpdir, prune = FALSE,
                         verbose = FALSE, seed = 42L)
    )

    expect_equal(sum(grepl("no tuning-settings fingerprint", gsub("\\s+", " ", warns))), 1L)
    expect_equal(second$evaluation$results$runtime_secs,
                 first$evaluation$results$runtime_secs)

  })

  ## configure()'s recipe settings (#62) are object-level, so the config id
  ## does not carry them; the settings record is what keeps a run with another
  ## window from resuming rows built with the old one.

  it("refuses to resume checkpoints built with a different sg_window or pca_threshold, naming it", {

    obj    <- make_eval_object(n_wn = 20, n_configs = 2)
    obj$config$recipe <- list(sg_window = 9L, pca_threshold = 0.995)
    tmpdir <- withr::local_tempdir()

    suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                              verbose = FALSE, seed = 42L))

    wider <- obj
    wider$config$recipe$sg_window <- 11L

    err <- tryCatch(
      suppressWarnings(evaluate(wider, output_dir = tmpdir, prune = FALSE,
                                verbose = FALSE, seed = 42L)),
      horizons_input_error = function(e) e
    )

    expect_s3_class(err, "horizons_input_error")
    expect_match(flat_message(err), "sg_window")
    expect_match(flat_message(err), "settings")

    tighter <- obj
    tighter$config$recipe$pca_threshold <- 0.9

    expect_error(
      suppressWarnings(evaluate(tighter, output_dir = tmpdir, prune = FALSE,
                                verbose = FALSE, seed = 42L)),
      "pca_threshold",
      class = "horizons_input_error"
    )

  })

  it("counts rows written before the recipe settings were recorded as unverified, and resumes", {

    obj    <- make_eval_object(n_wn = 20, n_configs = 2)
    tmpdir <- withr::local_tempdir()

    first <- suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                                       verbose = FALSE, seed = 42L))

    ## The shape rows have when a version recorded the tuning settings but not
    ## yet sg_window and pca_threshold: a record, lacking the two.
    for (f in list.files(file.path(tmpdir, "checkpoints"), full.names = TRUE)) {

      row <- readRDS(f)
      row$settings[[1]] <- row$settings[[1]][setdiff(names(row$settings[[1]]),
                                                     c("sg_window", "pca_threshold"))]
      saveRDS(row, f)

    }

    warns <- testthat::capture_warnings(
      second <- evaluate(obj, output_dir = tmpdir, prune = FALSE,
                         verbose = FALSE, seed = 42L)
    )

    expect_equal(sum(grepl("2 have no tuning-settings fingerprint", gsub("\\s+", " ", warns))), 1L)
    expect_equal(second$evaluation$results$runtime_secs,
                 first$evaluation$results$runtime_secs)

  })

})
