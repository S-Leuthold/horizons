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

    obj <- make_eval_object()
    obj$data$analysis$SOC <- NULL

    err <- expect_error(evaluate(obj, verbose = FALSE),
                        class = "horizons_input_error")

    expect_match(conditionMessage(err), "SOC", fixed = TRUE)
    expect_match(conditionMessage(err), "no such column", fixed = TRUE)
    expect_no_match(conditionMessage(err), "All outcome values are NA", fixed = TRUE)

  })

})

## =========================================================================
## Checkpointing
## =========================================================================

describe("evaluate() - checkpointing", {

  it("writes checkpoint file when output_dir is provided", {

    obj <- make_eval_object(n_configs = 2)
    tmpdir <- tempfile("eval_ckpt_write_")
    dir.create(tmpdir)
    on.exit(unlink(tmpdir, recursive = TRUE))

    checkpoint_path <- file.path(tmpdir, "eval_checkpoint.rds")
    checkpoint_dir  <- file.path(tmpdir, "checkpoints")

    result <- suppressWarnings(evaluate(obj, output_dir = tmpdir, verbose = FALSE, seed = 42L))

    ## Checkpoint file should exist after completion
    expect_true(file.exists(checkpoint_path))

    ## Per-config checkpoint files should also exist
    expect_true(dir.exists(checkpoint_dir))
    expect_gt(length(list.files(checkpoint_dir, pattern = "\\.rds$")), 0)

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
    obj$config$tuning$bayesian_iter <- 1L

    tmpdir <- tempfile("eval_ckpt_relabel_")
    dir.create(tmpdir)
    on.exit(unlink(tmpdir, recursive = TRUE))

    ## A first run that prunes both, standing in for the old code's rows
    first <- suppressWarnings(evaluate(obj, prune_threshold = 9999,
                                       output_dir = tmpdir, verbose = FALSE,
                                       seed = 42L))
    expect_true(all(first$evaluation$results$status == "pruned"))

    ## Rows written before the gate's reading was recorded do not carry it
    strip <- function(rows) {
      rows$below_prune_threshold <- NULL
      rows$prune_threshold       <- NULL
      rows
    }

    for (f in list.files(file.path(tmpdir, "checkpoints"), full.names = TRUE)) {
      saveRDS(strip(readRDS(f)), f)
    }

    single_file <- file.path(tmpdir, "eval_checkpoint.rds")
    single      <- readRDS(single_file)
    stripped    <- strip(single)
    attr(stripped, "data_hash")   <- attr(single, "data_hash")
    attr(stripped, "data_n_rows") <- attr(single, "data_n_rows")
    saveRDS(stripped, single_file)

    obj$config$tuning$bayesian_iter <- 0L

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

    unlink(file.path(tmpdir, "eval_checkpoint.rds"))

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
    expect_match(msg, "eval_checkpoint.rds", fixed = TRUE)

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

    rows <- tibble::tibble(
      config_id      = c("a", "b", "c"),
      scoring_schema = c(1L, SCORING_SCHEMA, NA)
    )

    kept <- suppressMessages(capture.output(out <- drop_foreign_schema_rows(rows, verbose = TRUE)))

    expect_equal(out$config_id, "b")
    expect_true(any(grepl("earlier scoring schema", kept)))

    legacy <- tibble::tibble(config_id = c("a", "b"))
    expect_equal(nrow(drop_foreign_schema_rows(legacy, verbose = FALSE)), 0)

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
    ## and the single-file checkpoint, which the sequential path also writes
    single <- file.path(tmpdir, "eval_checkpoint.rds")
    if (file.exists(single)) {
      s <- readRDS(single); s$scoring_schema <- NULL; s$cv_rpd[s$config_id == "cfg_001"] <- 999
      saveRDS(s, single)
    }

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

  })

})

describe("evaluate() - checkpoint data provenance", {

  it("stamps the fingerprint on results, checkpoints and the manifest", {

    obj    <- make_eval_object(n_configs = 2)
    tmpdir <- withr::local_tempdir()

    res <- suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                                     verbose = FALSE, seed = 42L))

    expect_true(all(!is.na(res$evaluation$results$data_hash)))
    expect_true(all(res$evaluation$results$data_n_rows ==
                      res$evaluation$n_train))

    manifest <- readRDS(file.path(tmpdir, "eval_manifest.rds"))
    expect_identical(manifest$schema_version, 3L)
    expect_identical(manifest$data_hash, res$evaluation$results$data_hash[1])
    expect_identical(manifest$data_n_rows, res$evaluation$n_train)

    row <- readRDS(file.path(tmpdir, "checkpoints", "cfg_001.rds"))
    expect_identical(row$data_hash, manifest$data_hash)

    single <- readRDS(file.path(tmpdir, "eval_checkpoint.rds"))
    expect_identical(attr(single, "data_hash"), manifest$data_hash)

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

    msg <- paste(conditionMessage(err), collapse = " ")

    expect_match(msg, "different training data")
    expect_match(msg, basename(tmpdir), fixed = TRUE)
    expect_match(msg, "training row")
    expect_match(msg, "output_dir")

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

    expect_error(
      suppressWarnings(evaluate(obj_clay, output_dir = tmpdir, prune = FALSE,
                                verbose = FALSE, seed = 42L)),
      class = "horizons_input_error"
    )

  })

  it("warns once and proceeds for legacy checkpoints with no fingerprint", {

    obj    <- make_eval_object(n_configs = 2)
    tmpdir <- withr::local_tempdir()

    first <- suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                                       verbose = FALSE, seed = 42L))

    ## Hand-write the pre-provenance shape: no columns, no attributes.
    for (f in list.files(file.path(tmpdir, "checkpoints"), full.names = TRUE)) {

      row <- readRDS(f)
      row$data_hash   <- NULL
      row$data_n_rows <- NULL
      saveRDS(row, f)

    }

    single <- file.path(tmpdir, "eval_checkpoint.rds")
    s      <- readRDS(single)
    s$data_hash   <- NULL
    s$data_n_rows <- NULL
    attr(s, "data_hash")   <- NULL
    attr(s, "data_n_rows") <- NULL
    saveRDS(s, single)

    warns <- testthat::capture_warnings(
      second <- evaluate(obj, output_dir = tmpdir, prune = FALSE,
                         verbose = FALSE, seed = 42L)
    )

    ## Once per run, not once per unverifiable file.
    expect_equal(sum(grepl("no training-data fingerprint", warns)), 1L)

    expect_s3_class(second, "horizons_eval")
    expect_equal(first$evaluation$best_config, second$evaluation$best_config)

  })

})
