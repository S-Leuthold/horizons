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
## Shared fixtures (helper-memo.R)
## =========================================================================
## Built on first use and shared by every test in this file that needs them.

## EV60: a finished evaluate() with no output_dir, at n = 60 and two configs.
build_ev60 <- function() {

  obj <- make_eval_object(n = 60, n_configs = 2)
  suppressWarnings(evaluate(obj, verbose = FALSE, seed = 42L))

}

ev60 <- function() memo_fixture("ev60", build_ev60)

## Checkpoint templates. Most checkpoint tests start from the same first run
## into an output_dir and then edit or resume it. That run is built once per
## settings variant as a template directory, and each test gets its own copy
## in `$dir` (deleted when the test ends), so a test that edits checkpoint
## rows cannot reach another. `$value` holds the object the template was run
## on (`obj`) and the first run's result (`first`); a test resumes that
## object from its copy.
##
##   CK-A  two configs, prune = FALSE
##   CK-B  two configs, prune = TRUE at the default threshold of 1
##   CK-C  as CK-A, on twelve predictors
##   CK-D  as CK-A, on twenty predictors
build_checkpoints <- function(dir, prune = FALSE, n_wn = 10) {

  obj   <- make_eval_object(n_wn = n_wn, n_configs = 2)
  first <- suppressWarnings(evaluate(obj, output_dir = dir, prune = prune,
                                     prune_threshold = 1, verbose = FALSE,
                                     seed = 42L))

  list(obj = obj, first = first)

}

## CK-D's object has no recipe record, and one of its users resumes it with
## an explicit sg_window = 9 and pca_threshold = 0.995. Those are the values
## the recipe runs without a record, so both stamp the same settings; the
## build checks the rows it wrote, so that a change to the defaults fails
## here rather than turning that test's refusals into refusals of something
## else.
build_checkpoints_d <- function(dir) {

  out <- build_checkpoints(dir, n_wn = 20)

  rows    <- lapply(list.files(file.path(dir, "checkpoints"), full.names = TRUE),
                    readRDS)
  stamped <- lapply(rows, function(r) r$settings[[1]][c("sg_window", "pca_threshold")])

  if (length(rows) != 2L ||
      !all(vapply(stamped, identical, logical(1),
                  eval_settings(sg_window = 9L, pca_threshold = 0.995)))) {
    stop("CK-D's checkpoint rows are not stamped sg_window = 9, pca_threshold = 0.995.",
         call. = FALSE)
  }

  out

}

ck_a <- function(.env = parent.frame()) {
  memo_dir("ck_a", build_checkpoints, prune = FALSE, .env = .env)
}

ck_b <- function(.env = parent.frame()) {
  memo_dir("ck_b", build_checkpoints, prune = TRUE, .env = .env)
}

ck_c <- function(.env = parent.frame()) {
  memo_dir("ck_c", build_checkpoints, n_wn = 12, .env = .env)
}

ck_d <- function(.env = parent.frame()) {
  memo_dir("ck_d", build_checkpoints_d, .env = .env)
}

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

  it("aborts when sample size is too small for CV, counting only rows with an observed outcome", {

    ## The abort carries no condition class yet, so its text is asserted
    obj <- make_eval_object(n = 4)

    expect_error(evaluate(obj, verbose = FALSE),
                 "Need at least 6 samples (cv_folds * 2), but only 4 available.",
                 fixed = TRUE)

    ## Eight rows, three without an outcome: five can be modelled
    obj <- make_eval_object(n = 8)
    obj$data$analysis$SOC[1:3] <- NA_real_

    expect_error(evaluate(obj, verbose = FALSE),
                 "Need at least 6 samples (cv_folds * 2), but only 5 available.",
                 fixed = TRUE)

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

  ## The object EV60 evaluates
  obj <- make_eval_object(n = 60, n_configs = 2)

  it("is a horizons_eval with every expected results column, the default rank metric, and a split that covers every row", {

    result <- ev60()

    ## The validator gates on horizons_eval alone; the rest of the class is
    ## asserted here
    expect_identical(class(result), c("horizons_eval", "horizons_data", "list"))

    expect_true(all(EXPECTED_EVAL_COLS %in% names(result$evaluation$results)))
    expect_equal(result$evaluation$rank_metric, "rpd")

    expect_true(result$evaluation$n_train > 0)
    expect_true(result$evaluation$n_test > 0)
    expect_equal(result$evaluation$n_train + result$evaluation$n_test,
                 nrow(obj$data$analysis))

  })

  it("writes exactly the evaluation keys new_horizons_data() declares (#71)", {

    result <- ev60()

    ## The constructor's empty slot is what configure() resets to, so it has
    ## to name what evaluate() actually writes.
    expect_identical(names(result$evaluation), names(new_horizons_data()$evaluation))

  })

  it("records that it screened the configurations (#45)", {

    result <- ev60()

    ## fit()'s cold start writes FALSE; evaluate() ranked, so TRUE.
    expect_true(result$evaluation$screened)

  })

  it("has non-NA metrics for successful configs", {

    result <- ev60()

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

    skip_unless_slow_tier()

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

    result <- ev60()

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

    unranked <- rows
    unranked$cv_rpd[1] <- NA_real_

    expect_warning(ranked <- rank_configs_by_cv(unranked, "rpd"), "skipped")
    expect_equal(ranked$config_id, c("b", "c"))

  })

  it("aborts with a re-run remedy when nothing can be ranked", {

    all_na <- rows
    all_na$cv_rpd <- NA_real_

    expect_error(rank_configs_by_cv(all_na, "rpd"), "[Rr]e-run evaluate")

    ## The abort carries no package class. The all-NA abort above also says
    ## to re-run evaluate(), so the missing column is matched by its own text.
    expect_error(rank_configs_by_cv(rows[, c("config_id", "status", "rpd")], "rpd"),
                 "Evaluation results carry no `cv_rpd` column", fixed = TRUE)

  })

})

## =========================================================================
## All configs fail
## =========================================================================

describe("evaluate() - all configs fail", {

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
    expect_match(msg, "All configurations failed", fixed = TRUE)
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

  it("outcome_complete_rows() refuses a role map that gives no column the outcome role", {

    ## An outcome column the table lacks is refused here too; that case is
    ## tested through fit()'s cold start (test-pipeline-fit.R, "names an
    ## outcome column the analysis table lacks"), since through evaluate() the
    ## validator refuses the object first (the test above).
    df <- tibble::tibble(sample_id = c("A", "B"), y = c(1, 2))

    expect_error(outcome_complete_rows(df, character(0)),
                 "The role map gives no column the \"outcome\" role",
                 fixed = TRUE, class = "horizons_input_error")

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

    ## No trim was requested, so none is applied or recorded (#77)
    expect_identical(trim_training_responses(drawn$split, "SOC", NULL, "sample_id"),
                     list(split = drawn$split, record = NULL))
    expect_true("response_trim" %in% names(ev$evaluation))
    expect_null(ev$evaluation$response_trim)

  })

})

## =========================================================================
## Response outliers are trimmed from the training partition (#77)
## =========================================================================
## validate(remove_outliers = "response") used to compute Tukey fences over
## every row and remove the rows outside them before any split existed, so
## the test set lost exactly its hardest cases, chosen by their own labels.
## validate() now records the request; evaluate() draws the split on the
## untrimmed rows and fences the training partition alone.

## make_eval_object() with eight extreme labels, four high and four low. At
## seed 307 the draw puts some of them on each side of the split, which the
## first test checks, since everything after it rests on that.
EXTREME_IDS <- sprintf("S%03d", c(1:4, 31:34))

make_extreme_object <- function() {

  obj <- make_eval_object(n = 60, n_configs = 1)
  obj$data$analysis$SOC[match(EXTREME_IDS, obj$data$analysis$sample_id)] <-
    c(20, 25, 30, 35, -15, -20, -25, -30)
  ## The low extremes are negative, so the outcome is signed (#76)
  obj$config$outcome_range <- c(-Inf, Inf)
  obj

}

## Run `expr`, keeping every warning's condition object (so a class can be
## checked) and muffling them all, so none reaches the test reporter.
collect_warnings <- function(expr) {

  caught <- list()

  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      caught[[length(caught) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )

  list(value = value, warnings = caught)

}

has_warning_class <- function(warnings, class) {
  any(vapply(warnings, inherits, logical(1), class))
}

## The Tukey fences on a set of values, computed here rather than through the
## package's helper, so the test does not check the helper against itself.
fences_of <- function(values, k = 1.5) {
  q <- stats::quantile(values, c(0.25, 0.75), names = FALSE)
  c(q[1] - k * (q[2] - q[1]), q[2] + k * (q[2] - q[1]))
}

describe("evaluate() - response outliers are trimmed from the training partition (#77)", {

  obj <- make_extreme_object()
  utils::capture.output(
    v <- suppressWarnings(validate(obj, remove_outliers = "response"))
  )
  request <- response_trim_request(v, "SOC")

  ## The split as drawn, before any trim
  plain       <- suppressWarnings(draw_eval_split(v$data$analysis, "SOC", 307L))$split
  train_plain <- rsample::training(plain)
  test_plain  <- rsample::testing(plain)

  fences        <- fences_of(train_plain$SOC)
  expected_trim <- train_plain$sample_id[train_plain$SOC < fences[1] | train_plain$SOC > fences[2]]

  out <- utils::capture.output(
    ev <- suppressWarnings(evaluate(v, prune = FALSE, verbose = TRUE, seed = 307L))
  )
  trim <- ev$evaluation$response_trim

  it("rests on a draw with extreme labels on both sides of the split", {

    expect_true(any(EXTREME_IDS %in% test_plain$sample_id))
    expect_true(any(EXTREME_IDS %in% train_plain$sample_id))

  })

  it("keeps every extreme row that lands in the test set in the test set", {

    test_ids <- rsample::testing(ev$evaluation$split)$sample_id

    expect_identical(test_ids, test_plain$sample_id)
    expect_true(all(intersect(EXTREME_IDS, test_plain$sample_id) %in% test_ids))
    expect_equal(ev$evaluation$n_test, nrow(test_plain))

  })

  it("trims the training rows outside fences from the training labels alone", {

    expect_gt(length(expected_trim), 0)
    expect_setequal(trim$trimmed_ids, expected_trim)
    expect_equal(c(trim$lower, trim$upper), fences)

    train_ids <- rsample::training(ev$evaluation$split)$sample_id

    expect_identical(train_ids, setdiff(train_plain$sample_id, expected_trim))
    expect_equal(ev$evaluation$n_train, length(train_ids))

    ## Fences over the whole table are others, so this test can tell the two
    ## rules apart
    expect_false(isTRUE(all.equal(fences_of(v$data$analysis$SOC), fences)))

  })

  it("records what it did, and says so in the tree", {

    expect_identical(trim$outcome, "SOC")
    expect_identical(trim$method, "iqr")
    expect_identical(trim$threshold, 1.5)
    expect_identical(trim$fences_from, "training")
    expect_identical(trim$n_training, nrow(train_plain))
    expect_true(is.na(trim$skipped))

    expect_true(any(grepl(paste0("Response outliers: ", length(expected_trim), " of ",
                                 nrow(train_plain), " training rows trimmed"),
                          out, fixed = TRUE)))
    expect_true(any(grepl("from the training partition; test rows untouched", out, fixed = TRUE)))

  })

  it("draws the CV folds from the trimmed training rows", {

    folds <- NULL

    testthat::with_mocked_bindings(
      evaluate_single_config = function(...) {
        folds <<- list(...)$cv_folds
        tibble::tibble(config_id = list(...)$config_row$config_id,
                       status = "failed", error_message = "mocked")
      },
      tryCatch(suppressWarnings(evaluate(v, verbose = FALSE, seed = 307L)),
               error = function(e) NULL),
      .package = "horizons"
    )

    fold_ids <- unique(unlist(lapply(folds$splits, function(s) {
      c(rsample::analysis(s)$sample_id, rsample::assessment(s)$sample_id)
    })))

    expect_setequal(fold_ids, setdiff(train_plain$sample_id, expected_trim))
    expect_false(any(expected_trim %in% fold_ids))

  })

  it("trims the same training rows whatever the test rows' labels are", {

    ## The partition is held fixed: the draw is stratified on every label by
    ## design, as it always was, so what must not read a test label is the
    ## trim. Every test row's outcome is moved a long way inside the drawn
    ## split, and the trim must come out the same.
    drawn   <- suppressWarnings(draw_eval_split(v$data$analysis, "SOC", 307L))
    is_test <- !seq_len(nrow(drawn$split$data)) %in% drawn$split$in_id

    relabelled <- drawn$split
    relabelled$data$SOC[is_test] <- relabelled$data$SOC[is_test] * 50 + 400

    ## Whole-table fences move under the relabelling, so a whole-table rule
    ## would trim otherwise
    expect_false(isTRUE(all.equal(fences_of(drawn$split$data$SOC),
                                  fences_of(relabelled$data$SOC))))

    a <- trim_training_responses(drawn$split, "SOC", request, "sample_id")
    b <- trim_training_responses(relabelled, "SOC", request, "sample_id")

    expect_gt(length(a$record$trimmed_ids), 0)
    expect_identical(b$record, a$record)
    expect_identical(rsample::training(b$split), rsample::training(a$split))

    ## and the test part is the relabelled one, untrimmed
    expect_identical(rsample::testing(b$split)$sample_id,
                     rsample::testing(drawn$split)$sample_id)
    expect_identical(rsample::testing(b$split)$SOC, relabelled$data$SOC[is_test])

  })

  it("fingerprints the trim: an untrimmed run's checkpoints are refused, naming it", {

    tmpdir <- withr::local_tempdir()

    untrimmed <- suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                                           verbose = FALSE, seed = 307L))

    expect_true(is.na(untrimmed$evaluation$results$settings[[1]]$response_threshold))
    expect_identical(ev$evaluation$results$settings[[1]]$response_threshold, 1.5)

    err <- expect_error(
      suppressWarnings(evaluate(v, output_dir = tmpdir, prune = FALSE,
                                verbose = FALSE, seed = 307L)),
      class = "horizons_input_error"
    )

    msg <- gsub("\\s+", " ", conditionMessage(err))   # undo cli line wrapping
    expect_match(msg, "different set of training samples", fixed = TRUE)
    expect_match(msg, "response_threshold = NA (this run: 1.5)", fixed = TRUE)

  })

  it("round-trips the trimmed split through the parallel transport", {

    split    <- ev$evaluation$split
    cv_folds <- rsample::vfold_cv(rsample::training(split), v = 3)
    rebuilt  <- rebuild_resamples(split$data, resample_indices(split, cv_folds))

    expect_identical(rsample::testing(rebuilt$split)$sample_id,
                     rsample::testing(split)$sample_id)
    expect_identical(rsample::training(rebuilt$split)$sample_id,
                     rsample::training(split)$sample_id)

  })

  it("refuses a request recorded for another outcome", {

    other <- v
    other$validation$outliers$response_trim$outcome <- "pH"

    err <- expect_error(evaluate(other, verbose = FALSE),
                        class = "horizons_input_error")

    expect_match(conditionMessage(err), "another outcome", fixed = TRUE)

  })

  it("warns and trims nothing when the training partition has no fences", {

    flat <- plain
    flat$data$SOC[flat$in_id] <- 2

    caught <- collect_warnings(trim_training_responses(flat, "SOC", request, "sample_id"))

    expect_true(has_warning_class(caught$warnings, "horizons_response_trim_warning"))
    expect_identical(caught$value$record$skipped, "zero_iqr")
    expect_length(caught$value$record$trimmed_ids, 0)
    expect_identical(caught$value$split, flat)

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
    expect_true("│  Tuning: 3-fold CV (unstratified), grid = 2, bayesian = 0" %in% out)
    expect_false(any(grepl("(stratified)", out, fixed = TRUE)))
    expect_false(any(grepl(", stratified)", out, fixed = TRUE)))

  })

  it("says stratified when the strata held", {

    out <- evaluate_header(make_eval_object(n = 60, n_configs = 1))

    expect_true("│  Split: 48 train / 12 test (80/20, stratified)" %in% out)
    expect_true("│  Tuning: 3-fold CV (stratified), grid = 2, bayesian = 0" %in% out)

  })

  it("says the folds are unstratified under a stratified split", {

    ## 45 rows bin into two strata; the 35 training rows the folds are drawn
    ## on are too few
    out <- evaluate_header(make_eval_object(n = 45, n_configs = 1))

    expect_true(any(grepl("^│  Split: 35 train / 10 test \\(80/20, stratified\\)$", out)))
    expect_true("│  Tuning: 3-fold CV (unstratified), grid = 2, bayesian = 0" %in% out)

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
    expect_identical(out[cv_line], "│  Tuning: 3-fold CV (unstratified), grid = 2, bayesian = 0")
    expect_identical(split_note, split_line + 1L)
    expect_identical(cv_note, cv_line + 1L)
    expect_true(all(c(split_note, cv_note) > header))

  })

  it("draws the split and the folds it drew before the check, on an outcome whose strata draw random numbers", {

    ## A pooled outcome (5 is under 10 % of the rows): make_strata() samples a
    ## stratum for each pooled row, in the split and again in the folds, which
    ## are drawn from the stream the split leaves, without a reseed. The
    ## check runs make_strata() before each draw, so without its RNG restore
    ## the folds would move.
    obj <- make_eval_object(n = 96, n_configs = 1)
    set.seed(3)
    obj$data$analysis$SOC <- sample(c(rep(1:4, each = 23), rep(5, 4)))

    captured <- NULL

    testthat::with_mocked_bindings(
      tryCatch(
        suppressWarnings(evaluate(obj, prune = FALSE, verbose = FALSE, seed = 42L)),
        header_rendered = function(e) NULL
      ),
      evaluate_single_config = function(config_row, split, cv_folds, ...) {
        captured <<- list(split = split, cv_folds = cv_folds)
        rlang::abort("stop", class = "header_rendered")
      },
      .package = "horizons"
    )

    ## The inline draws, as evaluate() made them before #91
    set.seed(42L)
    split <- rsample::initial_split(obj$data$analysis, prop = SPLIT_PROP,
                                    strata = dplyr::all_of("SOC"))
    folds <- rsample::vfold_cv(rsample::training(split), v = 3L,
                               strata = dplyr::all_of("SOC"))

    expect_identical(captured$split$in_id, split$in_id)
    expect_identical(lapply(captured$cv_folds$splits, `[[`, "in_id"),
                     lapply(folds$splits, `[[`, "in_id"))

  })

})

## =========================================================================
## Checkpointing
## =========================================================================

describe("evaluate() - checkpointing", {

  it("writes one checkpoint file per config, and no other store", {

    ck     <- ck_b()
    obj    <- ck$value$obj
    tmpdir <- ck$dir

    checkpoint_dir <- file.path(tmpdir, "checkpoints")

    ## The per-config files are the only store (#42); the manifest is for
    ## monitor_evaluate()
    expect_setequal(list.files(checkpoint_dir),
                    paste0(obj$config$configs$config_id, ".rds"))
    expect_setequal(list.files(tmpdir), c("checkpoints", "eval_manifest.rds"))

  })

  ## A config whose CV panel could not be recovered succeeds with no value to
  ## rank on. rank_configs_by_cv() refused such rows unclassed and without
  ## the results, and re-running evaluate() resumes them rather than
  ## re-running.
  it("signals horizons_all_configs_failed when no success has a cv_<metric>, naming the checkpoints", {

    ck     <- ck_b()
    obj    <- ck$value$obj
    tmpdir <- ck$dir

    cv_cols <- paste0("cv_", c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae"))

    for (f in list.files(file.path(tmpdir, "checkpoints"), full.names = TRUE)) {
      row <- readRDS(f)
      row[cv_cols] <- NA_real_
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

    ## One store (#42): the per-config files
    expect_match(msg, "delete their files, checkpoints/<config_id>.rds.", fixed = TRUE)

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

  it("drops rows of another schema, or recording less than the run, and keeps the rest", {

    run_fp   <- list(data_hash = "h", data_n_rows = 10L,
                     data_fields = list(ids = "i", outcome = "SOC"))
    settings <- eval_settings(cv_folds = 3L, seed = 42L)

    ## A row as this version writes it, then variants of it
    current <- tibble::tibble(config_id = "keep", scoring_schema = SCORING_SCHEMA)
    current <- stamp_eval_settings(stamp_data_fingerprint(current, run_fp), settings)

    variant <- function(id, edit) {
      row <- edit(current)
      row$config_id <- id
      list(row = row, source = paste0(id, ".rds"), path = paste0(id, ".rds"))
    }

    candidates <- list(
      variant("keep",        identity),
      variant("schema_1",    function(r) { r$scoring_schema <- 1L; r }),
      variant("schema_na",   function(r) { r$scoring_schema <- NA_integer_; r }),
      variant("schema_none", function(r) { r$scoring_schema <- NULL; r }),
      variant("no_hash",     function(r) { r$data_hash <- NULL; r }),
      variant("no_fields",   function(r) { r$data_fields <- NULL; r }),
      variant("one_field",   function(r) { r$data_fields <- list(run_fp$data_fields["ids"]); r }),
      variant("no_settings", function(r) { r$settings <- NULL; r }),
      variant("one_setting", function(r) { r$settings <- list(settings["seed"]); r })
    )

    gated <- gate_checkpoint_rows(candidates, run_fp, settings)

    expect_named(gated$kept, "keep")
    expect_equal(gated$n_foreign, 8L)
    expect_length(gated$refused, 0)

    ## A run with no fingerprint of its own resumes nothing
    unverifiable <- gate_checkpoint_rows(candidates[1], list(data_hash = NA_character_),
                                         settings)

    expect_length(unverifiable$kept, 0)
    expect_equal(unverifiable$n_foreign, 1L)

  })

  it("checks the fingerprint before the schema, field by field", {

    run_fp <- list(data_hash = "this", data_n_rows = 10L,
                   data_fields = list(ids = "i", outcome = "SOC"))

    ## Written on other rows AND under an earlier schema: refused, not
    ## quietly dropped.
    other <- tibble::tibble(config_id = "a", scoring_schema = 1L,
                            data_hash = "other", data_n_rows = 10L,
                            data_fields = list(list(ids = "j", outcome = "SOC")))

    v <- checkpoint_row_verdict(other, run_fp, settings = NULL)

    expect_identical(v$verdict, "data_mismatch")
    expect_identical(v$data_differ, "ids")

    ## The same fields under another hash, as when the ids were sorted in
    ## another locale's collation: resumed, not refused.
    same <- other
    same$scoring_schema <- SCORING_SCHEMA
    same$data_fields    <- list(run_fp$data_fields)

    expect_identical(checkpoint_row_verdict(same, run_fp, settings = NULL)$verdict,
                     "keep")

  })

  it("stamps every result row with the current schema", {

    res <- ev60()

    expect_true(all(res$evaluation$results$scoring_schema == SCORING_SCHEMA))

  })

  it("on resume, re-evaluates configs whose checkpoint was scored under an earlier schema", {

    ck     <- ck_b()
    obj    <- ck$value$obj
    tmpdir <- ck$dir

    first <- ck$value$first

    ## Rewrite one per-config checkpoint as a row of an earlier schema
    f   <- file.path(tmpdir, "checkpoints", "cfg_001.rds")
    row <- readRDS(f)
    row$scoring_schema <- SCORING_SCHEMA - 1L
    row$cv_rpd <- 999          # a value that would win if it were trusted
    saveRDS(row, f)

    out <- capture.output(
      second <- suppressWarnings(evaluate(obj, output_dir = tmpdir, verbose = TRUE, seed = 42L))
    )

    expect_true(any(grepl("written under an earlier schema", out)))
    expect_false(any(second$evaluation$results$cv_rpd == 999))
    expect_equal(second$evaluation$best_config, first$evaluation$best_config)

  })

  it("prints the checkpoint notes inside the tree, under the Configs line (#91)", {

    ck     <- ck_b()
    obj    <- ck$value$obj
    tmpdir <- ck$dir
    ckpt   <- file.path(tmpdir, "checkpoints")

    ## cfg_002 rewritten as a row of an earlier schema, and a copy of cfg_001
    ## filed under a config the grid does not have
    row <- readRDS(file.path(ckpt, "cfg_002.rds"))
    row$scoring_schema <- SCORING_SCHEMA - 1L
    saveRDS(row, file.path(ckpt, "cfg_002.rds"))

    stale <- readRDS(file.path(ckpt, "cfg_001.rds"))
    stale$config_id <- "cfg_009"
    saveRDS(stale, file.path(ckpt, "cfg_009.rds"))

    ## Resume, stopping at the first config the run has to evaluate
    out <- utils::capture.output(
      testthat::with_mocked_bindings(
        tryCatch(
          suppressWarnings(evaluate(obj, output_dir = tmpdir, seed = 42L)),
          header_rendered = function(e) NULL
        ),
        evaluate_single_config = function(...) rlang::abort("stop", class = "header_rendered"),
        .package = "horizons"
      )
    )

    header  <- grep("┌ Evaluation", out, fixed = TRUE)
    configs <- grep("│  Configs: ", out, fixed = TRUE)

    expect_length(header, 1L)
    expect_true(configs > header)
    expect_identical(out[configs + 0:2], c(
      "│  Configs: 2 total (1 from checkpoint)",
      "│  Dropped 1 stale checkpoint entries",
      "│  Dropped 1 checkpoint row written under an earlier schema (will be re-evaluated)"
    ))

    ## The loaded count is the Configs line's; its own line printed above
    ## the header
    expect_false(any(grepl("checkpointed results", out, fixed = TRUE)))

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
                                    "predictors"))
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

    ck     <- ck_a()
    tmpdir <- ck$dir

    res <- ck$value$first

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
                                   sg_window = 9L, pca_threshold = 0.995,
                                   ## and its outcome range (#76), likewise
                                   outcome_range = c(0, Inf),
                                   ## no response trim requested (#77)
                                   response_threshold = NA_real_))

    row <- readRDS(file.path(tmpdir, "checkpoints", "cfg_001.rds"))
    expect_identical(row$data_hash, manifest$data_hash)
    expect_identical(row$data_fields[[1]], manifest$data_fields)
    expect_identical(manifest$data_fields$outcome, "SOC")
    expect_identical(row$settings[[1]], manifest$settings)
    expect_identical(res$evaluation$results$settings[[1]], manifest$settings)

  })

  it("aborts when the same output_dir is resumed on a different row set", {

    ## CK-A's object has 40 rows
    tmpdir <- ck_a()$dir

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

    ck     <- ck_a()
    tmpdir <- ck$dir

    ## Same rows, same config ids, different response. Only the outcome name
    ## differs, which is the collision generate_config_id() cannot see.
    obj_soc <- ck$value$obj

    obj_clay <- obj_soc
    names(obj_clay$data$analysis)[names(obj_clay$data$analysis) == "SOC"] <- "clay"
    obj_clay$data$role_map$variable[obj_clay$data$role_map$variable == "SOC"] <- "clay"

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

    ## Twelve predictors (CK-C), so demoting one still leaves the spectrum
    ## wider than the default window of 9 and evaluate()'s window check (#62)
    ## does not refuse ahead of the checkpoint gate under test.
    ck     <- ck_c(.env = parent.frame())
    obj    <- ck$value$obj
    tmpdir <- ck$dir

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
    ck     <- ck_a()
    obj    <- ck$value$obj
    tmpdir <- ck$dir

    first <- ck$value$first

    lab <- tibble::tibble(sample_id = obj$data$analysis$sample_id,
                          clay      = seq_len(nrow(obj$data$analysis)))
    invisible(capture.output(wider <- add_response(obj, lab, variable = "clay")))

    caught <- collect_warnings(
      evaluate(wider, output_dir = tmpdir, prune = FALSE, verbose = FALSE,
               seed = 42L)
    )

    expect_false(has_warning_class(caught$warnings, "horizons_checkpoint_warning"))
    expect_equal(caught$value$evaluation$results$runtime_secs,
                 first$evaluation$results$runtime_secs)

  })

})

## =========================================================================
## One checkpoint store (#42)
## =========================================================================
## One file per config under checkpoints/, named by its config id, is the
## store; a file counts only for the config its name says.

describe("evaluate() - one checkpoint store (#42)", {

  it("warns naming a per-config file it cannot read, and re-evaluates that config", {

    ck     <- ck_a()
    obj    <- ck$value$obj
    tmpdir <- ck$dir

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

  it("never lets a stray file stand in for a config's own file", {

    skip_unless_slow_tier()

    ## A stray row file named like a temp file sorts ahead of most model
    ## prefixes, so a reader taking the first row per config would let it
    ## shadow the real file (review finding, #42). Config ids with a model
    ## prefix reproduce the order.
    obj <- make_eval_object(n_configs = 2)
    obj$config$configs$config_id <- c("rf_001", "rf_002")
    tmpdir <- withr::local_tempdir()

    suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                              verbose = FALSE, seed = 42L))

    real     <- readRDS(file.path(tmpdir, "checkpoints", "rf_001.rds"))
    leftover <- real
    leftover$cv_rpd <- 999      # a valid row, made to win
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

  it("refuses a foreign-schema row written on other data", {

    ck     <- ck_a()
    obj    <- ck$value$obj
    tmpdir <- ck$dir

    f       <- file.path(tmpdir, "checkpoints", "cfg_001.rds")
    foreign <- readRDS(f)
    ## Scored on other samples: another hash and another ids field
    foreign$scoring_schema <- 1L
    foreign$data_hash      <- "0000deadbeef"
    foreign$data_fields[[1]]$ids <- "0000deadbeef"
    saveRDS(foreign, f)

    ## The fingerprint is checked before the schema, so the row aborts rather
    ## than being dropped silently.
    err <- tryCatch(
      suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = FALSE,
                                verbose = FALSE, seed = 42L)),
      horizons_input_error = function(e) e
    )

    expect_s3_class(err, "horizons_input_error")
    expect_match(flat_message(err), "cfg_001.rds", fixed = TRUE)

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

    ck     <- ck_a()
    obj    <- ck$value$obj
    tmpdir <- ck$dir

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

    ## CK-B was run with prune = TRUE, prune_threshold = 1
    ck     <- ck_b()
    obj    <- ck$value$obj
    tmpdir <- ck$dir

    expect_error(
      suppressWarnings(evaluate(obj, output_dir = tmpdir, prune = TRUE,
                                prune_threshold = 2, verbose = FALSE, seed = 42L)),
      "prune_threshold",
      class = "horizons_input_error"
    )

    ## With pruning off the threshold is never read, so it cannot have
    ## changed what a row holds. CK-A was run with prune = FALSE,
    ## prune_threshold = 1.
    ck_off <- ck_a()
    off    <- ck_off$dir

    first  <- ck_off$value$first
    second <- suppressWarnings(evaluate(obj, output_dir = off, prune = FALSE,
                                        prune_threshold = 2, verbose = FALSE, seed = 42L))

    expect_equal(second$evaluation$results$runtime_secs,
                 first$evaluation$results$runtime_secs)

  })

  it("resumes when only the ranking metric changes, reporting nothing unreadable", {

    ## CK-A was ranked on the default metric, rpd
    ck     <- ck_a()
    obj    <- ck$value$obj
    tmpdir <- ck$dir

    first  <- ck$value$first
    caught <- collect_warnings(
      evaluate(obj, output_dir = tmpdir, prune = FALSE, metric = "rmse",
               verbose = FALSE, seed = 42L)
    )
    second <- caught$value

    ## Every row carries all six cv_ columns and the ranking is recomputed on
    ## each run, so the metric is not part of what a row holds.
    expect_equal(second$evaluation$results$runtime_secs,
                 first$evaluation$results$runtime_secs)
    expect_equal(second$evaluation$rank_metric, "rmse")

    ## A clean store reports nothing unreadable: these per-config files, so
    ## the resume raises no checkpoint warning, and a directory with no
    ## checkpoints/.
    expect_false(has_warning_class(caught$warnings, "horizons_checkpoint_warning"))

    store <- read_checkpoint_store(tmpdir)
    expect_length(store$rows, 2L)
    expect_length(store$unreadable, 0L)

    empty <- read_checkpoint_store(withr::local_tempdir())
    expect_length(empty$rows, 0L)
    expect_length(empty$unreadable, 0L)

  })

  ## configure()'s recipe settings (#62) are object-level, so the config id
  ## does not carry them; the settings record is what keeps a run with another
  ## window from resuming rows built with the old one.

  it("refuses to resume checkpoints built with a different sg_window or pca_threshold, naming it", {

    ## CK-D was run with no recipe record, which stamps the same settings as
    ## this one (checked when CK-D is built)
    ck     <- ck_d()
    obj    <- ck$value$obj
    obj$config$recipe <- list(sg_window = 9L, pca_threshold = 0.995)
    tmpdir <- ck$dir

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

})
