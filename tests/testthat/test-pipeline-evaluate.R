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
  suppressWarnings(evaluate(obj, seed = 42L))

}

ev60 <- function() memo_fixture("ev60", build_ev60)

## A result row for a mocked evaluate_single_config(): a real row with its
## status, metrics, error, warnings and runtime replaced, so the tree is
## rendered from the shape the runner returns.
mock_eval_row <- function(template, config_id, status, rpd = 1.5,
                          warnings = NULL, error = NA_character_,
                          runtime = 1.25) {

  row <- template
  row$config_id <- config_id
  row$status    <- status

  metrics <- c(rmse = 0.5, rrmse = 20, rsq = 0.6, ccc = 0.7, rpd = rpd, mae = 0.4)

  for (m in names(metrics)) {
    row[[m]]                  <- if (status == "failed") NA_real_ else unname(metrics[m])
    row[[paste0("cv_", m)]]   <- if (status == "failed") NA_real_ else unname(metrics[m]) + 0.25
  }

  row$error_message <- error
  row$warnings      <- list(warnings)
  row$runtime_secs  <- runtime
  row

}

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
  first <- suppressWarnings(evaluate(obj, prune = prune,
                                     prune_threshold = 1,
                                     seed = 42L, control = control_horizons(output_dir = dir)))

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

    expect_error(evaluate(obj), "No configurations found", class = "horizons_error_input")

  })

  it("aborts when config$configs has zero rows", {

    obj <- make_eval_object()
    obj$config$configs <- obj$config$configs[0, ]

    expect_error(evaluate(obj), "No configurations found", class = "horizons_error_input")

  })

  it("aborts when outcome column has no non-NA values, naming the remedy", {

    obj <- make_eval_object()
    outcome <- obj$data$role_map$variable[obj$data$role_map$role == "outcome"]
    obj$data$analysis[[outcome]] <- NA_real_

    err <- expect_error(evaluate(obj), "outcome.*NA", class = "horizons_error_input")
    expect_match(conditionMessage(err), "another outcome", fixed = TRUE)
    expect_identical(rlang::call_name(err$call), "evaluate")

  })

  it("refuses a training part without more rows than folds, counting only rows with an observed outcome", {

    local_mocked_bindings(evaluate_single_config = function(...) stop("a configuration ran"))

    ## Three rows: the split leaves two to train on, under the three folds
    err <- expect_error(evaluate(make_eval_object(n = 3)), class = "horizons_error_input")
    msg <- gsub("\\s+", " ", conditionMessage(err))   # undo cli line wrapping

    expect_match(msg, "The training part has 2 rows, but 3-fold cross-validation needs more training rows than folds.",
                 fixed = TRUE)
    expect_match(msg, "3 rows have an observed outcome; the test set holds 1.", fixed = TRUE)
    expect_match(msg, "fewer `cv_folds`", fixed = TRUE)

    ## Eight rows, five without an outcome: three can be modelled
    obj <- make_eval_object(n = 8)
    obj$data$analysis$SOC[1:5] <- NA_real_

    expect_error(evaluate(obj), "The training part has 2 rows", fixed = TRUE,
                 class = "horizons_error_input")

  })

  it("runs a small sample, warning once that its folds are small", {

    ## Read the fixture before the runner is mocked (helper-memo.R)
    template <- ev60()$evaluation$results[1, ]

    local_mocked_bindings(
      evaluate_single_config = function(config_row, ...) {
        mock_eval_row(template, config_row$config_id, "success")
      }
    )

    ## Eight rows, three without an outcome: five modelled, four to train on
    ## in three folds of one or two. Refused under 2 * cv_folds until 0.12.0.
    obj <- make_eval_object(n = 8)
    obj$data$analysis$SOC[1:3] <- NA_real_

    ## (helper-parallel.R's collect_warnings(): this file's own is defined below)
    got <- collect_warnings(evaluate(obj, seed = 42L), "")

    quality <- Filter(function(w) inherits(w, "horizons_warning_quality"), got$warnings)
    expect_length(quality, 1L)
    expect_match(conditionMessage(quality[[1]]), "The smallest cross-validation fold has 1 row to assess",
                 fixed = TRUE)
    expect_identical(sum(got$value$log$topic == "quality"), 1L)
    expect_identical(got$value$evaluation$n_train, 4L)

    ## Folds of ten or more rows are not small
    quiet <- collect_warnings(evaluate(make_eval_object(n = 40), seed = 42L), "")
    expect_false(any(vapply(quiet$warnings, inherits, logical(1), "horizons_warning_quality")))

  })

  it("aborts with invalid metric name", {

    obj <- make_eval_object()

    err <- expect_error(evaluate(obj, metric = "accuracy"), "`metric` must be one of",
                        class = "horizons_error_input")
    expect_match(conditionMessage(err), "\"accuracy\"", fixed = TRUE)

  })

  it("refuses an axis whose bands are out of order before anything is drawn (#121)", {

    ## Names the door validator does not parse as wavenumbers, so only the
    ## band-order check sees them
    local_mocked_bindings(
      draw_eval_split = function(...) stop("a split was drawn"),
      evaluate_single_config = function(...) stop("a configuration ran")
    )

    obj  <- make_eval_object()
    pred <- obj$data$role_map$role == "predictor"
    olds <- obj$data$role_map$variable[pred]
    news <- sub("^wn_", "wn_X", olds)
    names(obj$data$analysis)[match(olds, names(obj$data$analysis))] <- news

    ## The first two bands swapped
    obj$data$role_map$variable[pred] <- news[c(2, 1, 3:length(news))]

    expect_error(evaluate(obj), "not monotonically ordered", class = "horizons_error_input")

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
      evaluate(obj),
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

  it("refuses to return results that break the results contract", {

    ## Read the fixture before the runner is mocked (helper-memo.R). Nothing
    ## reads rmse before the validator does.
    template <- ev60()$evaluation$results[1, ]

    local_mocked_bindings(
      evaluate_single_config = function(config_row, ...) {
        row <- mock_eval_row(template, config_row$config_id, "success")
        row$rmse <- NULL
        row
      }
    )

    expect_error(
      suppressWarnings(evaluate(make_eval_object(n = 60, n_configs = 2), seed = 42L)),
      regexp = "the six metric columns", fixed = TRUE, class = "horizons_error_validation"
    )

  })

})

## =========================================================================
## Metric ranking
## =========================================================================

describe("evaluate() - its control", {

  it("records the control it ran with, defaults filled in", {

    ctrl <- ev60()$evaluation$control

    expect_s3_class(ctrl, "horizons_control")
    expect_identical(names(ctrl), names(resolve_control(NULL, "evaluate")))
    expect_false(ctrl$allow_par)
    expect_identical(ctrl$parallelize_over, "auto")
    expect_identical(ctrl$breaker, BREAKER_FAILURES)

  })

  it("refuses a control field it does not use, before anything runs", {

    local_mocked_bindings(evaluate_single_config = function(...) stop("a configuration ran"))

    expect_error(evaluate(make_eval_object(n = 60, n_configs = 1),
                          control = control_horizons(min_calib_rows = 10L)),
                 "min_calib_rows: fit(), ensemble()", fixed = TRUE, class = "horizons_error_input")

  })

  it("runs sequentially when allow_par = TRUE has no backend, and says so once, on the log too", {

    local_plan(future::sequential)

    got <- collect_warnings(
      evaluate(make_eval_object(n = 60, n_configs = 1), prune = FALSE, seed = 42L,
               control = control_horizons(allow_par = TRUE)),
      "offers 1 worker"
    )
    ev <- got$value
    w  <- got$warnings[[1]]

    expect_length(got$warnings, 1L)
    expect_s3_class(w, "horizons_warning_substitution")
    expect_match(conditionMessage(w), "evaluate\\(\\).*offers 1 worker")
    expect_identical(ev$log$verb, "evaluate")
    expect_identical(ev$log$topic, "substitution")
    expect_identical(w$log, ev$log)
    expect_identical(ev$evaluation$parallelize_over, "sequential")

  })

})


describe("evaluate() - metric ranking", {

  it("selects best config by specified metric", {

    skip_unless_slow_tier()

    obj <- make_eval_object(n_configs = 2)
    result <- suppressWarnings(evaluate(obj, metric = "rsq", seed = 42L))

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

  it("leaves out rows with NA cv_<metric>, silently: the verbs that rank say so", {

    unranked <- rows
    unranked$cv_rpd[1] <- NA_real_

    expect_no_warning(ranked <- rank_configs_by_cv(unranked, "rpd"))
    expect_equal(ranked$config_id, c("b", "c"))

    ## Nothing to rank: no rows, and the caller decides what that means
    all_na <- rows
    all_na$cv_rpd <- NA_real_

    expect_no_warning(none <- rank_configs_by_cv(all_na, "rpd"))
    expect_identical(nrow(none), 0L)
    expect_named(none, names(rows))

    ## Results with no cv_ column are refused by the eval validator at fit()'s
    ## door (test-class-core.R), before anything is ranked.

  })

})

## =========================================================================
## All configs fail
## =========================================================================

describe("evaluate() - all configs fail", {

  ## evaluate() aborts before it assigns x$evaluation, so the message's old
  ## hint ("Check evaluation$results") could not be followed. The results now
  ## ride on the condition, so a loop over subsets can recover them (#41).
  it("signals horizons_error_all_configs_failed, carrying the results and naming the errors", {

    obj <- make_eval_object(n_configs = 5)

    ## Every configuration fails as it runs, with four distinct messages; one
    ## carries braces, which must reach the message as text rather than be
    ## read as a cli template. (An unknown model would now be refused at the
    ## door, before any configuration runs.)
    tags <- stats::setNames(c("{wn_600}", "nope_2", "nope_2", "nope_4", "nope_5"),
                            obj$config$configs$config_id)
    local_mocked_bindings(build_recipe = function(config, ...) {
      stop("recipe broke for '", tags[[config$config_id]], "'", call. = FALSE)
    })

    err <- tryCatch(
      suppressWarnings(evaluate(obj)),
      horizons_error_all_configs_failed = function(e) e
    )

    expect_s3_class(err, "horizons_error_all_configs_failed")
    expect_s3_class(err, "horizons_error_run")

    ## The per-config results, error messages included
    expect_s3_class(err$results, "tbl_df")
    expect_identical(err$results$config_id, obj$config$configs$config_id)
    expect_true(all(err$results$status == "failed"))
    expect_true(all(grepl("recipe broke for", err$results$error_message, fixed = TRUE)))

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

  it("names three of the configurations loaded from checkpoints and counts the rest", {

    results <- tibble::tibble(config_id = sprintf("cfg_%03d", 1:5), status = "failed",
                              error_message = "mocked")

    err <- expect_error(
      abort_all_configs_failed(results, checkpoint_ids = results$config_id,
                               output_dir = "runs/soc"),
      class = "horizons_error_all_configs_failed"
    )

    msg <- gsub("\\s+", " ", conditionMessage(err))   # undo cli line wrapping
    expect_match(msg, "5 configurations were loaded from checkpoints in runs/soc rather than run (cfg_001, cfg_002, cfg_003 and 2 more).",
                 fixed = TRUE)

  })

})

## =========================================================================
## NA outcome handling
## =========================================================================

describe("evaluate() - NA outcome rows", {

  it("modelling_rows(), the rule fit() shares, drops and counts NA outcomes (#67)", {

    df <- tibble::tibble(sample_id = c("A", "B", "C", "D"),
                         y         = c(1, NA, 3, NA))

    kept <- modelling_rows(df, "y")

    expect_identical(kept$data$sample_id, c("A", "C"))
    expect_identical(kept$n_dropped, 2L)

    ## Nothing to drop: the table comes back untouched
    complete <- df[c(1, 3), ]
    expect_identical(modelling_rows(complete, "y")$data, complete)
    expect_identical(modelling_rows(complete, "y")$n_dropped, 0L)

    expect_error(modelling_rows(df[c(2, 4), ], "y"),
                 class = "horizons_error_input")

  })

  it("modelling_rows() leaves out the rows a step flagged, counted apart from the NA outcomes", {

    df <- tibble::tibble(sample_id = c("A", "B", "C", "D"),
                         y         = c(1, NA, 3, 4),
                         .flag     = c(NA, NA, "standardize", NA))

    kept <- modelling_rows(df, "y")

    expect_identical(kept$data$sample_id, c("A", "D"))
    expect_identical(kept$n_dropped, 1L)
    expect_identical(kept$n_flagged, 1L)

    ## No row left once the flagged and the unobserved are out
    expect_error(modelling_rows(df[2:3, ], "y"), "Every row with an outcome is flagged",
                 class = "horizons_error_input")

  })

  it("names an outcome column the analysis table lacks, rather than calling it all NA", {

    ## evaluate()'s own modelling_rows() check has a dedicated "no such
    ## column" message for exactly this case, distinct from "all NA" — but
    ## the entry-stage validate_horizons_data() call (#24) now certifies the
    ## base data contract first, and a role_map row with no matching analysis
    ## column is a structural defect that check catches before
    ## modelling_rows() ever runs. Either way the message names SOC
    ## and never claims the values are all NA.

    obj <- make_eval_object()
    obj$data$analysis$SOC <- NULL

    err <- expect_error(evaluate(obj),
                        class = "horizons_error_validation")

    expect_match(conditionMessage(err), "SOC", fixed = TRUE)
    expect_match(conditionMessage(err), "missing from", fixed = TRUE)
    expect_no_match(conditionMessage(err), "All outcome values are NA", fixed = TRUE)

  })

  ## A configured object with no outcome role is refused at the door
  ## (config_findings(), test-class-core.R), so modelling_rows() is
  ## only reached with an outcome column the table holds.

})

## =========================================================================
## draw_eval_split(): the one split draw evaluate() and fit() share (#45)
## =========================================================================
## fit() cold-starts a configured object with one configuration by drawing
## the split itself, so the draw moved out of evaluate() into a helper both
## verbs call. It has to give the split evaluate() drew before the move.

describe("check_fold_rows() and log_small_folds()", {

  it("refuses as many training rows as folds or fewer, and no more", {

    ## rsample refuses as many folds as rows
    expect_error(rsample::vfold_cv(data.frame(x = 1:3), v = 3))

    expect_no_error(check_fold_rows(4L, 3L, n_modelled = 5L, n_test = 1L))
    expect_error(check_fold_rows(3L, 3L, n_modelled = 4L, n_test = 1L),
                 "The training part has 3 rows, but 3-fold cross-validation needs more training rows than folds.",
                 fixed = TRUE, class = "horizons_error_input")

  })

  it("warns when the smallest fold has fewer than SMALL_FOLD_ROWS rows to assess, and only then", {

    folds_of <- function(n) rsample::vfold_cv(data.frame(x = seq_len(n)), v = 3)

    at <- log_open("evaluate")
    expect_identical(log_small_folds(at, folds_of(3L * SMALL_FOLD_ROWS)), SMALL_FOLD_ROWS)
    expect_identical(nrow(log_close(at)), 0L)

    under <- log_open("evaluate")
    log_small_folds(under, folds_of(3L * (SMALL_FOLD_ROWS - 1L)))
    log <- log_close(under)
    expect_identical(log$topic, "quality")
    expect_match(log$message, paste0("The smallest cross-validation fold has ", SMALL_FOLD_ROWS - 1L, " rows"),
                 fixed = TRUE)

  })

})

describe("draw_eval_split()", {

  obj <- make_eval_object(n = 60, n_configs = 1)
  obj$data$analysis$SOC[c(3, 17, 41)] <- NA_real_

  ## evaluate()'s draw before #45, as it was written inline: the rows with an
  ## observed outcome, then the seed, then a stratified split, falling back to
  ## an unstratified one.
  draw_before <- function(analysis, seed) {

    modelled <- modelling_rows(analysis, "SOC")$data

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

    ev    <- suppressWarnings(evaluate(obj, prune = FALSE, seed = 42L))
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
  request <- response_trim_request(v)

  ## The split as drawn, before any trim
  plain       <- suppressWarnings(draw_eval_split(v$data$analysis, "SOC", 307L))$split
  train_plain <- rsample::training(plain)
  test_plain  <- rsample::testing(plain)

  fences        <- fences_of(train_plain$SOC)
  expected_trim <- train_plain$sample_id[train_plain$SOC < fences[1] | train_plain$SOC > fences[2]]

  out <- utils::capture.output(
    ev <- suppressWarnings(evaluate(v, prune = FALSE, seed = 307L, control = control_horizons(verbose = TRUE)))
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
      tryCatch(suppressWarnings(evaluate(v, seed = 307L)),
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

  it("counts the training rows left by the trim when it refuses too few rows for the folds", {

    ## As many folds as the trimmed training part has rows, and fewer than
    ## the rows as drawn: the refusal counts what the folds would be drawn on
    n_left <- nrow(train_plain) - length(expected_trim)
    short  <- v
    short$config$tuning$cv_folds <- n_left

    local_mocked_bindings(evaluate_single_config = function(...) stop("a configuration ran"))

    err <- expect_error(suppressWarnings(evaluate(short, seed = 307L)), class = "horizons_error_input")
    msg <- gsub("\\s+", " ", conditionMessage(err))

    expect_match(msg, paste0("The training part has ", n_left, " rows, but ", n_left,
                             "-fold cross-validation needs more training rows than folds."), fixed = TRUE)
    expect_match(msg, paste0("the response trim left out ", length(expected_trim)), fixed = TRUE)

  })

  it("fingerprints the trim: an untrimmed run's checkpoints are refused, naming it", {

    tmpdir <- withr::local_tempdir()

    untrimmed <- suppressWarnings(evaluate(obj, prune = FALSE, seed = 307L, control = control_horizons(output_dir = tmpdir)))

    expect_true(is.na(untrimmed$evaluation$results$settings[[1]]$response_threshold))
    expect_identical(ev$evaluation$results$settings[[1]]$response_threshold, 1.5)

    err <- expect_error(
      suppressWarnings(evaluate(v, prune = FALSE, seed = 307L, control = control_horizons(output_dir = tmpdir))),
      class = "horizons_error_input"
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

    ## Refused at the door, as an object configure() and validate() could
    ## not have written
    err <- expect_error(evaluate(other),
                        class = "horizons_error_validation")

    expect_true(any(grepl("response-trim request is for pH, but the object models SOC",
                          err$findings, fixed = TRUE)))

  })

  it("refuses a request without a single positive threshold", {

    for (bad in list(0, -1, NA_real_, "1.5", c(1.5, 3))) {

      edited <- v
      edited$validation$outliers$response_trim$threshold <- bad

      err <- expect_error(validate_horizons_data(edited), class = "horizons_error_validation")

      expect_true(any(grepl("response_trim$threshold must be a single positive number",
                            err$findings, fixed = TRUE)), label = deparse(bad))

    }

  })

  it("records a skipped trim and trims nothing when the training partition has no fences", {

    ## The trim runs inside the verb, which records it and warns once at its
    ## end; here the verb's collector stands in
    trim_with <- function(split) {
      collector <- log_open("evaluate")
      out       <- trim_training_responses(split, "SOC", request, "sample_id", collector)
      log       <- log_close(collector)
      caught    <- collect_warnings(signal_log(collector))
      list(value = out, log = log, warnings = caught$warnings)
    }

    flat <- plain
    flat$data$SOC[flat$in_id] <- 2

    caught <- trim_with(flat)

    expect_identical(caught$log$topic, "skipped")
    expect_length(caught$warnings, 1L)
    expect_s3_class(caught$warnings[[1]], "horizons_warning_skipped")
    expect_identical(caught$value$record$skipped, "zero_iqr")
    expect_length(caught$value$record$trimmed_ids, 0)
    expect_identical(caught$value$split, flat)

    ## The warning gives the reason, which differs with three outcome values
    reason <- function(caught) gsub("\\s+", " ", conditionMessage(caught$warnings[[1]]))

    expect_match(reason(caught), "be drawn on the training partition: its outcome has a zero interquartile range.",
                 fixed = TRUE)
    expect_match(caught$log$message, "No training rows were trimmed.", fixed = TRUE)

    few <- plain
    few$data$SOC[few$in_id[-(1:3)]] <- NA_real_

    caught <- trim_with(few)

    expect_identical(caught$value$record$skipped, "too_few")
    expect_match(reason(caught), "be drawn on the training partition: it has fewer than four outcome values (3).",
                 fixed = TRUE)

    ## Fences drawn: nothing recorded
    expect_identical(nrow(trim_with(plain)$log), 0L)

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
        suppressWarnings(evaluate(obj, prune = FALSE, seed = 42L, control = control_horizons(verbose = TRUE))),
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
    expect_identical(out[split_note], "│  Stratified split failed, retrying without strata")
    expect_identical(out[cv_note], "│  Stratified CV failed, retrying without strata")
    expect_true(all(c(split_note, cv_note) > header))

  })

  it("warns once, recorded first, that a failed stratified draw fell back, with why", {

    ## Read the fixture before anything is mocked (helper-memo.R)
    template <- ev60()$evaluation$results[1, ]

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
    local_mocked_bindings(
      evaluate_single_config = function(config_row, ...) {
        mock_eval_row(template, config_row$config_id, "success")
      }
    )

    got <- collect_warnings(evaluate(make_eval_object(n = 60, n_configs = 1), seed = 42L))
    sub <- Filter(function(w) inherits(w, "horizons_warning_substitution"), got$warnings)

    expect_length(sub, 1L)
    msg <- gsub("\\s+", " ", conditionMessage(sub[[1]]))
    expect_match(msg, "The stratified draw of the train/test split failed, so it was drawn without strata",
                 fixed = TRUE)
    expect_match(msg, "The stratified draw of the cross-validation folds failed", fixed = TRUE)
    expect_match(msg, "stratification refused", fixed = TRUE)

    log <- got$value$log
    expect_identical(log$topic, "substitution")
    expect_identical(log$n, 2L)
    expect_identical(sub[[1]]$log, log)

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
        suppressWarnings(evaluate(obj, prune = FALSE, seed = 42L)),
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
## The progress tree
## =========================================================================

describe("evaluate() - the progress tree", {

  it("prints each config's result, the counts and the best config, fresh and resumed, and nothing when quiet", {

    ## Read the fixture before the runner is mocked (helper-memo.R)
    template <- ev60()$evaluation$results[1, ]

    ## One config of each status, so each count differs from the others
    rows <- list(
      cfg_001 = mock_eval_row(template, "cfg_001", "success", rpd = 2.1234, runtime = 3.21),
      cfg_002 = mock_eval_row(template, "cfg_002", "pruned", rpd = 0.8765,
                              warnings = "Pruned config warned", runtime = 0.54),
      cfg_003 = mock_eval_row(template, "cfg_003", "failed",
                              warnings = "Failed config warned",
                              error = "Grid search failed: mocked", runtime = 0.07)
    )

    local_mocked_bindings(
      evaluate_single_config = function(config_row, ...) rows[[config_row$config_id]]
    )

    ## Two outcomes missing, for the dropped-rows line
    obj <- make_eval_object(n = 60, n_configs = 3)
    obj$data$analysis$SOC[c(5, 9)] <- NA_real_

    dir    <- withr::local_tempdir()
    failed <- file.path(dir, "checkpoints", "cfg_003.rds")
    run    <- function(verbose = TRUE) {
      suppressWarnings(evaluate(obj, seed = 42L, control = control_horizons(output_dir = dir, verbose = verbose)))
    }

    expect_snapshot(invisible(run()))

    ## Quiet: nothing on stdout, and nothing on stderr (no GC report either)
    unlink(failed)
    err <- utils::capture.output(type = "message",
      out <- utils::capture.output(invisible(run(verbose = FALSE)))
    )
    expect_identical(out, character())
    expect_identical(err, character())

    ## Resumed: two configs from their checkpoints, the failed one run again
    unlink(failed)
    expect_snapshot(invisible(run()))

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

  it("creates an output_dir that does not exist yet, however deep", {

    obj <- make_eval_object(n = 60, n_configs = 1)
    dir <- file.path(withr::local_tempdir(), "runs", "soc")

    testthat::with_mocked_bindings(
      tryCatch(
        suppressWarnings(evaluate(obj, seed = 42L, control = control_horizons(output_dir = dir))),
        header_rendered = function(e) NULL
      ),
      evaluate_single_config = function(...) rlang::abort("stop", class = "header_rendered"),
      .package = "horizons"
    )

    expect_true(dir.exists(file.path(dir, "checkpoints")))
    expect_true(file.exists(file.path(dir, "eval_manifest.rds")))

  })

  ## A config can succeed with no value to rank on. rank_configs_by_cv()
  ## refused such rows unclassed and without the results, then evaluate()
  ## aborted; the configurations were scored, so the evaluation is returned,
  ## without a best_config, and says so once.
  it("returns without a best_config when no success has a cv_<metric>, says so once, and fit() asks for another metric", {

    ck     <- ck_b()
    obj    <- ck$value$obj
    tmpdir <- ck$dir

    for (f in list.files(file.path(tmpdir, "checkpoints"), full.names = TRUE)) {
      row <- readRDS(f)
      row$cv_rpd <- NA_real_
      saveRDS(row, f)
    }

    out <- utils::capture.output(got <- collect_warnings(
      evaluate(obj, seed = 42L, control = control_horizons(output_dir = tmpdir, verbose = TRUE))
    ))
    ev  <- got$value

    expect_s3_class(ev, "horizons_eval")
    expect_identical(ev$evaluation$best_config, NA_character_)
    expect_true("\u2514\u2500 Best: none, since no configuration that succeeded has a CV RPD" %in% out)
    expect_setequal(ev$evaluation$results$config_id, obj$config$configs$config_id)

    skipped <- Filter(function(w) inherits(w, "horizons_warning_skipped"), got$warnings)
    expect_length(skipped, 1L)
    expect_match(conditionMessage(skipped[[1]]), "2 succeeded, but none has a cv_rpd value", fixed = TRUE)
    expect_identical(ev$log$topic[ev$log$verb == "evaluate"], "skipped")
    expect_setequal(ev$log$ids[[1]], obj$config$configs$config_id)

    ## fit() refuses it at its door, naming a metric the results hold
    err <- expect_error(fit(ev, compute_uq = FALSE, compute_ad = FALSE),
                        "2 succeeded, but none has a cv_rpd value.", fixed = TRUE,
                        class = "horizons_input_error")
    msg <- gsub("\\s+", " ", conditionMessage(err))   # undo cli line wrapping
    expect_match(msg, "fit(x, metric = )", fixed = TRUE)
    expect_match(msg, "\"rmse\"", fixed = TRUE)
    expect_no_match(msg, "\"rpd\"", fixed = TRUE)

  })

  it("leaves a configuration without the ranking metric out of the ranking, and says so once", {

    ## Read the fixture before the runner is mocked (helper-memo.R)
    template <- ev60()$evaluation$results[1, ]

    rows <- list(
      cfg_001 = mock_eval_row(template, "cfg_001", "success", rpd = 2.1),
      cfg_002 = mock_eval_row(template, "cfg_002", "success", rpd = 1.4)
    )
    rows$cfg_001$cv_rpd <- NA_real_

    local_mocked_bindings(
      evaluate_single_config = function(config_row, ...) rows[[config_row$config_id]]
    )

    got <- collect_warnings(evaluate(make_eval_object(n = 60, n_configs = 2), seed = 42L))
    ev  <- got$value

    expect_identical(ev$evaluation$best_config, "cfg_002")

    skipped <- Filter(function(w) inherits(w, "horizons_warning_skipped"), got$warnings)
    expect_length(skipped, 1L)
    expect_match(conditionMessage(skipped[[1]]), "1 configuration left out of the ranking, with no cv_rpd value.",
                 fixed = TRUE)
    expect_identical(ev$log$ids[[1]], "cfg_001")
    expect_identical(skipped[[1]]$log, ev$log)

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
    result <- suppressWarnings(evaluate(obj, prune = TRUE, prune_threshold = 9999, seed = 42L))

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
      tryCatch(suppressWarnings(evaluate(obj, seed = 42L)),
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
      tryCatch(evaluate(obj, seed = 42L),
               error = function(e) e),
      .package = "horizons"
    )

    list(error = err, ran = ran)

  }

  it("refuses a window as wide as the spectrum, naming both numbers and the width", {

    narrow <- make_eval_object(n_wn = 9, n_configs = 1)
    narrow$config$recipe <- list(sg_window = 9L, pca_threshold = 0.995)

    out <- refused_without_running(narrow)

    expect_s3_class(out$error, "horizons_error_input")
    expect_match(conditionMessage(out$error), "9 grid points (18 cm", fixed = TRUE)
    expect_match(conditionMessage(out$error), "9 spectral columns", fixed = TRUE)
    expect_identical(out$ran, 0L)

  })

  it("refuses a window wider than the spectrum the same way", {

    obj$config$recipe <- list(sg_window = 21L, pca_threshold = 0.995)

    out <- refused_without_running(obj)

    expect_s3_class(out$error, "horizons_error_input")
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

    result <- suppressWarnings(evaluate(coarse, seed = 42L))

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
      second <- suppressWarnings(evaluate(obj, seed = 42L, control = control_horizons(output_dir = tmpdir, verbose = TRUE)))
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
          suppressWarnings(evaluate(obj, seed = 42L, control = control_horizons(output_dir = tmpdir, verbose = TRUE))),
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

    ## The outcome's role-map row is one of the rows that shape a row
    expect_setequal(compare_record(clay$data_fields, oc$data_fields)$differ,
                    c("outcome", "roles", "outcome_values"))

  })

  it("returns NA when no identifier column is available", {

    fp <- eval_data_fingerprint(tibble::tibble(x = 1:3), NULL)

    expect_identical(fp$data_hash, NA_character_)
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

  it("records the same fields whatever the session's collation", {

    ## Radix order puts "A" before "a"; a locale's collation puts "a" first
    mixed <- df_xy
    mixed$sample_id <- c("b", "A", "a")

    in_c <- withr::with_collate("C", eval_data_fingerprint(mixed, roles_xy)$data_fields)

    suppressWarnings(withr::local_collate("en_US.UTF-8"))
    skip_if(identical(sort(mixed$sample_id), sort(mixed$sample_id, method = "radix")),
            "no locale here that collates case apart from radix order")

    expect_identical(eval_data_fingerprint(mixed, roles_xy)$data_fields, in_c)

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
      suppressWarnings(evaluate(make_eval_object(n = 60, n_configs = 2), prune = FALSE, seed = 42L, control = control_horizons(output_dir = tmpdir))),
      class = "horizons_error_input"
    )

    err <- tryCatch(
      suppressWarnings(evaluate(make_eval_object(n = 60, n_configs = 2), prune = FALSE, seed = 42L, control = control_horizons(output_dir = tmpdir))),
      horizons_error_input = function(e) e
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
      suppressWarnings(evaluate(obj_clay, prune = FALSE, seed = 42L, control = control_horizons(output_dir = tmpdir))),
      horizons_error_input = function(e) e
    )

    expect_s3_class(err, "horizons_error_input")

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
      suppressWarnings(evaluate(edit(obj), prune = FALSE, seed = 42L, control = control_horizons(output_dir = tmpdir))),
      horizons_error_input = function(e) e
    )

  }

  it("aborts when the spectra change on the same samples", {

    err <- refuses_on(function(o) {
      o$data$analysis$wn_4000 <- o$data$analysis$wn_4000 + 0.01
      o
    })

    expect_s3_class(err, "horizons_error_input")
    msg <- flat_message(err)
    expect_match(msg, "different predictor values")
    expect_no_match(msg, "different outcome values")

  })

  it("aborts when the outcome is rescaled under the same name", {

    err <- refuses_on(function(o) {
      o$data$analysis$SOC <- o$data$analysis$SOC * 10
      o
    })

    expect_s3_class(err, "horizons_error_input")
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

    expect_s3_class(err, "horizons_error_input")
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
      evaluate(wider, prune = FALSE,
               seed = 42L, control = control_horizons(output_dir = tmpdir))
    )

    ## Nothing unusable in the store: no note on the log
    expect_named(caught$value$log, names(new_log()))
    expect_false(any(caught$value$log$type == "note"))
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

  it("notes a per-config file it cannot read, without warning, and re-evaluates that config", {

    ck     <- ck_a()
    obj    <- ck$value$obj
    tmpdir <- ck$dir

    writeLines("not an rds file", file.path(tmpdir, "checkpoints", "cfg_001.rds"))

    ## Only time is lost, so it is a note on the log and a line in the tree
    out <- utils::capture.output(warns <- testthat::capture_warnings(
      second <- evaluate(obj, prune = FALSE, seed = 42L,
                         control = control_horizons(output_dir = tmpdir, verbose = TRUE))
    ))

    ## (rsample's own warnings about the small strata are not under test)
    expect_false(any(grepl("cfg_001.rds", warns, fixed = TRUE)))
    note <- second$log[second$log$type == "note", ]
    expect_identical(nrow(note), 1L)
    expect_identical(note$ids[[1]], "checkpoints/cfg_001.rds")
    expect_match(note$message, "1 file in .* could not be used as a checkpoint, so its configuration was evaluated again.")
    expect_match(note$message, "checkpoints/cfg_001.rds: ", fixed = TRUE)
    expect_true(any(grepl("Could not use checkpoints/cfg_001.rds: .* \\(will be re-evaluated\\)$", out)))
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

    suppressWarnings(evaluate(obj, prune = FALSE, seed = 42L, control = control_horizons(output_dir = tmpdir)))

    real     <- readRDS(file.path(tmpdir, "checkpoints", "rf_001.rds"))
    leftover <- real
    leftover$cv_rpd <- 999      # a valid row, made to win
    saveRDS(leftover, file.path(tmpdir, "checkpoints", "file1a2b3c.rds"))

    expect_identical(
      list.files(file.path(tmpdir, "checkpoints"))[1], "file1a2b3c.rds"
    )

    second <- suppressWarnings(
      evaluate(obj, prune = FALSE, seed = 42L, control = control_horizons(output_dir = tmpdir))
    )

    got <- second$evaluation$results
    expect_equal(got$cv_rpd[got$config_id == "rf_001"], real$cv_rpd)

    note <- second$log$message[second$log$type == "note"]
    expect_length(note, 1L)
    expect_true(grepl("file1a2b3c.rds: holds config rf_001; not a checkpoint name",
                      note, fixed = TRUE))

  })

  it("reads a file as a checkpoint only when it holds one result row with a config_id", {

    dir   <- withr::local_tempdir()
    write <- function(x) {
      f <- tempfile(tmpdir = dir, fileext = ".rds")
      saveRDS(x, f)
      f
    }

    row <- tibble::tibble(config_id = "cfg_001", status = "success")

    expect_identical(read_checkpoint_file(write(row)), list(row = row, error = NULL))

    ## Two rows, the row as a list, and a row with no config_id
    for (bad in list(rbind(row, row), as.list(row), row["status"])) {
      expect_identical(read_checkpoint_file(write(bad)),
                       list(row = NULL, error = "not a one-row result with a config_id"))
    }

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
      suppressWarnings(evaluate(obj, prune = FALSE, seed = 42L, control = control_horizons(output_dir = tmpdir))),
      horizons_error_input = function(e) e
    )

    expect_s3_class(err, "horizons_error_input")
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
      suppressWarnings(evaluate(changed, prune = FALSE, seed = 42L, control = control_horizons(output_dir = tmpdir))),
      horizons_error_input = function(e) e
    )

    expect_s3_class(err, "horizons_error_input")

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
      suppressWarnings(evaluate(obj, prune = TRUE,
                                prune_threshold = 2, seed = 42L, control = control_horizons(output_dir = tmpdir))),
      "prune_threshold",
      class = "horizons_error_input"
    )

    ## With pruning off the threshold is never read, so it cannot have
    ## changed what a row holds. CK-A was run with prune = FALSE,
    ## prune_threshold = 1.
    ck_off <- ck_a()
    off    <- ck_off$dir

    first  <- ck_off$value$first
    second <- suppressWarnings(evaluate(obj, prune = FALSE,
                                        prune_threshold = 2, seed = 42L, control = control_horizons(output_dir = off)))

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
      evaluate(obj, prune = FALSE, metric = "rmse", seed = 42L, control = control_horizons(output_dir = tmpdir))
    )
    second <- caught$value

    ## Every row carries all six cv_ columns and the ranking is recomputed on
    ## each run, so the metric is not part of what a row holds.
    expect_equal(second$evaluation$results$runtime_secs,
                 first$evaluation$results$runtime_secs)
    expect_equal(second$evaluation$rank_metric, "rmse")

    ## A clean store reports nothing unreadable: these per-config files, so
    ## the resume records no note, and a directory with no checkpoints/.
    expect_named(second$log, names(new_log()))
    expect_false(any(second$log$type == "note"))

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
      suppressWarnings(evaluate(wider, prune = FALSE, seed = 42L, control = control_horizons(output_dir = tmpdir))),
      horizons_error_input = function(e) e
    )

    expect_s3_class(err, "horizons_error_input")
    expect_match(flat_message(err), "sg_window")
    expect_match(flat_message(err), "settings")

    tighter <- obj
    tighter$config$recipe$pca_threshold <- 0.9

    expect_error(
      suppressWarnings(evaluate(tighter, prune = FALSE, seed = 42L, control = control_horizons(output_dir = tmpdir))),
      "pca_threshold",
      class = "horizons_error_input"
    )

  })

})


## =========================================================================
## What the configurations report on the log
## =========================================================================

describe("evaluate() - the configurations' warnings and failures on the log", {

  it("lifts a configuration's horizons warning onto the log once, naming the configurations, and stores no records", {

    template <- ev60()$evaluation$results[1, ]

    ## Each row as evaluate_single_config() returns it: its unit's records
    ## on it, a substitution warning among them, and another package's
    ## warning, which stays on the row
    local_mocked_bindings(
      evaluate_single_config = function(config_row, ...) {
        id  <- config_row$config_id
        row <- mock_eval_row(template, id, "success", warnings = "Kept the grid's choice.")
        attr(row, "unit_records") <- run_unit({
          warn_in_unit("substitution", "Kept the grid's choice.")
          warning("ranger warned")
        }, "configuration", id)$records
        row
      }
    )

    dir <- withr::local_tempdir()
    got <- collect_warnings(evaluate(make_eval_object(n = 60, n_configs = 2), seed = 42L,
                                     control = control_horizons(output_dir = dir)))
    ev  <- got$value

    subs <- Filter(function(w) inherits(w, "horizons_warning_substitution"), got$warnings)
    expect_length(subs, 1L)
    expect_false(any(vapply(got$warnings, function(w) grepl("ranger warned", conditionMessage(w)),
                            logical(1))))

    row <- ev$log[ev$log$topic %in% "substitution", ]
    expect_identical(row$ids[[1]], c("cfg_001", "cfg_002"))
    expect_match(row$message, "Kept the grid's choice. In 2 configurations", fixed = TRUE)
    expect_identical(subs[[1]]$log, row)

    expect_null(attr(readRDS(file.path(dir, "checkpoints", "cfg_001.rds")), "unit_records"))

  })

  it("says which configurations failed in one partial_failure warning, with each distinct cause", {

    template <- ev60()$evaluation$results[1, ]

    rows <- list(
      cfg_001 = mock_eval_row(template, "cfg_001", "success"),
      cfg_002 = mock_eval_row(template, "cfg_002", "failed", error = "Grid search failed: mocked {cause}"),
      cfg_003 = mock_eval_row(template, "cfg_003", "failed", error = "Grid search failed: mocked {cause}")
    )

    local_mocked_bindings(
      evaluate_single_config = function(config_row, ...) rows[[config_row$config_id]]
    )

    got <- collect_warnings(evaluate(make_eval_object(n = 60, n_configs = 3), seed = 42L))
    ev  <- got$value

    partial <- Filter(function(w) inherits(w, "horizons_warning_partial_failure"), got$warnings)
    expect_length(partial, 1L)

    row <- ev$log[ev$log$topic %in% "partial_failure", ]
    expect_identical(row$ids[[1]], c("cfg_002", "cfg_003"))
    expect_identical(row$n, 2L)
    expect_match(row$message, "2 of 3 configurations failed, so the evaluation holds the rest.",
                 fixed = TRUE)
    expect_match(row$message, "Grid search failed: mocked {cause} (cfg_002, cfg_003)", fixed = TRUE)

  })

  it("raises no partial_failure warning when every configuration succeeded", {

    expect_false("partial_failure" %in% ev60()$log$topic)

  })

  it("names a configuration whose Bayesian search failed in one substitution warning (#209)", {

    ## A grid of one gives tune_bayes() a single initial point, so the search
    ## fails; configure() refuses this, the object here is set by hand
    obj <- make_eval_object(n = 60, n_configs = 1)
    obj$config$tuning$grid_size     <- 1L
    obj$config$tuning$bayesian_iter <- 2L

    got <- collect_warnings(evaluate(obj, prune = FALSE, seed = 42L))
    ev  <- got$value

    subs <- Filter(function(w) inherits(w, "horizons_warning_substitution"), got$warnings)
    expect_length(subs, 1L)
    expect_match(conditionMessage(subs[[1]]), "The Bayesian search failed", fixed = TRUE)

    row <- ev$log[ev$log$topic %in% "substitution", ]
    expect_identical(row$ids[[1]], "cfg_001")
    expect_identical(ev$evaluation$results$status, "success")

  })

  it("stops at the first configuration's failure under debug, with its own error", {

    local_mocked_bindings(build_recipe = function(...) stop("no recipe for this config"))

    ## The configuration's own error, not a contained failure: without debug
    ## both configurations fail and the call ends in the all-failed abort
    err <- expect_error(suppressWarnings(evaluate(make_eval_object(n = 60, n_configs = 2), seed = 42L,
                                                  control = control_horizons(debug = TRUE))),
                        "no recipe for this config", fixed = TRUE, class = "simpleError")
    expect_false(inherits(err, "horizons_error"))

  })

  it("keeps a failed configuration's class, and its backtrace under keep_traces", {

    local_mocked_bindings(build_recipe = function(...) stop("no recipe for this config"))

    run <- function(keep_traces) {
      suppressWarnings(tryCatch(
        evaluate(make_eval_object(n = 60, n_configs = 1), seed = 42L,
                 control = control_horizons(keep_traces = keep_traces)),
        horizons_error_all_configs_failed = function(e) e$results
      ))
    }

    kept <- run(TRUE)
    expect_identical(kept$class, "simpleError")
    expect_type(kept$trace[[1]], "character")

    expect_null(run(FALSE)$trace[[1]])

  })

})


## =========================================================================
## Stopping early: the circuit breaker and a worker's death
## =========================================================================

describe("evaluate() - the circuit breaker", {

  ## Failed rows from a mocked runner: the same failure in every
  ## configuration but for its id and a number, unless `differ` names it
  failing_runner <- function(template, ran, differ = character(0), succeed = character(0)) {
    function(config_row, ...) {
      id <- config_row$config_id
      ran$ids <- c(ran$ids, id)
      if (id %in% succeed) return(mock_eval_row(template, id, "success"))
      row <- mock_eval_row(template, id, "failed", error = paste0(
        if (id %in% differ) "Grid search failed: " else "Recipe building failed: ",
        "no recipe for ", id, " at ", nchar(id) * 7, " rows"))
      row$class <- "simpleError"
      row
    }
  }

  it("stops at the third identical failure across two models, with the rows so far, checkpointing none", {

    template <- ev60()$evaluation$results[1, ]
    ran      <- new.env()
    local_mocked_bindings(evaluate_single_config = failing_runner(template, ran))

    dir <- withr::local_tempdir()
    err <- expect_error(
      suppressWarnings(evaluate(make_eval_object(n = 60, n_configs = 5), seed = 42L,
                                control = control_horizons(output_dir = dir))),
      class = "horizons_error_systemic"
    )

    expect_true(inherits(err, "horizons_error_run"))
    expect_identical(ran$ids, c("cfg_001", "cfg_002", "cfg_003"))
    expect_identical(err$results$config_id, c("cfg_001", "cfg_002", "cfg_003"))
    expect_match(conditionMessage(err), "The first 3 configurations to run all failed the same way, across 2 models",
                 fixed = TRUE)
    expect_match(conditionMessage(err), "breaker = FALSE", fixed = TRUE)
    expect_length(list.files(file.path(dir, "checkpoints")), 0L)

  })

  it("does not stop when the identical failures are all on one model, and checkpoints them at the end", {

    template <- ev60()$evaluation$results[1, ]
    ran      <- new.env()
    local_mocked_bindings(evaluate_single_config = failing_runner(template, ran))

    obj <- make_eval_object(n = 60, n_configs = 4)
    obj$config$configs$model <- "rf"

    dir <- withr::local_tempdir()
    expect_error(suppressWarnings(evaluate(obj, seed = 42L, control = control_horizons(output_dir = dir))),
                 class = "horizons_error_all_configs_failed")

    expect_length(ran$ids, 4L)
    expect_setequal(list.files(file.path(dir, "checkpoints")), paste0(obj$config$configs$config_id, ".rds"))

  })

  it("is disarmed by a success, and its held rows are checkpointed then", {

    template <- ev60()$evaluation$results[1, ]
    ran      <- new.env()
    local_mocked_bindings(evaluate_single_config = failing_runner(template, ran, succeed = "cfg_003"))

    dir <- withr::local_tempdir()
    ev  <- suppressWarnings(evaluate(make_eval_object(n = 60, n_configs = 5), seed = 42L,
                                     control = control_horizons(output_dir = dir)))

    expect_length(ran$ids, 5L)
    expect_identical(ev$evaluation$best_config, "cfg_003")
    expect_length(list.files(file.path(dir, "checkpoints")), 5L)

  })

  it("is disarmed by a different failure, and is off at breaker = FALSE", {

    template <- ev60()$evaluation$results[1, ]

    ran <- new.env()
    local_mocked_bindings(evaluate_single_config = failing_runner(template, ran, differ = "cfg_002"))
    expect_error(suppressWarnings(evaluate(make_eval_object(n = 60, n_configs = 5), seed = 42L)),
                 class = "horizons_error_all_configs_failed")
    expect_length(ran$ids, 5L)

    ran <- new.env()
    local_mocked_bindings(evaluate_single_config = failing_runner(template, ran))
    expect_error(suppressWarnings(evaluate(make_eval_object(n = 60, n_configs = 5), seed = 42L,
                                           control = control_horizons(breaker = FALSE))),
                 class = "horizons_error_all_configs_failed")
    expect_length(ran$ids, 5L)

  })

  it("does not stop when nothing is left to run: the last identical failures end in the all-failed abort, checkpointed", {

    template <- ev60()$evaluation$results[1, ]
    ran      <- new.env()
    local_mocked_bindings(evaluate_single_config = failing_runner(template, ran))

    dir <- withr::local_tempdir()
    err <- expect_error(suppressWarnings(evaluate(make_eval_object(n = 60, n_configs = 3), seed = 42L,
                                                  control = control_horizons(output_dir = dir))),
                        class = "horizons_error_all_configs_failed")

    expect_false(inherits(err, "horizons_error_systemic"))
    expect_length(ran$ids, 3L)
    expect_identical(err$results$config_id, c("cfg_001", "cfg_002", "cfg_003"))
    expect_length(list.files(file.path(dir, "checkpoints")), 3L)

  })

  it("ends either way in an error of the run kind carrying the rows, which the subset loop in ?evaluate catches", {

    template <- ev60()$evaluation$results[1, ]
    local_mocked_bindings(evaluate_single_config = failing_runner(template, new.env()))

    obj    <- make_eval_object(n = 60, n_configs = 5)
    caught <- lapply(list(control_horizons(), control_horizons(breaker = FALSE)), function(control) {
      tryCatch(suppressWarnings(evaluate(obj, seed = 42L, control = control)),
               horizons_error_run = function(e) e)
    })

    expect_s3_class(caught[[1]], "horizons_error_systemic")
    expect_identical(caught[[1]]$results$config_id, sprintf("cfg_%03d", 1:3))
    expect_s3_class(caught[[2]], "horizons_error_all_configs_failed")
    expect_identical(caught[[2]]$results$config_id, sprintf("cfg_%03d", 1:5))

  })

  it("waits for its limit", {

    template <- ev60()$evaluation$results[1, ]
    ran      <- new.env()
    local_mocked_bindings(evaluate_single_config = failing_runner(template, ran))

    err <- expect_error(suppressWarnings(evaluate(make_eval_object(n = 60, n_configs = 6), seed = 42L,
                                                  control = control_horizons(breaker = 4L))),
                        class = "horizons_error_systemic")
    expect_length(ran$ids, 4L)
    expect_length(err$results$config_id, 4L)

  })

  it("counts two failures as one when they differ only in the configuration's id, numbers and spacing", {

    row <- function(id, message, class = "simpleError") {
      tibble::tibble(config_id = id, status = "failed", error_message = message, class = class)
    }

    ## Ids as configure() writes them: the model, the recipe and a hash
    a <- "rf_raw_none_none_a3f91c"
    b <- "cubist_snv_log_pca_0b7e2d"

    same <- breaker_key(row(a, paste0("Grid search failed: boom in ", a, " at 12  rows")))

    expect_identical(breaker_key(row(b, paste0("Grid search failed: boom in ", b, " at 15 rows"))), same)
    expect_false(identical(breaker_key(row(b, paste0("Recipe building failed: boom in ", b, " at 15 rows"))), same))
    expect_false(identical(breaker_key(row(b, paste0("Grid search failed: boom in ", b, " at 15 rows"),
                                           class = "horizons_error_run")), same))

  })

})

describe("evaluate() - a worker's death", {

  it("stops the run as systemic, chained to future's error, with the rows finished so far", {

    template <- ev60()$evaluation$results[1, ]

    local_mocked_bindings(evaluate_single_config = function(config_row, ...) {
      if (config_row$config_id == "cfg_002") {
        stop(structure(class = c("FutureInterruptError", "FutureError", "error", "condition"),
                       list(message = "A future worker died.", call = NULL)))
      }
      mock_eval_row(template, config_row$config_id, "success")
    })

    err <- expect_error(suppressWarnings(evaluate(make_eval_object(n = 60, n_configs = 3), seed = 42L)),
                        class = "horizons_error_systemic")

    expect_true(rlang::cnd_inherits(err, "FutureError"))
    ## Wrapped once: future's error is the direct parent, and the message is
    ## not repeated
    expect_s3_class(err$parent, "FutureError")
    expect_length(gregexpr("A parallel worker stopped while", conditionMessage(err), fixed = TRUE)[[1]], 1L)
    expect_identical(err$results$config_id, "cfg_001")
    expect_match(conditionMessage(err), "A parallel worker stopped", fixed = TRUE)
    expect_match(conditionMessage(err), "in this error's results field", fixed = TRUE)

    ## With an output_dir, the finished configurations are on disk to resume from
    dir <- withr::local_tempdir()
    err <- expect_error(suppressWarnings(evaluate(make_eval_object(n = 60, n_configs = 3), seed = 42L,
                                                  control = control_horizons(output_dir = dir))),
                        class = "horizons_error_systemic")
    expect_match(gsub("\\s+", " ", conditionMessage(err)), "calling `evaluate()` again with it resumes from them",
                 fixed = TRUE)
    expect_identical(list.files(file.path(dir, "checkpoints")), "cfg_001.rds")

  })

  it("stops a resamples-axis run whose worker is killed (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    local_plan(future::multisession, workers = 2)

    ## A recipe step that kills the worker it runs on, on an analysis set;
    ## never the parent, which preps the whole training part. The function
    ## is written into the step, so the worker needs nothing from here.
    kill <- bquote((function(x) {
      if (length(x) < 45 && Sys.getpid() != .(Sys.getpid())) tools::pskill(Sys.getpid(), tools::SIGKILL)
      x
    })(sample_id))
    real_recipe <- build_recipe
    local_mocked_bindings(build_recipe = function(...) {
      real_recipe(...) |> recipes::step_mutate(sample_id = !!kill)
    })

    err <- expect_error(
      suppressWarnings(evaluate(make_eval_object(n = 60, n_configs = 1), seed = 42L,
                                control = control_horizons(allow_par = TRUE, parallelize_over = "resamples"))),
      class = "horizons_error_systemic"
    )
    expect_true(rlang::cnd_inherits(err, "FutureError"))
    ## Wrapped once: future's error is the direct parent, and the message is
    ## not repeated
    expect_s3_class(err$parent, "FutureError")
    expect_length(gregexpr("A parallel worker stopped while", conditionMessage(err), fixed = TRUE)[[1]], 1L)

  })

  it("stops a resamples-axis run whose worker is killed during a Bayesian iteration, checkpointing nothing (installed build)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_dev_package()

    local_plan(future::multisession, workers = 2)

    ## tune's search catches future's error there, so the death reaches
    ## evaluate() only as future.apply's unclassed warning, matched by its
    ## text: if that wording changes, this fails rather than the death
    ## reading as a search that stopped early. The kill is armed at the
    ## search's second Gaussian-process prediction, once an iteration has
    ## run, and fires once, on a worker, from a recipe step that needs
    ## nothing from here.
    armed  <- withr::local_tempfile(pattern = "armed")
    killed <- withr::local_tempfile(pattern = "killed")
    kill <- bquote((function(x) {
      if (file.exists(.(armed)) && !file.exists(.(killed)) && Sys.getpid() != .(Sys.getpid())) {
        file.create(.(killed))
        tools::pskill(Sys.getpid(), tools::SIGKILL)
      }
      x
    })(sample_id))
    real_recipe <- build_recipe
    local_mocked_bindings(build_recipe = function(...) {
      real_recipe(...) |> recipes::step_mutate(sample_id = !!kill)
    })
    real_pred_gp <- tune:::pred_gp
    n_pred       <- 0L
    local_mocked_bindings(pred_gp = function(...) {
      n_pred <<- n_pred + 1L
      if (n_pred == 2L) file.create(armed)
      real_pred_gp(...)
    }, .package = "tune")

    obj <- make_eval_object(n = 60, n_configs = 1)
    obj$config$tuning$grid_size     <- 3L
    obj$config$tuning$bayesian_iter <- 4L

    dir <- withr::local_tempdir()
    err <- expect_error(
      suppressWarnings(evaluate(obj, seed = 42L,
                                control = control_horizons(allow_par = TRUE, parallelize_over = "resamples",
                                                           output_dir = dir))),
      class = "horizons_error_systemic"
    )

    expect_true(file.exists(killed))
    expect_match(conditionMessage(err), "A parallel worker stopped", fixed = TRUE)
    expect_match(conditionMessage(err$parent), "A parallel worker stopped during the Bayesian search", fixed = TRUE)
    ## Wrapped once: the unit's systemic error is the direct parent, and
    ## future.apply's warning is its cause
    expect_s3_class(err$parent, "horizons_error_systemic")
    expect_s3_class(err$parent$parent, "warning")
    expect_match(conditionMessage(err$parent$parent), "^Caught [A-Za-z]*Future[A-Za-z]*Error\\. Canceling all iterations")
    expect_length(list.files(file.path(dir, "checkpoints")), 0L)

  })

  it("lets an error that is not a worker's death through, unwrapped", {

    boom <- simpleError("Not a worker's death.")
    err  <- expect_error(stop_on_worker_death(stop(boom), list(), NULL), "Not a worker's death.", fixed = TRUE)

    expect_identical(class(err), class(boom))
    expect_null(err$parent)

  })

  it("is read from future.apply's warning for every class of future's errors, and for no other", {

    caught <- function(class) simpleWarning(paste0("Caught ", class, ". Canceling all iterations ..."))

    for (class in c("FutureInterruptError", "FutureError", "FutureLaunchError", "RngFutureError",
                    "UnexpectedFutureResultError", "GlobalEnvMisuseFutureError")) {
      expect_true(worker_death_warning(caught(class)), label = class)
    }

    ## future.apply warns the same way about an error the worker's own code raised
    expect_false(worker_death_warning(caught("simpleError")))
    expect_false(worker_death_warning(caught("rlang_error")))
    expect_false(worker_death_warning(simpleWarning("A FutureInterruptError was caught.")))

  })

})


## =========================================================================
## The ranking report: what was left out, and what to rank by instead
## =========================================================================

describe("evaluate() - the ranking report when pruned configurations hold the metric", {

  rows_with <- function(template, statuses, cv_rsq) {
    rows <- Map(function(id, status, rsq) {
      row <- mock_eval_row(template, id, status)
      row$cv_rsq <- rsq
      row
    }, sprintf("cfg_%03d", seq_along(statuses)), statuses, cv_rsq)
    stats::setNames(rows, sprintf("cfg_%03d", seq_along(statuses)))
  }

  it("names only the successes when none has the metric, says the pruned one is not ranked, and never suggests it", {

    template <- ev60()$evaluation$results[1, ]
    rows     <- rows_with(template, c("success", "pruned"), c(NA_real_, 0.6))
    local_mocked_bindings(evaluate_single_config = function(config_row, ...) rows[[config_row$config_id]])

    out <- utils::capture.output(got <- collect_warnings(
      evaluate(make_eval_object(n = 60, n_configs = 2), metric = "rsq", seed = 42L,
               control = control_horizons(verbose = TRUE))
    ))
    ev <- got$value

    expect_identical(ev$evaluation$best_config, NA_character_)
    expect_true("\u2514\u2500 Best: none, since no configuration that succeeded has a CV RSQ" %in% out)

    skipped <- Filter(function(w) inherits(w, "horizons_warning_skipped"), got$warnings)
    expect_length(skipped, 1L)
    msg <- gsub("\\s+", " ", conditionMessage(skipped[[1]]))
    expect_match(msg, "1 succeeded, but none has a cv_rsq value.", fixed = TRUE)
    expect_match(msg, "1 pruned configuration has one, but pruned configurations are ranked only when none succeeded.",
                 fixed = TRUE)
    expect_no_match(msg, "\"rsq\"", fixed = TRUE)
    expect_identical(ev$log$ids[ev$log$topic %in% "skipped"][[1]], "cfg_001")

    ## fit() refuses it the same way, and suggests only a metric that ranks
    err <- expect_error(fit(ev, metric = "rsq", compute_uq = FALSE, compute_ad = FALSE),
                        "1 succeeded, but none has a cv_rsq value.", fixed = TRUE)
    fit_msg <- gsub("\\s+", " ", conditionMessage(err))
    expect_match(fit_msg, "fit(x, metric = )", fixed = TRUE)
    expect_no_match(fit_msg, "\"rsq\"", fixed = TRUE)

  })

  it("ranks the pruned configurations when none succeeded, and says which lack the metric", {

    template <- ev60()$evaluation$results[1, ]
    rows     <- rows_with(template, c("pruned", "pruned"), c(NA_real_, 0.6))
    local_mocked_bindings(evaluate_single_config = function(config_row, ...) rows[[config_row$config_id]])

    got <- collect_warnings(evaluate(make_eval_object(n = 60, n_configs = 2), metric = "rsq", seed = 42L))
    ev  <- got$value

    expect_identical(ev$evaluation$best_config, "cfg_002")

    skipped <- Filter(function(w) inherits(w, "horizons_warning_skipped"), got$warnings)
    expect_length(skipped, 1L)
    expect_match(conditionMessage(skipped[[1]]), "1 configuration left out of the ranking, with no cv_rsq value.",
                 fixed = TRUE)
    expect_identical(ev$log$ids[ev$log$topic %in% "skipped"][[1]], "cfg_001")

  })

  it("says in the tree's footer that no pruned configuration has the metric when none succeeded and none has it", {

    template <- ev60()$evaluation$results[1, ]
    rows     <- rows_with(template, c("pruned", "pruned"), c(NA_real_, NA_real_))
    local_mocked_bindings(evaluate_single_config = function(config_row, ...) rows[[config_row$config_id]])

    out <- utils::capture.output(ev <- suppressWarnings(
      evaluate(make_eval_object(n = 60, n_configs = 2), metric = "rsq", seed = 42L,
               control = control_horizons(verbose = TRUE))
    ))

    expect_identical(ev$evaluation$best_config, NA_character_)
    expect_true("\u2514\u2500 Best: none, since no pruned configuration has a CV RSQ" %in% out)

  })

})
