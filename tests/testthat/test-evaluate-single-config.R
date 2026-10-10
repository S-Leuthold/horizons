## ---------------------------------------------------------------------------
## Tests: evaluate_single_config()
## ---------------------------------------------------------------------------

## Helper: create minimal data + split + folds + role_map for evaluation
make_eval_setup <- function(n = 40, n_wn = 10) {

  set.seed(42)

  ## Spectral data: n samples x n_wn wavelengths
  wn_names <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
  spec_mat <- matrix(rnorm(n * n_wn), nrow = n)
  colnames(spec_mat) <- wn_names

  df <- tibble::as_tibble(spec_mat)
  df$sample_id <- paste0("S", sprintf("%03d", seq_len(n)))

  ## Outcome with weak signal from first 3 predictors
  df$SOC <- 2 + rowMeans(spec_mat[, 1:min(3, n_wn)]) * 0.5 + rnorm(n, sd = 0.5)

  ## Role map
  roles <- tibble::tibble(
    variable = c("sample_id", wn_names, "SOC"),
    role     = c("id", rep("predictor", n_wn), "outcome")
  )

  ## Create split and folds (suppress stratification warnings for tiny data)
  split <- suppressWarnings(rsample::initial_split(df, prop = 0.75, strata = "SOC"))
  train <- rsample::training(split)
  folds <- suppressWarnings(rsample::vfold_cv(train, v = 3, strata = "SOC"))

  list(data = df, split = split, folds = folds, role_map = roles, train_data = train)

}

## Helper: create a config row (matching config$configs schema)
make_eval_config <- function(model             = "rf",
                             transformation    = "none",
                             preprocessing     = "raw",
                             feature_selection = "none",
                             config_id         = "test_cfg_001") {

  tibble::tibble(
    config_id         = config_id,
    model             = model,
    transformation    = transformation,
    preprocessing     = preprocessing,
    feature_selection = feature_selection
  )

}

## Expected columns in the result tibble
EXPECTED_RESULT_COLS <- c(
  "config_id", "status", "rmse", "rrmse", "rsq", "ccc", "rpd", "mae",
  "best_params", "error_message", "warnings", "runtime_secs"
)

## =========================================================================
## Shared evaluation (helper-memo.R)
## =========================================================================
## One plain successful evaluate_single_config() result (rf, grid of 2, no
## Bayesian stage, prune gate off), built on its first use in a process and
## read by every block below that asserts on that run.

build_esc <- function() {

  setup <- make_eval_setup()

  evaluate_single_config(
    config_row    = make_eval_config(),
    split         = setup$split,
    cv_folds      = setup$folds,
    role_map      = setup$role_map,
    grid_size     = 2,
    bayesian_iter = 0,
    prune         = FALSE,
    seed          = 42L
  )

}

esc <- function() memo_fixture("esc", build_esc)

## =========================================================================
## Success path
## =========================================================================

describe("evaluate_single_config() - success path", {

  it("is a single-row tibble with every expected column, a best_params list, no error, a positive runtime and its config_id", {

    result <- esc()
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 1)
    expect_true(all(EXPECTED_RESULT_COLS %in% names(result)))
    expect_true(is.list(result$best_params))
    expect_false(is.null(result$best_params[[1]]))
    expect_true(is.na(result$error_message))
    expect_true(result$runtime_secs > 0)
    expect_equal(result$config_id, "test_cfg_001")

  })

  it("sets status to 'success'", {

    result <- esc()
    expect_equal(result$status, "success")

  })

  it("has non-NA values for all six metrics", {

    result <- esc()
    expect_false(is.na(result$rmse))
    expect_false(is.na(result$rrmse))
    expect_false(is.na(result$rsq))
    expect_false(is.na(result$ccc))
    expect_false(is.na(result$rpd))
    expect_false(is.na(result$mae))

  })

  it("records the six cross-validated means at the selected hyperparameters (#50)", {

    result  <- esc()
    cv_cols <- paste0("cv_", c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae"))

    expect_true(all(cv_cols %in% names(result)))

    ## rmse / rpd are always defined; rsq / ccc may be NA on a degenerate fold
    expect_true(is.finite(result$cv_rmse))
    expect_true(is.finite(result$cv_rpd))

    ## The CV panel and the test panel are different quantities; if they were
    ## ever identical the lookup would be reading the wrong thing.
    expect_false(isTRUE(all.equal(result$cv_rmse, result$rmse)))

  })

  it("keeps .config on best_params so the CV panel can be looked up", {

    result <- esc()
    expect_true(".config" %in% names(result$best_params[[1]]))

  })

})

## =========================================================================
## cv_panel_at()
## =========================================================================

describe("cv_panel_at()", {

  ## A real tune_grid() result on a tiny workflow, so the panel lookup is
  ## checked against tune's own collect_metrics() rather than a mock.
  set.seed(4903)
  d <- tibble::tibble(x1 = stats::rnorm(40), x2 = stats::rnorm(40))
  d$y <- 1 + 2 * d$x1 + stats::rnorm(40, sd = 0.3)
  folds <- rsample::vfold_cv(d, v = 3)

  wf <- workflows::workflow() |>
    workflows::add_recipe(recipes::recipe(y ~ ., d)) |>
    workflows::add_model(parsnip::rand_forest(mtry = tune::tune(), trees = 50) |>
                           parsnip::set_engine("ranger") |>
                           parsnip::set_mode("regression"))

  ps <- dials::finalize(workflows::extract_parameter_set_dials(wf), d[, 1:2])

  set.seed(4904)
  tr <- tune::tune_grid(wf, folds, grid = 2, param_info = ps,
                        metrics = tuning_metric_set("none"))
  best <- tune::select_best(tr, metric = "rmse")

  it("returns the collect_metrics() means at best_params$.config", {

    panel <- cv_panel_at(tr, best)
    cm    <- tune::collect_metrics(tr)
    cm    <- cm[cm$.config == best$.config, ]

    for (m in c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae")) {
      expect_equal(panel[[paste0("cv_", m)]], cm$mean[cm$.metric == m],
                   info = m)
    }

  })

  it("returns all-NA rather than erroring when the lookup is impossible", {

    expect_true(all(is.na(unlist(cv_panel_at(NULL, best)))))
    expect_true(all(is.na(unlist(cv_panel_at(tr, best[, setdiff(names(best), ".config")])))))
    expect_true(all(is.na(unlist(cv_panel_at(tr, NULL)))))

  })

})

## =========================================================================
## Failure paths
## =========================================================================

describe("evaluate_single_config() - failure paths", {

  setup <- make_eval_setup()

  it("returns 'failed' for invalid model name", {

    config <- make_eval_config(model = "deep_learning_9000")

    result <- evaluate_single_config(
      config_row    = config,
      split         = setup$split,
      cv_folds      = setup$folds,
      role_map      = setup$role_map,
      grid_size     = 2,
      bayesian_iter = 0
    )

    expect_equal(result$status, "failed")
    expect_true(grepl("Model specification failed", result$error_message))

  })

  it("returns 'failed' for invalid feature_selection", {

    config <- make_eval_config(feature_selection = "quantum_entanglement")

    result <- evaluate_single_config(
      config_row    = config,
      split         = setup$split,
      cv_folds      = setup$folds,
      role_map      = setup$role_map,
      grid_size     = 2,
      bayesian_iter = 0
    )

    expect_equal(result$status, "failed")
    expect_true(grepl("Recipe building failed", result$error_message))

  })

  it("returns 'failed' when Boruta fails, rather than selecting by correlation (#75)", {

    skip_if_not_installed("Boruta")

    ## step_select_boruta() used to catch this and select the columns most
    ## correlated with the outcome, so the config succeeded under the boruta
    ## label. rf fails where evaluate_single_config() preps the recipe to
    ## bound mtry; elastic_net has no mtry and fails inside tune_grid().
    local_mocked_bindings(Boruta = function(...) stop("Boruta could not run"),
                          .package = "Boruta")

    for (model in c("rf", "elastic_net")) {

      config <- make_eval_config(model = model, feature_selection = "boruta")

      result <- evaluate_single_config(
        config_row    = config,
        split         = setup$split,
        cv_folds      = setup$folds,
        role_map      = setup$role_map,
        grid_size     = 2,
        bayesian_iter = 0
      )

      expect_equal(result$status, "failed", label = model)
      expect_true(is.na(result$cv_rmse), label = model)

    }

  })

  it("returns matching column structure and NA metrics on failure", {

    config <- make_eval_config(model = "nope")

    result <- evaluate_single_config(
      config_row    = config,
      split         = setup$split,
      cv_folds      = setup$folds,
      role_map      = setup$role_map,
      grid_size     = 2,
      bayesian_iter = 0
    )

    expect_true(is.na(result$rmse))
    expect_true(is.na(result$rpd))
    expect_true(is.na(result$rsq))
    expect_true(all(EXPECTED_RESULT_COLS %in% names(result)))

    ## cv_* columns exist on failure too, as NA, so bind_rows() is clean
    cv_cols <- paste0("cv_", c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae"))
    expect_true(all(cv_cols %in% names(result)))
    expect_true(all(is.na(unlist(result[cv_cols]))))

  })

})

## =========================================================================
## Pruning
## =========================================================================

describe("evaluate_single_config() - pruning", {

  setup  <- make_eval_setup()
  config <- make_eval_config()

  ## Prune threshold of 9999 will prune any realistic model (RPD < 9999).
  ## tune_bayes() is wrapped to record whether the Bayesian stage ran.
  bayes_ran  <- FALSE
  real_bayes <- tune::tune_bayes

  result <- testthat::with_mocked_bindings(
    evaluate_single_config(
      config_row      = config,
      split           = setup$split,
      cv_folds        = setup$folds,
      role_map        = setup$role_map,
      grid_size       = 2,
      bayesian_iter   = 5,
      prune           = TRUE,
      prune_threshold = 9999,
      seed            = 42L
    ),
    tune_bayes = function(...) {
      bayes_ran <<- TRUE
      real_bayes(...)
    },
    .package = "tune"
  )

  it("returns 'pruned' status when grid RPD is below threshold, and skips the Bayesian stage", {

    expect_equal(result$status, "pruned")
    expect_false(bayes_ran)

  })

  it("still computes metrics when pruned (from grid search)", {

    ## Pruned configs still get last_fit metrics — they just skip Bayesian
    expect_false(is.na(result$rmse))
    expect_false(is.na(result$rsq))

  })

  it("records the gate's reading and the threshold it was taken against", {

    expect_identical(result$below_prune_threshold, TRUE)
    expect_identical(result$prune_threshold, 9999)

  })

})

## =========================================================================
## Tuning metrics are on the original scale (#49 / #38)
## =========================================================================
## The response transform is a skip = TRUE recipe step, so tune never applies
## it to the assessment set. Before tuning_metric_set() the prune gate
## compared original-scale truth to log-scale predictions and stamped healthy
## log models "pruned". This fixture has a strong log-linear signal, so a
## correctly scored grid search must clear prune_threshold = 1.0 comfortably.
## It is also this file's only log-transformed run: test predictions scored
## without the back-transform fail the original-scale RPD check below.

describe("evaluate_single_config() - tuning on the original scale", {

  setup <- make_eval_setup(n = 100, n_wn = 30)

  ## Overwrite the weak-signal outcome with a strong log-linear one, keeping
  ## the split/fold row membership from make_eval_setup(). The signal sits in
  ## interior wavenumbers so the SG window (9) does not trim it away. plsr is
  ## linear in log space, so original-scale test RPD lands around 6 when the
  ## grid is scored honestly; scored cross-scale the same grid reads below 1
  ## and the config is pruned.
  set.seed(4902)
  wn_names <- paste0("wn_", seq(4000, by = -2, length.out = 30))
  signal   <- rowMeans(as.matrix(setup$data[, wn_names[10:15]]))
  setup$data$SOC <- expm1(1.5 + 1.2 * signal + stats::rnorm(nrow(setup$data), sd = 0.05))
  setup$split$data <- setup$data
  setup$folds <- suppressWarnings(
    rsample::vfold_cv(rsample::training(setup$split), v = 3, strata = "SOC")
  )

  config <- make_eval_config(model = "plsr", transformation = "log")

  ## bayesian_iter = 1: the prune gate only runs when there is a Bayesian
  ## stage to skip (#38), and this test is about the gate.
  ##
  ## select_best() is wrapped, passing through, to record the results the
  ## hyperparameters are chosen from. The returned row cannot show whether
  ## the Bayesian stage ran here: the grid already holds plsr's best
  ## num_comp, so the stage's one candidate does not change the choice.
  real_select_best <- tune::select_best
  selected_from    <- NULL

  result <- with_mocked_bindings(
    evaluate_single_config(
      config_row      = config,
      split           = setup$split,
      cv_folds        = setup$folds,
      role_map        = setup$role_map,
      grid_size       = 3,
      bayesian_iter   = 1,
      prune           = TRUE,
      prune_threshold = 1.0,
      seed            = 42L
    ),
    select_best = function(x, ...) {
      selected_from <<- x
      real_select_best(x, ...)
    },
    .package = "tune"
  )

  it("does not prune a healthy log-transformed config at prune_threshold = 1.0", {

    expect_equal(result$status, "success")

    ## Not pruned, so the Bayesian stage ran and its results were kept: the
    ## choice was made from tune_bayes() iteration results, the grid's
    ## candidates at .iter 0 and the one iteration's at .iter 1.
    expect_s3_class(selected_from, "iteration_results")
    expect_equal(sort(unique(tune::collect_metrics(selected_from)$.iter)), c(0, 1))

  })

  it("reports an original-scale test RPD consistent with the strong signal", {

    expect_gt(result$rpd, 1.5)

  })

})

## =========================================================================
## The prune gate is a no-op without a Bayesian stage (#38)
## =========================================================================
## The gate decides whether to skip Bayesian optimization. With
## bayesian_iter = 0 there is none to skip, yet a config below the threshold
## was labelled "pruned", and evaluate() and fit() treat that label as a
## fallback rank.

describe("evaluate_single_config() - prune gate at bayesian_iter = 0 (#38)", {

  setup  <- make_eval_setup()
  config <- make_eval_config()

  ## A threshold no model clears: with a Bayesian stage this config is pruned
  ## (see the pruning block above).
  result <- suppressWarnings(evaluate_single_config(
    config_row      = config,
    split           = setup$split,
    cv_folds        = setup$folds,
    role_map        = setup$role_map,
    grid_size       = 2,
    bayesian_iter   = 0,
    prune           = TRUE,
    prune_threshold = 9999,
    seed            = 42L
  ))

  it("labels the config a success, since no Bayesian stage was skipped", {

    expect_equal(result$status, "success")

  })

  it("still records that the config fell below the threshold", {

    ## The quality signal is kept apart from the status label, so fit() can
    ## warn at bayesian_iter = 0, where nothing is pruned.
    expect_identical(result$below_prune_threshold, TRUE)
    expect_identical(result$prune_threshold, 9999)

  })

  it("records no reading when prune = FALSE", {

    ## The shared evaluation is this config at bayesian_iter = 0 with
    ## prune = FALSE
    unpruned <- esc()

    expect_identical(unpruned$below_prune_threshold, NA)
    expect_identical(unpruned$prune_threshold, NA_real_)

  })

  it("defaults prune_threshold to 1.0, the value evaluate() passes", {

    expect_identical(formals(evaluate_single_config)$prune_threshold, 1.0)
    expect_identical(formals(evaluate_single_config)$prune_threshold,
                     formals(evaluate)$prune_threshold)

  })

})


## =========================================================================
## A Bayesian search that fails keeps the grid's choice, and the row says so
## (#209)
## =========================================================================

describe("evaluate_single_config() - a Bayesian search that fails (#209)", {

  setup <- make_eval_setup()

  run <- function(grid_size) {
    suppressWarnings(evaluate_single_config(
      config_row    = make_eval_config(),
      split         = setup$split,
      cv_folds      = setup$folds,
      role_map      = setup$role_map,
      grid_size     = grid_size,
      bayesian_iter = 2,
      prune         = FALSE,
      seed          = 42L
    ))
  }

  it("keeps the grid's choice and records on the row that the search failed", {

    ## A grid of one gives tune_bayes() a single initial point; it needs two,
    ## so the search aborts every time. configure() refuses this combination,
    ## but the unit takes its sizes directly. The recorder is the pruning
    ## test's, shown here to see a search that does run.
    bayes_ran  <- FALSE
    real_bayes <- tune::tune_bayes
    local_mocked_bindings(tune_bayes = function(...) {
      bayes_ran <<- TRUE
      real_bayes(...)
    }, .package = "tune")

    result <- run(grid_size = 1)

    expect_true(bayes_ran)
    expect_equal(result$status, "success")
    expect_true(any(grepl(
      "The Bayesian search failed, so the hyperparameters were chosen from the grid alone",
      result$warnings[[1]], fixed = TRUE
    )))

  })

  it("records it when the search returns without an iteration past the grid", {

    ## tune drops a candidate that fails in every fold, as this one does, and
    ## returns rather than erroring: the result holds only the grid.
    local_mocked_bindings(
      more_results = function(...) simpleError("All models failed for: x"),
      .package     = "tune"
    )

    result <- run(grid_size = 2)

    expect_equal(result$status, "success")
    expect_true(any(grepl(
      "The Bayesian search produced no results, so the hyperparameters were chosen from the grid alone.",
      result$warnings[[1]], fixed = TRUE
    )))

  })

})

## =========================================================================
## Each stage's failure exit
## =========================================================================
## The recipe and model stages' exits are pinned by the failure-path tests
## above. Each later stage runs inside safely_execute() and, when it errors,
## returns the failed row naming the stage; with its exit removed the config
## still fails, at a later stage under that stage's name, so each case asserts
## the stage and the cause.

describe("evaluate_single_config() - each stage's failure exit", {

  run_on_shared <- function() {

    setup <- make_eval_setup()

    evaluate_single_config(
      config_row    = make_eval_config(),
      split         = setup$split,
      cv_folds      = setup$folds,
      role_map      = setup$role_map,
      grid_size     = 2,
      bayesian_iter = 0,
      prune         = FALSE,
      seed          = 42L
    )

  }

  expect_failed_at <- function(result, stage, cause) {

    expect_equal(result$status, "failed")
    expect_match(result$error_message, paste0("^", stage, " failed: "))
    expect_match(result$error_message, cause, fixed = TRUE)
    expect_true(all(is.na(unlist(result[c("rmse", "rpd", "cv_rmse", "cv_rpd")]))))

  }

  it("names the workflow stage when the model cannot join the workflow", {

    local_mocked_bindings(define_model_spec = function(...) "not a model specification")

    expect_failed_at(run_on_shared(), "Workflow creation", "model_spec")

  })

  it("names the grid search when tune_grid() errors", {

    local_mocked_bindings(tune_grid = function(...) stop("the grid broke"),
                          .package = "tune")

    expect_failed_at(run_on_shared(), "Grid search", "the grid broke")

  })

  it("names the parameter selection when select_best() errors", {

    local_mocked_bindings(select_best = function(...) stop("nothing to select"),
                          .package = "tune")

    expect_failed_at(run_on_shared(), "Parameter selection", "nothing to select")

  })

  it("names the workflow finalization when it errors", {

    local_mocked_bindings(finalize_workflow = function(...) stop("cannot finalize"),
                          .package = "tune")

    expect_failed_at(run_on_shared(), "Workflow finalization", "cannot finalize")

  })

  it("names the test-set evaluation when last_fit() errors", {

    local_mocked_bindings(last_fit = function(...) stop("the last fit broke"),
                          .package = "tune")

    expect_failed_at(run_on_shared(), "Test evaluation", "the last fit broke")

  })

  it("names the back-transformation when it errors", {

    ## The tuning metrics back-transform inside tune; the runner's own call
    ## comes after last_fit()
    last_fit_done <- FALSE
    real_last_fit <- tune::last_fit
    real_bt       <- back_transform_predictions

    local_mocked_bindings(last_fit = function(...) {
      last_fit_done <<- TRUE
      real_last_fit(...)
    }, .package = "tune")
    local_mocked_bindings(back_transform_predictions = function(...) {
      if (last_fit_done) stop("the inverse broke")
      real_bt(...)
    })

    expect_failed_at(run_on_shared(), "Back-transformation", "the inverse broke")

  })

})


## =========================================================================
## A run whose stages warn and whose test metrics cannot be computed
## =========================================================================
## One run, read by four tests: every stage's warnings reach the row's log
## (#96); test metrics that cannot be computed come back as six NAs; a CV
## panel that cannot be recovered leaves a note; and tune's
## parallel_over = "everything" is accepted (a refusal would fail the build,
## and every test below with it). render_warning_log() shows five lines
## besides pinned notes, most frequent first, and tune adds notes of its own,
## so each stage's warning is raised ten times, more than any note can arrive.

build_esc_mocked <- function() {

  setup <- make_eval_setup()

  real_recipe   <- build_recipe
  real_spec     <- define_model_spec
  real_preds    <- prepped_predictors
  real_grid     <- tune::tune_grid
  real_last_fit <- tune::last_fit

  warn_then <- function(message, f) {
    function(...) {
      for (i in 1:10) warning(message, call. = FALSE)
      f(...)
    }
  }

  result <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      evaluate_single_config(
        config_row    = make_eval_config(),
        split         = setup$split,
        cv_folds      = setup$folds,
        role_map      = setup$role_map,
        grid_size     = 2,
        bayesian_iter = 0,
        prune         = FALSE,
        parallel_over = "everything",
        seed          = 42L
      ),
      tune_grid = warn_then("the grid stage warned", real_grid),
      last_fit  = warn_then("the test-set stage warned", real_last_fit),
      .package  = "tune"
    ),
    build_recipe       = warn_then("the recipe stage warned", real_recipe),
    define_model_spec  = warn_then("the model stage warned", real_spec),
    prepped_predictors = warn_then("the finalize stage warned", real_preds),
    ## What compute_original_scale_metrics() returns, with a warning, when
    ## fewer than two test rows have both a truth and a prediction; the
    ## runner calls it for the test rows only
    compute_original_scale_metrics = function(...) tibble::tibble(),
    cv_panel_at = function(...) {
      tibble::as_tibble(stats::setNames(
        as.list(rep(NA_real_, 6)),
        paste0("cv_", c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae"))
      ))
    }
  )

  list(setup = setup, result = result)

}

esc_mocked <- function() memo_fixture("esc_mocked", build_esc_mocked)

describe("evaluate_single_config() - a run on parallel_over = 'everything' whose stages warn and whose test metrics cannot be computed", {

  it("carries every stage's warnings into the row's log (#96)", {

    result <- esc_mocked()$result
    expect_equal(result$status, "success")

    for (message in c("the recipe stage warned", "the model stage warned",
                      "the finalize stage warned", "the grid stage warned",
                      "the test-set stage warned")) {
      expect_true(any(grepl(message, result$warnings[[1]], fixed = TRUE)), label = message)
    }

  })

  it("reports all six test metrics as NA when they cannot be computed", {

    result <- esc_mocked()$result
    expect_true(all(is.na(unlist(result[c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae")]))))
    expect_true(all(vapply(result[c("rmse", "rrmse", "rsq", "ccc", "rpd", "mae")],
                           is.double, logical(1))))

  })

  it("notes on the row when the CV panel at the selected parameters cannot be recovered", {

    expect_true(any(grepl(
      "CV metrics at the selected hyperparameters could not be recovered; cv_* columns are NA.",
      esc_mocked()$result$warnings[[1]], fixed = TRUE
    )))

  })

})


describe("evaluate_single_config() - parallel_over", {

  it("refuses anything but 'resamples' or 'everything', naming the argument", {

    ## The abort carries no package class (DECISIONS 2026-10-05), so its text
    ## is the check.
    setup <- make_eval_setup()

    expect_error(
      evaluate_single_config(
        config_row    = make_eval_config(),
        split         = setup$split,
        cv_folds      = setup$folds,
        role_map      = setup$role_map,
        parallel_over = "folds"
      ),
      "`parallel_over` must be one of"
    )

  })

})


## =========================================================================
## The RNG pin
## =========================================================================

describe("evaluate_single_config() - the RNG pin", {

  it("returns the shared run whatever the caller's RNG kind and state", {

    ## The grid's rf fits draw from the stream the runner pins on entry, and
    ## the CV panel reads them
    setup <- make_eval_setup()

    result <- withr::with_seed(99, .rng_kind = "L'Ecuyer-CMRG", evaluate_single_config(
      config_row    = make_eval_config(),
      split         = setup$split,
      cv_folds      = setup$folds,
      role_map      = setup$role_map,
      grid_size     = 2,
      bayesian_iter = 0,
      prune         = FALSE,
      seed          = 42L
    ))

    cols <- c("rmse", "rpd", "cv_rmse", "cv_rpd")
    expect_identical(result[cols], esc()[cols])

  })

})


## =========================================================================
## mtry ceiling — the predictor count comes from the recipe's roles
## =========================================================================

describe("the mtry upper bound", {

  ## A sibling lab measurement is held at role `response_hold`: it is in the
  ## baked frame and it is not a predictor. Subtracting outcome, id and meta
  ## off the baked frame therefore counted it, and tune_grid() could sample
  ## an mtry one larger than the model matrix is wide.

  make_setup_with_sibling <- function(n = 40, n_wn = 20) {

    setup <- make_eval_setup(n = n, n_wn = n_wn)

    setup$train_data$clay <- stats::runif(nrow(setup$train_data), 5, 60)
    setup$role_map        <- rbind(setup$role_map,
                                   tibble::tibble(variable = "clay", role = "response"))

    setup

  }

  ## The frame the old subtraction would have handed dials::finalize()
  subtracted_width <- function(baked) {

    length(setdiff(names(baked), c("SOC", "sample_id")))

  }

  it("counts only the predictor-role columns of the prepped recipe", {

    setup   <- make_setup_with_sibling()
    recipe  <- build_recipe(make_eval_config(), setup$train_data, setup$role_map)
    prepped <- recipes::prep(recipe)
    baked   <- recipes::bake(prepped, new_data = NULL)

    pred_vars <- prepped_predictors(prepped, baked)

    ## The sibling survives into the baked frame, and is not a predictor
    expect_true("clay" %in% names(baked))
    expect_false("clay" %in% pred_vars)
    expect_identical(length(pred_vars), subtracted_width(baked) - 1L)

  })

  it("finalizes mtry at the true predictor count, not the baked width", {

    setup   <- make_setup_with_sibling()
    recipe  <- build_recipe(make_eval_config(), setup$train_data, setup$role_map)
    prepped <- recipes::prep(recipe)
    baked   <- recipes::bake(prepped, new_data = NULL)

    pred_vars <- prepped_predictors(prepped, baked)

    wflow <- workflows::workflow() |>
      workflows::add_recipe(recipe) |>
      workflows::add_model(parsnip::rand_forest(mtry = hardhat::tune()) |>
                             parsnip::set_engine("ranger") |>
                             parsnip::set_mode("regression"))

    param_set <- workflows::extract_parameter_set_dials(wflow)

    finalized <- dials::finalize(param_set, baked[, pred_vars, drop = FALSE])

    mtry_range <- dials::range_get(finalized$object[[which(finalized$name == "mtry")]])

    expect_identical(as.integer(mtry_range$upper), length(pred_vars))
    expect_lt(as.integer(mtry_range$upper), subtracted_width(baked))

  })

})


## =========================================================================
## A PLS config's component range (#216)
## =========================================================================

describe("evaluate_single_config() - a PLS config", {

  it("hands tune_grid() num_comp up to the predictors and the smallest fold's rows less one, at most 30", {

    ## 20 wavenumbers keep 12 predictors past the raw step's edge trim, and
    ## the 60 training rows make 39-row analysis sets at the smallest, so the
    ## predictors set the cap. The grid search is stopped once it has the set.
    setup      <- make_eval_setup(n = 80, n_wn = 20)
    param_info <- NULL
    local_mocked_bindings(tune_grid = function(..., param_info) {
      param_info <<- param_info
      stop("stopped after the parameter set")
    }, .package = "tune")

    evaluate_single_config(
      config_row    = make_eval_config(model = "plsr"),
      split         = setup$split,
      cv_folds      = setup$folds,
      role_map      = setup$role_map,
      grid_size     = 2,
      bayesian_iter = 0,
      seed          = 42L
    )

    num_comp <- param_info$object[[which(param_info$name == "num_comp")]]
    expect_equal(c(num_comp$range$lower, num_comp$range$upper), c(1, 12))
    expect_gt(min_analysis_rows(setup$folds) - 1, 12)

  })

})
