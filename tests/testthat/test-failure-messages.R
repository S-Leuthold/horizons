## ---------------------------------------------------------------------------
## Tests: failed and warned configs record their cause (#96)
## ---------------------------------------------------------------------------
## recipes wraps every step error in a `recipes_error_step` condition whose
## `$message` is empty, and tune keeps the errors and warnings raised inside
## its resampling in `.notes` rather than signalling them. Capture sites that
## read `$message`, or only watched for signalled conditions, stored
## "Parameter finalization failed: " with nothing after it and recorded no
## warnings at all. These tests pin the cause into the stored text.

## ---------------------------------------------------------------------------
## Fixtures
## ---------------------------------------------------------------------------

## Called from inside a recipe step, so recipes wraps what it signals.
fail_in_step <- function(x) stop("boom from step")

## A recipe whose step errors at prep. Stands in for build_recipe(), so the
## failure reaches the runners the way a real step failure (Boruta, say) does.
failing_recipe <- function(config_row, train_data, role_map, ...) {

  recipes::recipe(SOC ~ ., data = train_data) |>
    recipes::update_role("sample_id", new_role = "id") |>
    recipes::step_mutate(boom = fail_in_step(wn_4000))

}

## A recipe whose step warns at prep and changes nothing: step_BoxCox()
## refuses the non-positive spectra and leaves them as they are.
warning_recipe <- function(config_row, train_data, role_map, ...) {

  recipes::recipe(SOC ~ ., data = train_data) |>
    recipes::update_role("sample_id", new_role = "id") |>
    recipes::step_BoxCox(recipes::all_predictors())

}

## Fails only on more rows than a cross-validation analysis set holds (about
## 20 here), so tuning succeeds and only last_fit()'s final fit on the whole
## training set (about 30) fails.
fail_on_full_training <- function(x) {

  if (length(x) > 25) stop("boom on the full training set")

  x

}

test_set_failing_recipe <- function(config_row, train_data, role_map, ...) {

  recipes::recipe(SOC ~ ., data = train_data) |>
    recipes::update_role("sample_id", new_role = "id") |>
    recipes::step_mutate(boom = fail_on_full_training(wn_4000))

}

## The other way round: fails on a cross-validation analysis set (about 20
## rows) and preps on the whole training set (about 30), so the failure is
## seen only inside tune.
fail_on_a_fold <- function(x) {

  if (length(x) < 25) stop("boom on a fold")

  x

}

fold_failing_recipe <- function(config_row, train_data, role_map, ...) {

  recipes::recipe(SOC ~ ., data = train_data) |>
    recipes::update_role("sample_id", new_role = "id") |>
    recipes::step_mutate(boom = fail_on_a_fold(wn_4000))

}

## Fails on the one cross-validation analysis set that lacks a given value
## (the fold whose assessment set holds it), and nowhere else: the whole
## training set holds the value, and the sets baked (assessment, test) are
## too small to be checked.
fail_without <- function(x, value) {

  if (length(x) > 15 && !any(x == value)) stop("boom on one fold")

  x

}

one_fold_failing_recipe <- function(config_row, train_data, role_map, ...) {

  value <- train_data$wn_4000[1]

  recipes::recipe(SOC ~ ., data = train_data) |>
    recipes::update_role("sample_id", new_role = "id") |>
    recipes::step_mutate(boom = fail_without(wn_4000, !!value))

}

## A tripwire inside a step: a horizons bug, which no unit contains
tripwire_recipe <- function(config_row, train_data, role_map, ...) {

  recipes::recipe(SOC ~ ., data = train_data) |>
    recipes::update_role("sample_id", new_role = "id") |>
    recipes::step_mutate(boom = horizons:::abort_horizons("Rows misaligned.", kind = "internal"))

}

## A tune_results-shaped object with hand-written notes, one tibble per fold.
fake_tune_results <- function(...) {

  folds <- list(...)

  x <- tibble::tibble(
    id     = paste0("Fold", seq_along(folds)),
    .notes = lapply(folds, function(f) {
      tibble::tibble(location = "preprocessor 1/1", type = f$type, note = f$note)
    })
  )

  class(x) <- c("tune_results", class(x))
  x

}

fold_notes <- function(type = character(0), note = character(0)) {

  list(type = type, note = note)

}

make_failure_setup <- function(n = 40, n_wn = 10) {

  set.seed(96)

  wn_names <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
  spec_mat <- matrix(stats::rnorm(n * n_wn), nrow = n)
  colnames(spec_mat) <- wn_names

  df <- tibble::as_tibble(spec_mat)
  df$sample_id <- paste0("S", sprintf("%03d", seq_len(n)))
  df$SOC       <- 2 + rowMeans(spec_mat[, 1:3]) * 0.5 + stats::rnorm(n, sd = 0.5)

  role_map <- tibble::tibble(
    variable = c("sample_id", wn_names, "SOC"),
    role     = c("id", rep("predictor", n_wn), "outcome")
  )

  split <- suppressWarnings(rsample::initial_split(df, prop = 0.75, strata = "SOC"))
  folds <- suppressWarnings(rsample::vfold_cv(rsample::training(split), v = 3,
                                              strata = "SOC"))

  list(split = split, folds = folds, role_map = role_map)

}

make_failure_config <- function(model) {

  tibble::tibble(
    config_id         = paste0("cfg_", model),
    model             = model,
    transformation    = "none",
    preprocessing     = "raw",
    feature_selection = "none"
  )

}

run_evaluate <- function(model, setup, ...) {

  evaluate_single_config(
    config_row    = make_failure_config(model),
    split         = setup$split,
    cv_folds      = setup$folds,
    role_map      = setup$role_map,
    grid_size     = 2,
    bayesian_iter = 0,
    seed          = 42L,
    ...
  )

}

run_fit <- function(model, setup) {

  fit_single_config(
    config_row          = make_failure_config(model),
    split_F             = setup$split,
    cv_resamples        = setup$folds,
    role_map            = setup$role_map,
    best_params_eval    = NULL,
    final_bayesian_iter = 0,
    grid_size           = 2,
    seed                = 42L
  )

}

## ---------------------------------------------------------------------------
## condition_summary()
## ---------------------------------------------------------------------------

describe("condition_summary()", {

  it("returns a base simpleError's message as it is", {

    e <- simpleError("plain base error", call = quote(f(x)))

    expect_identical(condition_summary(e), "plain base error")

  })

  it("names the root cause of an rlang parent chain and where it was wrapped", {

    wrapper <- function() {
      rlang::abort("could not build the thing", parent = simpleError("the real reason"))
    }

    e <- tryCatch(wrapper(), error = function(e) e)

    expect_identical(condition_summary(e), "the real reason (in `wrapper()`)")

  })

  it("passes over a handler frame or an operator to name the wrapper outside it", {

    ## The outermost wrapper's call says nothing useful: a handler frame,
    ## whose call has no plain function name, or an operator
    root <- simpleError("the real reason")
    step <- rlang::error_cnd(message = "step failed", call = quote(step_foo(x)),
                             parent = root)

    for (call in list(quote(value[[3L]](cond)), quote(x[[i]]))) {

      outer <- rlang::error_cnd(message = "rethrown", call = call, parent = step)

      expect_identical(condition_summary(outer), "the real reason (in `step_foo()`)",
                       info = deparse(call))

    }

  })

  it("keeps an rlang error without a parent whole", {

    e <- tryCatch(rlang::abort("no parent here"), error = function(e) e)

    expect_identical(condition_summary(e), "no parent here")

  })

  it("recovers a recipe step error whose $message is empty", {

    e <- tryCatch(
      recipes::prep(recipes::recipe(mpg ~ ., mtcars) |>
                      recipes::step_mutate(z = fail_in_step(cyl))),
      error = function(e) e
    )

    ## The reproduction from #96: the old capture read this field.
    expect_s3_class(e, "recipes_error_step")
    expect_identical(e$message, "")

    expect_identical(condition_summary(e), "boom from step (in `step_mutate()`)")

  })

  it("keeps braces from a cli abort literal and joins its bullets on one line", {

    e <- tryCatch(
      cli::cli_abort(c("Column {.val {'{wn_600}'}} is missing",
                       "i" = "Check {.arg x}.")),
      error = function(e) e
    )

    out <- condition_summary(e)

    expect_identical(out, "Column \"{wn_600}\" is missing; Check `x`.")

    ## Plain text: survives being interpolated into cli as a value.
    expect_no_error(cli::format_inline("{out}"))

  })

  it("strips ANSI styling and bullet glyphs", {

    e <- withr::with_options(list(cli.num_colors = 256), tryCatch(
      cli::cli_abort(c("Bad {.arg x}", "x" = "value {.val {3}} too big")),
      error = function(e) e
    ))

    expect_identical(condition_summary(e), "Bad `x`; value 3 too big")

  })

  it("falls back to the class when no condition in the chain has text", {

    e <- rlang::error_cnd("horizons_silent_error", message = "")

    expect_identical(condition_summary(e), "horizons_silent_error (no message)")

  })

  it("never returns an empty string", {

    blank <- structure(class = c("weird_error", "error", "condition"),
                       list(message = NULL, call = NULL))

    expect_true(nzchar(condition_summary(blank)))
    expect_true(nzchar(condition_summary("")))

  })

  it("falls back to the class when neither the message method nor the raw field can be read", {

    ## The last fallback in condition_own_text(): a conditionMessage() method
    ## that errors, and a field that cannot be read either.
    unreadable <- structure(class = c("unreadable_error", "error", "condition"),
                            list(message = "hidden", call = NULL))

    local_mocked_s3_method("conditionMessage", "unreadable_error",
                           function(c) stop("no message method"))
    local_mocked_s3_method("[[", "unreadable_error",
                           function(x, i, ...) stop("no field access"))

    expect_identical(condition_summary(unreadable), "unreadable_error (no message)")

  })

  it("keeps a message whose bytes are not valid in the session encoding", {

    ## Invalid UTF-8 in a UTF-8 session, which made strsplit() and nchar()
    ## fail; plain latin1 elsewhere. Either way the text comes back.
    e <- simpleError("caf\xe9 failed")

    expect_match(condition_summary(e), "^caf.* failed$")

  })

  it("reads a message tune already rendered into a note", {

    note <- paste(
      "Error in `step_mutate()`:",
      "Caused by error in `dplyr::mutate()`:",
      "\u2139 In argument: `z = s(cyl)`.",
      "Caused by error in `s()`:",
      "! boom from step",
      sep = "\n"
    )

    expect_identical(condition_summary(note), "boom from step (in `step_mutate()`)")

    ## A warning note rendered under ASCII bullets (testthat, a C locale)
    ascii_note <- "There was 1 warning in `f()`.\ni In argument: `z`.\nCaused by warning:\n! kept all columns\ni 3 were rejected"

    expect_identical(condition_summary(ascii_note), "kept all columns; 3 were rejected")

  })

  it("caps a long cause but keeps its front and the location", {

    wrapper <- function() {
      rlang::abort("wrapped", parent = simpleError(paste(rep("word", 200), collapse = " ")))
    }

    out <- condition_summary(tryCatch(wrapper(), error = function(e) e),
                             max_chars = 50L)

    expect_match(out, "^word word")
    expect_match(out, "... (in `wrapper()`)", fixed = TRUE)

  })

})

## ---------------------------------------------------------------------------
## Capture sites
## ---------------------------------------------------------------------------

describe("create_failed_result() and handle_results()", {

  step_error <- tryCatch(
    recipes::prep(recipes::recipe(mpg ~ ., mtcars) |>
                    recipes::step_mutate(z = fail_in_step(cyl))),
    error = function(e) e
  )

  it("stores the cause of a condition, not its empty $message", {

    row <- create_failed_result("cfg_x", step_error)

    expect_match(row$error_message, "boom from step", fixed = TRUE)

  })

  it("puts the cause in handle_results()'s abort", {

    safe <- list(result = NULL, error = step_error)

    expect_error(handle_results(safe, error_title = "Prep failed"),
                 "boom from step", fixed = TRUE)

  })

})

describe("evaluate_single_config() records the cause of a recipe step error", {

  setup <- make_failure_setup()

  it("names the cause and its class when the step fails as the recipe is prepped to bound mtry", {

    local_mocked_bindings(build_recipe = failing_recipe, .package = "horizons")

    result <- run_evaluate("rf", setup)

    expect_identical(result$status, "failed")
    expect_identical(result$error_message,
                     "Recipe preparation failed: boom from step (in `step_mutate()`)")
    expect_identical(result$class, "simpleError")

  })

  it("reproduces a step's failure inside tune by prepping the recipe, so it fails with its class", {

    ## elastic_net has no mtry, so its recipe is prepped only inside tune;
    ## tune's notes show the step failed, and the prep outside tune raises
    ## the step's own error
    local_mocked_bindings(build_recipe = failing_recipe, .package = "horizons")

    result <- run_evaluate("elastic_net", setup)

    expect_identical(result$status, "failed")
    expect_identical(result$error_message,
                     "Recipe preparation failed: boom from step (in `step_mutate()`)")
    expect_identical(result$class, "simpleError")

  })

  it("names the cause from tune's notes when every model failed during CV on a failure the prep does not reproduce (#96)", {

    local_mocked_bindings(build_recipe = fold_failing_recipe, .package = "horizons")

    result <- run_evaluate("elastic_net", setup)

    expect_identical(result$status, "failed")
    expect_match(result$error_message,
                 "^Grid search failed: all models failed during CV \u2014 ")
    expect_match(result$error_message,
                 "boom on a fold (in `step_mutate()`; 3 of 3 folds)", fixed = TRUE)

    ## tune keeps only the text, so the failure is a run failure; and its
    ## own "All models failed" warning is not repeated as a warning
    expect_identical(result$class, "horizons_error_run")
    expect_false(any(grepl("All models failed", result$warnings[[1]], fixed = TRUE)))

  })

  it("lets a tripwire in a step through, whether the recipe is prepped before tuning or inside it", {

    local_mocked_bindings(build_recipe = tripwire_recipe, .package = "horizons")

    for (model in c("rf", "elastic_net")) {

      err <- expect_error(run_evaluate(model, setup), "Rows misaligned.", fixed = TRUE)
      expect_true(rlang::cnd_inherits(err, "horizons_error_internal"), label = model)

    }

  })

  it("keeps the failure's backtrace on the row only under keep_traces", {

    local_mocked_bindings(build_recipe = failing_recipe, .package = "horizons")

    kept <- run_evaluate("rf", setup, keep_traces = TRUE)
    expect_type(kept$trace[[1]], "character")
    expect_gt(length(kept$trace[[1]]), 0L)

    expect_null(run_evaluate("rf", setup)$trace[[1]])

  })

  it("stops at the first failure inside tune under debug, carrying the trace tune kept", {

    ## An engine's error, for which tune keeps a backtrace, on the first fit
    ## only: one candidate on one fold, which outside debug the
    ## configuration survives
    real_glmnet <- glmnet::glmnet
    calls       <- 0L
    local_mocked_bindings(glmnet = function(...) {
      calls <<- calls + 1L
      if (calls == 1L) stop("the engine broke")
      real_glmnet(...)
    }, .package = "glmnet")

    contained <- suppressWarnings(run_evaluate("elastic_net", setup))
    expect_identical(contained$status, "success")

    calls <- 0L
    err <- expect_error(run_evaluate("elastic_net", setup, debug = TRUE),
                        "a fit failed inside tune", class = "horizons_error_run")
    expect_match(conditionMessage(err), "the engine broke", fixed = TRUE)
    expect_true(any(grepl("glmnet", format(err$trace), fixed = TRUE)))

  })

  it("stops at a step's failure under debug, with the step's own error", {

    local_mocked_bindings(build_recipe = failing_recipe, .package = "horizons")

    expect_error(run_evaluate("rf", setup, debug = TRUE), "boom from step", fixed = TRUE,
                 class = "recipes_error_step")

  })

})

describe("evaluate_single_config() records the warnings tune kept in .notes", {

  setup <- make_failure_setup()

  it("carries a recipe step's prep warning into the results row", {

    local_mocked_bindings(build_recipe = warning_recipe, .package = "horizons")

    result <- run_evaluate("elastic_net", setup)
    warns  <- result$warnings[[1]]

    expect_identical(result$status, "success")
    expect_true(any(grepl("Non-positive values in selected variable", warns, fixed = TRUE)))

  })

  it("records each message once across the captured warnings, CV and the test set", {

    local_mocked_bindings(build_recipe = warning_recipe, .package = "horizons")

    ## rf preps the recipe outside tune to finalize mtry, so the same warning
    ## arrives captured in full, as a note from every fold, and as a note
    ## from the final fit on the test set.
    warns <- run_evaluate("rf", setup)$warnings[[1]]

    non_positive <- grep("Non-positive values in selected variable", warns, fixed = TRUE)
    no_box_cox   <- grep("No Box-Cox transformation could be estimated", warns, fixed = TRUE)

    expect_length(non_positive, 1L)
    expect_length(no_box_cox, 1L)
    expect_match(warns[non_positive], "(3 of 3 folds; test set)", fixed = TRUE)

    ## One line, cleaned: no cli wrapping or bullets survive.
    expect_false(any(grepl("\n", warns)))

  })

})

describe("evaluate_single_config() fails a configuration whose final fit fails (#102)", {

  setup <- make_failure_setup()

  it("fails it with the step's class when the recipe fails on the whole training set", {

    ## Tuning succeeds on the smaller analysis sets; last_fit()'s prep on the
    ## training part fails, and the prep outside tune reproduces it
    local_mocked_bindings(build_recipe = test_set_failing_recipe, .package = "horizons")

    result <- run_evaluate("elastic_net", setup)

    expect_identical(result$status, "failed")
    expect_identical(result$error_message,
                     "Recipe preparation failed: boom on the full training set (in `step_mutate()`)")
    expect_identical(result$class, "simpleError")
    expect_true(all(is.na(unlist(result[c("rmse", "rpd", "cv_rmse", "cv_rpd")]))))

  })

  it("fails it with tune's cause when the engine fails on the training part", {

    real_glmnet <- glmnet::glmnet
    local_mocked_bindings(glmnet = function(x, ...) {
      if (nrow(x) > 25) stop("the engine broke on the training part")
      real_glmnet(x, ...)
    }, .package = "glmnet")

    result <- run_evaluate("elastic_net", setup)

    expect_identical(result$status, "failed")
    expect_match(result$error_message,
                 "^Test evaluation failed: the final fit failed \u2014 the engine broke on the training part")
    expect_identical(result$class, "horizons_error_run")

  })

})

describe("evaluate_single_config() chooses only among candidates scored on every fold (#101)", {

  setup <- make_failure_setup()

  it("fails a configuration whose every candidate failed on a fold, rather than ranking it on the rest", {

    local_mocked_bindings(build_recipe = one_fold_failing_recipe, .package = "horizons")

    result <- run_evaluate("elastic_net", setup)

    expect_identical(result$status, "failed")
    expect_match(result$error_message,
                 "^Grid search failed: no grid candidate was scored on every fold \u2014 ")
    expect_match(result$error_message, "boom on one fold (in `step_mutate()`; 1 of 3 folds)",
                 fixed = TRUE)
    expect_identical(result$class, "horizons_error_run")
    expect_true(is.na(result$cv_rmse))

  })

  it("leaves out a candidate that failed on a fold and chooses among the rest, saying so on the row", {

    ## The first engine fit fails: one candidate, on one fold
    real_glmnet <- glmnet::glmnet
    calls       <- 0L
    local_mocked_bindings(glmnet = function(...) {
      calls <<- calls + 1L
      if (calls == 1L) stop("the engine broke")
      real_glmnet(...)
    }, .package = "glmnet")

    result <- suppressWarnings(run_evaluate("elastic_net", setup))

    expect_identical(result$status, "success")
    expect_true(is.finite(result$cv_rmse))
    expect_true(any(grepl(
      "1 of 2 candidates was left out of the choice of hyperparameters, not being scored on every fold: the engine broke (1 of 3 folds).",
      result$warnings[[1]], fixed = TRUE
    )))

  })

})

describe("fit_single_config() records the cause of a recipe step error", {

  setup <- make_failure_setup()

  it("names the cause when the step fails at mtry finalization", {

    local_mocked_bindings(build_recipe = failing_recipe, .package = "horizons")

    result <- run_fit("rf", setup)

    expect_identical(result$status, "failed")
    expect_identical(result$error_message,
                     "Parameter finalization failed: boom from step (in `step_mutate()`)")

  })

  it("names the cause from tune's notes when every warm-start model failed", {

    local_mocked_bindings(build_recipe = failing_recipe, .package = "horizons")

    result <- run_fit("elastic_net", setup)

    expect_identical(result$status, "failed")
    expect_match(result$error_message,
                 "^Warm-start tuning failed: all models failed during CV \u2014 ")
    expect_match(result$error_message,
                 "boom from step (in `step_mutate()`; 3 of 3 folds)", fixed = TRUE)

    ## The re-tune started before it failed, so how it started is recorded.
    expect_false(result$warm_start)
    expect_false(is.na(result$start_grid_size))

  })

})

describe("fit_single_config() records the warnings tune kept in .notes", {

  setup <- make_failure_setup()

  it("carries a recipe step's prep warning from the re-tune into the member's warnings, recording each message once across the captured warnings, re-tune and OOF folds", {

    local_mocked_bindings(build_recipe = warning_recipe, .package = "horizons")

    ## Step 9's fold metrics run outside the capture, so yardstick's
    ## constant-estimate warning reaches the caller; it is not under test.
    result <- suppressWarnings(run_fit("rf", setup))
    warns  <- result$warnings

    expect_identical(result$status, "success")
    expect_true(any(grepl("Non-positive values in selected variable.*of 3 folds",
                          result$warnings)))

    non_positive <- grep("Non-positive values in selected variable", warns, fixed = TRUE)

    expect_length(non_positive, 1L)
    expect_match(warns[non_positive], "(3 of 3 folds)", fixed = TRUE)
    expect_false(any(grepl("\n", warns)))

  })

})

## ---------------------------------------------------------------------------
## The note readers, on hand-written notes
## ---------------------------------------------------------------------------

describe("tune_note_messages() and tune_failure_cause()", {

  results <- fake_tune_results(
    fold_notes(c("warning", "warning", "warning", "error"),
               c("w common", "w common", "rare", "e main")),
    fold_notes(c("warning", "warning", "error"),
               c("w common", "40 samples were requested but there were 20 rows", "e main")),
    fold_notes(c("warning", "warning", "error"),
               c("w common", "40 samples were requested but there were 21 rows", "e other"))
  )

  it("gives one line per distinct message, most frequent first, with its folds", {

    expect_identical(
      tune_note_messages(results, type = "warning"),
      c("w common (3 of 3 folds)",
        "40 samples were requested but there were 20 rows (2 of 3 folds; numbers vary)",
        "rare (1 of 3 folds)")
    )

  })

  it("names the most frequent error and counts the others", {

    expect_identical(tune_failure_cause(results),
                     "e main (2 of 3 folds), plus 1 other distinct error")

  })

  it("reads a rendered error chain in a note", {

    chained <- fake_tune_results(
      fold_notes("error", "Error in `step_mutate()`:\nCaused by error in `s()`:\n! boom"),
      fold_notes()
    )

    expect_identical(tune_failure_cause(chained), "boom (in `step_mutate()`; 1 of 2 folds)")

  })

  it("is empty when there are no notes of the type", {

    quiet <- fake_tune_results(fold_notes("warning", "only a warning"), fold_notes())

    expect_identical(tune_note_messages(quiet, type = "error"), character(0))
    expect_null(tune_failure_cause(quiet))

  })

  it("says so, rather than reading as clean, when the notes cannot be read", {

    lines <- tune_note_messages(list(), type = "warning")

    expect_length(lines, 1L)
    expect_match(lines, "^tune's notes could not be read: ")

    ## Notes of another shape are as unreadable
    local_mocked_bindings(collect_notes = function(...) tibble::tibble(x = 1), .package = "tune")
    expect_identical(tune_note_messages(list(), type = "warning"),
                     "tune's notes could not be read: they have no type and note columns")

  })

})

describe("the notes of the chosen candidate (#109)", {

  it("keeps the recipe's notes, the grid notes of the candidate's model stages, and its iteration's", {

    notes <- tibble::tibble(
      id       = c("Fold1", "Fold1", "Fold2", "Fold1", "Fold2", "Fold1", "Fold3"),
      .iter    = c(0L, 0L, 0L, 2L, 1L, 2L, 2L),
      location = c("preprocessor 1/1", "preprocessor 1/1, model 1/3", "preprocessor 1/1, model 2/3",
                   "preprocessor 1/1, model 1/1", "preprocessor 1/1, model 1/1",
                   "internal", "internal"),
      type     = "warning",
      note     = paste("note", 1:7)
    )

    ## A grid candidate in model stage 2, with a metric missing on Fold1
    grid_candidate <- list(iter = 0L, model_stages = 2L, na_folds = "Fold1")
    expect_identical(notes_of(notes, grid_candidate),
                     c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))

    ## The candidate of the second Bayesian iteration, missing a metric on Fold3
    bayes_candidate <- list(iter = 2L, model_stages = NULL, na_folds = "Fold3")
    expect_identical(notes_of(notes, bayes_candidate),
                     c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE))

  })

  it("maps a grid note to its candidate through the grid's own order, which tune numbers its model stages by", {

    ## The canary for tune's numbering: the grid's input order differs from
    ## .config's (mtry = 50, min_n = 5 is model 1/4 and pre0_mod4_post0), and
    ## only the mtry = 50 candidates warn, ranger capping mtry at the six
    ## predictors. If tune numbered stages in .config order, the warning
    ## would land on mtry = 2.
    set.seed(1)
    d <- as.data.frame(matrix(stats::rnorm(60 * 6), 60, 6,
                              dimnames = list(NULL, paste0("x", 1:6))))
    d$y   <- d$x1 + stats::rnorm(60, sd = 0.5)
    folds <- rsample::vfold_cv(d, v = 3)

    wf <- workflows::workflow() |>
      workflows::add_recipe(recipes::recipe(y ~ ., data = d)) |>
      workflows::add_model(parsnip::rand_forest(mtry = tune::tune(), min_n = tune::tune(), trees = 20) |>
                             parsnip::set_engine("ranger") |>
                             parsnip::set_mode("regression"))

    grid <- tibble::tibble(mtry = c(50L, 2L, 50L, 3L), min_n = c(5L, 5L, 2L, 2L))
    res  <- suppressWarnings(suppressMessages(
      tune::tune_grid(wf, folds, grid = grid, control = tune::control_grid(verbose = FALSE))
    ))

    cm     <- tune::collect_metrics(res)
    params <- function(mtry, min_n) {
      unique(cm[cm$mtry == mtry & cm$min_n == min_n, c("mtry", "min_n", ".config")])
    }
    warned <- function(mtry, min_n) {
      cand  <- candidate_of(res, params(mtry, min_n), grid, wf)
      lines <- render_warning_log(tune_note_records(res, candidate = cand))
      any(grepl("columns were requested", lines, fixed = TRUE))
    }

    expect_identical(params(50L, 5L)$.config, "pre0_mod4_post0")
    expect_true(warned(50L, 5L))
    expect_true(warned(50L, 2L))
    expect_false(warned(2L, 5L))
    expect_false(warned(3L, 2L))

    ## A schedule of another shape keeps every grid note, with the reason,
    ## rather than failing: the schedule is tune's developer interface
    local_mocked_bindings(schedule_grid = function(...) tibble::tibble(stage = 1L), .package = "tune")
    cand <- candidate_of(res, params(2L, 5L), grid, wf)
    expect_null(cand$model_stages)
    expect_identical(cand$unmapped, "tune's grid schedule has no model_stage column")

    local_mocked_bindings(schedule_grid = function(...) tibble::tibble(model_stage = list(tibble::tibble(mtry = 2L))),
                          .package = "tune")
    expect_identical(candidate_of(res, params(2L, 5L), grid, wf)$unmapped,
                     "tune's grid schedule has no predict_stage column")

    stage <- tibble::tibble(mtry = 2L, trees = 20L,
                            predict_stage = list(tibble::tibble(post_stage = list(tibble::tibble()))))
    local_mocked_bindings(schedule_grid = function(...) tibble::tibble(model_stage = list(stage)),
                          .package = "tune")
    expect_identical(candidate_of(res, params(2L, 5L), grid, wf)$unmapped,
                     "tune's grid schedule has parameters the candidate lacks: trees")

    ## A tuned recipe makes several preprocessor stages: every model note kept
    local_mocked_bindings(schedule_grid = function(...) tibble::tibble(model_stage = list(stage, stage)),
                          .package = "tune")
    cand <- candidate_of(res, params(2L, 5L), grid, wf)
    expect_null(cand$model_stages)
    expect_null(cand$unmapped)

  })

})

describe("render_warning_log()", {

  it("merges a captured warning with the same message from notes and the test set", {

    cv <- fake_tune_results(fold_notes("warning", "careful: 3 columns"),
                            fold_notes("warning", "careful: 3 columns"))

    last_fit_notes <- fake_tune_results(fold_notes("warning", "careful: 3 columns"))
    last_fit_notes$id <- NULL

    log <- dplyr::bind_rows(
      text_records(list("! careful:\n3 columns", "! careful:\n3 columns")),
      tune_note_records(cv, "warning", scope = "cv"),
      tune_note_records(last_fit_notes, "warning", scope = "test set")
    )

    expect_identical(render_warning_log(log), "careful: 3 columns (2 of 2 folds; test set)")

  })

  it("shows the notices, the five most frequent messages, and counts the rest", {

    log <- dplyr::bind_rows(
      text_records(c(rep("often", 5), rep("second", 4), rep("third", 3),
                     "a", "b", "c", "d")),
      text_records("a package notice", pinned = TRUE)
    )

    expect_identical(
      render_warning_log(log),
      c("a package notice", "often", "second", "third", "a", "b",
        "2 more distinct warnings not shown.")
    )

  })

  it("is NULL when there is nothing to report", {

    expect_null(render_warning_log(new_warning_log()))

  })

  it("merges a warning a unit caught with the same note, pins the package's own, and leaves out tune's all-failed warning", {

    cv <- fake_tune_results(fold_notes("warning", "careful: 3 columns"),
                            fold_notes("warning", "careful: 3 columns"))

    caught <- run_unit({
      warning("careful: 3 columns")
      warning("All models failed. Run `show_notes(.Last.tune.result)` for more information.")
      warn_in_unit("substitution", "Kept the grid's choice.")
    }, "configuration", "cfg_1")$records

    log <- dplyr::bind_rows(unit_warning_records(caught),
                            tune_note_records(cv, "warning", scope = "cv"))

    expect_identical(render_warning_log(log),
                     c("Kept the grid's choice.", "careful: 3 columns (2 of 2 folds)"))

  })

})
