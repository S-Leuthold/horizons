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
    feature_selection = "none",
    covariates        = NA_character_
  )

}

run_evaluate <- function(model, setup) {

  evaluate_single_config(
    config_row    = make_failure_config(model),
    split         = setup$split,
    cv_folds      = setup$folds,
    role_map      = setup$role_map,
    grid_size     = 2,
    bayesian_iter = 0,
    seed          = 42L
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

  it("names the cause when the step fails at mtry finalization", {

    local_mocked_bindings(build_recipe = failing_recipe, .package = "horizons")

    result <- run_evaluate("rf", setup)

    expect_identical(result$status, "failed")
    expect_identical(result$error_message,
                     "Parameter finalization failed: boom from step (in `step_mutate()`)")

  })

  it("names the cause from tune's notes when every model failed during CV", {

    local_mocked_bindings(build_recipe = failing_recipe, .package = "horizons")

    result <- run_evaluate("elastic_net", setup)

    expect_identical(result$status, "failed")
    expect_match(result$error_message,
                 "^Grid search failed: all models failed during CV \u2014 ")
    expect_match(result$error_message,
                 "boom from step (in `step_mutate()`; 3 of 3 folds)", fixed = TRUE)

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

  it("records each distinct note once, with how many folds raised it", {

    local_mocked_bindings(build_recipe = warning_recipe, .package = "horizons")

    warns <- run_evaluate("elastic_net", setup)$warnings[[1]]
    noted <- grep("Non-positive values in selected variable", warns,
                  fixed = TRUE, value = TRUE)

    expect_identical(anyDuplicated(warns), 0L)
    expect_true(any(grepl("(3 of 3 folds)", noted, fixed = TRUE)))

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

  })

})

describe("fit_single_config() records the warnings tune kept in .notes", {

  setup <- make_failure_setup()

  it("carries a recipe step's prep warning from the re-tune into the member's warnings", {

    local_mocked_bindings(build_recipe = warning_recipe, .package = "horizons")

    result <- run_fit("elastic_net", setup)

    expect_identical(result$status, "success")
    expect_true(any(grepl("Non-positive values in selected variable.*of 3 folds",
                          result$warnings)))

  })

})
