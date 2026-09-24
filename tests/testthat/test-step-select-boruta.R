## ---------------------------------------------------------------------------
## Tests: step_select_boruta()
## ---------------------------------------------------------------------------
##
## The step fixed ranger's mtry once, from all the cluster representatives,
## and handed it to every Boruta run. Boruta narrows the matrix each run, so
## once fewer than mtry / 2 attributes were left ranger aborted, and a
## tryCatch() swallowed the abort and selected the 50 columns most correlated
## with the outcome instead. A config labelled "boruta" then ran a correlation
## filter with nothing saying so (#75). These tests pin what the step does
## now: Boruta runs to completion and its decisions are the selection, a
## failed run fails the step, and a run that confirms nothing says so.

## Helper: noise columns with the outcome carried by three of them. Fifty
## columns is the most the step's clustering hands Boruta, so mtry sits at its
## ceiling of 14.
boruta_signal_data <- function(n = 60, p = 50, seed = 1) {

  set.seed(seed)

  m <- matrix(stats::rnorm(n * p), nrow = n)
  colnames(m) <- sprintf("spec%03d", seq_len(p))

  d     <- tibble::as_tibble(m)
  d$SOC <- 3 * d$spec005 + 3 * d$spec020 + 3 * d$spec035 + stats::rnorm(n, sd = 0.5)

  d

}

boruta_signal_cols <- c("spec005", "spec020", "spec035")

## Helper: the step alone on the spectral columns
boruta_recipe <- function(d) {

  recipes::recipe(SOC ~ ., data = d) |>
    step_select_boruta(dplyr::matches("^spec[0-9]+$"), outcome = "SOC")

}

## Helper: a stand-in for Boruta::Boruta() that returns the decisions
## `decide()` makes for the attribute names, without running a forest
mock_boruta_deciding <- function(decide) {

  function(x, y, ...) {

    decision <- factor(decide(colnames(x)),
                       levels = c("Tentative", "Confirmed", "Rejected"))
    names(decision) <- colnames(x)

    structure(list(finalDecision = decision), class = "Boruta")

  }

}

## =========================================================================
## Boruta's decisions are the selection
## =========================================================================

describe("step_select_boruta() selects by Boruta (#75)", {

  it("runs Boruta to completion, past the point where the attributes left fall below mtry / 2", {

    skip_if_not_installed("Boruta")

    d <- boruta_signal_data()

    set.seed(1)
    expect_no_warning(prepped <- recipes::prep(boruta_recipe(d), training = d))

    step <- prepped$steps[[1]]
    b    <- step$boruta

    ## Every run went to ranger, and the last one ended the loop: nothing
    ## left tentative, or the run cap reached.
    expect_identical(b$runs, length(b$mtry_by_run))
    expect_gt(b$runs, 0)
    expect_true(b$n_tentative == 0 || b$runs == b$max_runs - 1)

    ## The run the fixed mtry used to abort: a run handed fewer columns than
    ## mtry, which the importance source clamped rather than passed to ranger.
    expect_identical(b$mtry, 14L)
    expect_lt(min(b$mtry_by_run), b$mtry)
    expect_true(all(b$mtry_by_run >= 1L & b$mtry_by_run <= b$mtry))

    ## Every attribute has a decision, and the counts add up to them.
    expect_identical(b$n_attributes, 50L)
    expect_identical(b$n_confirmed + b$n_tentative + b$n_rejected, b$n_attributes)
    expect_false(anyNA(b$decision))

  })

  it("selects the signal columns Boruta confirmed, and not the noise", {

    skip_if_not_installed("Boruta")

    d <- boruta_signal_data()

    set.seed(1)
    prepped <- recipes::prep(boruta_recipe(d), training = d)

    step <- prepped$steps[[1]]
    b    <- step$boruta

    expect_setequal(names(b$decision)[b$decision == "Confirmed"], boruta_signal_cols)
    expect_false(b$retained_all)

    ## Fifty columns cluster into fifty singletons, so the selection is
    ## exactly the attributes Boruta did not reject. The correlation fallback
    ## kept all fifty.
    expect_setequal(step$selected_vars, names(b$decision)[b$decision != "Rejected"])
    expect_true(all(boruta_signal_cols %in% step$selected_vars))
    expect_lt(length(step$selected_vars), 50)

    baked <- recipes::bake(prepped, new_data = d)

    expect_setequal(setdiff(names(baked), "SOC"), step$selected_vars)

  })

  it("prints what Boruta decided", {

    skip_if_not_installed("Boruta")

    d <- boruta_signal_data()

    set.seed(1)
    prepped <- recipes::prep(boruta_recipe(d), training = d)

    expect_output(print(prepped$steps[[1]]), "3 confirmed")
    expect_output(print(prepped$steps[[1]]), "of 50 cluster representatives")

  })

  it("bakes and prints a step trained before the record existed", {

    skip_if_not_installed("Boruta")

    d <- boruta_signal_data()

    set.seed(1)
    prepped <- recipes::prep(boruta_recipe(d), training = d)
    baked   <- recipes::bake(prepped, new_data = d)

    old <- prepped
    old$steps[[1]]$boruta <- NULL

    expect_false("boruta" %in% names(old$steps[[1]]))
    expect_identical(recipes::bake(old, new_data = d), baked)
    expect_output(print(old$steps[[1]]), "wavenumbers retained after Boruta")

  })

})

## =========================================================================
## A failed Boruta run fails the step
## =========================================================================

describe("a failed Boruta run fails step_select_boruta() (#75)", {

  it("aborts with horizons_boruta_error rather than select by correlation", {

    skip_if_not_installed("Boruta")

    ## ranger refuses a missing outcome, so the first Boruta run fails.
    d        <- boruta_signal_data()
    d$SOC[3] <- NA

    rec <- boruta_recipe(d)

    expect_error(recipes::prep(rec, training = d), class = "horizons_boruta_error")
    expect_error(recipes::prep(rec, training = d), "Boruta feature selection failed")
    expect_error(recipes::prep(rec, training = d), "Missing data in dependent variable")

  })

  it("carries the upstream message as text, braces included", {

    skip_if_not_installed("Boruta")

    local_mocked_bindings(Boruta = function(...) stop("ranger said {mtry}"),
                          .package = "Boruta")

    d <- boruta_signal_data()

    expect_error(recipes::prep(boruta_recipe(d), training = d),
                 class = "horizons_boruta_error")
    expect_error(recipes::prep(boruta_recipe(d), training = d),
                 "ranger said {mtry}", fixed = TRUE)

  })

})

## =========================================================================
## Boruta confirming nothing is a result, and the step says so
## =========================================================================

describe("step_select_boruta() when Boruta confirms nothing (#75)", {

  it("keeps every column and warns when Boruta rejects them all", {

    skip_if_not_installed("Boruta")

    local_mocked_bindings(
      Boruta = mock_boruta_deciding(function(nm) rep("Rejected", length(nm))),
      .package = "Boruta"
    )

    d <- boruta_signal_data()

    expect_warning(prepped <- recipes::prep(boruta_recipe(d), training = d),
                   class = "horizons_boruta_warning")

    step <- prepped$steps[[1]]
    b    <- step$boruta
    spec <- grep("^spec", names(d), value = TRUE)

    expect_true(b$retained_all)
    expect_identical(b$n_rejected, 50L)
    expect_identical(b$n_confirmed, 0L)
    expect_setequal(step$selected_vars, spec)
    expect_setequal(setdiff(names(recipes::bake(prepped, new_data = d)), "SOC"), spec)
    expect_output(print(step), "Nothing selected")

  })

  it("keeps the tentative attributes and warns when none is confirmed", {

    skip_if_not_installed("Boruta")

    tentative <- c("spec005", "spec020")

    local_mocked_bindings(
      Boruta = mock_boruta_deciding(function(nm) ifelse(nm %in% tentative, "Tentative", "Rejected")),
      .package = "Boruta"
    )

    d <- boruta_signal_data()

    expect_warning(prepped <- recipes::prep(boruta_recipe(d), training = d),
                   class = "horizons_boruta_warning")

    step <- prepped$steps[[1]]
    b    <- step$boruta

    expect_false(b$retained_all)
    expect_identical(b$n_confirmed, 0L)
    expect_identical(b$n_tentative, 2L)
    expect_setequal(step$selected_vars, tentative)

  })

})
