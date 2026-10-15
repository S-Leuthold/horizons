## ---------------------------------------------------------------------------
## Tests: the custom recipe steps on parallel workers
## ---------------------------------------------------------------------------
##
## tune asks a workflow's steps which packages each worker must load before it
## preps and bakes the recipe there (required_pkgs()). Without the methods, a
## worker that had not loaded horizons failed every fold with "no applicable
## method for 'prep'" (#238). Slow tier: each workflow starts two workers.

## Helper: n spectra of p bands named wn_<wavenumber>, and an outcome carried
## by one band. Drawn under the caller's seed.
workers_data <- function(n = 40, p = 60) {

  m <- matrix(stats::rnorm(n * p), nrow = n)
  colnames(m) <- paste0("wn_", seq(4000, by = -2, length.out = p))

  d     <- tibble::as_tibble(m)
  d$SOC <- 2 * d$wn_3980 + stats::rnorm(n, sd = 0.5)

  d

}

describe("the custom steps on parallel workers", {

  it("names the packages a worker needs, horizons among them", {

    withr::local_seed(1)
    d <- workers_data()

    rec <- recipes::recipe(SOC ~ ., data = d) |>
      step_transform_spectra(dplyr::starts_with("wn_"), preprocessing = "raw") |>
      step_select_correlation(dplyr::matches("^spec[0-9]+$"), outcome = "SOC") |>
      step_select_cars(dplyr::matches("^spec[0-9]+$"), outcome = "SOC") |>
      step_select_boruta(dplyr::matches("^spec[0-9]+$"), outcome = "SOC")

    expect_true(all(c("horizons", "prospectr", "zoo", "pls", "Boruta", "ranger") %in%
                      recipes::required_pkgs(rec)))

  })

  it("resamples a workflow with each step on two multisession workers (#238)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_not_installed("Boruta")

    withr::local_seed(1)
    d     <- workers_data()
    folds <- rsample::vfold_cv(d, v = 2)

    transform <- function() {
      recipes::recipe(SOC ~ ., data = d) |>
        step_transform_spectra(dplyr::starts_with("wn_"), preprocessing = "raw")
    }

    recipes_by_step <- list(
      transform   = transform(),
      correlation = transform() |>
        step_select_correlation(dplyr::matches("^spec[0-9]+$"), outcome = "SOC"),
      cars        = transform() |>
        step_select_cars(dplyr::matches("^spec[0-9]+$"), outcome = "SOC"),
      boruta      = transform() |>
        step_select_boruta(dplyr::matches("^spec[0-9]+$"), outcome = "SOC")
    )

    local_plan(future::multisession, workers = 2)

    for (step in names(recipes_by_step)) {

      wf <- workflows::workflow() |>
        workflows::add_recipe(recipes_by_step[[step]]) |>
        workflows::add_model(parsnip::linear_reg())

      ## The steps' own messages and the small data's warnings are not the
      ## subject here; the notes say whether any fold failed.
      res <- suppressWarnings(suppressMessages(
        tune::fit_resamples(wf, folds, control = tune::control_resamples(allow_par = TRUE))
      ))

      notes <- tune::collect_notes(res)

      expect_identical(notes$note[notes$type == "error"], character(0), label = step)

    }

  })

})
