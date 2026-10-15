## ---------------------------------------------------------------------------
## Tests: the custom recipe steps on parallel workers
## ---------------------------------------------------------------------------
##
## tune asks a workflow's steps which packages each worker must load before it
## preps and bakes the recipe there (required_pkgs()). Without the methods, a
## worker that had not loaded horizons failed every fold with "no applicable
## method for 'prep'" (#238). Slow tier: each step starts two fresh workers.

## Helper: n spectra of p bands named wn_<wavenumber>, and an outcome carried
## by one band. Drawn under the caller's seed.
workers_data <- function(n = 40, p = 60) {

  m <- matrix(stats::rnorm(n * p), nrow = n)
  colnames(m) <- paste0("wn_", seq(4000, by = -2, length.out = p))

  d     <- tibble::as_tibble(m)
  d$SOC <- 2 * d$wn_3980 + stats::rnorm(n, sd = 0.5)

  d

}

## Each step alone on the raw bands, as the code that builds it
step_calls <- c(
  step_transform_spectra  = 'horizons::step_transform_spectra(dplyr::starts_with("wn_"), preprocessing = "raw")',
  step_select_correlation = 'horizons::step_select_correlation(dplyr::starts_with("wn_"), outcome = "SOC")',
  step_select_cars        = 'horizons::step_select_cars(dplyr::starts_with("wn_"), outcome = "SOC")',
  step_select_boruta      = 'horizons::step_select_boruta(dplyr::starts_with("wn_"), outcome = "SOC")'
)

## Helper: run `code` in an environment whose parent is the global
## environment, holding `d`. Anything built in the test's own environment
## closes over the horizons namespace, and a worker that unserializes it loads
## horizons whether or not the steps ask for it, which would let the
## multisession test pass without the methods.
in_global <- function(code, d) {

  env   <- new.env(parent = globalenv())
  env$d <- d

  eval(parse(text = code), envir = env)

}

describe("the custom steps on parallel workers", {

  it("each names horizons, and the packages it calls, among those a worker needs", {

    withr::local_seed(1)
    d <- workers_data()

    expected <- list(
      step_transform_spectra  = c("prospectr", "horizons"),
      step_select_correlation = c("zoo", "horizons"),
      step_select_cars        = c("pls", "horizons"),
      step_select_boruta      = c("Boruta", "ranger", "horizons")
    )

    for (step in names(step_calls)) {

      rec <- in_global(paste0("recipes::recipe(SOC ~ ., data = d) |> ", step_calls[[step]]), d)

      expect_true(all(expected[[step]] %in% recipes::required_pkgs(rec)), label = step)

    }

  })

  it("resamples a workflow with each step on fresh workers that have not loaded horizons (#238)", {

    skip_unless_slow_tier()
    skip_on_cran()
    skip_if_not_installed("Boruta")

    withr::local_seed(1)
    d <- workers_data()

    local_plan(future::sequential)

    for (step in names(step_calls)) {

      ## Fresh workers for each step, so one step's run cannot leave horizons
      ## loaded for the next
      future::plan(future::sequential)
      future::plan(future::multisession, workers = 2)

      loaded <- future::value(future::future(isNamespaceLoaded("horizons"), globals = FALSE))
      expect_false(loaded, label = paste(step, ": horizons loaded on a fresh worker"))

      ## The steps' own messages and the small data's warnings are not the
      ## subject here; the notes say whether any fold failed.
      res <- suppressWarnings(suppressMessages(in_global(paste0(
        "tune::fit_resamples(",
        "  workflows::workflow(recipes::recipe(SOC ~ ., data = d) |> ", step_calls[[step]], ",",
        "                      parsnip::linear_reg()),",
        "  rsample::vfold_cv(d, v = 2),",
        "  control = tune::control_resamples(allow_par = TRUE))"
      ), d)))

      notes <- tune::collect_notes(res)

      expect_identical(notes$note[notes$type == "error"], character(0), label = step)

    }

  })

})
