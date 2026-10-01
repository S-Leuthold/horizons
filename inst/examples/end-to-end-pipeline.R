## ===========================================================================
## horizons: the whole pipeline on synthetic spectra
## ===========================================================================
##
## Runs every step from reading spectra to predicting new samples:
##
##   spectra() -> standardize() -> add_response() -> configure()
##             -> validate()    -> evaluate()     -> fit() -> predict()
##
## The spectra are simulated below, so the script needs no data files. It
## uses one light model and a small grid so it finishes in a few minutes.
##
## Run with:  Rscript inst/examples/end-to-end-pipeline.R
## ===========================================================================

## From inside the source tree with devtools installed, load the working copy
## of the package; otherwise use the installed one.
if (file.exists("DESCRIPTION") &&
    any(grepl("^Package: horizons", readLines("DESCRIPTION"))) &&
    requireNamespace("devtools", quietly = TRUE)) {

  devtools::load_all(quiet = TRUE)

} else {

  library(horizons)

}

set.seed(42)

## ---------------------------------------------------------------------------
## Simulate MIR-like spectra and a soil property
## ---------------------------------------------------------------------------
## Each spectrum is a smooth baseline plus two absorption bands. The depth of
## the first band drives the response, so there is real signal to find.

n_samples   <- 240
wavenumbers <- seq(4000, 600, by = -20)
n_wn        <- length(wavenumbers)

make_spectrum <- function(band_depths) {

  baseline <- 0.2 + 0.0001 * (4000 - wavenumbers)
  band1    <- band_depths[1] * exp(-((wavenumbers - 1600)^2) / (2 * 80^2))
  band2    <- band_depths[2] * exp(-((wavenumbers - 1030)^2) / (2 * 60^2))
  baseline + band1 + band2 + rnorm(n_wn, sd = 0.005)

}

depths <- cbind(runif(n_samples, 0.1, 0.6), runif(n_samples, 0.1, 0.4))

spec_mat <- t(vapply(seq_len(n_samples), function(i) make_spectrum(depths[i, ]),
                     numeric(n_wn)))
colnames(spec_mat) <- paste0("wn_", wavenumbers)

spectra_df           <- tibble::as_tibble(spec_mat)
spectra_df$sample_id <- sprintf("SOIL_%03d", seq_len(n_samples))

response_df <- tibble::tibble(
  sample_id = spectra_df$sample_id,
  SOC       = 5 + 25 * depths[, 1] + rnorm(n_samples, sd = 1.0)
)

## Set ten samples aside as "unknowns" to predict at the end. They never
## enter the modeling.
unknown_idx   <- sample(n_samples, 10)
unknown_specs <- spectra_df[unknown_idx, ]
train_specs   <- spectra_df[-unknown_idx, ]
train_resp    <- response_df[!response_df$sample_id %in% unknown_specs$sample_id, ]

## ---------------------------------------------------------------------------
## Read, standardize and join the response
## ---------------------------------------------------------------------------
## standardize() resamples to a 2 cm-1 grid and trims to 600-4000 cm-1 by
## default.

hz <- spectra(train_specs, id_col = "sample_id") |>
  standardize() |>
  add_response(source = train_resp, variable = "SOC")

## ---------------------------------------------------------------------------
## Configure, validate and evaluate
## ---------------------------------------------------------------------------
## One model and one preprocessing method keep this fast. A real comparison
## would pass several of each, and evaluate() would score every combination.

hz <- hz |>
  configure(
    models            = "rf",
    preprocessing     = "snv",
    feature_selection = "none",
    transformation    = "none"
  ) |>
  validate() |>
  evaluate()

## ---------------------------------------------------------------------------
## Fit the best configuration, with prediction intervals
## ---------------------------------------------------------------------------

hz <- fit(hz, n_best = 1L, compute_uq = TRUE)

## ---------------------------------------------------------------------------
## Predict the unknowns
## ---------------------------------------------------------------------------
## New spectra must be standardized the same way as the training spectra.

unknown_hz <- spectra(unknown_specs, id_col = "sample_id") |>
  standardize()

preds <- predict(hz, unknown_hz)

## Because the data are simulated, the true values are known and can sit
## next to the predictions.
preds$truth <- response_df$SOC[match(preds$sample_id, response_df$sample_id)]

print(preds)
