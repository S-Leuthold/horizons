## ---------------------------------------------------------------------------
## Tests for pipeline-add-response.R: add_response() function
## ---------------------------------------------------------------------------

library(testthat)
library(horizons)


## ---------------------------------------------------------------------------
## Helper: Create horizons_data for testing
## ---------------------------------------------------------------------------

#' Create minimal horizons_data object for testing add_response
#'
#' Simulates a post-average() state: unique sample_ids, no replicates.
#'
#' @param sample_ids Character vector of sample IDs.
#' @return A horizons_data object.
#' @noRd
make_test_hd <- function(sample_ids = c("S001", "S002", "S003")) {

  n <- length(sample_ids)

  analysis <- tibble::tibble(
    sample_id = sample_ids,
    wn_4000   = runif(n),
    wn_3000   = runif(n)
  )

  role_map <- tibble::tibble(
    variable = c("sample_id", "wn_4000", "wn_3000"),
    role     = c("id", "predictor", "predictor")
  )

  obj <- list(
    data = list(
      analysis     = analysis,
      role_map     = role_map,
      n_rows       = n,
      n_predictors = 2,
      n_covariates = 0,
      n_responses  = 0
    ),
    provenance = list(
      spectra_source   = "test",
      spectra_type     = "opus",
      created          = Sys.time(),
      horizons_version = utils::packageVersion("horizons"),
      schema_version   = 1L
    )
  )

  class(obj) <- c("horizons_data", "list")
  obj

}


## ---------------------------------------------------------------------------
## add_response() — Basic tibble join
## ---------------------------------------------------------------------------

test_that("add_response() joins a single variable from tibble", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd()
  lab <- tibble::tibble(
    sample_id = c("S001", "S002", "S003"),
    SOC       = c(1.2, 3.4, 5.6)
  )

  ## Act -------------------------------------------------------------------

  result <- add_response(hd, lab, variable = "SOC")

  ## Assert ----------------------------------------------------------------

  expect_s3_class(result, "horizons_data")
  expect_true("SOC" %in% names(result$data$analysis))
  expect_equal(result$data$analysis$SOC, c(1.2, 3.4, 5.6))

})

test_that("add_response() joins multiple variables from tibble", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd()
  lab <- tibble::tibble(
    sample_id = c("S001", "S002", "S003"),
    SOC       = c(1.2, 3.4, 5.6),
    POM_C     = c(0.5, 1.0, 1.5),
    MAOM_C    = c(0.7, 2.4, 4.1)
  )

  ## Act -------------------------------------------------------------------

  result <- add_response(hd, lab, variable = c("SOC", "POM_C", "MAOM_C"))

  ## Assert ----------------------------------------------------------------

  expect_true(all(c("SOC", "POM_C", "MAOM_C") %in% names(result$data$analysis)))
  expect_equal(result$data$analysis$SOC, c(1.2, 3.4, 5.6))
  expect_equal(result$data$analysis$POM_C, c(0.5, 1.0, 1.5))
  expect_equal(result$data$analysis$MAOM_C, c(0.7, 2.4, 4.1))

})

test_that("add_response() only joins named variable columns", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd()
  lab <- tibble::tibble(
    sample_id = c("S001", "S002", "S003"),
    SOC       = c(1.2, 3.4, 5.6),
    analyst   = c("Alice", "Bob", "Carol"),
    batch     = c(1, 1, 2)
  )

  ## Act -------------------------------------------------------------------

  result <- add_response(hd, lab, variable = "SOC")

  ## Assert ----------------------------------------------------------------

  expect_true("SOC" %in% names(result$data$analysis))
  expect_false("analyst" %in% names(result$data$analysis))
  expect_false("batch" %in% names(result$data$analysis))

})


## ---------------------------------------------------------------------------
## add_response() — Role map updates
## ---------------------------------------------------------------------------

test_that("add_response() assigns role = 'response' in role_map", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd()
  lab <- tibble::tibble(
    sample_id = c("S001", "S002", "S003"),
    SOC       = c(1.2, 3.4, 5.6),
    POM_C     = c(0.5, 1.0, 1.5)
  )

  ## Act -------------------------------------------------------------------

  result <- add_response(hd, lab, variable = c("SOC", "POM_C"))

  ## Assert ----------------------------------------------------------------

  roles <- result$data$role_map

  expect_equal(roles$role[roles$variable == "SOC"], "response")
  expect_equal(roles$role[roles$variable == "POM_C"], "response")

})

test_that("add_response() updates n_responses count", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd()
  lab <- tibble::tibble(
    sample_id = c("S001", "S002", "S003"),
    SOC       = c(1.2, 3.4, 5.6),
    POM_C     = c(0.5, 1.0, 1.5)
  )

  ## Act -------------------------------------------------------------------

  result <- add_response(hd, lab, variable = c("SOC", "POM_C"))

  ## Assert ----------------------------------------------------------------

  expect_equal(result$data$n_responses, 2)

})


## ---------------------------------------------------------------------------
## add_response() — Join key mechanics
## ---------------------------------------------------------------------------

test_that("add_response() uses default by = 'sample_id'", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd()
  lab <- tibble::tibble(
    sample_id = c("S001", "S002", "S003"),
    SOC       = c(1.2, 3.4, 5.6)
  )

  ## Act -------------------------------------------------------------------

  result <- add_response(hd, lab, variable = "SOC")

  ## Assert ----------------------------------------------------------------

  expect_equal(result$data$analysis$SOC, c(1.2, 3.4, 5.6))

})

test_that("add_response() supports named vector for by", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd()
  lab <- tibble::tibble(
    Lab_ID = c("S001", "S002", "S003"),
    SOC    = c(1.2, 3.4, 5.6)
  )

  ## Act -------------------------------------------------------------------

  result <- add_response(hd, lab, variable = "SOC",
                         by = c("sample_id" = "Lab_ID"))

  ## Assert ----------------------------------------------------------------

  expect_equal(result$data$analysis$SOC, c(1.2, 3.4, 5.6))

})


## ---------------------------------------------------------------------------
## add_response() — Path source (CSV)
## ---------------------------------------------------------------------------

test_that("add_response() reads CSV path source", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd()
  lab <- tibble::tibble(
    sample_id = c("S001", "S002", "S003"),
    SOC       = c(1.2, 3.4, 5.6)
  )

  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv), add = TRUE)
  readr::write_csv(lab, temp_csv)

  ## Act -------------------------------------------------------------------

  result <- add_response(hd, temp_csv, variable = "SOC")

  ## Assert ----------------------------------------------------------------

  expect_equal(result$data$analysis$SOC, c(1.2, 3.4, 5.6))

})


## ---------------------------------------------------------------------------
## add_response() — NA handling for unmatched samples
## ---------------------------------------------------------------------------

test_that("add_response() assigns NA for unmatched horizons samples", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd(c("S001", "S002", "S003"))
  lab <- tibble::tibble(
    sample_id = c("S001", "S003"),
    SOC       = c(1.2, 5.6)
  )

  ## Act -------------------------------------------------------------------

  result <- add_response(hd, lab, variable = "SOC")

  ## Assert ----------------------------------------------------------------

  expect_equal(result$data$analysis$SOC[1], 1.2)
  expect_true(is.na(result$data$analysis$SOC[2]))
  expect_equal(result$data$analysis$SOC[3], 5.6)

})

test_that("add_response() warns about unmatched samples", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd(c("S001", "S002", "S003"))
  lab <- tibble::tibble(
    sample_id = c("S001"),
    SOC       = c(1.2)
  )

  ## Act & Assert ----------------------------------------------------------

  expect_warning(
    add_response(hd, lab, variable = "SOC"),
    "no matching response data"
  )

})

test_that("add_response() silently drops unmatched source rows", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd(c("S001", "S002"))
  lab <- tibble::tibble(
    sample_id = c("S001", "S002", "S999"),
    SOC       = c(1.2, 3.4, 99.9)
  )

  ## Act -------------------------------------------------------------------

  result <- add_response(hd, lab, variable = "SOC")

  ## Assert — still 2 rows, S999 not added --------------------------------

  expect_equal(nrow(result$data$analysis), 2)
  expect_equal(result$data$analysis$SOC, c(1.2, 3.4))

})


## ---------------------------------------------------------------------------
## add_response() — Duplicate key detection
## ---------------------------------------------------------------------------

test_that("add_response() errors on duplicate keys in source", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd()
  lab <- tibble::tibble(
    sample_id = c("S001", "S001", "S002", "S003"),
    SOC       = c(1.0, 1.5, 3.4, 5.6)
  )

  ## Act & Assert ----------------------------------------------------------

  expect_error(
    add_response(hd, lab, variable = "SOC"),
    "Duplicate"
  )

})

test_that("add_response() errors on duplicate keys in horizons (pre-average)", {

  ## Arrange — simulate pre-average state with replicate IDs ---------------

  analysis <- tibble::tibble(
    sample_id = c("S001", "S001", "S002"),
    wn_4000   = runif(3),
    wn_3000   = runif(3)
  )

  role_map <- tibble::tibble(
    variable = c("sample_id", "wn_4000", "wn_3000"),
    role     = c("id", "predictor", "predictor")
  )

  hd <- list(
    data = list(
      analysis     = analysis,
      role_map     = role_map,
      n_rows       = 3,
      n_predictors = 2,
      n_covariates = 0,
      n_responses  = 0
    ),
    provenance = list(spectra_source = "test", spectra_type = "opus",
                      created = Sys.time(),
                      horizons_version = utils::packageVersion("horizons"),
                      schema_version = 1L)
  )
  class(hd) <- c("horizons_data", "list")

  lab <- tibble::tibble(
    sample_id = c("S001", "S002"),
    SOC       = c(1.2, 3.4)
  )

  ## Act & Assert ----------------------------------------------------------

  expect_error(
    add_response(hd, lab, variable = "SOC"),
    "Duplicate.*horizons|average"
  )

})


## ---------------------------------------------------------------------------
## add_response() — Diagnostics (match quality)
## ---------------------------------------------------------------------------

test_that("add_response() aborts on zero matches with diagnostic", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd(c("S001", "S002", "S003"))
  lab <- tibble::tibble(
    sample_id = c("X100", "X200", "X300"),
    SOC       = c(1.0, 2.0, 3.0)
  )

  ## Act & Assert ----------------------------------------------------------

  expect_error(
    add_response(hd, lab, variable = "SOC"),
    "0.*matched|No matching"
  )

})

test_that("add_response() detects case mismatch pattern", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd(c("S001", "S002", "S003"))
  lab <- tibble::tibble(
    sample_id = c("s001", "s002", "s003"),
    SOC       = c(1.0, 2.0, 3.0)
  )

  ## Act & Assert — should mention case ------------------------------------

  expect_error(
    add_response(hd, lab, variable = "SOC"),
    "case"
  )

})


## ---------------------------------------------------------------------------
## add_response() — Provenance tracking
## ---------------------------------------------------------------------------

test_that("add_response() records provenance (tibble source)", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd()
  lab <- tibble::tibble(
    sample_id = c("S001", "S002", "S003"),
    SOC       = c(1.2, 3.4, 5.6)
  )

  ## Act -------------------------------------------------------------------

  result <- add_response(hd, lab, variable = "SOC")

  ## Assert ----------------------------------------------------------------

  prov <- result$provenance$add_response[[1]]

  expect_equal(prov$source, "tibble")
  expect_equal(prov$variables, "SOC")
  expect_equal(prov$n_matched, 3)
  expect_equal(prov$n_unmatched, 0)
  expect_s3_class(prov$applied_at, "POSIXct")

})

test_that("add_response() records provenance (CSV path source)", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd()
  lab <- tibble::tibble(
    sample_id = c("S001", "S002", "S003"),
    SOC       = c(1.2, 3.4, 5.6)
  )

  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv), add = TRUE)
  readr::write_csv(lab, temp_csv)

  ## Act -------------------------------------------------------------------

  result <- add_response(hd, temp_csv, variable = "SOC")

  ## Assert ----------------------------------------------------------------

  prov <- result$provenance$add_response[[1]]

  expect_equal(prov$source, temp_csv)

})

test_that("add_response() appends provenance on repeated calls", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd()
  lab1 <- tibble::tibble(sample_id = c("S001", "S002", "S003"),
                         SOC       = c(1.2, 3.4, 5.6))
  lab2 <- tibble::tibble(sample_id = c("S001", "S002", "S003"),
                         pH        = c(5.5, 6.0, 6.5))

  ## Act -------------------------------------------------------------------

  result <- add_response(hd, lab1, variable = "SOC")
  result <- add_response(result, lab2, variable = "pH")

  ## Assert ----------------------------------------------------------------

  expect_length(result$provenance$add_response, 2)
  expect_equal(result$provenance$add_response[[1]]$variables, "SOC")
  expect_equal(result$provenance$add_response[[2]]$variables, "pH")

})


## ---------------------------------------------------------------------------
## add_response() — Repeated calls
## ---------------------------------------------------------------------------

test_that("add_response() allows joining from multiple sources", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd()
  lab1 <- tibble::tibble(sample_id = c("S001", "S002", "S003"),
                         SOC       = c(1.2, 3.4, 5.6))
  lab2 <- tibble::tibble(sample_id = c("S001", "S002", "S003"),
                         pH        = c(5.5, 6.0, 6.5))

  ## Act -------------------------------------------------------------------

  result <- add_response(hd, lab1, variable = "SOC")
  result <- add_response(result, lab2, variable = "pH")

  ## Assert ----------------------------------------------------------------

  expect_true("SOC" %in% names(result$data$analysis))
  expect_true("pH" %in% names(result$data$analysis))
  expect_equal(result$data$n_responses, 2)

})

test_that("add_response() errors when variable already exists", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd()
  lab <- tibble::tibble(sample_id = c("S001", "S002", "S003"),
                        SOC       = c(1.2, 3.4, 5.6))

  result <- add_response(hd, lab, variable = "SOC")

  lab2 <- tibble::tibble(sample_id = c("S001", "S002", "S003"),
                         SOC       = c(9.9, 9.9, 9.9))

  ## Act & Assert ----------------------------------------------------------

  expect_error(
    add_response(result, lab2, variable = "SOC"),
    "already exist"
  )

})


## ---------------------------------------------------------------------------
## add_response() — Input validation errors
## ---------------------------------------------------------------------------

test_that("add_response() errors on non-horizons_data input", {

  lab <- tibble::tibble(sample_id = "A", SOC = 1.0)

  expect_error(
    add_response(data.frame(x = 1), lab, variable = "SOC"),
    "horizons_data"
  )

})

test_that("add_response() errors when variable not found in source", {

  hd  <- make_test_hd()
  lab <- tibble::tibble(sample_id = c("S001", "S002", "S003"),
                        SOC       = c(1.2, 3.4, 5.6))

  expect_error(
    add_response(hd, lab, variable = "nonexistent"),
    "not found"
  )

})

test_that("add_response() errors when variable is non-numeric", {

  hd  <- make_test_hd()
  lab <- tibble::tibble(sample_id = c("S001", "S002", "S003"),
                        texture   = c("clay", "loam", "sand"))

  expect_error(
    add_response(hd, lab, variable = "texture"),
    "numeric"
  )

})

test_that("add_response() errors when by column missing from source", {

  hd  <- make_test_hd()
  lab <- tibble::tibble(Lab_ID = c("S001", "S002", "S003"),
                        SOC    = c(1.2, 3.4, 5.6))

  ## Default by = "sample_id", but source has "Lab_ID" --------------------

  expect_error(
    add_response(hd, lab, variable = "SOC"),
    "not found.*source|sample_id"
  )

})

test_that("add_response() errors when by column missing from horizons", {

  hd  <- make_test_hd()
  lab <- tibble::tibble(sample_id = c("S001", "S002", "S003"),
                        SOC       = c(1.2, 3.4, 5.6))

  expect_error(
    add_response(hd, lab, variable = "SOC", by = "nonexistent"),
    "not found.*horizons"
  )

})

test_that("add_response() errors on non-existent CSV path", {

  hd <- make_test_hd()

  expect_error(
    add_response(hd, "/nonexistent/path.csv", variable = "SOC"),
    "does not exist|not found"
  )

})

test_that("add_response() errors on multi-column by", {

  hd  <- make_test_hd()
  lab <- tibble::tibble(sample_id = c("S001", "S002", "S003"),
                        site      = c("A", "B", "C"),
                        SOC       = c(1.2, 2.3, 3.4))

  expect_error(
    add_response(hd, lab, variable = "SOC",
                 by = c("sample_id" = "sample_id", "site" = "site")),
    "Multi-column joins not supported"
  )

})

test_that("add_response() errors on partially named by", {

  ## Partial naming only possible with length > 1, which hits the

  ## multi-column guard first. So we check the guard order is correct:
  ## length > 1 → multi-column error (not partial naming error).
  ## The partial-naming check exists for defensive completeness if
  ## multi-column joins are ever supported.

  hd  <- make_test_hd()
  lab <- tibble::tibble(sample_id = c("S001", "S002", "S003"),
                        SOC       = c(1.2, 2.3, 3.4))

  ## Length-2 partially-named by hits multi-column guard first
  expect_error(
    add_response(hd, lab, variable = "SOC",
                 by = c("sample_id" = "sample_id", "extra")),
    "Multi-column joins not supported"
  )

})

test_that("add_response() errors on invalid source type", {

  hd <- make_test_hd()

  expect_error(
    add_response(hd, 42, variable = "SOC"),
    "must be"
  )

})


## ---------------------------------------------------------------------------
## add_response() — Print/summary display
## ---------------------------------------------------------------------------

test_that("print.horizons_data shows response info after add_response()", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd()
  lab <- tibble::tibble(sample_id = c("S001", "S002", "S003"),
                        SOC       = c(1.2, 3.4, 5.6),
                        POM_C     = c(0.5, 1.0, 1.5))

  result <- add_response(hd, lab, variable = c("SOC", "POM_C"))

  ## Act -------------------------------------------------------------------

  output <- capture.output(print(result))

  ## Assert ----------------------------------------------------------------

  expect_true(any(grepl("Responses", output)))
  expect_true(any(grepl("SOC", output)))
  expect_true(any(grepl("POM_C", output)))

})

test_that("print.horizons_data omits response line when none exist", {

  ## Arrange ---------------------------------------------------------------

  hd <- make_test_hd()

  ## Act -------------------------------------------------------------------

  output <- capture.output(print(hd))

  ## Assert ----------------------------------------------------------------

  expect_false(any(grepl("Responses", output)))

})
