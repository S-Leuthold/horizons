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

  new_horizons_data(analysis = analysis, role_map = role_map)

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


## ---------------------------------------------------------------------------
## add_response() — Join key mechanics
## ---------------------------------------------------------------------------

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

test_that("add_response() reads CSV path source, and records the path", {

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
  expect_match(log_rows_added(hd, result)$message, temp_csv, fixed = TRUE)

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

  expect_no_warning(result <- add_response(hd, lab, variable = "SOC"))

  ## Assert ----------------------------------------------------------------

  expect_equal(result$data$analysis$SOC[1], 1.2)
  expect_true(is.na(result$data$analysis$SOC[2]))
  expect_equal(result$data$analysis$SOC[3], 5.6)

  ## Half or more matched: the unmatched sample is named in a note, not warned
  ### The event, then the call's record
  rows <- log_rows_added(hd, result)
  expect_identical(rows$type, c("note", "note"))
  row <- rows[1, ]
  expect_identical(row$ids[[1]], "S002")
  expect_match(row$message, "1 of 3 samples matched nothing in the source", fixed = TRUE)

  ## and counted in the tree
  out <- utils::capture.output(add_response(hd, lab, variable = "SOC", verbose = TRUE))
  expect_true(any(grepl("Matched: 2/3 samples (1 unmatched \u2192 NA)", out, fixed = TRUE)))

})

test_that("add_response() warns about unmatched samples", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd(c("S001", "S002", "S003"))
  lab <- tibble::tibble(
    sample_id = c("S001"),
    SOC       = c(1.2)
  )

  ## Act & Assert ----------------------------------------------------------

  w <- expect_warning(
    result <- add_response(hd, lab, variable = "SOC"),
    "1 of 3 samples matched the source, so 2 have no response values.", fixed = TRUE,
    class = "horizons_warning_coverage"
  )

  ## Recorded on the object before it was raised, naming the unmatched samples
  row <- result$log[result$log$verb == "add_response" & result$log$type == "warning", ]
  expect_identical(row$topic, "coverage")
  expect_identical(row$ids[[1]], c("S002", "S003"))
  expect_identical(w$log, row)

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

  hd <- new_horizons_data(analysis = analysis, role_map = role_map)

  lab <- tibble::tibble(
    sample_id = c("S001", "S002"),
    SOC       = c(1.2, 3.4)
  )

  ## Act & Assert ----------------------------------------------------------

  ## Refused at the door: average() collapses replicates before responses
  ## are joined
  err <- expect_error(add_response(hd, lab, variable = "SOC"),
                      class = "horizons_error_validation")
  expect_true(any(grepl("Duplicate sample_id values: S001", err$findings, fixed = TRUE)))

})


## ---------------------------------------------------------------------------
## add_response() — NA join keys (#139)
## ---------------------------------------------------------------------------

#' Add a meta column to join on, for keys other than sample_id
#' @noRd
with_site <- function(hd, site) {

  hd$data$analysis$site <- site
  hd$data$role_map      <- dplyr::bind_rows(hd$data$role_map,
                                            tibble::tibble(variable = "site", role = "meta"))
  hd

}

test_that("add_response() refuses NA keys on both sides rather than matching them (#139)", {

  ## Arrange — one NA key on each side passes both duplicate checks, and a
  ## default left_join() would join the two NA rows to each other
  hd  <- with_site(make_test_hd(), c("A", NA, "C"))
  lab <- tibble::tibble(site = c("A", NA, "C"), SOC = c(1.2, 3.4, 5.6))

  ## Act & Assert — refused, with the count on each side
  err <- expect_error(add_response(hd, lab, variable = "SOC", by = "site"),
                      class = "horizons_error_input")

  expect_match(conditionMessage(err), "1 in horizons data")
  expect_match(conditionMessage(err), "1 in source")

})

test_that("add_response() refuses NA keys on one side, counting them (#139)", {

  ## Source side only
  hd  <- with_site(make_test_hd(), c("A", "B", "C"))
  lab <- tibble::tibble(site = c("A", "B", NA, NA), SOC = c(1.2, 3.4, 5.6, 7.8))

  err <- expect_error(add_response(hd, lab, variable = "SOC", by = "site"),
                      class = "horizons_error_input")

  expect_match(conditionMessage(err), "2 in source")
  expect_no_match(conditionMessage(err), "horizons data")

  ## Horizons side only, on a meta key (an NA sample_id is refused at the
  ## door, before the join)
  hd  <- with_site(make_test_hd(), c("A", NA, "C"))
  lab <- tibble::tibble(site = c("A", "B", "C"), SOC = c(1.2, 3.4, 5.6))

  err <- expect_error(add_response(hd, lab, variable = "SOC", by = "site"),
                      class = "horizons_error_input")

  expect_match(conditionMessage(err), "1 in horizons data")
  expect_no_match(conditionMessage(err), "in source")

})

test_that("add_response() refuses a meta join key that repeats in the horizons data", {

  ## Duplicate sample_ids are refused at the door; a meta key can repeat
  hd  <- with_site(make_test_hd(), c("A", "A", "C"))
  lab <- tibble::tibble(site = c("A", "C"), SOC = c(1.2, 5.6))

  err <- expect_error(utils::capture.output(add_response(hd, lab, variable = "SOC", by = "site")),
                      class = "horizons_error_input")

  expect_match(conditionMessage(err), "Duplicate values in horizons join key site.", fixed = TRUE)

})

test_that("add_response() attaches values by key when the source is in another order", {

  hd  <- make_test_hd(c("S001", "S002", "S003"))
  lab <- tibble::tibble(sample_id = c("S003", "S001", "S002"), SOC = c(5.6, 1.2, 3.4))

  result <- add_response(hd, lab, variable = "SOC")

  expect_identical(result$data$analysis$sample_id, c("S001", "S002", "S003"))
  expect_equal(result$data$analysis$SOC, c(1.2, 3.4, 5.6))

})

test_that("add_response() adds columns to a promoted object and keeps its state", {

  ## Arrange — new columns move no rows, so add_response() runs on any class
  hd <- make_test_hd()
  hd$evaluation <- list(results = tibble::tibble(config_id = "cfg_a"))
  class(hd) <- c("horizons_eval", "horizons_data", "list")

  lab <- tibble::tibble(sample_id = c("S001", "S002", "S003"), SOC = c(1.2, 3.4, 5.6))

  ## Act
  result <- add_response(hd, lab, variable = "SOC")

  ## Assert
  expect_identical(class(result), class(hd))
  expect_identical(result$evaluation, hd$evaluation)
  expect_equal(result$data$analysis$SOC, c(1.2, 3.4, 5.6))
  expect_identical(result$data$n_responses, 1L)

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

  err <- expect_error(
    add_response(hd, lab, variable = "SOC"),
    "No sample in the horizons data matches the source.", fixed = TRUE,
    class = "horizons_error_input"
  )
  expect_identical(rlang::call_name(err$call), "add_response")

})

test_that("add_response() detects case mismatch pattern", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd(c("S001", "S002", "S003"))
  lab <- tibble::tibble(
    sample_id = c("s001", "s002", "s003"),
    SOC       = c(1.0, 2.0, 3.0)
  )

  ## Act & Assert — should mention case ------------------------------------

  ## The previews and the hint are bullets of the condition, and nothing is
  ## printed beside it (#109)
  expect_silent(err <- tryCatch(add_response(hd, lab, variable = "SOC"), error = identity))
  expect_s3_class(err, "horizons_error_input")
  expect_match(conditionMessage(err), "case mismatch: try `mutate(source, sample_id = tolower(sample_id))`",
               fixed = TRUE)

  expect_snapshot(add_response(hd, lab, variable = "SOC"), error = TRUE, cnd_class = TRUE)

})

test_that("add_response() detects a whitespace pattern", {

  ## Arrange — the source's ids carry stray spaces
  hd  <- make_test_hd(c("S001", "S002", "S003"))
  lab <- tibble::tibble(sample_id = c(" S001", "S002 ", " S003"), SOC = c(1, 2, 3))

  ## Act & Assert — the hint is a bullet of the refusal
  err <- expect_error(add_response(hd, lab, variable = "SOC"), class = "horizons_error_input")
  expect_match(conditionMessage(err),
               "This looks like a whitespace issue: try `mutate(source, sample_id = trimws(sample_id))`.",
               fixed = TRUE)
  expect_no_match(conditionMessage(err), "case mismatch", fixed = TRUE)

})

test_that("add_response() carries its low-match hints in the warning, and the tree keeps its order (#91)", {

  ## Arrange ---------------------------------------------------------------

  ## One of four ids matches exactly and the rest differ only in case: under
  ## half matched, with a pattern to name
  hd  <- make_test_hd(c("S001", "S002", "S003", "S004"))
  lab <- tibble::tibble(
    sample_id = c("S001", "s002", "s003", "s004"),
    SOC       = c(1.0, 2.0, 3.0, 4.0)
  )

  ## Act -------------------------------------------------------------------

  w <- expect_warning(
    out <- utils::capture.output(add_response(hd, lab, variable = "SOC", verbose = TRUE)),
    class = "horizons_warning_coverage"
  )

  ## Assert ----------------------------------------------------------------

  expect_match(conditionMessage(w), "1 of 4 samples matched the source", fixed = TRUE)
  expect_match(conditionMessage(w),
               "This looks like a case mismatch: try `mutate(source, sample_id = tolower(sample_id))`.",
               fixed = TRUE)

  ## The hints used to print above the header, as a block of their own; the
  ## tree opens with its header and names no hint
  header <- grep("Adding response data", out, fixed = TRUE)

  expect_identical(header, 1L)
  expect_identical(out[header + 1:3], c(
    "│  ├─ Source: tibble",
    "│  ├─ Matched: 1/4 samples",
    "│  └─ Variables:"
  ))

})


## ---------------------------------------------------------------------------
## add_response() — Provenance tracking
## ---------------------------------------------------------------------------

test_that("add_response() records a data frame source in its note", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd()
  lab <- tibble::tibble(
    sample_id = c("S001", "S002", "S003"),
    SOC       = c(1.2, 3.4, 5.6)
  )

  ## Act -------------------------------------------------------------------

  result <- add_response(hd, lab, variable = "SOC")

  ## Assert ----------------------------------------------------------------

  note <- log_rows_added(hd, result)

  expect_match(note$message, "from a data frame", fixed = TRUE)
  expect_identical(note$ids[[1]], "SOC")
  expect_match(note$message, "3 of 3 samples matched", fixed = TRUE)

})


test_that("add_response() records the call as its last row, a note with the variables, the source and the counts", {

  ## Arrange ---------------------------------------------------------------

  hd  <- make_test_hd(c("S001", "S002", "S003", "S004"))
  lab <- tibble::tibble(
    sample_id = c("S001", "S002", "S003", "S004"),
    clay      = c(12, NA, 30, NA),
    oc        = c(1.1, 2.2, 3.3, 4.4)
  )

  ## Act -------------------------------------------------------------------

  expect_no_warning(result <- add_response(hd, lab, variable = c("clay", "oc"), verbose = FALSE))

  ## Assert ----------------------------------------------------------------

  note <- log_rows_added(hd, result)
  expect_identical(note$verb, "add_response")
  expect_identical(note$type, "note")
  expect_true(is.na(note$topic))
  expect_identical(note$ids[[1]], c("clay", "oc"))
  expect_identical(note$message,
                   "Joined clay and oc from a data frame on sample_id: 4 of 4 samples matched, with measured values per variable: clay 2, oc 4.")

  ### A file source by its path, a named key, and the coverage warning first
  lab <- tibble::tibble(ID = "S001", SOC = 1.2)
  csv <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(lab, csv)
  result <- suppressWarnings(add_response(hd, csv, variable = "SOC", by = c(sample_id = "ID"),
                                          verbose = FALSE))
  rows <- log_rows_added(hd, result)
  expect_identical(rows$type, c("warning", "note"))
  expect_identical(rows$ids[[2]], "SOC")
  expect_identical(rows$message[2], paste0(
    "Joined SOC from '", csv, "' on sample_id = ID: 1 of 4 samples matched, with measured values per variable: SOC 1."
  ))

})


## ---------------------------------------------------------------------------
## add_response() — Non-missing counts (#39)
## ---------------------------------------------------------------------------

test_that("add_response() reports and records non-missing counts, not only key matches (#39)", {

  ## Arrange — every key matches, but clay is measured on half the samples --

  hd  <- make_test_hd(c("S001", "S002", "S003", "S004"))
  lab <- tibble::tibble(
    sample_id = c("S001", "S002", "S003", "S004"),
    clay      = c(12, NA, 30, NA),
    oc        = c(1.1, 2.2, 3.3, 4.4)
  )

  ## Act -------------------------------------------------------------------

  out <- utils::capture.output(
    result <- add_response(hd, lab, variable = c("clay", "oc"), verbose = TRUE)
  )

  ## Assert ----------------------------------------------------------------

  expect_true(any(grepl("Matched: 4/4 samples", out, fixed = TRUE)))
  expect_true(any(grepl("clay: 2/4 non-missing", out, fixed = TRUE)))
  expect_true(any(grepl("oc: 4/4 non-missing", out, fixed = TRUE)))

  note <- log_rows_added(hd, result)

  expect_match(note$message, "4 of 4 samples matched", fixed = TRUE)
  expect_match(note$message, "measured values per variable: clay 2, oc 4.", fixed = TRUE)

  ## Rows are kept; NA outcomes are dropped per outcome when modelled
  expect_equal(nrow(result$data$analysis), 4)

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

  ## Each call appends its own note
  notes <- result$log[result$log$verb == "add_response", ]
  expect_identical(notes$type, c("note", "note"))
  expect_identical(notes$ids, list("SOC", "pH"))

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

  err <- expect_error(
    add_response(result, lab2, variable = "SOC"),
    "A variable is already in the data.", fixed = TRUE,
    class = "horizons_error_input"
  )
  expect_match(conditionMessage(err), "Rename the source column, or join a different variable.",
               fixed = TRUE)

})


## ---------------------------------------------------------------------------
## add_response() — Input validation errors
## ---------------------------------------------------------------------------

test_that("add_response() errors on non-horizons_data input", {

  lab <- tibble::tibble(sample_id = "A", SOC = 1.0)

  expect_error(
    add_response(data.frame(x = 1), lab, variable = "SOC"),
    "`x` must be a horizons_data object, not a data frame.", fixed = TRUE,
    class = "horizons_error_input"
  )

})

test_that("add_response() errors when variable not found in source", {

  hd  <- make_test_hd()
  lab <- tibble::tibble(sample_id = c("S001", "S002", "S003"),
                        SOC       = c(1.2, 3.4, 5.6))

  expect_error(
    add_response(hd, lab, variable = "nonexistent"),
    "A variable is not in the source data.", fixed = TRUE,
    class = "horizons_error_input"
  )

})

test_that("add_response() errors when variable is non-numeric", {

  hd  <- make_test_hd()
  lab <- tibble::tibble(sample_id = c("S001", "S002", "S003"),
                        texture   = c("clay", "loam", "sand"))

  expect_error(
    add_response(hd, lab, variable = "texture"),
    "Not numeric: texture (character).", fixed = TRUE,
    class = "horizons_error_input"
  )

})

test_that("add_response() errors when by column missing from source", {

  hd  <- make_test_hd()
  lab <- tibble::tibble(Lab_ID = c("S001", "S002", "S003"),
                        SOC    = c(1.2, 3.4, 5.6))

  ## Default by = "sample_id", but source has "Lab_ID" --------------------

  expect_error(
    add_response(hd, lab, variable = "SOC"),
    "The join column sample_id is not in the source data.", fixed = TRUE,
    class = "horizons_error_input"
  )

})

test_that("add_response() errors when by column missing from horizons", {

  hd  <- make_test_hd()
  lab <- tibble::tibble(sample_id = c("S001", "S002", "S003"),
                        SOC       = c(1.2, 3.4, 5.6))

  expect_error(
    add_response(hd, lab, variable = "SOC", by = "nonexistent"),
    "The join column nonexistent is not in the horizons data.", fixed = TRUE,
    class = "horizons_error_input"
  )

})

test_that("add_response() errors on non-existent CSV path", {

  hd <- make_test_hd()

  expect_error(
    add_response(hd, "/nonexistent/path.csv", variable = "SOC"),
    "The source file does not exist.", fixed = TRUE,
    class = "horizons_error_input"
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
    "`by` must name one join column, not 2.", fixed = TRUE,
    class = "horizons_error_input"
  )

})

test_that("add_response() refuses a variable that is not column names, or names one twice", {

  hd  <- make_test_hd()
  lab <- tibble::tibble(sample_id = c("S001", "S002", "S003"), SOC = c(1, 2, 3))

  for (bad in list(character(0), 1, NULL)) {

    expect_error(add_response(hd, lab, variable = bad),
                 "`variable` must be a character vector of column names", fixed = TRUE,
                 class = "horizons_error_input")

  }

  expect_error(add_response(hd, lab, variable = c("SOC", "SOC")),
               "Repeated: SOC.", fixed = TRUE,
               class = "horizons_error_input")

})

test_that("add_response() errors on invalid source type", {

  hd <- make_test_hd()

  expect_error(
    add_response(hd, 42, variable = "SOC"),
    "`source` must be a data frame or the path of a CSV file, not a number.", fixed = TRUE,
    class = "horizons_error_input"
  )

})


test_that("add_response() is quiet under horizons.verbose = FALSE, prints when asked, and checks verbose", {

  hd  <- make_test_hd()
  lab <- tibble::tibble(sample_id = c("S001", "S002", "S003"), SOC = c(1, 2, 3))

  withr::local_options(horizons.verbose = FALSE)
  expect_silent(add_response(hd, lab, variable = "SOC"))
  expect_output(add_response(hd, lab, variable = "SOC", verbose = TRUE), "Adding response data")

  withr::local_options(horizons.verbose = TRUE)
  expect_output(add_response(hd, lab, variable = "SOC"), "Adding response data")

  expect_error(add_response(hd, lab, variable = "SOC", verbose = "yes"),
               "`verbose` must be `TRUE` or `FALSE`", fixed = TRUE,
               class = "horizons_error_input")

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


## ---------------------------------------------------------------------------
## add_response() — Structural validation on return (#24)
## ---------------------------------------------------------------------------

test_that("add_response() catches a corrupt input the verb itself never checks (#24)", {

  ## Arrange — an increasing wavenumber axis. add_response() only joins and
  ## checks the response columns; it never reads or reorders the predictor
  ## axis, so nothing about the join would ever catch this. The new
  ## end-of-verb validate_horizons_data() call (#24) is what catches it.
  hd <- make_test_hd()

  pred_rows <- which(hd$data$role_map$role == "predictor")
  hd$data$role_map[pred_rows, ] <- hd$data$role_map[rev(pred_rows), ]

  lab <- tibble::tibble(sample_id = hd$data$analysis$sample_id,
                        SOC       = c(1.2, 3.4, 5.6))

  ## Act & Assert --------------------------------------------------------------

  expect_error(
    add_response(hd, lab, variable = "SOC"),
    class = "horizons_error_validation"
  )

})
