#' Serialized size of an object, in bytes
#'
#' The memory-footprint guards measure serialized rather than resident size,
#' because R's serializer does not deduplicate data frames: a recipe or rset
#' holding several references to one training table costs one copy in memory
#' and N copies on the wire. `object.size()` and `lobstr::obj_size()` both
#' report the small, correct number, which is why the duplication went
#' undetected — only serialization reveals it.
#'
#' The ratio to the training data is scale-invariant, so small fixtures catch a
#' regression that only bites at library scale.
#'
#' @param x Any R object.
#'
#' @return Integer number of bytes.
serialized_size <- function(x) {

  length(serialize(x, NULL))

}

#' The rows one verb call added to an object's log
#'
#' A verb appends its rows after the log it was given, so the call's rows are
#' the ones past the input's.
#'
#' @param before The object the verb was given, or `NULL` for `spectra()`.
#' @param after The object it returned.
#'
#' @return The call's rows of `after$log`, in order.
log_rows_added <- function(before, after) {

  n_before <- if (is.null(before)) 0L else nrow(before$log)

  after$log[setdiff(seq_len(nrow(after$log)), seq_len(n_before)), , drop = FALSE]

}

#' The events one verb call added to an object's log, less its record
#'
#' A data verb's last row is the note recording the call; the rows before it
#' are what happened to particular samples, columns or files. The row
#' dropped is checked to be that note, by the opening each data verb's
#' record has, so an event in its place is never dropped silently.
#'
#' @inheritParams log_rows_added
#'
#' @return The call's rows of `after$log` but the last, in order.
log_events_added <- function(before, after) {

  rows <- log_rows_added(before, after)

  openings <- c(spectra      = "Read ",
                parse_ids    = "Parsed sample ids with ",
                standardize  = "Standardized with ",
                average      = "Averaged ",
                add_response = "Joined ")

  last <- rows[nrow(rows), , drop = FALSE]

  if (nrow(rows) == 0 || !identical(last$type, "note") || !last$verb %in% names(openings) ||
      !startsWith(last$message, openings[[last$verb]])) {
    stop("The call's last row is not a data verb's record note.", call. = FALSE)
  }

  rows[setdiff(seq_len(nrow(rows)), nrow(rows)), , drop = FALSE]

}

#' The note one data verb call wrote recording itself
#'
#' The call's last row, checked as [log_events_added()] checks it.
#'
#' @inheritParams log_rows_added
#'
#' @return The record's row of `after$log`.
log_record_added <- function(before, after) {

  rows   <- log_rows_added(before, after)
  events <- log_events_added(before, after)

  rows[nrow(events) + 1L, , drop = FALSE]

}

#' A log without its time column
#'
#' Two calls that write the same rows are stamped at different times, so logs
#' from separate calls are compared on everything else.
#'
#' @param log A log, shaped like `new_log()`'s.
#'
#' @return `log` less `time`.
without_time <- function(log) {

  log[, setdiff(names(log), "time"), drop = FALSE]

}

#' Whether each Bayesian iteration improved on the best before it
#'
#' tune's stopping rule counts the iterations since the last strict
#' improvement of the first metric's mean on the best so far, the initial
#' grid's included, and an iteration whose candidate it dropped counts as
#' none. So a search stopped at `no_improve = 1` improved at every iteration
#' but its last.
#'
#' @param results A `tune_bayes()` result.
#' @param n_iter Integer. The iterations the search began.
#' @param metric Character(1). The metric tune optimised.
#'
#' @return Logical, one per iteration.
improved_each <- function(results, n_iter, metric = "rmse") {

  means <- tune::collect_metrics(results)
  means <- means[means$.metric == metric, ]
  best  <- min(means$mean[means$.iter == 0])

  vapply(seq_len(n_iter), function(i) {
    now    <- means$mean[means$.iter == i]
    better <- length(now) == 1 && now < best
    if (better) best <<- now
    better
  }, logical(1))

}

## The share of the training outcome's standard deviation a fit's
## cross-validated RMSE must come under to count as having learned
## something. Predicting the training rows' mean lands near the standard
## deviation, and the mean of the folds' RMSEs can land just under it: fits
## to a shuffled outcome come out at 0.97 of it or more, while the tier
## tests' fits on their fixtures sit under 0.8 of it.
NONTRIVIAL_RMSE_SHARE <- 0.9

#' Expect every fit an evaluation or a fit holds to have learned something
#'
#' A tier test says a result is the same under two settings, which shows
#' nothing when the fits compared learned nothing from their data (a tuned
#' model on a fixture with no signal tends to choose its most regularized
#' candidate, which predicts one value, and a model whose predictions vary
#' fits noise). So every success in `x`'s results has an R² defined on the
#' test set and on the folds, which a constant prediction leaves undefined,
#' and a cross-validated RMSE under NONTRIVIAL_RMSE_SHARE of the standard
#' deviation of the outcome on the training rows: it beats predicting their
#' mean by a tenth of that standard deviation, which a fit to noise does not.
#' A fit's results hold no cross-validated R², so for a fit the folds'
#' condition is that its out-of-fold predictions vary within every fold.
#'
#' @param x A `horizons_eval` or a `horizons_fit` with at least one success.
#'
#' @return `x`, invisibly.
expect_fits_nontrivial <- function(x) {

  act <- testthat::quasi_label(rlang::enquo(x), arg = "x")

  role_map <- x$data$role_map
  outcome  <- role_map$variable[role_map$role == "outcome"]
  sd_train <- stats::sd(rsample::training(rebuild_split(x)$split)[[outcome]])

  if (inherits(x, "horizons_fit")) {

    res   <- x$models$results
    oof   <- x$models$cv_predictions
    folds <- vapply(res$config_id, function(id) {
      own <- oof[oof$config_id == id, ]
      nrow(own) > 0 && isTRUE(all(tapply(own$.pred, own$.fold, stats::sd) > 0))
    }, logical(1))
    cv_rmse <- res$cv_rmse_mean

  } else {

    res     <- x$evaluation$results
    folds   <- !is.na(res$cv_rsq)
    cv_rmse <- res$cv_rmse

  }

  ok      <- !is.na(res$rsq) & folds & !is.na(cv_rmse) & cv_rmse < NONTRIVIAL_RMSE_SHARE * sd_train
  success <- res$status == "success"
  trivial <- res$config_id[success & !ok]

  testthat::expect(
    any(success) && length(trivial) == 0L,
    if (!any(success)) {
      sprintf("%s holds no successful fit to read.", act$lab)
    } else {
      sprintf(paste("%s holds fits that learned nothing: %s (an R² undefined on the test set or the folds,",
                    "or a cross-validated RMSE not under %s of the training outcome's standard deviation, %.3f)."),
              act$lab, paste(trivial, collapse = ", "), format(NONTRIVIAL_RMSE_SHARE), sd_train)
    }
  )

  invisible(act$val)

}
