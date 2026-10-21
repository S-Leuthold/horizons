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
#' are what happened to particular samples, columns or files.
#'
#' @inheritParams log_rows_added
#'
#' @return The call's rows of `after$log` but the last, in order.
log_events_added <- function(before, after) {

  rows <- log_rows_added(before, after)

  rows[setdiff(seq_len(nrow(rows)), nrow(rows)), , drop = FALSE]

}
