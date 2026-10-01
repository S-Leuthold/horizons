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
