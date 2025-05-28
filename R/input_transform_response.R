#' Transform Outcome Variable Prior to Recipe Construction
#'
#' Applies a static transformation (e.g., log, sqrt) to the outcome variable (`Response`)
#' directly within the input data. This allows compatibility with prediction and workflow
#' tools that do not handle recipe-based transformations on the outcome.
#'
#' @importFrom dplyr mutate case_when
#' @importFrom cli cli_abort
#'
#' @param data A data frame containing a column named `Response`.
#' @param method A character string specifying the transformation to apply. Options include:
#'   \itemize{
#'     \item{"No Transformation"}
#'     \item{"Log Transformation"}
#'     \item{"Square Root Transformation"}
#'     \item{"Box-Cox Transformation"}
#'   }
#'
#' @return A modified data frame with `Response` transformed.
#' @export
#'

transform_outcome <- function(data, method) {
  if (!"Response" %in% names(data)) {
    cli::cli_abort("Input data must contain a `Response` column.")
  }

  data <- dplyr::mutate(data,
                        Response = dplyr::case_when(
                          method == "Log Transformation"         ~ log(Response),
                          method == "Square Root Transformation" ~ sqrt(Response),
                          TRUE                                   ~ Response
                        )
  )

  return(data)
}
