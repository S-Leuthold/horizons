#' Define Parsnip Model Specifications for Soil Spectral Regression
#'
#' Returns a tunable `parsnip` model specification object for the specified model type.
#' Each model is initialized with appropriate hyperparameters marked for tuning using `tune()`,
#' and is intended for use in model grids and workflow sets. Currently supports a suite
#' of interpretable and machine-learning models commonly used in soil spectroscopy.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @import plsmod
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom parsnip rand_forest pls cubist_rules svm_poly mlp set_engine set_mode
#' @importFrom tune tune
#' @importFrom cli cli_abort
#'
#' @param model_type Character string specifying model type. Must be one of:
#'   \itemize{
#'     \item{"Random Forest"}
#'     \item{"Partial Least Squares Regression"}
#'     \item{"Cubist"}
#'     \item{"Support Vector Machine"}
#'     \item{"Bagged Neural Network"}
#'   }
#'
#' @return A `parsnip` model specification object with tunable hyperparameters,
#' ready to be added to a `workflow()` or `workflow_set()`.
#'
#' @seealso \code{\link[parsnip]{set_engine}}, \code{\link[tune]{tune}}, \code{\link{build_model_grid}}
#'
#' @examples
#' \dontrun{
#' define_model_specifications("Random Forest")
#' define_model_specifications("Partial Least Squares Regression")
#' }
#'
#' @keywords internal

define_model_specifications <- function(model_type) {

  switch(model_type,

         ## -------------------------------------------------------------------------
         ## Random Forest
         ## -------------------------------------------------------------------------

         "Random Forest" = parsnip::rand_forest(mtry = tune(),
                                                trees = tune(),
                                                min_n = tune()) %>%
                            parsnip::set_engine("ranger") %>%
                            parsnip::set_mode("regression"),

         ## -------------------------------------------------------------------------
         ## Partial Least Squares Regression
         ## -------------------------------------------------------------------------

         "Partial Least Squares Regression" = parsnip::pls(num_comp       = tune(),
                                                           predictor_prop = tune()) %>%
                                                parsnip::set_engine("mixOmics") %>%
                                                parsnip::set_mode("regression"),

         ## -------------------------------------------------------------------------
         ## Cubist
         ## -------------------------------------------------------------------------

         "Cubist" = parsnip::cubist_rules(committees = tune(),
                                          neighbors  = tune(),
                                          max_rules  = tune()) %>%
                      parsnip::set_engine("Cubist") %>%
                      parsnip::set_mode("regression"),

         ## -------------------------------------------------------------------------
         ## Support Vector Machine
         ## -------------------------------------------------------------------------

         "Support Vector Machine" = parsnip::svm_poly(cost         = tune(),
                                                      degree       = tune(),
                                                      scale_factor = tune(),
                                                      margin       = tune()) %>%
                                      parsnip::set_engine("kernlab") %>%
                                      parsnip::set_mode("regression"),

         ## -------------------------------------------------------------------------
         ## Bagged Neural Network
         ## -------------------------------------------------------------------------

         "Bagged Neural Network" = parsnip::mlp(penalty      = tune(),
                                                hidden_units = tune(),
                                                epochs       = tune()) %>%
                                    parsnip::set_engine("nnet") %>%
                                    parsnip::set_mode("regression"),

         ## -------------------------------------------------------------------------
         ## Error handling
         ## -------------------------------------------------------------------------

         cli::cli_abort("Unsupported {.field model type}: {.val {model_type}}")
  )
}
