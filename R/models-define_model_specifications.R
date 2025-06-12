#' Define Parsnip Model Specifications for Soil Spectral Regression
#'
#' Returns a tunable `parsnip` model specification object for the specified model type.
#' Each model is initialized with appropriate hyperparameters marked for tuning using `tune()`,
#' and is intended for use in model grids and workflow sets. Supports a curated set of models
#' relevant to soil spectroscopy and ensemble learning.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom parsnip rand_forest cubist_rules boost_tree linear_reg svm_rbf mars pls mlp set_engine set_mode
#' @importFrom tune tune
#' @importFrom cli cli_abort
#'
#' @param model_type Character string specifying model type. Must be one of:
#'   \itemize{
#'     \item{"random_forest"}
#'     \item{"cubist"}
#'     \item{"xgboost"}
#'     \item{"elastic_net"}
#'     \item{"svm_rbf"}
#'     \item{"mars"}
#'     \item{"plsr"}
#'     \item{"mlp_nn"}
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
#' define_model_specifications("Elastic Net")
#' define_model_specifications("Neural Network")
#' }
#'
#' @keywords internal


define_model_specifications <- function(model_type) {

  switch(as.character(model_type),

         ## -------------------------------------------------------------------------
         ## Random Forest
         ## -------------------------------------------------------------------------

         "random_forest" = parsnip::rand_forest(mtry  = tune(),
                                                trees = tune(),
                                                min_n = tune()) %>%
                            parsnip::set_engine("ranger") %>%
                            parsnip::set_mode("regression"),

         ## -------------------------------------------------------------------------
         ## Partial Least Squares Regression
         ## -------------------------------------------------------------------------

         "plsr" = parsnip::pls(num_comp       = tune(),
                               predictor_prop = tune()) %>%
                    parsnip::set_engine("mixOmics") %>%
                    parsnip::set_mode("regression"),

         ## -------------------------------------------------------------------------
         ## Cubist
         ## -------------------------------------------------------------------------

         "cubist" = parsnip::cubist_rules(committees = tune(),
                                          neighbors  = tune(),
                                          max_rules  = tune()) %>%
                      parsnip::set_engine("Cubist") %>%
                      parsnip::set_mode("regression"),

         ## -------------------------------------------------------------------------
         ## Gradient Boosted Trees
         ## -------------------------------------------------------------------------

         "xgboost" = parsnip::boost_tree(trees          = tune(),
                                         tree_depth     = tune(),
                                         learn_rate     = tune(),
                                         loss_reduction = tune(),
                                         sample_size    = tune(),
                                         mtry           = tune()) %>%
                       parsnip::set_engine("xgboost") %>%
                       parsnip::set_mode("regression"),

         ## -------------------------------------------------------------------------
         ## LightGBM
         ## -------------------------------------------------------------------------

         "lightgbm" = parsnip::boost_tree(trees          = tune(),
                                          tree_depth     = tune(),
                                          learn_rate     = tune(),
                                          loss_reduction = tune(),
                                          sample_size    = tune(),
                                          mtry           = tune()) %>%
                        parsnip::set_engine("lightgbm") %>%
                        parsnip::set_mode("regression"),

         ## -------------------------------------------------------------------------
         ## Elastic Net (Linear Regression)
         ## -------------------------------------------------------------------------

         "elastic_net" = parsnip::linear_reg(penalty  = tune(),
                                             mixture  = tune()) %>%
                          parsnip::set_engine("glmnet") %>%
                          parsnip::set_mode("regression"),

         ## -------------------------------------------------------------------------
         ## Support Vector Machine (RBF Kernel)
         ## -------------------------------------------------------------------------

         "svm_rbf" = parsnip::svm_rbf(cost      = tune(),
                                      rbf_sigma = tune(),
                                      margin    = tune()) %>%
                      parsnip::set_engine("kernlab") %>%
                      parsnip::set_mode("regression"),

         ## -------------------------------------------------------------------------
         ## Multivariate Adaptive Regression Spline (MARS)
         ## -------------------------------------------------------------------------

         "mars" = parsnip::mars(num_terms   = tune(),
                                prod_degree = tune()) %>%
                    parsnip::set_engine("earth") %>%
                    parsnip::set_mode("regression"),

         ## -------------------------------------------------------------------------
         ## MLP Neural Network
         ## -------------------------------------------------------------------------

         "mlp_nn" = parsnip::mlp(penalty      = tune(),
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
