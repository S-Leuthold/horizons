#' Define a Parsnip Model Specification for Soil Spectral Modeling
#'
#' Constructs a tunable `parsnip` model specification for one of several supported
#' regression algorithms commonly used in soil spectroscopy and ensemble modeling.
#' Each specification includes `tune()` placeholders for relevant hyperparameters,
#' and is intended for use in `workflow()` objects or within model grids.
#'
#' @param model_type Character. One of the following canonical model slugs:
#'   \itemize{
#'     \item{\code{"random_forest"}}
#'     \item{\code{"cubist"}}
#'     \item{\code{"xgboost"}}
#'     \item{\code{"lightgbm"}}
#'     \item{\code{"elastic_net"}}
#'     \item{\code{"svm_rbf"}}
#'     \item{\code{"mars"}}
#'     \item{\code{"plsr"}}
#'     \item{\code{"mlp_nn"}}
#'   }
#'
#' @return A `parsnip` model specification object with tunable hyperparameters.
#'
#' @details
#' This function standardizes model setup across workflows. Each model spec is returned
#' with regression mode enabled, an appropriate modeling engine selected, and all key
#' hyperparameters marked as tunable. Designed for compatibility with `tune_grid()`,
#' `workflowsets::workflow_set()`, and `stacks::stacks()`.
#'
#' @seealso
#'   \code{\link[parsnip]{set_engine}},
#'   \code{\link[tune]{tune}},
#'   \code{\link{build_model_grid}}
#'
#' @examples
#' \dontrun{
#' define_model_specifications("random_forest")
#' define_model_specifications("plsr")
#' define_model_specifications("mlp_nn")
#' }
#'
#' @importFrom parsnip rand_forest boost_tree linear_reg svm_rbf mars mlp pls cubist_rules set_engine set_mode
#' @importFrom tune tune
#' @importFrom cli cli_abort
#' @import plsmod
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
