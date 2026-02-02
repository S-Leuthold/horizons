#' Define a Parsnip Model Specification
#'
#' @description
#' Maps a short model name (from `VALID_MODELS`) to a tunable parsnip model
#' specification. Each spec has `tune()` placeholders for hyperparameters
#' and the correct engine set. Used by `evaluate_single_config()` to build
#' the workflow for each config.
#'
#' @param model Character. One of the short names in `VALID_MODELS`:
#'   "rf", "cubist", "xgboost", "plsr", "elastic_net", "svm_rbf",
#'   "mars", "lightgbm", "mlp".
#'
#' @return A `parsnip` model specification with `mode = "regression"`.
#' @keywords internal
#' @export
define_model_spec <- function(model) {

  switch(model,

    "rf" = parsnip::rand_forest(
      trees = 500,
      mtry  = tune::tune(),
      min_n = tune::tune()
    ) %>%
      parsnip::set_engine("ranger", importance = "impurity") %>%
      parsnip::set_mode("regression"),

    "cubist" = parsnip::cubist_rules(
      committees = tune::tune(),
      neighbors  = tune::tune(),
      max_rules  = tune::tune()
    ) %>%
      parsnip::set_engine("Cubist") %>%
      parsnip::set_mode("regression"),

    "xgboost" = parsnip::boost_tree(
      trees          = 500,
      tree_depth     = tune::tune(),
      learn_rate     = tune::tune(),
      mtry           = tune::tune(),
      min_n          = tune::tune(),
      loss_reduction = tune::tune(),
      sample_size    = tune::tune()
    ) %>%
      parsnip::set_engine("xgboost") %>%
      parsnip::set_mode("regression"),

    "lightgbm" = parsnip::boost_tree(
      trees      = 500,
      tree_depth = tune::tune(),
      learn_rate = tune::tune(),
      mtry       = tune::tune(),
      min_n      = tune::tune()
    ) %>%
      parsnip::set_engine("lightgbm") %>%
      parsnip::set_mode("regression"),

    "elastic_net" = parsnip::linear_reg(
      penalty = tune::tune(),
      mixture = tune::tune()
    ) %>%
      parsnip::set_engine("glmnet") %>%
      parsnip::set_mode("regression"),

    "svm_rbf" = parsnip::svm_rbf(
      cost      = tune::tune(),
      rbf_sigma = tune::tune()
    ) %>%
      parsnip::set_engine("kernlab") %>%
      parsnip::set_mode("regression"),

    "mars" = parsnip::mars(
      prod_degree = tune::tune(),
      num_terms   = tune::tune()
    ) %>%
      parsnip::set_engine("earth") %>%
      parsnip::set_mode("regression"),

    "plsr" = parsnip::pls(
      num_comp       = tune::tune(),
      predictor_prop = 1
    ) %>%
      parsnip::set_engine("mixOmics") %>%
      parsnip::set_mode("regression"),

    "mlp" = parsnip::mlp(
      hidden_units = tune::tune(),
      penalty      = tune::tune(),
      epochs       = tune::tune()
    ) %>%
      parsnip::set_engine("nnet", MaxNWts = 10000) %>%
      parsnip::set_mode("regression"),

    ## Default: unknown model
    rlang::abort(paste0("Unknown model type: '", model, "'. ",
                        "Valid models: ", paste(VALID_MODELS, collapse = ", ")))

  )

}
