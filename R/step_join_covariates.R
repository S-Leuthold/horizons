#' @title Add Scaled Covariates to Recipe Dataset
#' @description Adds scaled covariates to a recipe dataset via left join using
#'   a shared Sample ID column. Scales covariate data during `prep()` and joins
#'   it to the dataset at `bake()` time.
#'
#' @param recipe A `recipe` object.
#' @param covariate_data A data frame of covariates to be joined.
#' @param role Character. Variable role for new covariates (default: "predictor").
#' @param trained Logical. Has the step been trained? (required by `recipes`).
#' @param skip Logical. Should this step be skipped when baking?
#' @param id Character. Unique step identifier.
#' @param sample_id_column Optional. Name of sample ID column. If NULL, will attempt
#'   to detect it based on variables with role = "id".
#'
#' @return A `step_add_covariates` step added to the recipe.
#'
#' @importFrom recipes step add_step rand_id bake prep
#' @importFrom dplyr select mutate across everything bind_cols relocate left_join filter pull
#' @importFrom glue glue
#' @importFrom rlang %||%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' recipe(data = soil_data) %>%
#'   update_role(Sample_ID, new_role = "id") %>%
#'   step_add_covariates(covariate_data = predicted_covs)
#' }
# ------------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Step 1: User-facing step function
## -----------------------------------------------------------------------------

step_add_covariates <- function(recipe,
                                covariate_data,
                                role             = "predictor",
                                trained          = FALSE,
                                skip             = FALSE,
                                id               = recipes::rand_id("add_covariates"),
                                sample_id_column = NULL) {

  sample_id_column <- rlang::as_string(rlang::ensym(sample_id_column))

  recipes::add_step(
    recipe,
    step_add_covariates_new(
      covariate_data     = covariate_data,
      role               = role,
      trained            = trained,
      skip               = skip,
      id                 = id,
      sample_id_column   = sample_id_column
    )
  )
}

## -----------------------------------------------------------------------------
## Step 2: Constructor
## -----------------------------------------------------------------------------

#' @export

step_add_covariates_new <- function(covariate_data,
                                    role,
                                    trained,
                                    skip,
                                    id,
                                    sample_id_column = NULL,
                                    terms = NULL) {
  recipes::step(
    subclass         = "add_covariates",
    covariate_data   = covariate_data,
    role             = role,
    trained          = trained,
    skip             = skip,
    id               = id,
    sample_id_column = sample_id_column,
    terms            = terms
  )
}

## -----------------------------------------------------------------------------
## Step 3: prep() method
## -----------------------------------------------------------------------------

#' @export
prep.step_add_covariates <- function(x, training, info = NULL, ...) {

  if (is.null(info)) {
    cli::cli_abort("The `info` argument was not provided during prep. This may indicate the recipe step is being used outside of the expected context.")
  }

  sid <- x$sample_id_column

  if (is.null(sid)) {
    id_vars <- dplyr::filter(info, role == "id") %>% dplyr::pull(variable)
    if (length(id_vars) != 1) {
      cli::cli_abort("Specify `sample_id_column`, or ensure exactly one variable has role = 'id'.")
    }
    sid <- id_vars[1]
  }

  if (!sid %in% names(training)) {
    cli::cli_abort("ID column `{sid}` not found in training data.")
  }

  if (!sid %in% names(x$covariate_data)) {
    cli::cli_abort("ID column `{sid}` not found in covariate data.")
  }

  scaled_covs <- x$covariate_data %>%
    dplyr::select(-dplyr::all_of(sid)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.numeric(scale(.x))))

  scaled_covs[[sid]] <- x$covariate_data[[sid]]
  scaled_covs <- dplyr::relocate(scaled_covs, dplyr::all_of(sid))

  step_add_covariates_new(
    covariate_data     = scaled_covs,
    role               = x$role,
    trained            = TRUE,
    skip               = x$skip,
    id                 = x$id,
    sample_id_column   = sid,
    terms              = NULL
  )
}

## -----------------------------------------------------------------------------
## Step 4: bake() method
## -----------------------------------------------------------------------------

#' @export
bake.step_add_covariates <- function(object, new_data, ...) {

  id_col <- object$sample_id_column %||% attr(object, "sample_id_column")

  if (is.null(id_col) || !id_col %in% names(new_data)) {
    cli::cli_abort("ID column `{id_col}` not found in `new_data`.")
  }

  joined <- dplyr::left_join(new_data,
                             object$covariate_data,
                             by = id_col)

  return(joined)
}

## -----------------------------------------------------------------------------
## Step 5: print() method
## -----------------------------------------------------------------------------

#' @export
print.step_add_covariates <- function(x,
                                      width = max(20, options()$width - 30),
                                      ...) {
  id_col <- x$sample_id_column %||% "unknown"
  cat(glue::glue("Covariate join via `{id_col}` ({ncol(x$covariate_data) - 1} columns)\n"))
  invisible(x)
}
