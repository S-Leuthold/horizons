#' Add Scaled Covariates to a Recipe Dataset
#'
#' Adds scaled covariate columns to a recipe via left join using a common sample ID column.
#' Covariates are standardized (mean = 0, SD = 1) during `prep()` and joined to the recipe
#' data during `bake()`. Useful for integrating precomputed covariates (e.g., from external
#' models or databases) into tidymodels workflows.
#'
#' @param recipe A `recipes::recipe` object.
#' @param covariate_data A data frame of covariates to join. Must include an identifier column for merging.
#' @param role Character string indicating the role for the covariates. Default is `"predictor"`.
#' @param trained Logical. Has the step been trained? Required for step methods.
#' @param skip Logical. Should the step be skipped when baking? Default is FALSE.
#' @param id Character string. Unique identifier for the step.
#' @param sample_id_column Optional. Name of the ID column used for joining. If NULL, attempts to detect a variable with role = `"id"`.
#'
#' @return A `step_add_covariates` step to be added to a `recipe` pipeline.
#'
#' @examples
#' \dontrun{
#' recipe(data = soil_data) %>%
#'   update_role(Sample_ID, new_role = "id") %>%
#'   step_add_covariates(covariate_data = predicted_covs)
#' }
#'
#' @seealso
#'   \code{\link[recipes]{recipe}}, \code{\link[recipes]{prep}}, \code{\link[recipes]{bake}}
#'
#' @importFrom cli cli_abort
#' @importFrom dplyr filter pull mutate across select relocate left_join all_of everything
#' @importFrom glue glue
#' @importFrom recipes add_step step prep bake rand_id
#' @importFrom rlang as_string ensym
#'
#' @export

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

  ## Convert sample_id_column to string if provided ------------------------------

  if (!is.null(sample_id_column)) {
    sample_id_column <- rlang::as_string(rlang::ensym(sample_id_column))
  }

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

  ## Standardize covariates (mean = 0, SD = 1) for modeling -----------------------
  ## Excludes ID column, then re-adds it at the front after scaling

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
