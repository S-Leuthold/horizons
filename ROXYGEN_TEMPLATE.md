# horizons Roxygen Documentation Template

## Standard Template for Public Functions

```r
#' Action Verb + Object (What the Function Does)
#'
#' One-sentence summary ending with period. Then a paragraph explaining when/why 
#' you'd use this function and how it fits into the horizons workflow.
#'
#' @param required_param `[type]` Description starting with capital. Must include
#'   any requirements or constraints.
#' @param optional_param `[type]` (Optional) Description. Default: `default_value`.
#' @param choice_param `[character]` Description. Must be one of: `"option1"`, 
#'   `"option2"`, `"option3"`. Default: `"option1"`.
#' @param df_param `[data.frame]` Description. Required columns:
#'   * `Sample_ID`: Unique identifiers
#'   * `Response`: Target variable
#'   * Numeric columns: Wavenumbers (e.g., `"600"`, `"602"`)
#'
#' @return A `[class_name]` object/data.frame/list containing:
#'   * `element1`: Description of first element
#'   * `element2`: Description of second element  
#'   * `metrics`: Data frame with evaluation metrics
#'
#' @section Details:
#' Extended explanation of algorithm or implementation:
#' 1. First step in the process
#' 2. Second step in the process
#' 3. Final step and output
#'
#' @section Warning:
#' Important caveats or limitations:
#' - Memory usage scales with n_samples^2
#' - Requires at least 3 spectral columns
#' - Results depend on random seed
#'
#' @examples
#' # Basic usage with Sam's style
#' build_recipe(input_data              = spectral_data,
#'              spectral_transformation = "snv",
#'              response_transformation = "No Transformation",
#'              feature_selection       = "pca") -> recipe
#' 
#' # With covariates (Sam's right assignment style)
#' build_recipe(input_data              = spectral_data,
#'              spectral_transformation = "deriv1",
#'              response_transformation = "Log Transformation",
#'              feature_selection       = "boruta",
#'              covariate_selection     = c("Clay", "pH"),
#'              covariate_data          = soil_properties) -> recipe_with_covs
#' 
#' \dontrun{
#' # Complex pipeline with Sam's safely_execute style
#' safely_execute(expr = {
#'   build_recipe(input_data              = large_dataset,
#'                spectral_transformation = "snv_deriv1", 
#'                response_transformation = "Square Root Transformation",
#'                feature_selection       = "correlation",
#'                covariate_selection     = all_covariates,
#'                covariate_data          = covariate_df)
#' },
#' default_value = NULL,
#' error_message = "Recipe building failed") -> recipe_safe
#' 
#' recipe <- recipe_safe$result
#' }
#'
#' @seealso
#' * [step_transform_spectra()] for spectral preprocessing details
#' * [create_project_configs()] for batch configuration
#' * `vignette("preprocessing-workflow")` for complete workflow
#'
#' @export
```

## Template for Internal/Helper Functions

```r
#' Helper Function Title (Internal)
#'
#' Brief description of what this internal function does and why it exists.
#'
#' @param param1 `[type]` Description
#' @param param2 `[type]` Description  
#'
#' @return `[type]` Description of return value
#'
#' @examples
#' # Internal use only
#' validate_spectral_data(data = input_data,
#'                        check_response = TRUE) -> validation_result
#'
#' @keywords internal
#' @noRd
```

## Template for S3 Methods (Recipe Steps)

```r
#' Apply Custom Transformation to Recipe
#'
#' Adds a preprocessing step that performs specific transformation.
#' Designed for use in tidymodels recipe pipelines with spectral data.
#'
#' @param recipe `[recipe]` A recipes object to add the step to.
#' @param ... Selector functions for columns (e.g., `all_predictors()`).
#' @param parameter `[type]` Control parameter for transformation. Default: `value`.
#' @param role `[character]` Role for new variables. Default: `"predictor"`.
#' @param trained `[logical]` Has the step been trained? Default: `FALSE`.
#' @param skip `[logical]` Skip in bake()? Default: `FALSE`.
#' @param id `[character]` Step ID. Default: `rand_id("step_name")`.
#'
#' @return An updated `[recipe]` with the new step added.
#'
#' @examples
#' # Add to recipe with Sam's style
#' recipe %>%
#'   step_transform_custom(all_predictors(),
#'                        parameter = 10) %>%
#'   step_normalize(all_predictors()) -> processed_recipe
#'
#' @export
step_transform_custom <- function(recipe,
                                 ...,
                                 parameter = 5,
                                 role      = "predictor", 
                                 trained   = FALSE,
                                 skip      = FALSE,
                                 id        = rand_id("transform_custom")) {
  # Implementation
}
```

## Documentation Style Rules

### 1. Parameter Documentation
- **Always include type**: `[type]` in square brackets
- **Mark optional params**: Use "(Optional)" after type
- **List requirements**: For data frames, list required columns with bullets
- **Document defaults**: Always state "Default: `value`"

### 2. Return Documentation  
- **State the type**: Start with "A `[type]`..."
- **Use bullets**: For complex returns with multiple elements
- **Be specific**: Don't just say "a list" - explain what's in it

### 3. Examples Section (Sam's Style)
```r
@examples
# Use Sam's alignment style - align equals signs
function_name(param1     = value1,
              param2     = value2,
              long_param = value3) -> result

# Use right assignment for emphasis
complex_operation() %>%
  process_step1() %>%
  process_step2() -> final_result

# No line break between function and first arg  
safely_execute(expr = {operation()},
               default_value = NULL) -> safe_result
```

### 4. Sections to Include

#### Always include:
- **Title**: Action verb + object
- **Description**: Summary + context paragraph  
- **@param**: All parameters with types
- **@return**: Clear return value description
- **@examples**: At least one working example

#### Include when relevant:
- **@section Details**: For complex algorithms
- **@section Warning**: For important caveats
- **@section Note**: For tips or best practices
- **@seealso**: Related functions and vignettes

#### For exported functions:
- **@export**: Always include for public functions

#### For internal functions:
- **@keywords internal**
- **@noRd**: Prevents Rd file generation

### 5. Common Parameter Types

```r
# Use these standard type annotations:
@param df `[data.frame]` Input data frame
@param x `[numeric]` Numeric vector  
@param n `[integer]` Number of iterations
@param flag `[logical]` Whether to enable feature
@param method `[character]` Method name
@param formula `[formula]` Model formula
@param fn `[function]` Function to apply
@param list_param `[list]` Named list of options
@param ... Additional arguments passed to next function
```

### 6. Cross-References

```r
# Modern style (recommended):
@seealso
* [function_name()] for related functionality
* [package::function()] for external packages
* `vignette("name")` for tutorials

# Legacy style (still works):
@seealso \code{\link{function_name}}, \code{\link[package]{function}}
```

## Complete Example Following Template

```r
#' Evaluate Model Configurations in Parallel
#'
#' Evaluates multiple model configurations using parallel processing. This is the
#' core engine for horizons' comprehensive model comparison workflow, enabling
#' evaluation of thousands of preprocessing and model combinations.
#'
#' @param configs `[data.frame]` Configuration grid with columns:
#'   * `spectral_transformation`: Preprocessing method
#'   * `response_transformation`: Response transformation  
#'   * `feature_selection`: Feature selection method
#'   * `model`: Model type specification
#' @param project_data `[data.frame]` Spectral data with `Sample_ID`, `Response`,
#'   and numeric wavenumber columns.
#' @param n_workers `[integer]` (Optional) Number of parallel workers. 
#'   Default: `parallel::detectCores() - 1`.
#' @param cache_dir `[character]` (Optional) Directory for caching results.
#'   Default: `"cache/"`.
#' @param verbose `[logical]` Print progress messages? Default: `TRUE`.
#'
#' @return A `[list]` containing:
#'   * `results`: Data frame of all model results
#'   * `metrics`: Summary metrics for each configuration
#'   * `best_model`: Configuration with best performance
#'   * `runtime`: Total execution time in minutes
#'
#' @section Details:
#' The evaluation process follows these steps:
#' 1. Validates input data and configurations
#' 2. Sets up parallel backend with specified workers
#' 3. Distributes configurations across workers
#' 4. Evaluates each configuration with cross-validation
#' 5. Collects and aggregates results
#' 6. Identifies best performing configuration
#'
#' @section Warning:
#' - Memory usage scales with n_configs × n_samples
#' - Parallel processing may conflict with nested parallelization
#' - Large configurations (>1000) should use batch processing
#'
#' @examples
#' # Basic evaluation with Sam's style
#' evaluate_models(configs      = model_configs,
#'                 project_data = spectral_data,
#'                 n_workers    = 4,
#'                 verbose      = TRUE) -> evaluation_results
#' 
#' # Extract best model
#' evaluation_results$best_model -> best_config
#' 
#' \dontrun{
#' # Large-scale evaluation with safety wrapper
#' safely_execute(expr = {
#'   evaluate_models(configs      = full_config_grid,
#'                   project_data = all_samples,
#'                   n_workers    = 20,
#'                   cache_dir    = "results/cache",
#'                   verbose      = TRUE)
#' },
#' default_value = NULL,
#' error_message = "Model evaluation failed") -> eval_safe
#' 
#' evaluation_results <- eval_safe$result
#' }
#'
#' @seealso
#' * [create_project_configs()] to generate configuration grids
#' * [build_recipe()] for preprocessing pipeline
#' * `vignette("model-evaluation")` for complete workflow
#'
#' @export
```

## Quick Checklist for Documentation

Before marking documentation complete:

- [ ] Title starts with action verb
- [ ] One-line summary + context paragraph  
- [ ] All parameters have `[type]` annotations
- [ ] Optional parameters marked "(Optional)"
- [ ] Default values documented
- [ ] Return type and structure clear
- [ ] At least one example using Sam's style
- [ ] Related functions in @seealso
- [ ] @export for public functions
- [ ] Warnings/limitations documented if relevant