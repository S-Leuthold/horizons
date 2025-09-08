# Back-Transformation Analysis Report: horizons Package

## Executive Summary

**CRITICAL FINDING**: The horizons package has a significant bug in its prediction pipeline. Models using response transformations (log, sqrt, Box-Cox) are producing predictions in the **transformed scale** rather than the original scale, but metrics are being calculated as if predictions are in the original scale. This affects model evaluation, comparison, and ensemble stacking.

## 1. Current State Assessment

### What's Actually Happening

With `skip = TRUE` in the transformation steps (as configured in lines 136-141 of `models-recipes.R`):

1. **During Training (prep)**: The response variable IS transformed
   - Log transformation: `y` becomes `log(y)`
   - Sqrt transformation: `y` becomes `sqrt(y)`
   - Models are trained on transformed response

2. **During Prediction (bake/predict)**: 
   - The transformation is SKIPPED (due to `skip = TRUE`)
   - Models produce predictions in the **transformed scale**
   - No automatic back-transformation occurs

3. **During Evaluation**:
   - Metrics compare transformed predictions to original-scale outcomes
   - This produces incorrect metric values
   - Model comparisons are invalid

### Empirical Evidence

Our test revealed:
- Original data y range: `[1.97, 23.42]`
- Log model predictions: `[1.22, 3.16]` (clearly in log scale)
- Sqrt model predictions: `[1.03, 5.17]` (clearly in sqrt scale)
- No-transform predictions: `[-2.39, 23.66]` (in original scale)

## 2. Tidymodels Behavior Explained

### Why `skip = TRUE` is Used

The `skip = TRUE` parameter is necessary for outcome transformations because:
- With `skip = FALSE`, tidymodels would try to transform the outcome in new data
- At prediction time, we don't have the outcome variable
- This would cause an error: "The following required column is missing from new_data: y"

### What Tidymodels Does NOT Do

Contrary to what might be expected:
- Tidymodels does **NOT** automatically back-transform predictions
- The `skip` parameter only controls whether the step runs during `bake()`
- There is no built-in mechanism to track and invert transformations for predictions

### The Intended Workflow

The correct tidymodels approach requires manual back-transformation:
```r
# After getting predictions
predictions <- predict(workflow, new_data)
# Must manually back-transform based on the transformation used
if (used_log_transform) {
  predictions$.pred <- exp(predictions$.pred)
}
```

## 3. Impact Analysis

### Affected Components

1. **Model Evaluation** (`evaluation-core.R`, lines 725-740)
   - RMSE, MAE, and other metrics are calculated incorrectly
   - Transformed models appear to perform much better than they actually do
   - Example: A log-transformed model with RMSE of 0.5 in log scale appears better than RMSE of 10 in original scale

2. **Model Comparison**
   - Models with different transformations cannot be fairly compared
   - The "best" model selection is likely incorrect
   - Transformation choice affects apparent performance more than actual model quality

3. **Ensemble Stacking** (`evaluation-ensemble.R`, lines 260-270)
   - **CRITICAL**: The ensemble is blending predictions on different scales
   - Log models contribute values ~[0, 4]
   - Sqrt models contribute values ~[1, 10]  
   - Non-transformed models contribute values ~[0, 100]
   - The weighted average is mathematically meaningless
   - Stacking coefficients are optimized for the wrong objective

4. **Cross-Validation** (`evaluation-finalize.R`, lines 620-650)
   - CV predictions saved for stacking are in transformed scale
   - CV metrics are calculated incorrectly
   - Model selection based on CV is flawed

### Severity Assessment

This is a **CRITICAL BUG** that:
- Invalidates all model comparisons involving transformations
- Produces incorrect performance metrics
- Makes ensemble predictions unreliable
- Could lead to wrong scientific conclusions

## 4. Historical Context

### Evidence of Awareness

The codebase shows the author was aware of this issue:

1. **Unused Function** (`models-specifications.R`, lines 359-370)
   ```r
   back_transform_predictions <- function(predictions, transformation) {
     # Function exists but is never called
   }
   ```

2. **Archived Solution** (`archive/model_backtransform_tune_results.R`)
   - Contains a function specifically for back-transforming tune_results
   - Designed for stacked ensembles
   - Was removed/archived rather than integrated

3. **Function Documentation**
   - Comments mention "predictions must be on the original scale" for stacking
   - Suggests understanding of the requirement

### Likely Scenario

The author likely:
1. Discovered the issue during development
2. Wrote functions to fix it
3. Encountered integration challenges
4. Archived the solution rather than fully implementing it
5. Left the bug unresolved

## 5. Solution Recommendations

### Recommended Approach: Minimal Disruption Fix

Keep `skip = TRUE` (required for tidymodels) but add back-transformation at key points:

#### Option 1: Transform at Prediction Points (RECOMMENDED)

Modify prediction collection points to back-transform immediately:

```r
# In evaluation-core.R after line 735
last_fit_result <- tune::last_fit(...)
# Add back-transformation
last_fit_result <- back_transform_tune_results(last_fit_result, config_clean$response_transformation)

# In evaluation-finalize.R after line 634
cv_fit <- tune::fit_resamples(...)
# Add back-transformation
cv_fit <- back_transform_tune_results(cv_fit, response_transformation)
```

#### Option 2: Workflow Post-Processor

Add a post-processing step to workflows:

```r
# Create a custom predict method that auto-back-transforms
finalized_workflow <- finalized_workflow %>%
  add_post_processor(back_transform_predictions, transformation_type)
```

#### Option 3: Recipe Step with Inverse

Create a custom recipe step that tracks its inverse:

```r
step_log_invertible <- function(...) {
  step_log(..., skip = TRUE) %>%
  attr("inverse") <- exp
}
```

### Implementation Priority

1. **Immediate**: Fix ensemble stacking predictions
2. **High**: Fix evaluation metrics calculation  
3. **Medium**: Fix CV predictions for model selection
4. **Low**: Add validation tests to prevent regression

## 6. Specific Code Changes Needed

### File: `R/evaluation-core.R`

After line 740, add:
```r
# Back-transform predictions if transformation was applied
if (config_clean$response_transformation != "No Transformation") {
  final_fit_result$result <- back_transform_last_fit(
    final_fit_result$result,
    config_clean$response_transformation
  )
}
```

### File: `R/evaluation-finalize.R`

After line 650, add:
```r
# Back-transform CV predictions for correct metrics and stacking
if (response_transformation != "No Transformation") {
  cv_fit <- back_transform_fit_resamples(
    cv_fit,
    response_transformation
  )
}
```

### File: `R/evaluation-ensemble.R`

Before line 266, add:
```r
# Ensure predictions are in original scale
if (!is.null(current_model$transformation)) {
  current_model$cv_predictions <- back_transform_cv_predictions(
    current_model$cv_predictions,
    current_model$transformation
  )
}
```

### New File: `R/utils-backtransform.R`

Create utility functions:
```r
back_transform_predictions <- function(predictions, transformation) {
  switch(transformation,
    "Log Transformation" = exp(predictions),
    "Square Root Transformation" = predictions^2,
    "Box-Cox Transformation" = stop("Box-Cox requires lambda parameter"),
    predictions
  )
}

back_transform_last_fit <- function(last_fit_obj, transformation) {
  # Implementation to transform .pred column in collected predictions
}

back_transform_fit_resamples <- function(fit_obj, transformation) {
  # Implementation to transform predictions in each fold
}
```

## 7. Risks and Edge Cases

### Risks

1. **Box-Cox Transformation**: Requires storing lambda parameter for inverse
2. **Negative Predictions**: Sqrt back-transformation always produces positive values
3. **Existing Results**: All previous analyses need re-evaluation
4. **Performance**: Back-transformation adds computational overhead

### Edge Cases to Handle

1. **Missing Predictions**: Check for NULL before transformation
2. **Infinite Values**: Log of 0 produces -Inf, need bounds
3. **Mixed Workflows**: Some models may not have transformations
4. **Nested Lists**: CV predictions are nested structures

### Validation Requirements

1. Add unit tests for each transformation type
2. Verify metrics match manual calculations
3. Ensure ensemble predictions are in correct scale
4. Test with edge cases (zeros, negative values, missing data)

## 8. Conclusion

This is a critical bug that affects the core functionality of the package. The issue stems from a misunderstanding of how tidymodels handles response transformations with `skip = TRUE`. While the transformations are correctly applied during training, the predictions remain in the transformed scale, invalidating all downstream analyses.

The fix is straightforward but requires careful implementation at multiple points in the pipeline. The existence of archived back-transformation code suggests this issue was discovered but not fully resolved. 

**Immediate action is required** to:
1. Implement back-transformation at all prediction points
2. Re-evaluate all existing model results
3. Add comprehensive tests to prevent regression
4. Document the transformation behavior clearly

Without this fix, the package produces scientifically invalid results when using response transformations.