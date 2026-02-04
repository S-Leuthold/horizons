# Error Messaging Pattern

Standard pattern for tree-style error output in horizons.

## Structure

```text
! [Header message in sentence case]
   ├─ [Error 1]
   ├─ [Error 2]
   └─ [Final error]
```

## Key Elements

1. **Header line**: Bold, red, starts with `!`
2. **No blank line** between header and tree
3. **3-space indent** before tree characters
4. **Tree characters**: `├─` for intermediate items, `└─` for final item
5. **Sentence case** for all messages
6. **Field markup**: Use `{.field name}` for column/field names (renders styled in ANSI terminals)

## Implementation Pattern

```r
# Collect errors as formatted strings
errors <- character()

if (some_check_fails) {
  errors <- c(errors, cli::format_inline("Sentence case message with {.field field_name}"))
}

# Report if errors exist
if (length(errors) > 0) {

  cat(cli::col_red(cli::style_bold("! Header message:\n")))

  for (i in seq_along(errors)) {

    branch <- if (i < length(errors)) "\u251C\u2500" else "\u2514\u2500"
    cat(cli::col_red(paste0("   ", branch, " ", errors[i], "\n")))

  }

  cat("\n")
  rlang::abort(
    paste(c("Header message:", errors), collapse = "\n"),
    class = "horizons_specific_error"
  )

}
```

## Why This Pattern

- **`cli::format_inline()`**: Pre-processes glue + cli markup where variables are in scope
- **`cat()` over `cli_text()`**: Preserves leading whitespace for indentation
- **Errors in abort message**: Makes errors testable with `expect_error()`
- **Custom error class**: Allows programmatic catching (e.g., `horizons_validation_error`)

## Unicode Characters

- `\u251C` = ├ (branch)
- `\u2500` = ─ (horizontal line)
- `\u2514` = └ (corner)
- `\u2502` = │ (vertical line, if needed)

## Style Guidelines

- Sentence case (capitalize first word only)
- No period at end of error messages
- Use `{.field name}` for: column names, slot names, argument names
- Keep messages concise - details can go in the listed values
- When listing values (e.g., duplicate IDs), limit to first few with "..." if many

## Messaging Contract (capture vs render)

**The tree IS the messaging system.** Every warning, error, and info message
that fires during a pipeline verb appears *inside* the tree, not as bare R
warnings floating in the console.

### Two layers

1. **Capture layer** (internal utilities: `safely_execute()`,
   `back_transform_predictions()`, metric functions):
   - Run silently
   - Capture errors/warnings/messages as structured data
   - Return them — never print to console
   - Use `warning(..., call. = FALSE)` for edge-case alerts (catchable,
     quiet, no styling)

2. **Render layer** (pipeline verbs: `evaluate()`, `validate()`,
   `configure()`, `fit()`):
   - Owns ALL console output via `cat()` + box-drawing characters
   - Reads structured data from capture layer
   - Renders warnings/errors/progress *within* the tree
   - Uses `abort_nested()` for fatal errors (tree + `rlang::abort()`)

### Example: evaluate() config loop

```text
│
├─ Config 1/24: rf + log + snv + pca
│  ├─ Tuning: 10-fold CV, grid = 10...
│  │  └─ Best RRMSE: 12.3 (mtry = 15, min_n = 5)
│  ├─ Refit on full training set...
│  │  ⚠ Very large values in log-scale predictions (>50)
│  └─ Test-set metrics: RPD = 2.1, R² = 0.89
│
├─ Config 2/24: cubist + sqrt + raw + none
│  ├─ Tuning: 10-fold CV, grid = 10...
│  └─ ✗ FAILED: convergence failure
│
└─ Complete: 22/24 configs succeeded
```

### safely_execute() calling convention

```r
# In evaluate() config loop — SILENT capture:
safe_result <- safely_execute(
  fit_and_tune(workflow, folds, grid),
  default_value      = NULL,
  log_error          = FALSE,          # tree handles all output
  capture_conditions = TRUE            # capture warnings for tree
)

# In standalone/debugging context — self-logging OK:
safe_result <- safely_execute(
  risky_op(data),
  error_message = "Failed to process {property}",
  log_error     = TRUE                 # emit warning for interactive use
)
```
