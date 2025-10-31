#' Parse ID Template String into Parsing Instructions
#'
#' @description
#' Converts a user-friendly template string like `"{project}_{sample_id}_<scan>"`
#' into structured parsing instructions. This is the foundation for simplified
#' input workflows where users can specify ID formats without understanding
#' the underlying parsing machinery.
#'
#' **Template Syntax:**
#' - `{identifier}`: Core sample identifiers (aggregate/group by these)
#' - `<replicate>`: Technical replicates (average across these)
#' - Delimiter: Automatically detected from template (`_`, `-`, `.`, etc.)
#'
#' @param template `[character(1)]` Template string with placeholders
#'   - Identifiers use curly braces: `{project}`, `{sample_id}`
#'   - Replicates use angle brackets: `<scan>`, `<well>`
#'   - Delimiters separate fields: `_`, `-`, `.`
#'
#' @return `[list]` Parsing instructions with components:
#'   - `identifiers`: Character vector of identifier field names (lowercase)
#'   - `replicates`: Character vector of replicate field names (lowercase)
#'   - `delimiter`: Single character delimiter detected from template
#'   - `format_string`: Format string compatible with `parse_filename_metadata()`
#'   - `aggregate_by`: Character vector of identifier names in Proper_Case
#'
#' @details
#' **Case Handling:**
#' - Input placeholders are case-insensitive: `{PROJECT}`, `{project}`, `{Project}` all work
#' - Output `identifiers` and `replicates` are lowercase: `"project"`, `"sample_id"`
#' - Output `aggregate_by` uses Proper_Case: `"Project"`, `"Sample_ID"`
#'
#' **Validation:**
#' - All braces must be properly matched
#' - Placeholders cannot be empty
#' - Must contain at least one placeholder
#' - Delimiters must be consistent (can't mix `_` and `-`)
#' - Field names must start with a letter and contain only alphanumeric + underscore
#'
#' @examples
#' # Simple template with two identifiers
#' parse_id_template("{project}_{sample_id}")
#' # → identifiers: c("project", "sample_id")
#' # → aggregate_by: c("Project", "Sample_ID")
#' # → delimiter: "_"
#'
#' # Template with replicates to average over
#' parse_id_template("{project}_{sample_id}_<scan>_<well>")
#' # → identifiers: c("project", "sample_id")
#' # → replicates: c("scan", "well")
#'
#' # Complex real-world example
#' parse_id_template("{site}_{plot}_{depth}_{fraction}_<rep>")
#' # → identifiers: c("site", "plot", "depth", "fraction")
#' # → replicates: c("rep")
#' # → format_string: "site_plot_depth_fraction_rep"
#'
#' @importFrom cli cli_abort
#' @importFrom glue glue
#'
#' @keywords internal
parse_id_template <- function(template) {

  ## ---------------------------------------------------------------------------
  ## Pre-validation: Check for empty placeholders and mismatched braces
  ## ---------------------------------------------------------------------------

  # Check for empty placeholders before extraction
  if (grepl("\\{\\}", template) || grepl("<>", template)) {
    cli::cli_abort(c(
      "empty placeholder found in template",
      "x" = "Placeholders cannot be empty: {{}} or <>",
      "i" = "Template: {.val {template}}"
    ))
  }

  # Count opening and closing braces
  open_curly <- lengths(regmatches(template, gregexpr("\\{", template)))
  close_curly <- lengths(regmatches(template, gregexpr("\\}", template)))
  open_angle <- lengths(regmatches(template, gregexpr("<", template)))
  close_angle <- lengths(regmatches(template, gregexpr(">", template)))

  if (open_curly != close_curly) {
    cli::cli_abort(c(
      "mismatched curly braces in template",
      "x" = "Found {open_curly} opening {{ but {close_curly} closing }}",
      "i" = "Template: {.val {template}}"
    ))
  }

  if (open_angle != close_angle) {
    cli::cli_abort(c(
      "mismatched angle brackets in template",
      "x" = "Found {open_angle} opening < but {close_angle} closing >",
      "i" = "Template: {.val {template}}"
    ))
  }

  ## ---------------------------------------------------------------------------
  ## Step 1: Extract identifiers from {curly braces}
  ## ---------------------------------------------------------------------------

  # Extract all {identifier} placeholders
  identifier_matches <- gregexpr("\\{([^}]+)\\}", template, perl = TRUE)
  identifier_captures <- regmatches(template, identifier_matches)[[1]]

  if (length(identifier_captures) > 0) {
    # Remove braces and convert to lowercase
    identifiers <- gsub("\\{|\\}", "", identifier_captures)
    identifiers <- tolower(identifiers)
  } else {
    identifiers <- character(0)
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Extract replicates from <angle brackets>
  ## ---------------------------------------------------------------------------

  # Extract all <replicate> placeholders
  replicate_matches <- gregexpr("<([^>]+)>", template, perl = TRUE)
  replicate_captures <- regmatches(template, replicate_matches)[[1]]

  if (length(replicate_captures) > 0) {
    # Remove brackets and convert to lowercase
    replicates <- gsub("<|>", "", replicate_captures)
    replicates <- tolower(replicates)
  } else {
    replicates <- character(0)
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Detect delimiter
  ## ---------------------------------------------------------------------------

  # Remove all placeholders to leave only delimiters
  temp_template <- template
  temp_template <- gsub("\\{[^}]+\\}", "", temp_template)  # Remove {identifiers}
  temp_template <- gsub("<[^>]+>", "", temp_template)      # Remove <replicates>

  # What's left should be delimiters
  if (nchar(temp_template) == 0) {
    # Single field, no delimiter
    delimiter <- ""
  } else {
    # Get unique characters remaining (should all be same delimiter)
    delim_chars <- unique(strsplit(temp_template, "")[[1]])

    if (length(delim_chars) == 1) {
      delimiter <- delim_chars[1]
    } else {
      # Mixed delimiters - will be caught in validation
      # For now, take the first one
      delimiter <- delim_chars[1]
    }
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Build format string
  ## ---------------------------------------------------------------------------

  # Collect all fields with their positions in the template
  all_fields <- list()

  # Add identifiers with positions
  if (length(identifier_captures) > 0) {
    for (i in seq_along(identifier_captures)) {
      pos <- regexpr(identifier_captures[i], template, fixed = TRUE)[1]
      field_name <- identifiers[i]
      all_fields[[length(all_fields) + 1]] <- list(pos = pos, name = field_name)
    }
  }

  # Add replicates with positions
  if (length(replicate_captures) > 0) {
    for (i in seq_along(replicate_captures)) {
      pos <- regexpr(replicate_captures[i], template, fixed = TRUE)[1]
      field_name <- replicates[i]
      all_fields[[length(all_fields) + 1]] <- list(pos = pos, name = field_name)
    }
  }

  # Sort by position to preserve template order
  if (length(all_fields) > 0) {
    positions <- sapply(all_fields, function(x) x$pos)
    field_names <- sapply(all_fields, function(x) x$name)
    sorted_order <- order(positions)
    ordered_fields <- field_names[sorted_order]

    # Remove underscores from field names for format string
    format_fields <- gsub("_", "", ordered_fields)

    # Join with underscore (format_string is canonical representation)
    # The actual delimiter is stored separately in the delimiter field
    format_string <- paste(format_fields, collapse = "_")
  } else {
    format_string <- ""
  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Convert to proper case for aggregate_by
  ## ---------------------------------------------------------------------------

  # Convert identifier field names to Proper_Case for display
  # Examples: "sample_id" → "Sample_ID", "soil_type" → "Soil_Type"

  if (length(identifiers) > 0) {
    aggregate_by <- sapply(identifiers, function(field) {
      # Split on underscores
      parts <- strsplit(field, "_")[[1]]
      # Capitalize first letter of each part
      parts_proper <- sapply(parts, function(part) {
        # Special case: "id" should become "ID" (all caps)
        if (tolower(part) == "id") {
          return("ID")
        }
        # Otherwise, capitalize first letter only
        paste0(toupper(substring(part, 1, 1)), substring(part, 2))
      })
      # Join back with underscores
      paste(parts_proper, collapse = "_")
    }, USE.NAMES = FALSE)
  } else {
    aggregate_by <- character(0)
  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Validate template
  ## ---------------------------------------------------------------------------

  ## Check 1: Must have at least one placeholder --------------------------------

  if (length(identifiers) == 0 && length(replicates) == 0) {
    cli::cli_abort(c(
      "Template must contain at least one placeholder",
      "x" = "Template provided: {.val {template}}",
      "i" = "Use {{field_name}} for identifiers or <field_name> for replicates",
      "i" = "Example: {{project}}_{{sample_id}}"
    ))
  }

  ## Check 2: Must have at least one identifier (for aggregation) --------------

  if (length(identifiers) == 0) {
    cli::cli_abort(c(
      "Template must contain at least one identifier field {{...}}",
      "x" = "Template only contains replicate fields: {.val {template}}",
      "i" = "Identifiers define sample grouping, replicates are averaged within groups",
      "i" = "Example: {{sample_id}}_<scan>_<well>"
    ))
  }

  ## Check 3: Validate placeholder names ---------------------------------------

  all_field_names <- c(identifiers, replicates)

  for (field in all_field_names) {
    # Check for invalid characters (only alphanumeric and underscore allowed)
    if (grepl("[^a-z0-9_]", field)) {
      cli::cli_abort(c(
        "invalid characters in placeholder name: {.val {field}}",
        "x" = "Placeholder names can only contain letters, numbers, and underscores",
        "i" = "Template: {.val {template}}",
        "i" = "Invalid field: {.val {field}}"
      ))
    }

    # Check that field doesn't start with a number
    if (grepl("^[0-9]", field)) {
      cli::cli_abort(c(
        "invalid placeholder name: {.val {field}}",
        "x" = "Field names must start with a letter",
        "i" = "Template: {.val {template}}",
        "i" = "Invalid field: {.val {field}}"
      ))
    }
  }

  ## Check 4: Validate delimiter consistency (if multiple delimiters) ---------

  temp_template <- template
  temp_template <- gsub("\\{[^}]+\\}", "", temp_template)
  temp_template <- gsub("<[^>]+>", "", temp_template)

  if (nchar(temp_template) > 0) {
    delim_chars <- unique(strsplit(temp_template, "")[[1]])
    if (length(delim_chars) > 1) {
      cli::cli_abort(c(
        "Template uses mixed delimiters",
        "x" = "Found multiple delimiter characters: {.val {delim_chars}}",
        "i" = "Template: {.val {template}}",
        "i" = "Please use consistent delimiters throughout (e.g., all _ or all -)"
      ))
    }
  }

  ## Check 5: Warning if no sample_id field ------------------------------------

  if (!"sample_id" %in% identifiers) {
    cli::cli_warn(c(
      "Template does not include a {{sample_id}} field",
      "i" = "A composite Sample_ID will be created by combining: {.val {identifiers}}",
      "i" = "If this is not intended, include {{sample_id}} in your template"
    ))
  }

  ## ---------------------------------------------------------------------------
  ## Return structured parsing instructions
  ## ---------------------------------------------------------------------------

  list(
    identifiers   = identifiers,
    replicates    = replicates,
    delimiter     = delimiter,
    format_string = format_string,
    aggregate_by  = aggregate_by
  )

}
