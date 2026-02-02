#' [One-line title in sentence case]
#'
#' @description
#' [2-3 sentence description of what this function does and why it exists.
#' Written in active voice. Focus on the "what" at a high level.]
#'
#' @details
#' [Deeper explanation for those who want to understand the internals.
#' Design decisions, caveats, edge cases, relationship to other functions.
#' This is where scientific/technical depth lives. Multiple paragraphs are
#' encouraged for complex functions.]
#'
#' @param param_name [Type.] Description of parameter. Default: `value`.
#' @param another_param [Type.] Description. Default: `NULL`.
#'
#' @return [Type.] Description of what's returned and in what state.
#'
#' @seealso [related_function()] for validation, [other_function()] for usage.
#'
#' @noRd
function_name <- function(param_name, another_param = NULL) {
  # Implementation
}


## -----------------------------------------------------------------------------
## Template Notes
## -----------------------------------------------------------------------------
##
## This template is for INTERNAL functions (not exported). Key points:
##
## 1. @description: The "what" - accessible to anyone skimming the code
##    - 2-3 sentences max
##    - Active voice ("Builds..." not "This function builds...")
##    - Focus on purpose, not implementation
##
## 2. @details: The "why/how" - for those diving deep
##    - Design decisions and rationale
##    - Edge cases and how they're handled
##    - Relationship to other functions in the system
##    - Caveats or assumptions
##    - Multiple paragraphs encouraged for complex functions
##
## 3. @param: Document every parameter
##    - Start with type in brackets: [Type.]
##    - Include default value if non-NULL: Default: `value`.
##    - Be specific about valid values/constraints
##
## 4. @return: What comes back
##    - Include type
##    - Note any modifications to input (e.g., "validated and unchanged")
##    - Note side effects if any
##
## 5. @seealso: Connect the dots
##    - Link to validator if this is a constructor
##    - Link to user-facing wrapper if this is internal
##    - Link to related functions in the pipeline
##
## 6. @noRd: Keeps function internal (no man page generated)
##    - Use for all internal functions
##    - Remove if function becomes exported
##
## Style:
##    - Professional but accessible prose
##    - No jargon without explanation
##    - Assume reader is a scientist who codes, not a software engineer
##
## -----------------------------------------------------------------------------
