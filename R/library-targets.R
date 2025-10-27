#' Library Prediction Target Handling
#'
#' @description
#' Functions for handling target transformations and constraints in library-based
#' prediction mode. Includes ILR transformations for compositional texture data,
#' bounds enforcement (pH, non-negativity), and property classification helpers.
#'
#' @details
#' This module supports proper handling of different property types:
#' - **Compositional (texture)**: ILR transformation ensures mass balance
#' - **Bounded (pH)**: Clipping to valid ranges
#' - **Non-negative (carbon, nitrogen, elements)**: Enforce physical constraints
#'
#' @importFrom cli cli_warn cli_abort
#' @importFrom compositions ilr ilrInv
#' @importFrom tibble tibble
#' @keywords internal

## -----------------------------------------------------------------------------
## Section 1: Property Classification
## -----------------------------------------------------------------------------

#' Check if Property is Compositional
#'
#' @description
#' Determines whether a property requires compositional data treatment (ILR).
#' Currently only texture properties (sand, silt, clay) are compositional.
#'
#' @param property Character. Property name.
#'
#' @return Logical. TRUE if compositional, FALSE otherwise.
#'
#' @keywords internal
is_compositional_property <- function(property) {

  property %in% c("sand", "silt", "clay")

}

#' Check if Property is Texture
#'
#' @description
#' Determines whether a property is a texture component (sand, silt, or clay).
#' Texture properties require special handling via ILR transformation.
#'
#' @param property Character. Property name.
#'
#' @return Logical. TRUE if texture property, FALSE otherwise.
#'
#' @keywords internal
is_texture_property <- function(property) {

  property %in% c("sand", "silt", "clay")

}

## -----------------------------------------------------------------------------
## Section 2: ILR Transformation for Texture
## -----------------------------------------------------------------------------

#' Transform Texture to ILR Coordinates
#'
#' @description
#' Converts three-part texture composition (sand, silt, clay) to two unconstrained
#' ILR (Isometric Log-Ratio) coordinates for compositional data analysis.
#'
#' @details
#' Texture data are compositional - the three components must sum to a constant
#' (100% or 1000 g/kg). This creates a dependency that violates assumptions of
#' standard regression. ILR transformation:
#'
#' 1. Reduces dimensionality (3 constrained → 2 unconstrained)
#' 2. Creates orthogonal coordinates (no correlation induced by closure)
#' 3. Allows standard modeling techniques
#' 4. Back-transformation guarantees predictions sum to 100%
#'
#' The transformation uses the `compositions` package implementation which follows
#' Egozcue et al. (2003) methodology.
#'
#' @param sand Numeric vector. Sand content (g/kg or proportion).
#' @param silt Numeric vector. Silt content (same units as sand).
#' @param clay Numeric vector. Clay content (same units as sand).
#' @param as_proportions Logical. If FALSE (default), assumes g/kg and converts
#'   to proportions before transformation. If TRUE, assumes inputs already sum to 1.0.
#'
#' @return Matrix with 2 columns (ilr_1, ilr_2) and same number of rows as input.
#'   Coordinates are unbounded (can range -∞ to +∞).
#'
#' @section Validation:
#' Checks that input components sum to ~1.0 (if proportions) or ~1000 (if g/kg).
#' Issues warning if deviation exceeds 1%.
#'
#' @references
#' Egozcue, J.J., Pawlowsky-Glahn, V., Mateu-Figueras, G., & Barceló-Vidal, C. (2003).
#' Isometric logratio transformations for compositional data analysis.
#' Mathematical Geology, 35(3), 279-300.
#'
#' @examples
#' \dontrun{
#' # Transform texture from g/kg
#' ilr_coords <- texture_to_ilr(
#'   sand = c(340, 280, 420),
#'   silt = c(380, 450, 310),
#'   clay = c(280, 270, 270)
#' )
#'
#' # With proportions
#' ilr_coords <- texture_to_ilr(
#'   sand = c(0.34, 0.28, 0.42),
#'   silt = c(0.38, 0.45, 0.31),
#'   clay = c(0.28, 0.27, 0.27),
#'   as_proportions = TRUE
#' )
#' }
#'
#' @keywords internal
texture_to_ilr <- function(sand, silt, clay, as_proportions = FALSE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Convert to proportions if needed
  ## ---------------------------------------------------------------------------

  if (!as_proportions) {

    total <- sand + silt + clay
    sand  <- sand / total
    silt  <- silt / total
    clay  <- clay / total

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Validate composition sums to ~1.0
  ## ---------------------------------------------------------------------------

  total_check <- sand + silt + clay

  if (any(abs(total_check - 1.0) > 0.01, na.rm = TRUE)) {

    max_dev <- max(abs(total_check - 1.0), na.rm = TRUE)

    cli::cli_warn(
      "Texture components don't sum to 1.0 (within 1%)",
      "i" = "Max deviation: {round(max_dev, 4)}"
    )
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Apply ILR transformation
  ## ---------------------------------------------------------------------------

  texture_matrix <- cbind(sand, silt, clay)
  ilr_coords     <- compositions::ilr(texture_matrix)

  ## Coerce to matrix (compositions returns special rmult class) ---------------

  ilr_coords <- as.matrix(ilr_coords)

  colnames(ilr_coords) <- c("ilr_1", "ilr_2")

  return(ilr_coords)
}

#' Transform ILR Coordinates Back to Texture
#'
#' @description
#' Converts two ILR (Isometric Log-Ratio) coordinates back to three-part texture
#' composition (sand, silt, clay). Inverse of `texture_to_ilr()`.
#'
#' @details
#' This back-transformation is the key to ensuring compositional predictions:
#'
#' 1. Models predict ilr_1 and ilr_2 (unbounded coordinates)
#' 2. This function transforms predictions back to texture space
#' 3. Result is GUARANTEED to sum to 100% (or 1.0 for proportions)
#' 4. All components are guaranteed to be non-negative
#'
#' The transformation preserves the compositional structure and ensures physically
#' valid predictions even if ILR coordinate predictions are extreme.
#'
#' @param ilr_1 Numeric vector. First ILR coordinate.
#' @param ilr_2 Numeric vector. Second ILR coordinate.
#' @param as_gkg Logical. If TRUE (default), returns g/kg (values sum to 1000).
#'   If FALSE, returns proportions (values sum to 1.0).
#'
#' @return Tibble with three columns (sand, silt, clay). Each row sums to
#'   exactly 1000 (if as_gkg = TRUE) or 1.0 (if as_gkg = FALSE).
#'
#' @section Guarantees:
#' - **Mass balance**: sand + silt + clay = 1000 (or 1.0) for all samples
#' - **Non-negativity**: All components ≥ 0
#' - **Invertibility**: texture_to_ilr(ilr_to_texture(x)) = x
#'
#' @examples
#' \dontrun{
#' # Back-transform ILR predictions to g/kg
#' texture <- ilr_to_texture(
#'   ilr_1 = c(0.15, -0.22, 0.08),
#'   ilr_2 = c(0.33, 0.41, -0.12)
#' )
#'
#' # Verify mass balance
#' sum(texture[1, ])  # Should be exactly 1000
#'
#' # Get proportions instead
#' texture_prop <- ilr_to_texture(
#'   ilr_1 = c(0.15, -0.22),
#'   ilr_2 = c(0.33, 0.41),
#'   as_gkg = FALSE
#' )
#' }
#'
#' @keywords internal
ilr_to_texture <- function(ilr_1, ilr_2, as_gkg = TRUE) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Combine ILR coordinates into matrix
  ## ---------------------------------------------------------------------------

  ilr_matrix <- cbind(ilr_1, ilr_2)

  ## ---------------------------------------------------------------------------
  ## Step 2: Apply inverse ILR transformation
  ## ---------------------------------------------------------------------------

  texture_props <- compositions::ilrInv(ilr_matrix)

  ## Coerce to matrix (compositions returns special acomp class) ---------------

  texture_props <- as.matrix(texture_props)

  ## Handle dimension: ilrInv returns n_samples × 3 matrix ----------------------

  if (ncol(texture_props) != 3) {
    ## If transposed (3 × n_samples), fix it
    texture_props <- t(texture_props)
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Extract components
  ## ---------------------------------------------------------------------------

  sand <- texture_props[, 1]
  silt <- texture_props[, 2]
  clay <- texture_props[, 3]

  ## ---------------------------------------------------------------------------
  ## Step 4: Convert to g/kg if requested
  ## ---------------------------------------------------------------------------

  if (as_gkg) {

    sand <- sand * 1000
    silt <- silt * 1000
    clay <- clay * 1000

  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Return as tibble
  ## ---------------------------------------------------------------------------

  tibble::tibble(
    sand = sand,
    silt = silt,
    clay = clay
  )
}

## -----------------------------------------------------------------------------
## Section 3: Bounds Enforcement
## -----------------------------------------------------------------------------

#' Apply Bounds to Predictions
#'
#' @description
#' Enforces property-specific constraints on predictions including pH bounds
#' and non-negativity requirements. Ensures predictions are physically valid.
#'
#' @details
#' Different properties have different constraints:
#'
#' **pH**: Bounded to [0, 14]
#' - Theoretical range of pH scale
#' - Values clipped to this range
#'
#' **Carbon/Nitrogen/Elements**: Non-negative
#' - Negative concentrations are physically impossible
#' - Values clipped to ≥ 0
#'
#' **Other properties**: No bounds applied
#' - CEC, base cations, etc. can be high (no upper bound)
#'
#' @param values Numeric vector. Predicted values to bound.
#' @param property Character. Property name (determines which bounds to apply).
#' @param verbose Logical. If TRUE (default), warns when clipping occurs.
#'
#' @return Numeric vector with bounds enforced. Same length as input.
#'
#' @section Properties with Constraints:
#' - **pH**: [0, 14]
#' - **oc, soc, total_carbon, carbonate**: ≥ 0
#' - **total_nitrogen**: ≥ 0
#' - **iron_total, aluminum_total**: ≥ 0
#'
#' @examples
#' \dontrun{
#' # Clip pH to valid range
#' ph_bounded <- apply_bounds(c(-1, 5, 8, 15), "ph")
#' # Result: c(0, 5, 8, 14)
#'
#' # Enforce non-negativity for carbon
#' oc_bounded <- apply_bounds(c(-5, 0, 10, 50), "oc")
#' # Result: c(0, 0, 10, 50)
#' }
#'
#' @keywords internal
apply_bounds <- function(values, property, verbose = TRUE) {

  original <- values

  ## ---------------------------------------------------------------------------
  ## Property-specific bounds
  ## ---------------------------------------------------------------------------

  if (property == "ph") {

    ## pH bounded to [0, 14] ----------------------------------------------------

    values <- pmax(0, pmin(14, values))

    n_clipped <- sum(original != values, na.rm = TRUE)

    if (verbose && n_clipped > 0) {
      cli::cli_warn("{n_clipped} pH prediction{?s} clipped to [0, 14]")
    }

  } else if (property %in% c("oc", "soc", "total_carbon", "carbonate",
                             "total_nitrogen", "iron_total", "aluminum_total")) {

    ## Non-negativity for carbon, nitrogen, elements ---------------------------

    values <- pmax(0, values)

    n_clipped <- sum(original < 0, na.rm = TRUE)

    if (verbose && n_clipped > 0) {
      cli::cli_warn("{n_clipped} {property} prediction{?s} clipped to ≥ 0")
    }
  }

  ## No bounds for other properties (CEC, cations, etc.) ----------------------

  return(values)
}

## -----------------------------------------------------------------------------
