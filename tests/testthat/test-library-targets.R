## =============================================================================
## Test: Library Target Handling (ILR, Bounds, Constraints)
## =============================================================================

library(testthat)
library(horizons)

## =============================================================================
## Helper: Create Synthetic Texture Data
## =============================================================================

make_test_texture <- function(n = 10, seed = 123) {

  set.seed(seed)

  ## Generate realistic texture values (g/kg)
  ## Constrain to realistic ranges for soil texture classes
  sand <- runif(n, min = 150, max = 550)  # 15-55% sand
  clay <- runif(n, min = 100, max = 400)  # 10-40% clay
  silt <- 1000 - sand - clay              # Force sum to 1000

  ## Check for invalid compositions (negative silt)
  while (any(silt < 50)) {  # Minimum 5% silt for realism

    neg_idx <- silt < 50

    ## Regenerate problematic samples
    sand[neg_idx] <- runif(sum(neg_idx), min = 150, max = 450)
    clay[neg_idx] <- runif(sum(neg_idx), min = 100, max = 350)
    silt[neg_idx] <- 1000 - sand[neg_idx] - clay[neg_idx]
  }

  ## Verify all compositions are valid
  stopifnot(all(sand + silt + clay == 1000))
  stopifnot(all(sand >= 0 & silt >= 0 & clay >= 0))

  tibble::tibble(
    sand = sand,
    silt = silt,
    clay = clay
  )
}

## =============================================================================
## TEST GROUP 1: ILR Transformation
## =============================================================================

test_that("texture_to_ilr returns correct structure", {

  ## Create test data
  texture <- make_test_texture(n = 5)

  ## Transform
  ilr_coords <- horizons:::texture_to_ilr(
    sand = texture$sand,
    silt = texture$silt,
    clay = texture$clay,
    as_proportions = FALSE
  )

  ## Check structure
  expect_type(ilr_coords, "double")
  expect_equal(ncol(ilr_coords), 2)
  expect_equal(nrow(ilr_coords), 5)
  expect_equal(colnames(ilr_coords), c("ilr_1", "ilr_2"))

  ## All values should be finite (not NA, not Inf)
  expect_true(all(is.finite(ilr_coords)))
})

test_that("texture_to_ilr handles proportions correctly", {

  ## Test with proportions (sum to 1.0)
  sand_prop <- c(0.34, 0.28, 0.42)
  silt_prop <- c(0.38, 0.45, 0.31)
  clay_prop <- c(0.28, 0.27, 0.27)

  ## Transform with as_proportions = TRUE
  ilr_coords <- horizons:::texture_to_ilr(
    sand = sand_prop,
    silt = silt_prop,
    clay = clay_prop,
    as_proportions = TRUE
  )

  ## Should work without conversion
  expect_equal(ncol(ilr_coords), 2)
  expect_equal(nrow(ilr_coords), 3)
  expect_true(all(is.finite(ilr_coords)))
})

test_that("texture_to_ilr handles g/kg conversion", {

  ## Test with g/kg (sum to 1000)
  sand_gkg <- c(340, 280, 420)
  silt_gkg <- c(380, 450, 310)
  clay_gkg <- c(280, 270, 270)

  ## Transform with as_proportions = FALSE (default)
  ilr_coords <- horizons:::texture_to_ilr(
    sand = sand_gkg,
    silt = silt_gkg,
    clay = clay_gkg,
    as_proportions = FALSE
  )

  ## Should automatically convert to proportions
  expect_equal(ncol(ilr_coords), 2)
  expect_equal(nrow(ilr_coords), 3)
  expect_true(all(is.finite(ilr_coords)))
})

## =============================================================================
## TEST GROUP 2: Inverse ILR Transformation
## =============================================================================

test_that("ilr_to_texture returns correct structure", {

  ## Create arbitrary ILR coordinates
  ilr_1 <- c(0.15, -0.22, 0.08)
  ilr_2 <- c(0.33, 0.41, -0.12)

  ## Back-transform
  texture <- horizons:::ilr_to_texture(
    ilr_1 = ilr_1,
    ilr_2 = ilr_2,
    as_gkg = TRUE
  )

  ## Check structure
  expect_s3_class(texture, "tbl_df")
  expect_equal(ncol(texture), 3)
  expect_equal(nrow(texture), 3)
  expect_true(all(c("sand", "silt", "clay") %in% names(texture)))

  ## All values should be positive and finite
  expect_true(all(texture$sand >= 0))
  expect_true(all(texture$silt >= 0))
  expect_true(all(texture$clay >= 0))
  expect_true(all(is.finite(texture$sand)))
  expect_true(all(is.finite(texture$silt)))
  expect_true(all(is.finite(texture$clay)))
})

test_that("ilr_to_texture guarantees mass balance", {

  ## Create various ILR coordinates (including extreme values)
  ilr_1 <- c(0.5, -0.8, 0.0, 1.2, -0.3)
  ilr_2 <- c(0.1, 0.9, -0.5, 0.3, -0.7)

  ## Back-transform to g/kg
  texture <- horizons:::ilr_to_texture(ilr_1, ilr_2, as_gkg = TRUE)

  ## Check mass balance (should sum to exactly 1000)
  total <- texture$sand + texture$silt + texture$clay

  expect_equal(total, rep(1000, 5), tolerance = 1e-10)
})

test_that("ilr_to_texture can return proportions", {

  ## Create ILR coordinates
  ilr_1 <- c(0.2, -0.1)
  ilr_2 <- c(0.3, 0.4)

  ## Back-transform to proportions
  texture <- horizons:::ilr_to_texture(ilr_1, ilr_2, as_gkg = FALSE)

  ## Should sum to 1.0 (not 1000)
  total <- texture$sand + texture$silt + texture$clay

  expect_equal(total, c(1.0, 1.0), tolerance = 1e-10)

  ## Values should be between 0 and 1
  expect_true(all(texture$sand >= 0 & texture$sand <= 1))
  expect_true(all(texture$silt >= 0 & texture$silt <= 1))
  expect_true(all(texture$clay >= 0 & texture$clay <= 1))
})

## =============================================================================
## TEST GROUP 3: ILR Invertibility
## =============================================================================

test_that("ilr_to_texture is inverse of texture_to_ilr", {

  ## Create original texture data
  texture_orig <- make_test_texture(n = 20)

  ## Forward transform
  ilr_coords <- horizons:::texture_to_ilr(
    sand = texture_orig$sand,
    silt = texture_orig$silt,
    clay = texture_orig$clay,
    as_proportions = FALSE
  )

  ## Back transform
  texture_back <- horizons:::ilr_to_texture(
    ilr_1 = ilr_coords[, 1],
    ilr_2 = ilr_coords[, 2],
    as_gkg = TRUE
  )

  ## Should recover original values (within numerical tolerance)
  expect_equal(texture_back$sand, texture_orig$sand, tolerance = 0.01)
  expect_equal(texture_back$silt, texture_orig$silt, tolerance = 0.01)
  expect_equal(texture_back$clay, texture_orig$clay, tolerance = 0.01)
})

test_that("texture_to_ilr is inverse of ilr_to_texture", {

  ## Create original ILR coordinates
  ilr_1_orig <- c(0.25, -0.15, 0.42, -0.33, 0.08)
  ilr_2_orig <- c(0.18, 0.52, -0.28, 0.11, -0.41)

  ## Back-transform
  texture <- horizons:::ilr_to_texture(ilr_1_orig, ilr_2_orig, as_gkg = FALSE)

  ## Forward transform
  ilr_coords <- horizons:::texture_to_ilr(
    sand = texture$sand,
    silt = texture$silt,
    clay = texture$clay,
    as_proportions = TRUE
  )

  ## Should recover original ILR coordinates
  expect_equal(ilr_coords[, 1], ilr_1_orig, tolerance = 1e-10)
  expect_equal(ilr_coords[, 2], ilr_2_orig, tolerance = 1e-10)
})

## =============================================================================
## TEST GROUP 4: Edge Cases
## =============================================================================

test_that("ILR handles single sample correctly", {

  ## Single texture sample
  sand <- 340
  silt <- 380
  clay <- 280

  ## Forward
  ilr_coords <- horizons:::texture_to_ilr(sand, silt, clay)

  expect_equal(nrow(ilr_coords), 1)
  expect_equal(ncol(ilr_coords), 2)

  ## Back
  texture <- horizons:::ilr_to_texture(ilr_coords[1, 1], ilr_coords[1, 2])

  expect_equal(nrow(texture), 1)

  ## Sum should be exactly 1000
  total <- as.numeric(texture$sand + texture$silt + texture$clay)
  expect_equal(total, 1000, tolerance = 1e-10)
})

test_that("ILR handles extreme compositions", {

  ## Very sandy soil (85% sand)
  sand_1 <- 850
  silt_1 <- 100
  clay_1 <- 50

  ## Very clayey soil (60% clay)
  sand_2 <- 200
  silt_2 <- 200
  clay_2 <- 600

  ## Transform both
  ilr_coords <- horizons:::texture_to_ilr(
    sand = c(sand_1, sand_2),
    silt = c(silt_1, silt_2),
    clay = c(clay_1, clay_2)
  )

  ## Should handle without error
  expect_true(all(is.finite(ilr_coords)))

  ## Back-transform
  texture <- horizons:::ilr_to_texture(ilr_coords[, 1], ilr_coords[, 2])

  ## Should preserve extreme values
  expect_equal(texture$sand[1], sand_1, tolerance = 0.01)
  expect_equal(texture$clay[2], clay_2, tolerance = 0.01)
})

test_that("ILR handles equal proportions (loam)", {

  ## Perfect loam: 33.3% each
  sand <- c(333, 334, 333)
  silt <- c(333, 333, 334)
  clay <- c(334, 333, 333)

  ## Transform
  ilr_coords <- horizons:::texture_to_ilr(sand, silt, clay)

  ## Should work (coordinates near zero)
  expect_true(all(is.finite(ilr_coords)))

  ## Back-transform should be close to original
  texture <- horizons:::ilr_to_texture(ilr_coords[, 1], ilr_coords[, 2])

  total <- texture$sand + texture$silt + texture$clay
  expect_equal(total, c(1000, 1000, 1000), tolerance = 1e-8)
})

## =============================================================================
## TEST GROUP 5: Property Classification Helpers
## =============================================================================

test_that("is_compositional_property identifies texture correctly", {

  expect_true(horizons:::is_compositional_property("sand"))
  expect_true(horizons:::is_compositional_property("silt"))
  expect_true(horizons:::is_compositional_property("clay"))

  expect_false(horizons:::is_compositional_property("ph"))
  expect_false(horizons:::is_compositional_property("oc"))
  expect_false(horizons:::is_compositional_property("total_carbon"))
})

test_that("is_texture_property identifies texture correctly", {

  expect_true(horizons:::is_texture_property("sand"))
  expect_true(horizons:::is_texture_property("silt"))
  expect_true(horizons:::is_texture_property("clay"))

  expect_false(horizons:::is_texture_property("ph"))
  expect_false(horizons:::is_texture_property("sodium"))
})

## =============================================================================
## TEST GROUP 6: Bounds Enforcement
## =============================================================================

test_that("apply_bounds clips pH to [0, 14]", {

  ## Test various pH values including out-of-bounds
  ph_vals <- c(-2, 0, 3.5, 7, 10, 14, 16, 20)

  bounded <- horizons:::apply_bounds(ph_vals, "ph", verbose = FALSE)

  ## Check clipping
  expect_equal(bounded, c(0, 0, 3.5, 7, 10, 14, 14, 14))

  ## No values should be outside [0, 14]
  expect_true(all(bounded >= 0))
  expect_true(all(bounded <= 14))
})

test_that("apply_bounds enforces non-negativity for carbon", {

  ## Test organic carbon with negative values
  oc_vals <- c(-5, -0.1, 0, 5, 10, 50)

  bounded <- horizons:::apply_bounds(oc_vals, "oc", verbose = FALSE)

  ## Check non-negativity
  expect_equal(bounded, c(0, 0, 0, 5, 10, 50))
  expect_true(all(bounded >= 0))
})

test_that("apply_bounds enforces non-negativity for nitrogen", {

  n_vals <- c(-1, 0, 2, 8)

  bounded <- horizons:::apply_bounds(n_vals, "total_nitrogen", verbose = FALSE)

  expect_equal(bounded, c(0, 0, 2, 8))
  expect_true(all(bounded >= 0))
})

test_that("apply_bounds doesn't modify properties without constraints", {

  ## CEC has no explicit bounds (can be high)
  cec_vals <- c(5, 20, 50, 100)

  bounded <- horizons:::apply_bounds(cec_vals, "cec", verbose = FALSE)

  ## Should be unchanged
  expect_equal(bounded, cec_vals)
})

test_that("apply_bounds handles NA values correctly", {

  ph_vals <- c(5, NA, 15, 8, NA)

  bounded <- horizons:::apply_bounds(ph_vals, "ph", verbose = FALSE)

  ## NAs should remain NA
  expect_true(is.na(bounded[2]))
  expect_true(is.na(bounded[5]))

  ## Non-NA values should be clipped
  expect_equal(bounded[c(1, 3, 4)], c(5, 14, 8))
})

## =============================================================================
