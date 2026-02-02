test_that("rrmse_vec calculates correct relative RMSE", {
  data <- data.frame(truth = c(1, 2, 3), estimate = c(1, 2, 4))
  expected <- sqrt(mean((data$truth - data$estimate)^2)) / mean(data$truth) * 100
  result <- rrmse_vec(data, truth, estimate)
  expect_equal(result$.estimate, expected)
})

test_that("rrmse_vec handles NA values with na_rm = TRUE", {
  data <- data.frame(truth = c(1, 2, NA, 4), estimate = c(1.1, 1.9, 3, 4.2))
  result <- rrmse_vec(data, truth, estimate, na_rm = TRUE)
  filtered <- na.omit(data)
  expected <- sqrt(mean((filtered$truth - filtered$estimate)^2)) /
    mean(filtered$truth) * 100
  expect_equal(result$.estimate, expected)
})
