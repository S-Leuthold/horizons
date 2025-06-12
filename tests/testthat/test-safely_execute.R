test_that("safely_execute returns value when no error", {
  res <- horizons:::safely_execute({1 + 1})
  expect_equal(res, 2)
})

test_that("safely_execute can return result list", {
  res <- horizons:::safely_execute({1 + 1}, return_result_list = TRUE)
  expect_equal(res$result, 2)
  expect_null(res$error)
})

test_that("safely_execute returns default and error on failure", {
  res <- horizons:::safely_execute({stop("oops")}, default_value = NA,
                                    log_error = FALSE, return_result_list = TRUE,
                                    capture_trace = TRUE)
  expect_true(inherits(res$error, "error"))
  expect_true(is.na(res$result))
  expect_false(is.null(res$trace))
})
