test_that("step_add_covariates integrates covariates and scales them", {
  skip_if_not_installed("recipes")

  library(recipes)

  # Training data with an ID column and a simple predictor
  train <- data.frame(
    Sample_ID = c("A", "B", "C", "D"),
    x = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  # Covariate data to be joined; will be scaled during prep
  covs <- data.frame(
    Sample_ID = c("A", "B", "C", "D"),
    cov1 = c(10, 12, 14, 16),
    cov2 = c(1, 1, 2, 3),
    stringsAsFactors = FALSE
  )

  rec <- recipe(x ~ ., data = train) %>%
    update_role(Sample_ID, new_role = "id") %>%
    horizons::step_add_covariates(covariate_data = covs)

  rec_trained <- prep(rec)
  baked <- bake(rec_trained, new_data = train)

  # Columns added
  expect_true(all(c("cov1", "cov2") %in% names(baked)))

  # Scaling check: joined covariates should have approx mean 0 and sd 1
  # Compute over rows where covariates are present (all in this case)
  expect_equal(round(mean(baked$cov1), 6), 0, tolerance = 1e-6)
  expect_equal(round(mean(baked$cov2), 6), 0, tolerance = 1e-6)
  # sd may have small numerical drift; check close to 1
  expect_true(abs(sd(baked$cov1) - 1) < 1e-6)
  expect_true(abs(sd(baked$cov2) - 1) < 1e-6)
})

test_that("step_add_covariates works with explicit sample_id_column", {
  skip_if_not_installed("recipes")

  library(recipes)

  train <- data.frame(id = c("a", "b"), x = c(1, 2), stringsAsFactors = FALSE)
  covs  <- data.frame(id = c("a", "b"), z = c(5, 7), stringsAsFactors = FALSE)

  rec <- recipe(x ~ ., data = train) %>%
    horizons::step_add_covariates(covariate_data = covs, sample_id_column = "id")

  out <- bake(prep(rec), new_data = train)
  expect_true("z" %in% names(out))
})

test_that("prep.step_add_covariates errors when info is missing", {
  train <- data.frame(Sample_ID = c("A", "B"), x = c(1, 2), stringsAsFactors = FALSE)
  covs  <- data.frame(Sample_ID = c("A", "B"), z = c(5, 7), stringsAsFactors = FALSE)

  step <- horizons::step_add_covariates_new(
    covariate_data = covs,
    role = "predictor",
    trained = FALSE,
    skip = FALSE,
    id = "add_covariates",
    sample_id_column = "Sample_ID"
  )

  expect_error(
    horizons:::prep.step_add_covariates(step, training = train, info = NULL),
    "info.*not provided"
  )
})

test_that("prep errors when ID role is not uniquely specified", {
  skip_if_not_installed("recipes")

  library(recipes)
  train <- data.frame(Sample_ID = c("A", "B"), Other_ID = c(1, 2), x = c(1, 2), stringsAsFactors = FALSE)
  covs  <- data.frame(Sample_ID = c("A", "B"), z = c(5, 7), stringsAsFactors = FALSE)

  # No id role -> ambiguous/missing id detection
  rec <- recipe(x ~ ., data = train) %>%
    horizons::step_add_covariates(covariate_data = covs)

  expect_error(prep(rec), "Specify `sample_id_column`|
                           ensure exactly one variable has role = 'id'",
               fixed = FALSE)

  # Two id roles -> also ambiguous
  rec2 <- recipe(x ~ ., data = train) %>%
    update_role(Sample_ID, new_role = "id") %>%
    update_role(Other_ID, new_role = "id") %>%
    horizons::step_add_covariates(covariate_data = covs)

  expect_error(prep(rec2), "exactly one variable has role = 'id'", fixed = FALSE)
})

test_that("prep errors when ID is missing in training or covariate data", {
  skip_if_not_installed("recipes")

  library(recipes)
  train <- data.frame(Sample_ID = c("A", "B"), x = c(1, 2), stringsAsFactors = FALSE)
  covs_wrong <- data.frame(ID = c("A", "B"), z = c(5, 7), stringsAsFactors = FALSE)

  # Missing in covariate data
  rec <- recipe(x ~ ., data = train) %>%
    update_role(Sample_ID, new_role = "id") %>%
    horizons::step_add_covariates(covariate_data = covs_wrong)

  expect_error(prep(rec), "ID column `\\{?Sample_ID\\}?` not found in covariate data", fixed = FALSE)

  # Missing in training data (explicit sample_id_column)
  train_noid <- data.frame(id2 = c("A", "B"), x = c(1, 2), stringsAsFactors = FALSE)
  covs <- data.frame(id = c("A", "B"), z = c(5, 7), stringsAsFactors = FALSE)

  rec2 <- recipe(x ~ ., data = train_noid) %>%
    horizons::step_add_covariates(covariate_data = covs, sample_id_column = "id")

  expect_error(prep(rec2), "ID column `id` not found in training data", fixed = FALSE)
})

test_that("bake errors when ID column missing in new_data", {
  skip_if_not_installed("recipes")

  library(recipes)
  train <- data.frame(Sample_ID = c("A", "B"), x = c(1, 2), stringsAsFactors = FALSE)
  covs  <- data.frame(Sample_ID = c("A", "B"), z = c(5, 7), stringsAsFactors = FALSE)

  rec <- recipe(x ~ ., data = train) %>%
    update_role(Sample_ID, new_role = "id") %>%
    horizons::step_add_covariates(covariate_data = covs)

  rec_trained <- prep(rec)

  new_data <- tibble::tibble(x = c(3, 4))
  expect_error(bake(rec_trained, new_data = new_data), 'required columns.*Sample_ID', fixed = FALSE)
})

test_that("print method summarizes covariates", {
  skip_if_not_installed("recipes")

  library(recipes)
  train <- tibble::tibble(Sample_ID = c("A", "B"), x = c(1, 2))
  covs  <- tibble::tibble(Sample_ID = c("A", "B"), z1 = c(5, 7), z2 = c(1, 0))

  rec <- recipe(x ~ ., data = train) %>%
    update_role(Sample_ID, new_role = "id") %>%
    horizons::step_add_covariates(covariate_data = covs)

  rec_trained <- prep(rec)

  expect_output(print(rec_trained$steps[[1]]), "Covariate join via`? `?Sample_ID`? ")
})
