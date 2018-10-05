context("test-randomforest")

skip_if_not_installed("randomForest")
library(randomForest)

reg <- randomForest(mpg ~ ., mtcars)
multi <- randomForest(Species ~ ., iris)

test_that("randomForest basics - regression", {
  expect_silent(safe_pred <- safe_predict(reg, mtcars))
  check_predict_basic(safe_pred, mtcars)
  pred <- predict(reg, mtcars)
  expect_same_content(safe_pred, pred)
})

test_that("randomForest basics - multiclass", {
  expect_silent(safe_pred <- safe_predict(multi, iris))
  check_predict_basic(safe_pred, iris)
  pred <- predict(multi, iris)
  expect_same_content(safe_pred, pred)
})
