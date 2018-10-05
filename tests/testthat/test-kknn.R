context("test-kknn")

skip_if_not_installed("kknn")
library(kknn)

reg <- train.kknn(mpg ~ ., data = mtcars)
multi <- train.kknn(Species ~ ., data = iris)

test_that("knn basics - regression", {
  expect_silent(safe_pred <- safe_predict(reg, mtcars))
  check_predict_basic(safe_pred, mtcars)
  pred <- predict(reg, mtcars, type = "raw")
  expect_same_content(safe_pred, pred)
})

test_that("kknn basics - classes", {
  expect_silent(safe_pred <- safe_predict(multi, iris))
  check_predict_basic(safe_pred, iris)
  pred <- predict(multi, iris)
  expect_same_content(safe_pred, pred)
})

test_that("kknn basics - probabilities", {
  expect_silent(safe_pred <- safe_predict(multi, iris, type = "prob"))
  check_predict_basic(safe_pred, iris)
  pred <- predict(multi, iris, type = "prob")
  expect_same_content(safe_pred, pred)
})
