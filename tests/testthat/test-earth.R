context("test-earth")

skip_if_not_installed("earth")
library(earth)

reg <- earth(mpg ~ ., data = mtcars)
binary <- earth(
  I(Species == "setosa") ~ Sepal.Length,
  iris[1:55, ],
  glm = list(family = "binomial")
)
multi <- earth(formula = Species ~ ., data = iris)

test_that("earth basics - regression", {
  expect_silent(safe_pred <- safe_predict(reg, mtcars))
  check_predict_basic(safe_pred, mtcars)
  pred <- predict(reg, mtcars)
  expect_same_content(safe_pred, pred)
})

test_that("earth basics - binary", {
  expect_silent(safe_pred <- safe_predict(binary, iris))
  check_predict_basic(safe_pred, iris)
  pred <- predict(binary, iris)
  expect_same_content(safe_pred, pred)
})

test_that("earth basics - multiclass", {
  expect_silent(safe_pred <- safe_predict(multi, iris))
  check_predict_basic(safe_pred, iris)
  pred <- predict(multi, iris)
  expect_same_content(safe_pred, pred)
})
