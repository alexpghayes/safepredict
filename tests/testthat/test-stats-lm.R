context("test-lm")

library(modeltests)

fit <- lm(hp ~ ., mtcars)

test_that("stats::lm() basics", {
  expect_silent(safe_pred <- safe_predict(fit, mtcars))
  check_predict_basic(safe_pred, mtcars)
  pred <- predict(fit, mtcars)
  expect_same_content(safe_pred, pred)
})

test_that("stats::lm() experimental tests", {
  check_predict(
    predict_method = safe_predict.lm,
    object = fit,
    new_data = mtcars,
    outcome = "hp"
  )
})
