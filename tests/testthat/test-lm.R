context("test-lm")

library(modeltests)

fit <- lm(hp ~ ., mtcars)

test_that("arguments", {
  check_predict_arguments(safe_predict.lm)
})

test_that("predicts", {
  check_predict(
    predict_method = safe_predict.lm,
    object = fit,
    new_data = mtcars,
    outcome = "hp"
  )
})
