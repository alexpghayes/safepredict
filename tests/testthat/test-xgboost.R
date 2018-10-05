context("test-xgboost")

skip_if_not_installed("xgboost")
library(xgboost)

data("agaricus.train")
data("agaricus.test")

reg <- xgboost(
  data = agaricus.train$data,   # matrix
  label = agaricus.train$label, # response (numeric)
  nrounds = 50,
  objective = "binary:logistic",
  verbose = FALSE
)

test_that("xgboost basics - binary", {
  expect_silent(safe_pred <- safe_predict(reg, agaricus.test$data))
  pred <- predict(reg, agaricus.test$data)
  expect_same_content(safe_pred, pred)
})
