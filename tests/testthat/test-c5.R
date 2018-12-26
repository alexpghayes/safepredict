context("test-C5.0")

skip_if_not_installed("C50")
library(C50)

data(churn)

tree <- C5.0(x = churnTrain[, -20], y = churnTrain$churn)
rule <- C5.0(churn ~ ., data = churnTrain, rules = TRUE)

predict(tree, churnTest, type = "prob")

test_that("function signature", {
  check_safepredict_signature(safe_predict.C5.0)
})

test_that("input validation", {

  expect_error(
    safe_predict(tree),
    "argument \"new_data\" is missing, with no default"
  )

  expect_error(
    safe_predict(tree, churnTest, type = "infinite fun space"),
    "`type` should be one of: \"prob\" or \"class\""
  )

  expect_error(
    safe_predict(tree, churnTest, type = "prob", threshold = 2),
    "`threshold` must be a vector with one element strictly between 0 and 1."
  )

  expect_warning(
    safe_predict(tree, churnTest, type = "conf_int", levl = 0.2),
    "Some components of ... were not used: levl"
  )

  expect_warning(
    safe_predict(tree, churnTest, std.dev = TRUE),
    "Some components of ... were not used: std.dev"
  )
})

## checks on returned predictions

test_that("default type", {
  default_preds <- safe_predict(tree, churnTest)
  check_predict_output(default_preds, churnTest, type = "prob")
})

test_that("type = \"prob\"", {
  check_predict(safe_predict.C5.0, tree, churnTest, "churn", type = "prob")
  check_predict(safe_predict.C5.0, rule, churnTest, "churn", type = "prob")
})

test_that("type = \"class\"", {
  check_predict(safe_predict.C5.0, tree, churnTest, "churn", type = "class")
  check_predict(safe_predict.C5.0, rule, churnTest, "churn", type = "class")
})
