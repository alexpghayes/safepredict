context("test-C5.0")

skip_if_not_installed("C50")
library(C50)

data(churn)

tree <- C5.0(x = churnTrain[, -20], y = churnTrain$churn)
rule <- C5.0(churn ~ ., data = churnTrain, rules = TRUE)

# test arguments

test_that("C5.0 basics - class", {

  expect_silent({
    safe_class_tree <- safe_predict(tree, churnTest)
    safe_class_rule <- safe_predict(rule, churnTest)
  })

  check_predict_basic(safe_class_tree, churnTest)
  check_predict_basic(safe_class_rule, churnTest)

  class_tree <- predict(tree, churnTest)
  class_rule <- predict(rule, churnTest)

  expect_same_content(safe_class_tree, class_tree)
  expect_same_content(safe_class_rule, class_rule)
})

test_that("C5.0 basics - prob", {

  expect_silent({
    safe_prob_tree <- safe_predict(tree, churnTest, type = "prob")
    safe_prob_rule <- safe_predict(rule, churnTest, type = "prob")
  })

  check_predict_basic(safe_prob_tree, churnTest)
  check_predict_basic(safe_prob_rule, churnTest)

  prob_tree <- predict(tree, churnTest, type = "prob")
  prob_rule <- predict(rule, churnTest, type = "prob")

  expect_same_content(safe_prob_tree, prob_tree)
  expect_same_content(safe_prob_rule, prob_rule)
})
