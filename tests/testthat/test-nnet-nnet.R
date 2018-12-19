# context("test-nnet-nnet")
#
# skip_if_not_installed("nnet")
# library(nnet)
#
# reg <- nnet(mpg ~ ., data = mtcars, size = 50, linout = TRUE, trace = FALSE)
# binary <- nnet(I(Species == "setosa") ~ ., iris, size = 50, trace = FALSE)
# multi <- nnet(formula = Species ~ ., data = iris, size = 50, trace = FALSE)
#
# test_that("nnet basics - regression", {
#   expect_silent(safe_pred <- safe_predict(reg, mtcars))
#   check_predict_basic(safe_pred, mtcars)
#   pred <- predict(reg, mtcars, type = "raw")
#   expect_same_content(safe_pred, pred)
# })
#
# test_that("nnet basics - binary", {
#   expect_silent(safe_pred <- safe_predict(binary, iris))
#   check_predict_basic(safe_pred, iris)
#   pred <- predict(binary, iris)
#   expect_same_content(safe_pred, pred)
# })
#
# test_that("nnet basics - multiclass", {
#   expect_silent(safe_pred <- safe_predict(multi, iris))
#   check_predict_basic(safe_pred, iris)
#   pred <- predict(multi, iris)
#   expect_same_content(safe_pred, pred)
# })
