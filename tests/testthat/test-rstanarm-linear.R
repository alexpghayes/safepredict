# context("test-rstanarm-linear")
#
# skip_if_not_installed("rstanarm")
# library(rstanarm)
#
# set.seed(27)
#
# unwanted_messages <- capture.output({
#
#   reg <- stan_lm(
#     mpg ~ .,
#     data = mtcars,
#     prior = R2(0.5),
#     chains = 1,
#     iter = 500
#   )
#
#   binary <- stan_glm(
#     I(Species == "setosa") ~ .,
#     iris,
#     family = "binomial",
#     chains = 1,
#     iter = 500
#   )
# })
#
# test_that("rstanarm basics - linear regression", {
#   expect_silent(safe_pred <- safe_predict(reg, mtcars))
#   check_predict_basic(safe_pred, mtcars)
#   pred <- predict(reg, mtcars)
#   expect_same_content(safe_pred, pred)
# })
#
# test_that("rstanarm basics - binary probabilities", {
#   expect_silent(safe_pred <- safe_predict(binary, iris))
#   check_predict_basic(safe_pred, iris)
#   pred <- predict(binary, iris, type = "response")
#   expect_same_content(safe_pred, pred)
# })
