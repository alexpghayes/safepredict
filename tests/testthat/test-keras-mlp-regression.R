# context("test-keras-mlp-regression")
#
#
# test_that('keras regression prediction', {
#
#   skip_if_not_installed("keras")
#
#   xy_fit <- parsnip::fit_xy(
#     mlp(mode = "regression", hidden_units = 2, epochs = 500, penalty = .1),
#     x = mtcars[, c("cyl", "disp")],
#     y = mtcars$mpg,
#     engine = "keras",
#     control = ctrl
#   )
#
#   xy_pred <- predict(xy_fit$fit, x = as.matrix(mtcars[1:8, c("cyl", "disp")]))[,1]
#   expect_equal(xy_pred, predict_num(xy_fit, new_data = mtcars[1:8, c("cyl", "disp")]))
#
#   keras::backend()$clear_session()
#
#   form_fit <- parsnip::fit(
#     car_basic,
#     mpg ~ .,
#     data = mtcars[, c("cyl", "disp", "mpg")],,
#     engine = "keras",
#     control = ctrl
#   )
#
#   form_pred <- predict(form_fit$fit, x = as.matrix(mtcars[1:8, c("cyl", "disp")]))[,1]
#   expect_equal(form_pred, predict_num(form_fit, new_data = mtcars[1:8, c("cyl", "disp")]))
#
#   keras::backend()$clear_session()
# })
