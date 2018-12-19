context("test-stats-loess")

fit <- loess(mpg ~ wt, mtcars)
fit2 <- loess(mpg ~ wt, mtcars, control = loess.control(surface = "direct"))
new_wt_df <- tibble(wt = 1:10)

test_that("function signature", {
  check_safepredict_signature(safe_predict.loess)
})

test_that("input validation", {

  expect_error(
    safe_predict(fit),
    "argument \"new_data\" is missing, with no default"
  )

  expect_error(
    safe_predict(fit, mtcars, type = "infinite fun space"),
    "`type` should be one of: \"response\", \"conf_int\" or \"pred_int\""
  )

  expect_error(
    safe_predict(fit, mtcars, std_error = "not a logical"),
    "std_error must be a logical vector with one element."
  )

  expect_error(
    safe_predict(fit, mtcars, type = "conf_int", level = 1),
    "level must be a vector with one element strictly between 0 and 1."
  )

  expect_warning(
    safe_predict(fit, mtcars, type = "conf_int", levl = 0.2),
    "Some components of ... were not used: levl"
  )

  expect_warning(
    safe_predict(fit, mtcars, std.dev = TRUE),
    "Some components of ... were not used: std.dev"
  )
})

## checks on returned predictions

test_that("default type", {
  default_preds <- safe_predict(fit, mtcars)
  check_predict_output(default_preds, mtcars, type = "response")
})

test_that("type = \"response\"", {
  check_predict(safe_predict.loess, fit, mtcars, "mpg", type = "response")
  check_predict(safe_predict.loess, fit2, mtcars, "mpg", type = "response")

  check_predict(safe_predict.loess, fit, new_wt_df, "mpg", type = "response")
  check_predict(safe_predict.loess, fit2, new_wt_df, "mpg", type = "response")
})

test_that("type = \"conf_int\"", {
  check_predict(safe_predict.loess, fit, mtcars, "mpg", type = "conf_int")
  check_predict(safe_predict.loess, fit2, mtcars, "mpg", type = "conf_int")

  check_predict(safe_predict.loess, fit, new_wt_df, "mpg", type = "conf_int")
  check_predict(safe_predict.loess, fit2, new_wt_df, "mpg", type = "conf_int")
})

test_that("type = \"pred_int\"", {
  check_predict(safe_predict.loess, fit, mtcars, "mpg", type = "pred_int")
  check_predict(safe_predict.loess, fit2, mtcars, "mpg", type = "pred_int")

  check_predict(safe_predict.loess, fit, new_wt_df, "mpg", type = "pred_int")
  check_predict(safe_predict.loess, fit2, new_wt_df, "mpg", type = "pred_int")
})
