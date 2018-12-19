context("test-stats-smoothspline")

fit <- smooth.spline(mtcars$mpg, mtcars$hwy, cv = TRUE)

## bit non-standard because `new_data` needs to be a vector. ugh.
## also missing data is gonna be nightmare

test_that("function signature", {
  check_safepredict_signature(safe_predict.smooth.spline)
})

test_that("input validation", {

  expect_error(
    safe_predict(fit),
    "argument \"new_data\" is missing, with no default"
  )

  expect_error(
    safe_predict(fit, mtcars$wt, type = "infinite fun space"),
    "`type` should be one of: \"response\""
  )

  expect_warning(
    safe_predict(fit, mtcars$wt, bad_arg = 0.2),
    "Some components of ... were not used: bad_arg"
  )
})

## checks on returned predictions

test_that("default type", {
  default_preds <- safe_predict(fit, mtcars$wt)
  check_predict_output(default_preds, mtcars$mpg, type = "response")
})

# in general `smooth.spline` isn't worth full testing since it's barely
# implemented, so we keep it short here

test_that("type = \"response\"", {
  preds <- safe_predict(fit, c(30, NA, 40))

  # mtcars[1:3, is a hack]  -- we just need to tell check_predict_output that
  # we gave three observations as input to safe_predict
  check_predict_output(preds, mtcars[1:3, ], type = "response")
})

