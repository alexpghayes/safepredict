context("test-stats-nls")

fit <- nls(demand ~ SSasympOrig(Time, A, lrc), data = BOD)

test_that("function signature", {
  check_safepredict_signature(safe_predict.nls)
})

test_that("input validation", {

  expect_error(
    safe_predict(fit),
    "argument \"new_data\" is missing, with no default"
  )

  expect_error(
    safe_predict(fit, BOD, type = "infinite fun space"),
    "`type` should be one of: \"response\""
  )

  expect_warning(
    safe_predict(fit, BOD, bad_arg = 0.2),
    "Some components of ... were not used: bad_arg"
  )
})

## checks on returned predictions

test_that("default type", {
  default_preds <- safe_predict(fit, BOD)
  check_predict_output(default_preds, BOD, type = "response")
})

test_that("type = \"response\"", {
  check_predict(safe_predict.nls, fit, BOD, "demand", type = "response")
})
