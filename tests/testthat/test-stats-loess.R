context("test-stats-loess")

# not sure why you would do this but you can
fit <- loess(mpg ~ wt, mtcars)
fit2 <- loess(mpg ~ wt, mtcars, control = loess.control(surface = "direct"))
new_wt_df <- tibble(wt = 1:10)

## function signature

test_that("function signature", {
  expect_safepredict_signature(safe_predict.loess)
})

## input validation works

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

## input edge cases

# - missing data
# - spline trap
# - single observation
# - repeated observations

## checks on returned predictions

# - correct class given type
# - in a tibble
# - correct number of rows
# - level and interval attributes for type = conf_int and type = pred_int
# - correct column names given type

# not sure why you would do this but you can
fit <- loess(mpg ~ poly(wt), mtcars)
predict(fit, mtcars)

fit <- loess(mpg ~ wt, mtcars)
predict(fit, tibble(wt = 1:10), se = TRUE)

safe_predict(fit, mtcars)
safe_predict(fit, mtcars, std_error = TRUE)

safe_predict(fit, mtcars, type = "conf_int")
safe_predict(fit, mtcars, type = "pred_int") %>%
  bind_cols(mtcars) %>%
  ggplot(aes(wt, mpg)) +
  geom_point() +
  geom_ribbon(aes(wt, ymin = .pred_lower, ymax = .pred_upper), alpha = 0.3)

# extrapolation
# to get extrapolation
fit2 <- loess(mpg ~ wt, mtcars, control = loess.control(surface = "direct"))
predict(fit2, tibble(wt = 1:10), se = TRUE)
