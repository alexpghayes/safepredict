#' Safe predictions from a loess object
#'
#' @param object A `loess` object returned from a call to [stats::loess()].
#' @param new_data TODO
#' @param type What kind of predictions to return. Options are:
#'   - `"response"` (default): Standard predictions from LOESS regression.
#'   - `"conf_int"`: Fitted values plus a confidence interval for the fit.
#'   - `"pred_int"`: Predictions with accompanying prediction interval.
#'
#' @template unused_dots
#' @template std_error
#' @template level
#'
#' @template return
#'
#' @export
#' @examples
#'
#' fit <- loess(mpg ~ wt, mtcars)
#' safe_predict(fit, mtcars)
#' safe_predict(fit, mtcars, std_error = TRUE)
#'
#' # the default behavior of loess() is return NA for values
#' # outside the range of the training data:
#'
#' safe_predict(fit, data.frame(wt = 1:10))
#'
#' # to enable extrapolation, use:
#'
#' fit2 <- loess(
#'   mpg ~ wt,
#'   mtcars,
#'    control = loess.control(surface = "direct")
#' )
#'
#' safe_predict(fit2, data.frame(wt = 1:10), type = "pred_int", level = 0.9)
#'
safe_predict.loess <- function(
  object,
  new_data,
  type = c(
    "response",
    "conf_int",
    "pred_int"
  ),
  ...,
  std_error = FALSE,
  level = 0.95) {

  ## check if any suggested packages are needed

  # NA

  ## input validation

  # TODO: validation for `new_data`, and corresponding tests

  type <- arg_match(type)
  validate_logical(std_error)
  validate_probability(level)

  ## dispatch on type

  if (type == "response")
    pred <- predict_loess_basic(object, new_data, std_error)
  else
    pred <- predict_loess_interval(object, new_data, type, level)

  pred
}

predict_loess_basic <- function(object, new_data, std_error) {

  if (!std_error) {
    pred <- tibble(.pred = predict(object, new_data, na.action = na.pass))
  } else {
    pred_list <- predict(object, new_data, se = TRUE, na.action = na.pass)
    pred <- with(pred_list, tibble(.pred = fit, .std_error = se.fit))
  }

  pred
}


predict_loess_interval <- function(object, new_data, type, level) {

  pred_list <- predict(object, new_data, se = TRUE, na.action = na.pass)
  interval <- if (type == "conf_int") "confidence" else "prediction"

  if (type == "conf_int")
    se <- pred_list$se.fit
  else
    se <- sqrt(pred_list$se.fit^2 + pred_list$residual.scale^2)

  pred <- safe_confint(pred_list$fit, se, level, pred_list$df)
  attr(pred, "interval") <- interval
  pred
}
