#' Safe predictions from a loess object
#'
#' @param object A `loess` object returned from a call to [stats::loess()].
#' @param type What kind of predictions to return. Options are:
#'   - `"response"` (default): Standard predictions from LOESS regression.
#'   - `"conf_int"`: Fitted values plus a confidence interval for the fit.
#'   - `"pred_int"`: Predictions with accompanying prediction interval.
#' @template boilerplate
#'
#' @template intervals
#'
#' @export
#' @examples
#'
safe_predict.loess <- function(
  object,
  new_data,
  type = c(
    "response",
    "conf_int",
    "pred_int"
  ),
  std_error = FALSE,
  level = 0.95,
  ...) {

  ## input validation

  new_data <- safe_tibble(new_data)
  type <- arg_match(type)

  validate_logical(std_error)
  validate_probability(level)

  ## dispatch on type

  # NOTE: predict.lm deals with missing data nicely.
  # in other circumstances, passing na.action = na.pass may be necessary
  # or you may need to set rownames on a data.frame, see which rownames
  # are retained in the predictions and join on these rownames

  if (type == "response")
    pred <- predict_loess_response(object, new_data, std_error)
  else
    pred <- predict_loess_interval(object, new_data, type, level)

  pred
}

predict_loess_response <- function(object, new_data, std_error) {

  if (!std_error) {
    pred <- tibble(.pred = predict(object, new_data, na.action = na.pass))
  } else {
    pred_list <- predict(object, new_data, se = TRUE, na.action = na.pass)
    pred <- tibble(.pred = pred_list$fit, .pred_std_error = pred_list$se.fit)
  }

  pred
}

predict_loess_interval <- function(object, new_data, type, level) {

  pred_list <- predict(object, new_data, se = TRUE, na.action = na.pass)
  interval <- if (type == "conf_int") "confidence" else "prediction"

  # sanity check that i'm getting prediction intervals in a mathematically
  # apppropriate way
  if (type == "conf_int")
    se <- pred_list$se.fit
  else
    se <- sqrt(pred_list$se.fit^2 + pred_list$residual.scale^2)

  # TODO: sanity check that I'm getting the right quantiles elsewhere
  pred <- tibble(
    .pred = pred_list$fit,
    .pred_lower = .pred - qt((1 - level) / 2, pred_list$df) * se,
    .pred_upper = .pred + qt((1 - level) / 2, pred_list$df) * se
  )

  attr(pred, "interval") <- interval
  attr(pred, "level") <- level

  pred
}
