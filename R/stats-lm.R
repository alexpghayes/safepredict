#' Safe predictions from a linear model
#'
#' @param object An `lm` object returned from a call to [stats::lm()].
#'
#' @param type What kind of predictions to return. Options are:
#'   - `"response"` (default): Standard predictions from linear regression.
#'   - `"conf_int"`: Fitted values plus a confidence interval for the fit.
#'   - `"pred_int"`: Predictions with accompanying prediction interval.
#' @template boilerplate
#'
#' @details Do not use on model objects that only subclass `lm`. This will result
#'   in an error.
#'
#' @section Confidence intervals versus predictions intervals:
#'
#' TODO
#'
#' @export
#' @examples
#'
#' fit <- lm(hp ~ ., mtcars)
#'
#' safe_predict(fit, mtcars)
#'
#' mt2 <- mtcars
#' diag(mt2) <- NA  # overly aggressive
#'
#' safe_predict(fit, mt2, std_error = TRUE)
#' safe_predict(fit, mt2, type = "pred_int", level = 0.9)
#'
safe_predict.lm <- function(
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

  type <- match.arg(type)
  validate_logical(std_error)
  validate_probability(level)

  ## dispatch on type

  # NOTE: predict.lm deals with missing data nicely.
  # in other circumstances, passing na.action = na.pass may be necessary
  # or you may need to set rownames on a data.frame, see which rownames
  # are retained in the predictions and join on these rownames

  if (type == "response")
    pred <- predict_lm_response(object, new_data, std_error)
  else
    pred <- predict_lm_interval(object, new_data, type, level)

  pred
}

predict_lm_response <- function(object, new_data, std_error) {
  if (!std_error) {
    pred <- tibble(.pred = predict(object, new_data, na.action = na.pass))
  } else{
    pred_list <- predict(object, new_data, se.fit = TRUE, na.action = na.pass)
    pred <- tibble(.pred = pred_list$fit, .pred_std_error = pred_list$se.fit)
  }

  pred
}

predict_lm_interval <- function(object, new_data, type, level) {

  interval <- if (type == "conf_int") "confidence" else "prediction"

  pred_mat <- predict(
    object,
    new_data,
    interval = interval,
    level = level,
    na.action = na.pass
  )

  pred <- dplyr::rename(
    as_tibble(pred_mat),
    ".pred" = "fit",
    ".pred_lower" = "lwr",
    ".pred_upper" = "upr"
  )

  attr(pred, "interval") <- interval
  attr(pred, "level") <- level

  pred
}
