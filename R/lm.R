#' Safe predictions from a linear model
#'
#' @param object An `lm` object returned from a call to [stats::lm()].
#' @param newdata A dataset as a [data.frame] object. In theory this should
#'   work with any object accepted by the [stats::predict.lm()] `newdata`
#'   argument that can be reasonably coerced into a tibble.
#' @param type What kind of predictions to return. Options are:
#'   - `"response"` (default): Standard predictions from linear regression.
#'   - `"conf_int"`: Fitted values plus a confidence interval for the fit.
#'   - `"pred_int"`: Predictions with accompanying prediction interval.
#' @param se_fit Logical indicating whether or not to also calculate standard
#'   errors for the fit at each point. These standard errors do not including
#'   the residual variance. That is, they can be used to calculate confidence
#'   intervals but not prediction intervals.
#' @param level A number between 0 and 1 to use as the confidence level to
#'   use when calculating confidence and prediction intervals.
#' @param ... Unused. TODO: boilerplate for this.
#'
#' @details *What is the difference between confidence intervals and prediction
#'   intervals*? TODO.
#'
#' @return A [tibble::tibble()] with one row for each row of `newdata`.
#'   predictions for observations with missing data will be `NA`. Predictions
#'   are contained in the
#'   boilerplate about safe_predict_guarantees
#' @export
#'
#' @details Do not use on model objects that only subclass `lm`. Will error.
#'
#' @examples
safe_predict.lm <- function(
  object,
  newdata,
  type = c(
    "response",
    "conf_int",
    "pred_int"
  ),
  se_fit = TRUE,
  level = 0.95,
  ...) {

  ## input validation

  if (length(class(object)) > 1)
    stop(
      paste0(
        "The `safe_predict` lm method is not intended to be used with objects",
        "that subclass lm.",
        call. = FALSE
      )
    )

  newdata <- safe_tibble(newdata)
  type <- match.arg(type)

  validate_logical(se_fit)
  validate_probability(level)

  ## dispatch on type

  # NOTE: predict.lm deals with missing data nicely.
  # in other circumstances, passing na.action = na.pass may be necessary
  # or you may need to set rownames on a data.frame, see which rownames
  # are retained in the predictions and join on these rownames

  if (type == "response")
    pred <- predict_lm_response(object, newdata, se_fit)
  else
    pred <- predict_lm_interval(object, newdata, type, level)

  pred
}

predict_lm_response <- function(object, newdata, se_fit) {
  if (!se_fit) {
    pred <- tibble(.pred = predict(object, newdata, na.action = na.pass))
  } else{
    pred_list <- predict(object, newdata, se.fit = TRUE, na.action = na.pass)
    pred <- tibble(.pred = pred_list$fit, .pred_std_error = pred_list$se.fit)
  }

  pred
}

predict_lm_interval <- function(object, newdata, type, level) {

  interval <- if (type == "conf_int") "confidence" else "prediction"

  pred_mat <- predict(
    object,
    newdata,
    interval = interval,
    level = level,
    na.action = na.pass
  )

  pred <-rename(
    as_tibble(pred_mat),
    ".pred" = "fit",
    ".pred_lower" = "lwr",
    ".pred_upper" = "upr"
  )

  attr(pred, "interval") <- int_type
  attr(pred, "level") <- level

  pred
}
