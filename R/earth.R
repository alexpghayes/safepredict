# TODO: confint, predint, se arguments for varmod.method != "none"
#' @export
safe_predict.earth <- function(
  object,
  new_data,
  type = c(
    "response",
    "class",
    "prob"
  ),
  threshold = 0.5,
  level = 0.95,
  ...) {

  type <- match.arg(type)

  stop("Check that a model is actually a classification model before asking
       for class probabilities")

  # ignore fancy post processing / options due to time crunch
  outcome <- if (!is.null(object$levels)) "classification" else "regression"

  if (outcome == "classification" && type == "response")
    type <- "class"

  type_by_outcome <- tibble::tribble(
    ~ param, ~ type,
    "regression", "response",
    "classification", c("class", "prob")
  )

  check_type_by_param(type_by_outcome, type, outcome)

  if (type == "response")
    predict_earth_response(object, new_data, std_error)
  else if (type %in% c("class", "prob"))
    predict_earth_prob_class(object, new_data, type, threshold)
  else
    no_method_for_type_error()
}

predict_earth_response <- function(object, new_data, std_error) {
  pred <- predict(object, newdata = new_data)
  as_pred_tibble(pred)
}

predict_earth_prob_class <- function(object, new_data, type, threshold) {
  pred <- predict(object, newdata = new_data, type = "response")
  class_prob_helper(pred, object$levels, type, threshold)
}
