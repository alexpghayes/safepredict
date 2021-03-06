#' @export
safe_predict.train.kknn <- function(
  object,
  new_data,
  type = c(
    "response",
    "class",
    "prob"
  ),
  ...) {

  type <- arg_match(type)

  # avoid bad match.arg() default for classification problems
  response <- object$response
  if (response != "continuous" && type == "response")
    type <- "class"

  type_by_response <- tibble::tribble(
    ~ param, ~ type,
    "continuous", "response",
    "ordinal", c("class", "prob"),
    "nominal", c("class", "prob")
  )

  check_type_by_param(type_by_response, type, response)

  if (type %in% c("response", "class"))
    predict_kknn_raw(object, new_data)
  else if (type == "prob")
    predict_kknn_prob(object, new_data)
  else
    no_method_for_type_error()
}

predict_kknn_raw <- function(object, new_data, ...) {
  pred <- predict(object, newdata = new_data, type = "raw")
  as_pred_tibble(pred)
}

predict_kknn_prob <- function(object, new_data, ...) {
  pred <- predict(object, newdata = new_data, type = "prob")
  as_pred_tibble(pred)
}
