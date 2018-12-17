#' @export
safe_predict.nnet <- function(
  object,
  new_data,
  type = c(
    "response",
    "class"
  ),
  ...) {

  new_data <- safe_tibble(new_data)
  type <- arg_match(type)

  if (type == "response")
    predict_nnet_response(object, new_data)
  else if (type == "class")
    predict_nnet_class(object, new_data)
  else
    no_method_for_type_error()
}

predict_nnet_response <- function(object, new_data, ...) {
  pred <- predict(object, newdata = new_data, type = "raw")
  as_tibble(pred)
}

predict_nnet_class <- function(object, new_data, ...) {
  pred <- predict(object, newdata = new_data, type = "class")
  as_tibble(pred)
}
