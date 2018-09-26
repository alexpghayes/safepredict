safe_predict.nnet <- function(
  object,
  new_data,
  type = c(
    "response",
    "class"
  ),
  ...) {

  new_data <- safe_tibble(new_data)
  type <- match.arg(type)

  if (type == "response")
    predict_nnet_response(object, new_data)
  else if (type == "class")
    predict_nnet_class(object, new_data)
  else
    no_method_for_type_error()

  pred
}

predict_nnet_response <- function(object, new_data, ...) {
  pred <- predict(object, newdata = new_data, type = "raw")
  maybe_multivariate(pred)
}

predict_nnet_class <- function(object, new_data, ...) {
  predict(object, newdata = new_data, type = "class")
}
