#' @export
safe_predict.keras <- function(
  object,
  new_data,
  type = c(
    "response",
    "class",
    "prob"
  ),
  ...) {

  type <- match.arg(type)
  new_data <- as.matrix(new_data)

  ## TODO: dispatch on type
  if (type == "response")
    predict_keras_response(object, new_data)
  else if (type == "class")
    predict_keras_class(object, new_data)
  else if (type == "prob")
    predict_keras_prob(object, new_data)
  else
    no_method_for_type_error()

  pred
}


predict_keras_response <- function(object, new_data, ...) {
  pred <- predict(object, newdata = new_data)
  maybe_multivariate(pred)
}

predict_keras_class <- function(object, new_data, ...) {
  pred <- predict_classes(object, newdata = new_data)

  # need to recall levels of factor from model_fit object
  post = function(x, object) {
    object$lvl[x + 1]
  }

  post(pred)
}

predict_keras_prob <- function(object, new_data, ...) {

  pred <- predict_proba(object, newdata = new_data)

  function(x, object) {
    x <- as_tibble(x)
    colnames(x) <- object$lvl
    x
  }

  post(pred)
}

