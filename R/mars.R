safe_predict.mars <- function(
  object,
  new_data,
  type = c(
    "response",
    "class",
    "prob"
  ),
  ...) {

  new_data <- safe_tibble(new_data)
  type <- match.arg(type)

  ## TODO: dispatch on type
  if (type == "response")
    predict_mars_response(object, new_data)
  else if (type == "class")
    predict_mars_class(object, new_data)
  else if (type == "prob")
    predict_mars_prob(object, new_data)
  else
    no_method_for_type_error()

  pred
}

predict_mars_response <- function(object, new_data, ...) {
  pred <- predict(object, newdata = new_data)
  maybe_multivariate(pred)
}

predict_mars_class <- function(object, new_data, ...) {
  pred <- predict(object, newdata = new_data, type = "response")

  post = function(x, object) {
    x <- ifelse(x[,1] >= 0.5, object$lvl[2], object$lvl[1])
    x
  }

  post(pred)
}

predict_mars_prob <- function(object, new_data, ...) {

  pred <- predict(object, newdata = new_data, type = "response")

  post = function(x, object) {
    x <- x[,1]
    x <- tibble(v1 = 1 - x, v2 = x)
    colnames(x) <- object$lvl
    x
  }

  post(pred)
}
