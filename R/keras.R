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

  type <- arg_match(type)

  new_data <- as.matrix(new_data)

  switch(type,
    "response" = predict_keras_response(object, new_data),
    "class" = predict_keras_class(object, new_data),
    "prob" = predict_keras_prob(object, new_data),
    no_method_for_type_error()
  )
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

  # TODO: use class_prob_helper here

  # MK missing a `post =` here?
  function(x, object) {
    x <- as_tibble(x)
    colnames(x) <- object$lvl
    x
  }

  post(pred)
}

