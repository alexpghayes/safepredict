# TODO: confint, predint, se arguments, classification
# only make "response" available at the moment

#' @export
safe_predict.earth <- function(
  object,
  new_data,
  type = c(
    "response"
  ),
  threshold = 0.5,
  ...) {

  type <- arg_match(type)

  pred <- predict(object, new_data, type = "response")
  return(as_tibble(pred))

  # ignore fancy post processing / options due to time crunch

  switch(type,
    "response" = predict_earth_response(object, new_data),
    "class" = predict_earth_class(object, new_data, threshold),
    "prob" = predict_earth_prob(object, new_data),
    no_method_for_type_error()
  )

}

predict_earth_response <- function(object, new_data, ...) {
  pred <- predict(object, newdata = new_data)
  as_tibble(pred)
}

predict_earth_class <- function(object, new_data, threshold) {
  pred <- predict(object, newdata = new_data, type = "class", thresh = threshold)
  as_pred_tibble(pred)
}

predict_earth_prob <- function(object, new_data, ...) {

  pred <- predict(object, newdata = new_data, type = "response")

  # TODO: use the binomial helper
}
