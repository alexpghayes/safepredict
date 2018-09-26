safe_predict.stanreg <- function(
  object,
  new_data,
  type = c(
    "response",
    "class",
    "prob",
    "conf_int",
    "pred_int",
    "link"
  ),
  std_error = FALSE,
  level = 0.95,
  ...) {

  new_data <- safe_tibble(new_data)
  type <- match.arg(type)

  ## TODO: dispatch on type
  if (type == "response")
    predict_stanreg_response(object, new_data)
  else
    no_method_for_type_error()

  pred
}

predict_stanreg_response <- function(object, new_data, ...) {
  predict(object, newdata = new_data)
}

predict_stanreg_class <- function(object, new_data, ...) {
  predict(object, newdata = new_data)
}

predict_stanreg_prob <- function(object, new_data, ...) {
  predict(object, newdata = new_data)
}

predict_stanreg_posterior <- function(object, new_data, ...) {
  predict(object, newdata = new_data)
}

predict_stanreg_posterior_pred <- function(object, new_data, ...) {
  predict_posterior(object, newdata = new_data)
}
