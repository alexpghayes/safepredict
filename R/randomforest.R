safe_predict.randomForest <- function(
  object,
  new_data,
  type = c(
    "response",
    "class"
  ),
  ...) {

  new_data <- safe_tibble(new_data)
  type <- match.arg(type)

  if (type %in% c("response", "class"))
    predict_rf_helper(object, new_data)
  else if (type == "prob")
    predict_rf_prob(object, new_data)
  else
    no_method_for_type_error()

  pred
}

predict_rf_helper <- function(object, new_data, ...) {
  predict(object, newdata = new_data)
}

predict_rf_prob <- function(object, new_data, ...) {
  pred <- predict(object, newdata = new_data, type = "prob")
  as_pred_tibble(pred)
}
