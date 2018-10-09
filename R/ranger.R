library(ranger)

# DECISION: deal with all_tree in multi_predict method

# TODO: implement a quantile method for the predictions

#' @export
safe_predict.ranger  <- function(
  object,
  new_data,
  type = c(
    "response",
    "class",
    "prob",
    "conf_int"
  ),
  std_error = FALSE,
  level = 0.95,
  verbose = FALSE,
  seed = sample.int(10^5, 1),
  ...) {

  mode <- object$treetype

  # check for mode/type compatibility

  # warn when trying to get quantile and probability predictions
  # from objects fit with the wrong stuff

  # TODO: make verbose and seed actually work
  # MK use ...? 

  if (mode == "Probability estimation")
    predict_ranger_helper(object, new_data, std_error = FALSE)
  else if (mode == "Classification")
    predict_ranger_helper(object, new_data, std_error = FALSE)
  else if (mode == "Regression" && type == "response")
    predict_ranger_helper(object, new_data, std_error = std_error)
  else if (mode == "Regression" && type == "conf_int")
    predict_ranger_confint(object, new_data, level, std_error)
  else
    could_not_dispatch_error()
}

predict_ranger_helper <- function(
  object,
  new_data,
  std_error = FALSE) {

  pred_obj <- predict(object, new_data, type = "response")
  pred <- as_tibble(pred_obj$predictions)

  if (std_error)
    pred$.std_error <- predict(object, new_data, type = "se")$predictions

  pred
}

predict_ranger_confint <- function(object, new_data, level, std_error) {
  pred_se <- predict_ranger_helper(object, new_data, std_error = TRUE)
  pred_se_to_confint(pred_se, level, std_error)
}
