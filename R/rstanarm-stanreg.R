#' @export
safe_predict.stanreg <- function(
  object,
  new_data,
  type = c(
    "response",
    "prob"
  ),
  std_error = FALSE,
  level = 0.95,
  ...) {

  type <- match.arg(type)
  pred <- predict(object, new_data, type = "response")
  as_pred_tibble(pred)
}
