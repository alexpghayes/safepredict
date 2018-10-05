#' @export
safe_predict.C5.0 <- function(
  object,
  new_data,
  type = c(
    "class",
    "prob"
  ),
  ...) {
  type <- match.arg(type)
  pred <- predict(object, new_data, type = type, na.action = na.pass)
  as_pred_tibble(pred)
}
