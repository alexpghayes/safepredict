#' @export
safe_predict.C5.0 <- function(
  object,
  new_data,
  type = c(
    "class",
    "prob"
  ),
  ...) {

  new_data <- safe_tibble(new_data)
  type <- match.arg(type)

  pred <- predict(object, new_data, type = type, na.action = na.pass)
  as_tibble(pred)
}
