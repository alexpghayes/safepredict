#' @export
safe_predict.C5.0 <- function(
  object,
  new_data,
  type = c(
    "class",
    "prob"
  ),
  ...) {

  type <- arg_match(type)

  pred <- predict(object, new_data, type = type, na.action = na.pass)
  as_tibble(pred)
}
