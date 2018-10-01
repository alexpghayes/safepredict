#' @export
safe_predict.mlm <- function(
  object,
  new_data,
  type = c(
    "response"
  ),
  ...) {

  ## input validation

  new_data <- safe_tibble(new_data)
  type <- match.arg(type)

  raw <- predict(object, newdata = new_data, na.action = na.pass)
  as_pred_tibble(raw, paste0(".pred_", colnames(raw)))
}