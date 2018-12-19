#' Safe predictions from a multiple linear model
#'
#' @param object An `mlm` object returned from a call to [stats::lm()].
#'
#' @param type What kind of predictions to return. Options are:
#'   - `"response"` (default): Standard predictions from linear regression.
#'   - `"conf_int"`: Fitted values plus a confidence interval for the fit.
#'   - `"pred_int"`: Predictions with accompanying prediction interval.
#' @template boilerplate
#'
#' @details Do not use on model objects that only subclass `lm`. This will result
#'   in an error.
#'
#' @section Estimating uncertainty:
#'
#' - bootstrap
#'
#' @export
#' @examples
#'
#' fit <- lm(hp ~ ., mtcars)
#'
#' safe_predict(fit, mtcars)
#'
#' mt2 <- mtcars
#' diag(mt2) <- NA  # overly aggressive
#'
#' safe_predict(fit, mt2, std_error = TRUE)
#' safe_predict(fit, mt2, type = "pred_int", level = 0.9)
#'
safe_predict.mlm <- function(
  object,
  new_data,
  type = c(
    "response"
  ),
  ...) {

  ## input validation
  type <- arg_match(type)
  raw <- predict(object, newdata = new_data, na.action = na.pass)
  as_pred_tibble(raw, paste0(".pred_", colnames(raw)))
}
