#' Safe predictions from a multiple linear model object
#'
#' @param object An `mlm` object returned from a call to [stats::lm()].
#'
#' @param new_data **Required**. A data frame or matrix containing the
#'  necessary predictors.
#'
#' @param type What kind of predictions to return. Options are:
#'
#'   - `"response"` (default): Standard predictions from multiple regression.
#'
#' @template unused_dots
#'
#' @template return
#'
#' @section Estimating uncertainty:
#'
#'  `stats::predict.mlm()`` provides neither confidence nor prediction
#'  intervals, although there is not theoretical issue with calculating
#'  these.
#'
#'  At some point in the future we may implement these intervals within
#'  `safepredict`. If you are interested in this, you can move intervals
#'  for `mlm` objects up the priority list by opening an issue on
#'  [Github](https://github.com/alexpghayes/safepredict).
#'
#' @export
#' @examples
#'
#' fit <- lm(cbind(hp, mpg) ~ ., mtcars)
#'
#' safe_predict(fit, mtcars)
#'
#' mt2 <- mtcars
#' diag(mt2) <- NA
#'
#' safe_predict(fit, mt2)
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
