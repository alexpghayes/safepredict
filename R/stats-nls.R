#' Safe predictions from an nls object
#'
#' @param object An `nls` object returned from a call to [stats::nls()].
#'
#' @param type What kind of predictions to return. Options are:
#'   - `"response"` (default): Standard predictions from non-linear regression.
#' @template boilerplate
#'
#' @export
#' @examples
#'
#' @section Uncertainty in predictions
#'
#'   Note: stats::predict.nls has an se.fit argument, but it is currently
#'   ignored
#'
#'   Uncertainty in predictions from non-linear least squares is hard. Two
#'   options:
#'
#'   1. Bootstrapping (recommended)
#'   2. Delta Method (see [car::DeltaMethod] or [emdbook::deltamethod])
#'
#' # TODO: examples of both
#'
#' ```
#' a <- 1L
#' ```
#'
safe_predict.nls <- function(
  object,
  new_data,
  type = "response",
  ...) {

  new_data <- safe_tibble(new_data)
  type <- arg_match(type)

  tibble(.pred = predict(object, new_data, na.action = na.pass))
}
