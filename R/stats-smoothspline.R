#' Safe predictions from a smooth.spline object
#'
#' Recommendation: use [mgcv::gam()] instead, it will make your life way
#' better.
#'
#' @param object An `nls` object returned from a call to [stats::nls()].
#' @param new_data Gotta be a vector: TODO
#' @param deriv Defulats
#'
#' @param type What kind of predictions to return. Options are:
#'   - `"response"` (default): Standard predictions from non-linear regression.
#' @template boilerplate
#'
#' @export
#' @examples
#'
#' @details `deriv` argument is not supported. Use [gratia::fderiv()] for this
#'   instead.
#'
#'   Recommendation: use [mgcv::gam()] instead. For example, the following
#'   are about equivalent:
#'
#'   ```
#'   TODO
#'   ```
#'
#'   Alternatively, you can get uncertainty with bootstrapping
#'
safe_predict.smooth.spline <- function(
  object,
  new_data,
  type = "response",
  ...) {

  # input validation: new_data must be a vector with no missing values

  type <- arg_match(type)
  # TODO: what on earth happens with missing data?
  tibble(.pred = predict(object, new_data))
}


