#' @param new_data A dataset as a [data.frame] or similar object. Must be
#'   coercable to a tibble using [tibble::as_tibble()].
#' @param se_fit Logical indicating whether or not to also calculate standard
#'   errors for the fit at each point. These standard errors do not include
#'   the residual variance. Ignored when calculating a confidence or prediction
#'   interval.
#' @param level A number between `0` and `1` to use as the confidence level
#'   when calculating confidence and prediction intervals. Ignored
#'   otherwise.
#' @param ... Unused.
#'
#' @return A [tibble::tibble()] with one row for each row of `newdata`.
#'   Predictions for observations with missing data will be `NA`. Returned
#'   tibble has different columns depending on `type`:
#'
#'   - `"response"`: `.pred` (numeric)
#'   - `"class"`: `.pred` (factor)
#'   - `"prob"`: `.pred_{level}` columns (numerics between 0 and 1)
#'   - `"link"`: `.pred` (numeric)
#'   - `"conf_int"`: `.pred`, `.pred_lower`, `.pred_upper` (all numeric)
#'   - `"pred_int"`: `.pred`, `.pred_lower`, `.pred_upper` (all numeric)
#'
#'   For interval predictions, the tibble has additional attributes `level`
#'   and `interval`. The `level` is the same as the `level` argument and is
#'   between 0 and 1. `interval` is either `"confidence"` or `"prediction"`.
#'   Some models may also set a `method` attribute to detail the method
#'   used to calculate the intervals.
#'
