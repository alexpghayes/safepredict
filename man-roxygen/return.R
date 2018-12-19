#' @return A [tibble::tibble()] with one row for each row of `new_data`.
#'   Predictions for observations with missing data will be `NA`. Returned
#'   tibble has different columns depending on `type`:
#'
#'   - `"response"`:
#'     - univariate outcome: `.pred` (numeric)
#'     - multivariate outcomes: `.pred_{outcome name}` (numeric) for each
#'       outcome
#'   - `"class"`: `.pred_class` (factor)
#'   - `"prob"`: `.pred_{level}` columns (numerics between 0 and 1)
#'   - `"link"`: `.pred` (numeric)
#'   - `"conf_int"`: `.pred`, `.pred_lower`, `.pred_upper` (all numeric)
#'   - `"pred_int"`: `.pred`, `.pred_lower`, `.pred_upper` (all numeric)
#'
#'   If you request standard errors with `std_error = TRUE`, an additional
#'   column `.std_error`.
#'
#'   For interval predictions, the tibble has additional attributes `level`
#'   and `interval`. The `level` is the same as the `level` argument and is
#'   between 0 and 1. `interval` is either `"confidence"` or `"prediction"`.
#'   Some models may also set a `method` attribute to detail the method
#'   used to calculate the intervals.
