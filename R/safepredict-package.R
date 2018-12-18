#' @keywords internal
#'
#' @importFrom glue glue
#'
#' @importFrom rlang abort
#' @importFrom rlang arg_match
#'
#' @importFrom stats family
#' @importFrom stats model.frame
#' @importFrom stats model.response
#' @importFrom stats na.pass
#' @importFrom stats predict
#' @importFrom stats qnorm
#' @importFrom stats qt
#'
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#'
#'
"_PACKAGE"

#' Safely predict from a model object
#'
#' @param object An object or model you would like to get predictions from.
#'
#' @param new_data **Required**. Data in the same format as required for
#'  the `predict()` (or relevant method) for `object`.
#'
#'  `new_data`:
#'
#'  - does not need to contain the model outcome
#'  - can contain additional columns not used for prediction
#'  - can have one or more rows when specified via a table-like object such
#'   as a [tibble::tibble()], [data.frame()] or [matrix()].
#'
#'  We do our best to support missing data specified via `NA`s in `new_data`
#'  although this is somewhat dependent on the underlying `predict()` method.
#'
#'  Currently we do not have a robust way to check for novel factor levels in
#'  factor predictors. We recommend using the
#'  [recipes][recipes::`recipes-package`] to consistently handle categorical
#'  predictors.
#'
#' @param type A character vector indicating what kind of predictions you
#'  would like.
#'
#'  Options are:
#'
#'  - `"response"`: continuous/numeric predictions
#'  - `"class"`: hard class predictions
#'  - `"prob"`: class or survival probabilities
#'  - `"link"`: predictors on the linear scale (GLMs only)
#'  - `"conf_int"`: confidence intervals for means of continuous predictions
#'  - `"pred_int"`: prediction intervals for continuous outcomes
#'
#'  In most cases, only a subset of these options are available. For details
#'  on the difference between confidence and prediction intervals, see
#'  the [online documentation](TODO). This is also available as a vignette
#'  that you can access with:
#'
#'  ```
#'  vignette("confidence-and-prediction-intervals", package = "safepredict")
#'  ```
#'
#' @param ... Unused. `safe_predict()` checks that all arguments in `...` are
#'  evaluated via the `ellipsis` package. The idea is to prevent silent errors
#'  when arguments are mispelled. This feature is experimental and feedback
#'  is welcome.
#'
#' @param level A number strictly between `0` and `1` to use as the
#'  confidence level when calculating confidence and prediction intervals.
#'  Setting `level = 0.90` correspondings to a 90 percent confidence interval.
#'  Ignored except when `type = "conf_int"` or `type = "pred_int"`. Defaults
#'  to `0.95`.
#'
#' @param std_error Logical indicating whether or not calculate standard
#'  errors for the fit at each point. Not available for all models, and can
#'  be computationally expensive to compute. The standard error is *always*
#'  the standard error for the mean, and *never* the standard error for
#'  predictions. Standard errors are returned in a column called `.std_error`.
#'  Defaults to `FALSE`.
#'
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
#'
#' @section Recommended implementations:
#'
#'  The goal of `safepredict` is to make prediction as painless and consistent
#'  as possible across a wide variety of model objects. In some cases, the
#'  existing intrafrastructure is insufficient to provide a consistent and
#'  feature-rich prediction interface. As a result, we support a number of
#'  model objects that we do not actually recommend using. In these cases,
#'  we try to link to better and more feature-rich implementations.
#'
#' @export
safe_predict <- function(
  object,
  new_data,
  type = NULL,
  ...,
  level = 0.95,
  std_error = FALSE) {
  ellipsis::check_dots_used()
  UseMethod("safe_predict")
}

#' @rdname safe_predict
#' @export
safe_predict.default <- function(object, ...) {
  cls <- class(object)[1]
  glubort("There is no safe_predict() method for objects of class {cls}.")
}

#' Safely predict from many fits at once
#'
#' Experimental and untested, not recommended for general use.
#'
#' @param object TODO
#' @param ... TODO
multi_predict <- function(object, ...) {
  ellipsis::check_dots_used()
  UseMethod("multi_predict")
}

#' @rdname multi_predict
#' @export
multi_predict.default <- function(object, ...) {
  cls <- class(object)[1]
  glubort("There is no multi_predict() method for objects of class {cls}.")
}

