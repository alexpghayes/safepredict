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
#'  the `predict()` (or relevant method) for `object`. We do our best to
#'  support missing data specified via `NA`s in `new_data` although this
#'  is somewhat dependent on the underlying `predict()` method.
#'
#'  `new_data`:
#'
#'  - does not need to contain the model outcome
#'  - can contain additional columns not used for prediction
#'  - can have one or more rows when specified via a table-like object such
#'   as a [tibble::tibble()], [data.frame()] or [matrix()].
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
#'  In most cases, only a subset of these options are available.
#'
#' @template unused_dots
#' @template level
#' @template std_error
#'
#' @template return
#'
#' @template intervals
#' @template novel_factor_levels
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

