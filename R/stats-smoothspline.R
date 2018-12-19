#' Safe predictions from a smooth.spline object
#'
#' We **strongly recommend** using [mgcv::gam()] instead of `smooth.spline()`.
#' `mgcv` provides a feature-rich and frequently updated GAM implementation.
#'
#' @param object A `smooth.spline` object returned from a call to
#'  [stats::smooth.spline()].
#'
#' @param new_data A numeric vector. Can contain `NA`s.
#'
#' @param type What kind of predictions to return. Options are:
#'
#'   - `"response"` (default): Standard predictions from smoothing splines.
#'
#' @template return
#'
#' @export
#'
#' @section Uncertainty in predictions:
#'
#'  If you use `mgcv`, [mgcv::predict.gam()] has a `se.fit` argument
#'  that will allow you get standard errors for predictions.
#'
#'  If you insist on using `smooth.spline()`, your best bet to estimate
#'  uncertainty in predictions is bootstrapping. See
#'
#'  ```
#'  vignette("bootstrapping", package = "safepredict")
#'  ```
#'
#'  for worked examples and details on how you might do this.
#'
#' @section Derivatives of smooths:
#'
#'  While [stats:::predict.smooth.spline()] has a `deriv` argument we
#'  do not support it. If you would like to estimate derivates of smooths we
#'  recommend using the `gratia` package, and in particular [gratia::fderiv()].
#'  At the time of writing, `gratia` is not yet on CRAN, but can be installed
#'  from Github with `devtools::install_github(gavinsimpson/gratia)`.
#'  Additional documentation on `gratia` is available
#'  [here](https://gavinsimpson.github.io/gratia/).
#'
#' @examples
#'
#' fit <- smooth.spline(mtcars$mpg, mtcars$wt, cv = TRUE)
#' safe_predict(fit, c(30, NA, 40))
#'
#' # the following will fail, however
#' \dontrun{
#' predict(fit, c(30, NA, 40))
#' }
#'
#' # however, we recommend using mgcv instead
#' library(mgcv)
#'
#' fit2 <- gam(mpg ~ s(wt), data = mtcars)
#' predict(fit2, mtcars)  # TODO: update with safe_predict once implemented
#'
#' # to get estimated derivatives from smooths use gratia,
#' # which extends mgcv
#'
#' library(gratia)  # TODO: gratia isn't on CRAN yet
#' fderiv(fit2)
#'
safe_predict.smooth.spline <- function(
  object,
  new_data,
  type = "response",
  ...) {

  type <- arg_match(type)

  if (any(is.na(new_data))) {
    # predict on a single observation, or throw an NA
    predict_or_na <- function(obs) {
      pred <- tryCatch(predict(object, obs)$y, error = function(cnd) NA)
      if (!is.numeric(pred) || !(length(pred) == 1)) NA else pred
    }

    vapply(new_data, predict_or_na, numeric(1))
  } else {
    tibble(.pred = predict(object, new_data)$y)
  }
}


