#' Safe predictions from an nls object
#'
#' @param object An `nls` object returned from a call to [stats::nls()].
#' @param new_data **Required**. A data frame containing predictors.
#' @param type What kind of predictions to return. Options are:
#'
#'   - `"response"` (default): Standard predictions from non-linear regression.
#'
#' @return TODO
#'
#' @export
#'
#' @section Determining uncertainty in predictions:
#'
#'  Note that [stats::predict.nls()] has an `se.fit` argument, but it is
#'  currently ignored. There is no build-in capability in base R to determine
#'  the uncertainty in the predictions from `nls`.
#'
#'  In practice, there are two options to get these:
#'
#'  1. Bootstrapping (recommended)
#'  2. The Delta Method
#'
#'  **Bootstrapping**: We recommend using the `rsample` package for
#'  bootstrapping, in particular [rsample::bootstraps()]. See
#'
#'  ```
#'  vignette("bootstrapping", package = "safepredict")
#'  ```
#'
#'  for worked examples.
#'
#'  **Delta Method**: Some people seem to be happy to use the delta method,
#'  and others claim it is numerically unstable. Two options include
#'  [car::DeltaMethod()] and [emdbook::deltavar()].
#'
#' @examples
#'
#' fit <- nls(demand ~ SSasympOrig(Time, A, lrc), data = BOD)
#' safe_predict(fit, BOD)
#'
#' fit2 <- nls(mpg ~ k / wt + b, mtcars, start = list(k = 1, b = 0))
#' safe_predict(fit2, mtcars)
#'
safe_predict.nls <- function(
  object,
  new_data,
  type = "response",
  ...) {

  type <- arg_match(type)
  tibble(.pred = predict(object, new_data, na.action = na.pass))
}
