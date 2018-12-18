#' Predictions from time series models
#'
#' Gracefully error out and suggest using `sweep` instead, with some code
#' examples of what that looks like.
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
suggest_sweep <- function(object, ...) {
  cls <- class(object)[1]
  glubort("We recommend using the `sweep` package for {cls} objects.")
}

#' @rdname suggest_sweep
#' @export
safe_predict.ar <- suggest_sweep

#' @rdname suggest_sweep
#' @export
safe_predict.Arima <- suggest_sweep

#' @rdname suggest_sweep
#' @export
safe_predict.Arima0 <- suggest_sweep

#' @rdname suggest_sweep
#' @export
safe_predict.HoltWinters <- suggest_sweep

#' @rdname suggest_sweep
#' @export
safe_predict.StructTS <- suggest_sweep
