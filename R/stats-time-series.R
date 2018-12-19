#' Predictions from time series models
#'
#' `safepredict` does not support time series models. We recommend using
#' [sweep][sweep::sweep_package].
#'
#' @param object Ignored.
#' @param new_data Ignored.
#' @param type Ignored.
#' @param ... Ignored.
#'
#' @export
#'
suggest_sweep <- function(object, new_data, type, ...) {
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
