#' @keywords internal
"_PACKAGE"

safe_predict <- function(object, newdata, ...) {
  UseMethod("safe_predict")
}
