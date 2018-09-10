#' @keywords internal
#'
#' @importFrom stats family model.frame model.response na.pass predict qnorm
#' @importFrom tibble as_tibble tibble
"_PACKAGE"


#' Safely predict from a model object
#'
#' @param object TODO
#' @param new_data TODO
#' @param ... Unused.
#'
#' @return TODO
#' @export
safe_predict <- function(object, new_data, ...) {
  UseMethod("safe_predict")
}
