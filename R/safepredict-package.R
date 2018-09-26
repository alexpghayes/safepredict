#' @keywords internal
#'
#' @importFrom stats family model.frame model.response na.pass predict qnorm
#' @importFrom tibble as_tibble tibble
"_PACKAGE"

#' Safely predict from a model object
#'
#' @param object TODO
#' @param new_data TODO
#' @param type
#' @param opts
#' @param level
#' @param std_error
#' @param ... Unused.
#'
#' @return TODO
#' @export
safe_predict <- function(
  object,
  new_data,
  type = NULL,
  opts = list(),
  level = 0.95,
  std_error = FALSE,
  ...) {

  if (type != "raw" && length(opts) > 0)
    warning("`opts` is only used with `type = 'raw'` and was ignored.")

  if (!is.null(type) && type == "raw") {
    call <- quote(predict(object, new_data))
    call <- rlang::call_modify(call, !!!opts)
    rlang::eval_tidy(call)
  } else
    UseMethod("safe_predict")
}

safe_predict.default <- function(...) {
  stop("I don't know about type = '", "'", type, call. = FALSE)
}
