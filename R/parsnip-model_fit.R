


#' Safely predict from a model_fit object
#'
#' WHY THIS: need a special case where `type = "raw"` is acceptable
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
safe_predict.model_fit <- function(
  object,
  new_data,
  type = NULL,
  ...,
  opts = list()) {

  if (!is.null(type) && type != "raw" && length(opts) > 0)
    warning("`opts` is only used with `type = 'raw'` and was ignored.")

  if (!is.null(type) && type == "raw") {
    call <- quote(predict(object, new_data))
    call <- rlang::call_modify(call, !!!opts)
    return(rlang::eval_tidy(call))
  }

  safe_predict(object$fit, new_data, type, ...)
}
