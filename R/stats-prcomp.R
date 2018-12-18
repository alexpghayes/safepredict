#' Safe predictions from a prcomp object
#'
#' @param object A `prcomp` object returned from a call to [stats::prcomp()]
#'   or a `princomp` object returned from a call to [stats::princomp()].
#' @param new_data TODO: what is acceptable here?
#'
#' @param type What kind of predictions to return. There is only one option:
#'
#'   - `"response"` (default): Standard PCA rotation without scaling
#'
#' @export
#' @examples
#' @family unsupervised
#'
#' @details
#'
#' @return What columns you get back, and the details of the computation
#'
#'   - rotation details
#'   - PC1, ..., PCn
#'
safe_predict.prcomp <- function(
  object,
  new_data,
  type = "response",
  ...) {
  type <- arg_match(type)
  pred_mat <- predict(object, new_data, na.action = na.pass)
  as_tibble(pred_mat)
}

#' @rdname safe_predict.prcomp
#' @export
safe_predict.princomp <- safe_predict.prcomp
