#' Safe predictions for cross-validated glmnet objects
#'
#' @param object TODO
#' @param new_data TODO
#' @param type TODO
#' @param params TODO
#'
#' @template boilerplate
#'
#' @export
multi_predict.cv.glmnet <- function(
  object,
  new_data,
  type = c(
    "response",
    "class",
    "prob",
    "link"
  ),
  params = NULL) {
  multi_predict.glmnet(object$glmnet.fit, new_data, type, params)
}
