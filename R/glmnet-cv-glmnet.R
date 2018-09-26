#' Safe predictions for cross-validated glmnet objects
#'
#' @param object TODO
#' @param new_data TODO
#' @param type TODO
#' @param penalty TODO
#'
#' @template boilerplate
#'
#' @export
#'
safe_predict.cv.glmnet <- function(
  object,
  new_data,
  type = c(
    "response",
    "class",
    "prob",
    "link"
  ),
  penalty = c("1-se", "min")) {

  penalty <- match.arg(penalty)
  penalty <- if (penalty == "1-se") "lambda.1se" else "lambda.min"
  penalty <- object[[penalty]]

  safe_predict.glmnet(object$glmnet.fit, new_data, type, penalty)
}