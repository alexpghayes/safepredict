#' Safe predictions for cross-validated glmnet objects
#'
#' @param object TODO
#' @param new_data TODO
#' @param type TODO
#' @param rule TODO
#'
#' @template boilerplate
#'
#' @export
safe_predict.cv.glmnet <- function(
  object,
  new_data,
  type = c(
    "response",
    "class",
    "prob",
    "link"
  ),
  rule = c("1-se", "min"),
  threshold = 0.5) {

  # MK Should `threshold` be here? It's only applicable to 2 class models.

  rule <- arg_match(rule)
  rule <- if (rule == "1-se") "lambda.1se" else "lambda.min"

  safe_predict.glmnet(
    object = object$glmnet.fit,
    new_data = new_data,
    type = type,
    penalty = object[[rule]],
    threshold = threshold
  )
}
