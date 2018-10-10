#' Safe predictions for glmnet objects
#'
#' @param object TODO
#' @param new_data TODO
#' @param type TODO
#' @param penalty Unlike [safe_predict.cv.glmnet()], here you can explicitly
#'   set a value of `penalty`. In the CV version you have to pick one of the
#'   cross-validated versions of the penalty.
#'
#' @template boilerplate
#'
#' @export
#'
safe_predict.glmnet <- function(
  object,
  new_data,
  type = c(
    "response",
    "class",
    "prob",
    "link"
  ),
  penalty = NULL,
  # MK Should `threshold` be here? It's only applicable to 2 class models.
  # AH I could dispatch on the subclasses instead? Then link to the all those
  # AH documentation pages in the doc for `safe_predict.glmnet?`?
  threshold = 0.5,
  ...) {

  ## input validation

  new_data <- Matrix::as.matrix(new_data)
  type <- match.arg(type)

  if (is.null(penalty) || length(penalty) > 1)
    stop(
      "`predict` doesn't work with multiple penalties (i.e. lambdas). ",
      "Please specify a single value using `penalty = some_value` or use ",
      "`multi_predict` to get multiple predictions per row of data.",
      call. = FALSE
    )

  ## hyperparameter validations

  family <- object$call$family
  family <- if (is.null(family)) "gaussian" else family

  # a more sane default for classification problems
  if (family %in% c("binomial", "multinomial") && type == "response")
    type <- "class"

  type_by_param <- tibble::tribble(
    ~ param, ~ type,
    "binomial", c("class", "prob"),
    "multinomial", c("class", "prob"),
    "gaussian", "response",
    "mgaussian", "response",
    "poisson", "response"
  )

  check_type_by_param(type_by_param, type, family, all = "link")

  # MK GAK!

  if (type == "link")
    predict_glmnet_link(object, new_data, penalty)
  else if (family == "gaussian")
    predict_glmnet_numeric(object, new_data, penalty)
  else if (family == "mgaussian")
    predict_glmnet_mgaussian(object, new_data, penalty)
  else if (family == "binomial")
    predict_glmnet_binomial(object, new_data, type, penalty, threshold)
  else if (family == "multinomial")
    predict_glmnet_multinomial(object, new_data, type, penalty)
  else if (family == "poisson")
    predict_glmnet_numeric(object, new_data, penalty)
  else
    could_not_dispatch_error()
}


predict_glmnet_link <- function(object, new_data, penalty) {
  # TODO: fix for mgaussian and multinomial families
  # (breaks due to multivariate outcome)

  pred_mat <- predict(object, new_data, type = "link", s = penalty)
  as_pred_tibble(pred_mat)
}

predict_glmnet_numeric <- function(object, new_data, penalty) {
  pred_mat <- predict(object, new_data, type = "response", s = penalty)
  as_pred_tibble(pred_mat)
}

predict_glmnet_mgaussian <- function(object, new_data, penalty) {
  pred_array <- predict(object, new_data, s = penalty)
  pred_mat <- pred_array[, , 1]
  response_names <- paste0(".pred_", colnames(pred_mat))
  as_pred_tibble(pred_mat, response_names)
}

predict_glmnet_binomial <- function(
  object,
  new_data,
  type,
  penalty,
  threshold) {
  pred_mat <- predict(object, new_data, type = "response", s = penalty)
  levels <- object$classnames
  binomial_helper(pred_mat, levels, type, threshold)
}

predict_glmnet_multinomial <- function(object, new_data, type, penalty) {
  pred_array <- predict(object, new_data, type = "response", s = penalty)
  levels <- object$classnames
  multinomial_helper(pred_array[, , 1], levels, type)
}


