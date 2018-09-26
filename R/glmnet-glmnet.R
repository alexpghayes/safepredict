# TODO: how to specify the penalty value

#' Safe predictions for glmnet objects
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


# NOTE: to explicitly specify the value of the penalty, use multi_predict!
# should multi-predict also accept new_data augmented with hyperparameter
# columns for individualized prediction stuffs?

safe_predict.glmnet <- function(
  object,
  new_data,
  type = c(
    "response",
    "class",
    "prob",
    "link"
  ),
  penalty = NULL) {

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

  ## type x family validation: binomial shouldn't do "response", etc

  if (family %in% c("binomial", "multinomial") &&
      type %notin% c("prob", "class", "param_pred"))
    stop(
      "`type` must be \"prob\", \"class\" or \"param_pred\" for binomial",
      "and multinomial families.", call. = FALSE
    )

  if (type == "link")
    predict_glmnet_link(object, new_data, penalty)
  else if (family == "gaussian")
    predict_glmnet_numeric(object, new_data, type, penalty)
  else if (family == "mgaussian")
    predict_glmnet_mgaussian(object, new_data, type, penalty)
  else if (family == "binomial")
    predict_glmnet_binomial(object, new_data, type, penalty)
  else if (family == "multinomial")
    predict_glmnet_multinomial(object, new_data, type, penalty)
  else if (family == "poisson")
    predict_glmnet_numeric(object, new_data, type, penalty)
  else
    stop("Family: ", family, " not yet supported.", call. = FALSE)
}


predict_glmnet_link <- function(object, new_data, penalty) {
  pred_mat <- predict(object, new_data, type = "link", s = penalty)
  as_pred_tibble(pred_mat)
}

predict_glmnet_numeric <- function(object, new_data, type, penalty) {
  pred_mat <- predict(object, new_data, type = "response", s = penalty)
  as_pred_tibble(pred_mat)
}

predict_glmnet_mgaussian <- function(object, new_data, type, penalty) {
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
  levels <- object$glmnet.fit$classnames
  binomial_helper(pred_mat, levels, type, threshold)
}

predict_glmnet_multinomial <- function(object, new_data, type, penalty) {
  pred_array <- predict(object, new_data, type = "response", s = penalty)
  levels <- object$glmnet.fit$classnames
  multinomial_helper(pred_mat[, , 1], levels, type)
}


