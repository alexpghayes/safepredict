#' Safe predictions for cross-validated glmnet objects
#'
#' @param object TODO
#' @param new_data TODO
#' @param type TODO
#' @param params
#'   - works with a single value for the penalty. otherwise CV is gonna be
#'   inconsistent a wild pain in the rear.
#'   - works with a vector of values for the penalty
#'   - in the future will work with a tibble defining a parameter grid
#'
#' @template boilerplate
#'
#' @export
multi_predict.glmnet <- function(
  object,
  new_data,
  type = c(
    "response",
    "class",
    "prob"
  ),
  params = NULL) {

  ## input validation

  new_data <- Matrix::as.matrix(new_data)
  type <- match.arg(type)

  # TODO: some validation on the hyperparameter grid.
  # return some vector of penalties to consider

  if (is.null(params))
    params <- object$lambda

  # TODO: when you get a data frame of parameters

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

  check_type_by_param(type_by_param, type, family)

  switch(family,
    "gaussian" = multi_glmnet_numeric(object, new_data, type, params),
    "mgaussian" = multi_glmnet_mgaussian(object, new_data, type, params),
    "binomial" = multi_glmnet_binomial(object, new_data, type, params),
    "multinomial" = multi_glmnet_multinomial(object, new_data, type, params),
    "poisson" = multi_glmnet_numeric(object, new_data, type, params),
    could_not_dispatch_error()
  )
}


multi_glmnet_link <- function(object, new_data, params) {
  pred_mat <- predict(object, new_data, type = "link", s = params)
  as_pred_tibble(pred_mat)
}

multi_glmnet_numeric <- function(object, new_data, type, params) {
  pred_mat <- predict(object, new_data, type = "response", s = params)
  pred <- as_pred_tibble(pred_mat, names = params)
  pred <- add_id_column(pred)
  pred <- tidyr::gather(pred, lambda, .pred, -id)
  pred <- tidyr::nest(pred, -id)
  tibble(.pred = pred$data)
}

add_id_and_lambda <- function(data, lambda) {
  data <- tibble::add_column(data, lambda = lambda, .before = TRUE)
  add_id_column(data)
}

multi_glmnet_mgaussian <- function(object, new_data, type, params) {
  pred_array <- predict(object, new_data, s = params)
  pred_list <- apply(pred_array, 3, as_tibble)
  pred_list <- purrr::map2(pred_list, params, add_id_and_lambda)
  pred <- dplyr::bind_rows(pred_list)
  pred <- tidyr::nest(pred, -id)
  # MK Does this return a DF or matrix back. The specification has multivariate
  # MK outcomes being named `.pred_{column name}`
  tibble(.pred = pred$data)
}

# forcing class probs rather than factor predictions due to
# interface limitations

multi_glmnet_binomial <- function(
  object,
  new_data,
  type,
  params,
  threshold) {
  pred_mat <- predict(object, new_data, type = "response", s = params)
  levels <- object$classnames
  pred_list <- apply(pred_mat, 2, binomial_helper, levels, "prob")
  pred_list <- purrr::map2(pred_list, params, add_id_and_lambda)
  pred <- dplyr::bind_rows(pred_list)
  pred <- tidyr::nest(pred, -id)
  tibble(.pred = pred$data)
}

multi_glmnet_multinomial <- function(object, new_data, type, params) {
  pred_array <- predict(object, new_data, type = "response", s = params)
  levels <- object$classnames
  pred_list <- apply(pred_array, 3, multinomial_helper, levels, "prob")
  pred_list <- purrr::map2(pred_list, params, add_id_and_lambda)
  pred <- dplyr::bind_rows(pred_list)
  pred <- tidyr::nest(pred, -id)
  tibble(.pred = pred$data)
}
