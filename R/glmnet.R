#' Safe predictions from a linear model
#'
#' @param object An `lm` object returned from a call to [stats::lm()].
#'
#' @param type What kind of predictions to return. Options are:
#'   - `"response"` (default): Standard predictions from linear regression.
#'   - `"conf_int"`: Fitted values plus a confidence interval for the fit.
#'   - `"pred_int"`: Predictions with accompanying prediction interval.
#' @template boilerplate
#'
#' @details Do not use on model objects that only subclass `lm`. This will result
#'   in an error.
#'
#' @section Confidence intervals versus predictions intervals:
#'
#' TODO
#'
#' @export
#' @examples
#'
#'
#'
#'
safe_predict.cv.glmnet <- function(
  object,
  new_data,
  type = c(
    "response",
    "class",
    "prob",
    "link",
    "param_pred"
  ),
  params = c("1-se", "min")) {

  # params:
  #   - either a positive linear interpolation when
  #     not fit with these exact lambda
  #   - "1-se" or "min"
  #   - a grid of lambda to predict across as a tidy tibble, with
  #     one column for each hyperparameter and one row for each set of
  #     predictions to make on the data

  ## input validation

  new_data <- Matrix::as.matrix(new_data)
  type <- match.arg(type)

  ## hyperparameter validations

  # validate_param_grid(params)

  if (is.character(params)) {
    params <- match.arg(params)
    params <- if (params == "1-se") "lambda.1se" else "lambda.min"
  }

  if (is.numeric(params))
    validate_positive(params)

  if (is.data.frame(params) && type != "param_pred")
    stop(
      "`type` must be `param_pred` when requesting predictions over a ",
      "hyperparameter grid.",
      call. = FALSE
    )

  family <- object$glmnet.fit$call$family
  family <- if (is.null(family)) "gaussian" else family

  ## type x family validation: binomial shouldn't do "response", etc

  if (family %in% c("binomial", "multinomial") &&
        type %notin% c("prob", "class"))
    stop(
      "`type` must be \"prob\" or \"class\" for binomial and multinomial",
      "families.", call. = FALSE
    )

  if (type == "link")
    predict_cv_glmnet_link(object, new_data, params)
  else if (family == "gaussian")
    predict_cv_glmnet_response(object, new_data, type, params)
  else if (family == "mgaussian")
    predict_cv_glmnet_mgaussian(object, new_data, type, params)
  else if (family == "binomial")
    predict_cv_glmnet_binomial(object, new_data, type, params)
  else if (family == "multinomial")
    predict_cv_glmnet_multinomial(object, new_data, type, params)
  else if (family == "poisson")
    predict_cv_glmnet_response(object, new_data, type, params)
  else
    stop("Family: ", family, " not yet supported.", call. = FALSE)
}


predict_cv_glmnet_link <- function(object, new_data, params) {
  pred_mat <- predict(object, new_data, type = "link", s = params)
  as_pred_tibble(pred_mat)
}

predict_cv_glmnet_response <- function(object, new_data, type, params) {

  if (type == "param_pred")
    params <- params$lambda

  pred_mat <- predict(object, new_data, type = "response", s = params)

  if (type == "response")
    return(as_pred_tibble(pred_mat))

  untidy <- as_pred_tibble(pred_mat, names = params)
  untidy <- add_id_column(untidy)
  tidyr::gather(untidy, lambda, .pred, -id)
}

predict_cv_glmnet_mgaussian <- function(object, new_data, type, params) {

  if (type == "param_pred")
    params <- params$lambda

  pred_array <- predict(object, new_data, s = params)

  if (type == "response") {
    pred_mat <- pred_array[, , 1]
    response_names <- paste0(".pred_", colnames(pred_mat))
    return(as_pred_tibble(pred_mat, response_names))
  }

  add_id_and_lambda <- function(data, lambda) {
    data <- tibble::add_column(data, lambda = lambda, .before = TRUE)
    add_id_column(data)
  }

  pred_list <- apply(pred_array, 3, as_tibble)
  pred_list <- purrr::map2(pred_list, params, add_id_and_lambda)
  bind_rows(pred_list)
}

predict_cv_glmnet_binomial <- function(
  object,
  new_data,
  type,
  lambda,
  threshold) {

  pred_mat <- predict(object, new_data, type = "response", s = lambda)
  classes <- object$glmnet.fit$classnames

  binomial_helper(
    raw = as.vector(pred_mat),
    levels = classes,
    type = type,
    threshold = threshold
  )
}

predict_cv_glmnet_multinomial <- function(object, new_data, type, lambda) {

  pred_array <- predict(object, new_data, type = "response", s = lambda)
  pred_mat <- pred_array[, , 1]

  multinomial_helper(
    raw = pred_mat,
    levels = object$glmnet.fit$classnames,
    type = type
  )
}
