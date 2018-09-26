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
#'

# TODO: what `types` of predictions make sense
# TODO: ugh I should probably be dispatching on the subclass rather
#   than switching on the family attribute

# params:
#   - either a positive linear interpolation when
#     not fit with these exact lambda
#   - "1-se" or "min"
#   - a grid of lambda to predict across as a tidy tibble, with
#     one column for each hyperparameter and one row for each set of
#     predictions to make on the data

# should type always agree for multi_predict and safe_predict?

multi_predict.glmnet <- function(
  object,
  new_data,
  type = c(
    "response",
    "class",
    "prob",
    "link"
  ),
  params = NULL) {

  ## input validation

  new_data <- Matrix::as.matrix(new_data)
  type <- match.arg(type)

  if (length(params) == 1)
    return(safe_predict(object, new_data, type, params))

  # TODO: some validation on the hyperparameter grid.
  # return some vector of penalties to consider

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
    multi_glmnet_link(object, new_data, params)
  else if (family == "gaussian")
    multi_glmnet_numeric(object, new_data, type, params)
  else if (family == "mgaussian")
    multi_glmnet_mgaussian(object, new_data, type, params)
  else if (family == "binomial")
    multi_glmnet_binomial(object, new_data, type, params)
  else if (family == "multinomial")
    multi_glmnet_multinomial(object, new_data, type, params)
  else if (family == "poisson")
    multi_glmnet_numeric(object, new_data, type, params)
  else
    stop("Family: ", family, " not yet supported.", call. = FALSE)
}


multi_glmnet_link <- function(object, new_data, params) {
  pred_mat <- predict(object, new_data, type = "link", s = params)
  as_pred_tibble(pred_mat)
}

multi_glmnet_numeric <- function(object, new_data, type, params) {
  pred_mat <- predict(object, new_data, type = "response", s = params)
  untidy <- as_pred_tibble(pred_mat, names = params)
  untidy <- add_id_column(untidy)
  tidyr::gather(untidy, lambda, .pred, -id)
}

add_id_and_lambda <- function(data, lambda) {
  data <- tibble::add_column(data, lambda = lambda, .before = TRUE)
  add_id_column(data)
}

multi_glmnet_mgaussian <- function(object, new_data, type, params) {
  pred_array <- predict(object, new_data, s = params)
  pred_list <- apply(pred_array, 3, as_tibble)
  pred_list <- purrr::map2(pred_list, params, add_id_and_lambda)
  dplyr::bind_rows(pred_list)
}

multi_glmnet_binomial <- function(
  object,
  new_data,
  type,
  params,
  threshold) {
  pred_mat <- predict(object, new_data, type = "response", s = params)
  levels <- object$glmnet.fit$classnames
  # forcing class probs here rather than factor predictions due to
  # interface limitations
  pred_list <- apply(pred_mat, 2, binomial_helper, levels, "prob")
  pred_list <- purrr::map2(pred_list, params, add_id_and_lambda)
  dplyr::bind_rows(pred_list)

}

multi_glmnet_multinomial <- function(object, new_data, type, params) {
  pred_array <- predict(object, new_data, type = "response", s = params)
  levels <- object$glmnet.fit$classnames
  # forcing class probs here rather than factor predictions due to
  # interface limitations
  pred_list <- apply(pred_array, 3, multinomial_helper, levels, "prob")
  pred_list <- purrr::map2(pred_list, params, add_id_and_lambda)
  dplyr::bind_rows(pred_list)
}
#'
#' #' @importFrom dplyr full_join as_tibble arrange
#' #' @importFrom tidyr gather
#' #' @export
#' multi_predict._lognet <-
#'   function(object, new_data, type = NULL, penalty = NULL, ...) {
#'     dots <- list(...)
#'     if (is.null(penalty))
#'       penalty <- object$lambda
#'
#'     if (is.null(type))
#'       type <- "class"
#'     if (!(type %in% c("class", "prob", "link"))) {
#'       stop ("`type` should be either 'class', 'link', or 'prob'.", call. = FALSE)
#'     }
#'     if (type == "prob")
#'       dots$type <- "response"
#'     else
#'       dots$type <- type
#'
#'     dots$s <- penalty
#'     pred <- predict(object, new_data = new_data, type = "raw", opts = dots)
#'     param_key <- tibble(group = colnames(pred), penalty = penalty)
#'     pred <- as_tibble(pred)
#'     pred$.row <- 1:nrow(pred)
#'     pred <- gather(pred, group, .pred, -.row)
#'     if (dots$type == "class") {
#'       pred[[".pred"]] <- factor(pred[[".pred"]], levels = object$lvl)
#'     } else {
#'       if (dots$type == "response") {
#'         pred[[".pred2"]] <- 1 - pred[[".pred"]]
#'         names(pred) <- c(".row", "group", paste0(".pred_", rev(object$lvl)))
#'         pred <- pred[, c(".row", "group", paste0(".pred_", object$lvl))]
#'       }
#'     }
#'     pred <- full_join(param_key, pred, by = "group")
#'     pred$group <- NULL
#'     pred <- arrange(pred, .row, penalty)
#'     .row <- pred$.row
#'     pred$.row <- NULL
#'     pred <- split(pred, .row)
#'     names(pred) <- NULL
#'     tibble(.pred = pred)
#'   }
#'
#' #' @importFrom dplyr full_join as_tibble arrange
#' #' @importFrom tidyr gather
#' #' @export
#' multi_predict._elnet <-
#'   function(object, new_data, type = NULL, penalty = NULL, ...) {
#'     dots <- list(...)
#'     if (is.null(penalty))
#'       penalty <- object$fit$lambda
#'     dots$s <- penalty
#'     pred <- predict(object, new_data = new_data, type = "raw", opts = dots)
#'     param_key <- tibble(group = colnames(pred), penalty = penalty)
#'     pred <- as_tibble(pred)
#'     pred$.row <- 1:nrow(pred)
#'     pred <- gather(pred, group, .pred, -.row)
#'     pred <- full_join(param_key, pred, by = "group")
#'     pred$group <- NULL
#'     pred <- arrange(pred, .row, penalty)
#'     .row <- pred$.row
#'     pred$.row <- NULL
#'     pred <- split(pred, .row)
#'     names(pred) <- NULL
#'     tibble(.pred = pred)
#'   }
#'
#'
#'
#'
#' organize_glmnet_pred <- function(x, object) {
#'   if (ncol(x) == 1) {
#'     res <- x[, 1]
#'     res <- unname(res)
#'   } else {
#'     n <- nrow(x)
#'     res <- utils::stack(as.data.frame(x))
#'     if (!is.null(object$spec$args$penalty))
#'       res$lambda <- rep(object$spec$args$penalty, each = n) else
#'         res$lambda <- rep(object$fit$lambda, each = n)
#'     res <- res[, colnames(res) %in% c("values", "lambda")]
#'   }
#'   res
#' }
#'
#' organize_glmnet_class <- function(x, object) {
#'   if (ncol(x) == 1) {
#'     res <- prob_to_class_2(x[, 1], object)
#'   } else {
#'     n <- nrow(x)
#'     res <- utils::stack(as.data.frame(x))
#'     res$values <- prob_to_class_2(res$values, object)
#'     if (!is.null(object$spec$args$penalty))
#'       res$lambda <- rep(object$spec$args$penalty, each = n) else
#'         res$lambda <- rep(object$fit$lambda, each = n)
#'     res <- res[, colnames(res) %in% c("values", "lambda")]
#'   }
#'   res
#' }
#'
#' organize_glmnet_prob <- function(x, object) {
#'   if (ncol(x) == 1) {
#'     res <- tibble(v1 = 1 - x[, 1], v2 = x[, 1])
#'     colnames(res) <- object$lvl
#'   } else {
#'     n <- nrow(x)
#'     res <- utils::stack(as.data.frame(x))
#'     res <- tibble(v1 = 1 - res$values, v2 = res$values)
#'     colnames(res) <- object$lvl
#'     if (!is.null(object$spec$args$penalty))
#'       res$lambda <- rep(object$spec$args$penalty, each = n) else
#'         res$lambda <- rep(object$fit$lambda, each = n)
#'   }
#'   res
#' }
#'
#' #' @importFrom utils globalVariables
#' utils::globalVariables(c("group", ".pred"))
