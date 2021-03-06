#' #' @export
#' safe_predict.xgb.Booster <- function(
#'   object,
#'   new_data,
#'   type = c(
#'     "response",
#'     "class",
#'     "prob"
#'   ),
#'   ...) {
#'
#'   # the following seems to mess things up, perhaps because of how missingness
#'   # is encoded
#'   # new_data <- to_xgb_input(new_data)
#'   type <- arg_match(type)
#'
#'   pred <- xgb_pred(object, new_data)
#'   return(as_tibble(pred))
#'
#'   # another early exit to get to MVP stage ASAP
#'
#'   switch(
#'     type,
#'     "response" = predict_earth_response(object, new_data),
#'     "class" = predict_earth_class(object, new_data, threshold),
#'     "prob" = predict_earth_prob(object, new_data),
#'     no_method_for_type_error()
#'   )
#' }
#'
#' to_xgb_input <- function(data) {
#'   # MK check to make sure all columns are numeric first?
#'
#'   if (!inherits(data, "xgb.DMatrix"))
#'     data <- xgboost::xgb.DMatrix(data = as.matrix(data), missing = NA)
#'   data
#' }
#'
#' xgb_pred <- function(object, new_data, ...) {
#'   res <- predict(object, to_xgb_input(new_data), ...)
#'
#'   x = switch(
#'     object$params$objective,
#'     "reg:linear" =, "reg:logistic" =, "binary:logistic" = res,
#'     "binary:logitraw" = stats::binomial()$linkinv(res),
#'     "multi:softprob" = matrix(res, ncol = object$params$num_class, byrow = TRUE),
#'     res
#'   )
#'   x
#' }
#'
#' predict_xgb_response <- function(object, new_data, ...) {
#'   pred <- predict(object, newdata = new_data)
#' }
#'
#' predict_xgb_class <- function(object, new_data, ...) {
#'   pred <- xgb_pred(object, newdata = new_data, type = "response")
#'
#'   # TODO: use class_prob_helper, for class and probability responses
#' }
#'
#' predict_xgb_prob <- function(object, new_data, ...) {
#'   pred <- xgb_pred(object, newdata = new_data, type = "response")
#'
#'   post = function(x, object) {
#'     if (is.vector(x)) {
#'       x <- tibble(v1 = 1 - x, v2 = x)
#'     } else {
#'       x <- as_tibble(x)
#'     }
#'     colnames(x) <- object$lvl
#'     x
#'   }
#' }
