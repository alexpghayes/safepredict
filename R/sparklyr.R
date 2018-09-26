# pred = list(
#   pre = NULL,
#   post = function(results, object) {
#     results <- dplyr::rename(results, pred = prediction)
#     results <- dplyr::select(results, pred)
#     results
#   },
#   func = c(pkg = "sparklyr", fun = "ml_predict"),
#   args =
#     list(
#       x = quote(object$fit),
#       dataset = quote(new_data)
#     )
# )

#
#
# format_num <- function(x) {
#   if (inherits(x, "tbl_spark"))
#     return(x)
#
#   if (isTRUE(ncol(x) > 1)) {
#     x <- as_tibble(x)
#     names(x) <- paste0(".pred_", names(x))
#   } else {
#     x <- tibble(.pred = x)
#   }
#
#   x
# }
#
# format_class <- function(x) {
#   if (inherits(x, "tbl_spark"))
#     return(x)
#
#   tibble(.pred_class = x)
# }
#
# format_classprobs <- function(x) {
#   x <- as_tibble(x)
#   names(x) <- paste0(".pred_", names(x))
#   x
# }
#
# type <- check_pred_type(object, type)
#
# res <- switch(
#   type,
#   numeric  = predict_num(object = object, new_data = new_data, ...),
#   class    = predict_class(object = object, new_data = new_data, ...),
#   prob     = predict_classprob(object = object, new_data = new_data, ...),
#   conf_int = predict_confint(object = object, new_data = new_data, ...),
#   pred_int = predict_predint(object = object, new_data = new_data, ...),
#   raw      = predict_raw(object = object, new_data = new_data, opts = opts, ...),
#   stop("I don't know about type = '", "'", type, call. = FALSE)
# )
#
# if (!inherits(res, "tbl_spark")) {
#   res <- switch(
#     type,
#     numeric = format_num(res),
#     class   = format_class(res),
#     prob    = format_classprobs(res),
#     res
#   )
# }
# res

# some spark helper functions

#' @importFrom dplyr starts_with rename rename_at vars funs
format_spark_probs <- function(results, object) {
  results <- dplyr::select(results, starts_with("probability_"))
  results <- dplyr::rename_at(
    results,
    vars(starts_with("probability_")),
    funs(gsub("probability", "pred", .))
  )
  results
}

format_spark_class <- function(results, object) {
  results <- dplyr::select(results, predicted_label)
  results <- dplyr::rename(results, pred_class = predicted_label)
  results
}

format_spark_num <- function(results, object) {
  results <- dplyr::select(results, prediction)
  results <- dplyr::rename(results, pred = prediction)
  results
}

#' @importFrom utils globalVariables
utils::globalVariables(c(".", "predicted_label", "prediction"))


,
classes = list(
  pre = NULL,
  post = format_spark_class,
  func = c(pkg = "sparklyr", fun = "ml_predict"),
  args =
    list(
      x = quote(object$fit),
      dataset = quote(new_data)
    )
),
prob = list(
  pre = NULL,
  post = format_spark_probs,
  func = c(pkg = "sparklyr", fun = "ml_predict"),
  args =
    list(
      x = quote(object$fit),
      dataset = quote(new_data)
    )
)
