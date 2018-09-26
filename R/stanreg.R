# pred = list(
#   pre = NULL,
#   post = NULL,
#   func = c(fun = "predict"),
#   args =
#     list(
#       object = quote(object$fit),
#       newdata = quote(new_data)
#     )
# ),
# confint = list(
#   pre = NULL,
#   post = function(results, object) {
#     res <-
#       tibble(
#         .pred_lower =
#           convert_stan_interval(
#             results,
#             level = object$spec$method$confint$extras$level
#           ),
#         .pred_upper =
#           convert_stan_interval(
#             results,
#             level = object$spec$method$confint$extras$level,
#             lower = FALSE
#           ),
#       )
#     if(object$spec$method$confint$extras$std_error)
#       res$.std_error <- apply(results, 2, sd, na.rm = TRUE)
#     res
#   },
#   func = c(pkg = "rstanarm", fun = "posterior_linpred"),
#   args =
#     list(
#       object = quote(object$fit),
#       newdata = quote(new_data),
#       transform = TRUE,
#       seed = expr(sample.int(10^5, 1))
#     )
# ),
# predint = list(
#   pre = NULL,
#   post = function(results, object) {
#     res <-
#       tibble(
#         .pred_lower =
#           convert_stan_interval(
#             results,
#             level = object$spec$method$predint$extras$level
#           ),
#         .pred_upper =
#           convert_stan_interval(
#             results,
#             level = object$spec$method$predint$extras$level,
#             lower = FALSE
#           ),
#       )
#     if(object$spec$method$predint$extras$std_error)
#       res$.std_error <- apply(results, 2, sd, na.rm = TRUE)
#     res
#   },
#   func = c(pkg = "rstanarm", fun = "posterior_predict"),
#   args =
#     list(
#       object = quote(object$fit),
#       newdata = quote(new_data),
#       seed = expr(sample.int(10^5, 1))
#     )
# ),
# raw = list(
#   pre = NULL,
#   func = c(fun = "predict"),
#   args =
#     list(
#       object = quote(object$fit),
#       newdata = quote(new_data)
#     )
# )

#'
#' #' @importFrom stats quantile
#' convert_stan_interval <- function(x, level = 0.95, lower = TRUE) {
#'   alpha <- (1 - level) / 2
#'   if (!lower) {
#'     alpha <- 1 - alpha
#'   }
#'   res <- apply(x, 2, quantile, probs = alpha, na.rm = TRUE)
#'   res <- unname(res)
#'   res
#' }


,
classes = list(
  pre = NULL,
  post = function(x, object) {
    x <- object$fit$family$linkinv(x)
    x <- ifelse(x >= 0.5, object$lvl[2], object$lvl[1])
    unname(x)
  },
  func = c(fun = "predict"),
  args =
    list(
      object = quote(object$fit),
      newdata = quote(new_data)
    )
),
prob = list(
  pre = NULL,
  post = function(x, object) {
    x <- object$fit$family$linkinv(x)
    x <- tibble(v1 = 1 - x, v2 = x)
    colnames(x) <- object$lvl
    x
  },
  func = c(fun = "predict"),
  args =
    list(
      object = quote(object$fit),
      newdata = quote(new_data)
    )
),
raw = list(
  pre = NULL,
  func = c(fun = "predict"),
  args =
    list(
      object = quote(object$fit),
      newdata = quote(new_data)
    )
),
confint = list(
  pre = NULL,
  post = function(results, object) {
    res <-
      tibble(
        .pred_lower =
          convert_stan_interval(
            results,
            level = object$spec$method$confint$extras$level
          ),
        .pred_upper =
          convert_stan_interval(
            results,
            level = object$spec$method$confint$extras$level,
            lower = FALSE
          ),
      )
    if(object$spec$method$confint$extras$std_error)
      res$.std_error <- apply(results, 2, sd, na.rm = TRUE)
    res
  },
  func = c(pkg = "rstanarm", fun = "posterior_linpred"),
  args =
    list(
      object = quote(object$fit),
      newdata = quote(new_data),
      transform = TRUE,
      seed = expr(sample.int(10^5, 1))
    )
),
predint = list(
  pre = NULL,
  post = function(results, object) {
    res <-
      tibble(
        .pred_lower =
          convert_stan_interval(
            results,
            level = object$spec$method$predint$extras$level
          ),
        .pred_upper =
          convert_stan_interval(
            results,
            level = object$spec$method$predint$extras$level,
            lower = FALSE
          ),
      )
    if(object$spec$method$predint$extras$std_error)
      res$.std_error <- apply(results, 2, sd, na.rm = TRUE)
    res
  },
  func = c(pkg = "rstanarm", fun = "posterior_predict"),
  args =
    list(
      object = quote(object$fit),
      newdata = quote(new_data),
      seed = expr(sample.int(10^5, 1))
    )
)
