# library(ranger)
#
# safe_predict.ranger  <- function(
#   object,
#   new_data,
#   type = c(
#     "response",
#     "class",
#     "prob",
#     "param_pred"
#   ),
#   params = NULL) {
#
#
#
#   mode <- object$treetype
#
#   # check for mode/type compatibility
#
#   # warn when trying to get quantile and probability predictions
#   # from objects fit with the wrong stuff
#
#   if (mode == "Probability estimation")
#     predict_ranger_prob(object, new_data, type)
#   else if (mode == "Classification")
#     predict_ranger_class(object, new_data, type)
#   else if (mode == "Regression")
#     predict_ranger_reg(object, new_data, type)
# }
#
# predict_ranger_reg <- function(
#   object,
#   new_data,
#   all_trees = FALSE,
#   std_error = FALSE,
#   others = NULL) {
#
#   # make sure others works
#
#   if (all_trees && std_error)
#     stop("These don't work at the same time")
#
#   if (!all_trees && !std_error) {
#     pred_obj <- predict(object, new_data, type = "response")
#     return(as_pred_tibble(pred_obj$predictions))
#   }
#
#   if (std_error) {
#     pred_obj <- predict(object, new_data, type = "response")
#     pred <- as_pred_tibble(pred_obj$predictions)
#     pred$.std_error <- predict(object, new_data, type = "se")$predictions
#     return(pred)
#   }
#
#   if (all_trees) {
#     pred_obj <- predict(object, new_data, type = "response", predict.all = TRUE)
#     pred_mat <- pred_obj$predictions
#     pred <- as_pred_tibble(pred_mat, 1:pred_obj$num.trees)
#     # TODO: is tree going to be character here? should be numeric
#     gather(pred, tree, .pred)
#   }
# }
#
#
# reg_rf <- ranger(Sepal.Width ~ ., data = iris)
# reg_rf2 <- ranger(Sepal.Width ~ ., data = iris, keep.inbag = TRUE)
#
# predict_ranger_reg(reg_rf, iris)
# predict_ranger_reg(reg_rf2, iris, std_error = TRUE)
#
# # why do there two disagree?
# predict_ranger_reg(reg_rf, iris, all_trees = TRUE)
# predict_ranger_reg(reg_rf2, iris, all_trees = TRUE)
#
#
# predict_ranger_class <- function(
#   object,
#   new_data,
#   all_trees = FALSE,
#   others = NULL) {
#
#   # make sure others works
#
#   if (!all_trees) {
#     pred_obj <- predict(object, new_data, type = "response")
#     return(as_pred_tibble(pred_obj$predictions))
#   }
#
#   if (all_trees) {
#     pred_obj <- predict(object, new_data, type = "response", predict.all = TRUE)
#     pred_mat <- pred_obj$predictions
#     pred <- as_pred_tibble(pred_mat, 1:pred_obj$num.trees)
#     # TODO: is tree going to be character here? should be numeric
#     gather(pred, tree, .pred)
#   }
# }
#
# class_rf <- ranger(Species ~ ., data = iris)
# class_rf2 <- ranger(Species ~ ., data = iris, keep.inbag = TRUE)
#
# predict_ranger_class(class_rf, iris)
# predict_ranger_class(class_rf2, iris)
#
# predict_ranger_class(class_rf, iris, all_trees = TRUE)
# predict_ranger_class(class_rf2, iris, all_trees = TRUE)
#
# predict_ranger_prob <- function(
#   object,
#   new_data,
#   all_trees = FALSE) {
#
#   if (!all_trees) {
#     pred_obj <- predict(object, new_data, type = "response")
#     pred <- as_tibble(pred_obj$predictions)
#     colnames(pred) <- paste0(".pred", colnames(pred))
#     return(pred)
#   }
#
#   if (all_trees) {
#     pred_obj <- predict(object, new_data, type = "response", predict.all = TRUE)
#     pred_array <- pred_obj$predictions
#
#     pred_list <- apply(pred_array, 3, predict_ranger_prob)
#     pred_list <-
#     do.call(bind_rows, pred_list, .id = ".tree")
#
#     pred <- as_pred_tibble(pred_mat, 1:pred_obj$num.trees)
#     # TODO: is tree going to be character here? should be numeric
#     gather(pred, tree, .pred)
#   }
# }
#
# class_prob_rf <- ranger(Species ~ ., data = iris, probability = TRUE)
# predict(class_prob_rf, iris, type = "response")$predictions
# str()
# str(predict(class_prob_rf, iris, predict.all = TRUE))
#
#
# # this should error, and ask for Sepal.Width to be a factor
# reg_prob_rf <- ranger(Sepal.Width ~ ., data = iris, probability = TRUE)
# predict(reg_prob_rf, iris, predict.all = TRUE)$predictions %>% dim()
#
# # For classification and predict.all = TRUE, a factor levels are returned as numerics. To retrieve the corresponding factor levels, use rf$forest$levels, if rf is the ranger object.
#
#
#
#
# str(class_prob_rf, 2)
# str(reg_prob_rf, 2)
#
# # regression only
# predict(reg_rf, iris, type = "se")$predictions
#
#
# reg_rf <- ranger(Sepal.Width ~ ., data = iris, keep.inbag = TRUE, quantreg = TRUE)
#
# predict(reg_rf, iris, type = "quantiles")$predictions
# predict(reg_rf, iris, type = "se")$predictions
# # ranger prediction cases:
# # - regression
# #   - aggregate predictions
# #   - predictions by tree
# # - classification
# #   - aggregate predictions
# #   - aggregate class probs
# #   - class probs by tree
#
# # param_pred: number of trees to use
#
# # response types:
# #   - response
# #   - se
# #   - terminalNodes
# #   - quantiles
#
# # vector of quantiles to use in quantile prediction
#
# # treetype: probability estimation
#
# # punt on survival for the moment
#
# str(predict(fit, iris, predict.all = TRUE))
#
# fit <- ranger(Species ~ ., data = iris, probability = TRUE)
# str(predict(fit, iris))
# str(predict(fit, iris, predict.all = TRUE))
#
#
# str(predict(fit, iris, predict.all = TRUE))
#
# fit2 <- ranger(Sepal.Length ~ ., data = iris)
# str(fit2, 2)
#
#
# # treetype: regression
# rf <- ranger(mpg ~ ., mtcars[1:26, ], quantreg = TRUE)
# str(rf, 2)
#
#
#
# pred <- predict(rf, mtcars[27:32, ], type = "quantiles")
# pred$predictions
#
# library(survival)
# rg.veteran <- ranger(Surv(time, status) ~ ., data = veteran)
# str(predict(rg.veteran, veteran), 2)
#
# ## Alternative interface
# fit4 <- ranger(dependent.variable.name = "Species", data = iris)
#



# wrappers for ranger
ranger_class_pred <-
  function(results, object)  {
    if (results$treetype == "Probability estimation") {
      res <- colnames(results$predictions)[apply(results$predictions, 1, which.max)]
    } else {
      res <- results$predictions
    }
    res
  }

#' @importFrom stats qnorm
ranger_num_confint <- function(object, new_data, ...) {
  hf_lvl <- (1 - object$spec$method$confint$extras$level)/2
  const <- qnorm(hf_lvl, lower.tail = FALSE)

  res <-
    tibble(
      .pred = predict(object$fit, data = new_data, type = "response", ...)$predictions
    )
  std_error <- predict(object$fit, data = new_data, type = "se", ...)$se
  res$.pred_lower <- res$.pred - const * std_error
  res$.pred_upper <- res$.pred + const * std_error
  res$.pred <- NULL

  if(object$spec$method$confint$extras$std_error)
    res$.std_error <- std_error
  res
}
ranger_class_confint <- function(object, new_data, ...) {
  hf_lvl <- (1 - object$spec$method$confint$extras$level)/2
  const <- qnorm(hf_lvl, lower.tail = FALSE)

  pred <- predict(object$fit, data = new_data, type = "response", ...)$predictions
  pred <- as_tibble(pred)

  std_error <- predict(object$fit, data = new_data, type = "se", ...)$se
  colnames(std_error) <- colnames(pred)
  std_error <- as_tibble(std_error)
  names(std_error) <- paste0(".std_error_", names(std_error))

  lowers <- pred - const * std_error
  names(lowers) <- paste0(".pred_lower_", names(lowers))
  uppers <- pred + const * std_error
  names(uppers) <- paste0(".pred_upper_", names(uppers))

  res <- cbind(lowers, uppers)
  res[res < 0] <- 0
  res[res > 1] <- 1
  res <- as_tibble(res)
  lvl <- rep(object$fit$forest$levels, each = 2)
  col_names <- paste0(c(".pred_lower_", ".pred_upper_"), lvl)
  res <- res[, col_names]

  if(object$spec$method$confint$extras$std_error)
    res <- bind_cols(res, std_error)

  res
}

ranger_confint <- function(object, new_data, ...) {
  if(object$fit$forest$treetype == "Regression") {
    res <- ranger_num_confint(object, new_data, ...)
  } else {
    if(object$fit$forest$treetype == "Probability estimation") {
      res <- ranger_class_confint(object, new_data, ...)
    } else {
      stop ("Cannot compute confidence intervals for a ranger forest ",
            "of type ", object$fit$forest$treetype, ".", call. = FALSE)
    }
  }
  res
}


,
pred = list(
  pre = NULL,
  post = function(results, object) results$predictions,
  func = c(fun = "predict"),
  args =
    list(
      object = quote(object$fit),
      data = quote(new_data),
      type = "response",
      seed = expr(sample.int(10^5, 1)),
      verbose = FALSE
    )
),
classes = list(
  pre = NULL,
  post = ranger_class_pred,
  func = c(fun = "predict"),
  args =
    list(
      object = quote(object$fit),
      data = quote(new_data),
      type = "response",
      seed = expr(sample.int(10^5, 1)),
      verbose = FALSE
    )
),
prob = list(
  pre = function(x, object) {
    if (object$fit$forest$treetype != "Probability estimation")
      stop("`ranger` model does not appear to use class probabilities. Was ",
           "the model fit with `probability = TRUE`?",
           call. = FALSE)
    x
  },
  post = function(x, object) {
    x <- x$prediction
    as_tibble(x)
  },
  func = c(fun = "predict"),
  args =
    list(
      object = quote(object$fit),
      data = quote(new_data),
      seed = expr(sample.int(10^5, 1)),
      verbose = FALSE
    )
),
raw = list(
  pre = NULL,
  func = c(fun = "predict"),
  args =
    list(
      object = quote(object$fit),
      data = quote(new_data),
      seed = expr(sample.int(10^5, 1))
    )
),
confint = list(
  pre = NULL,
  post = NULL,
  func = c(fun = "ranger_confint"),
  args =
    list(
      object = quote(object),
      new_data = quote(new_data),
      seed = expr(sample.int(10^5, 1))
    )
)
