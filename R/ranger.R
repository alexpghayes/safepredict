library(ranger)

safe_predict.ranger  <- function(
  object,
  new_data,
  type = c(
    "response",
    "class",
    "prob",
    "param_pred"
  ),
  params = NULL) {



  mode <- object$treetype

  # check for mode/type compatibility

  # warn when trying to get quantile and probability predictions
  # from objects fit with the wrong stuff

  if (mode == "Probability estimation")
    predict_ranger_prob(object, new_data, type)
  else if (mode == "Classification")
    predict_ranger_class(object, new_data, type)
  else if (mode == "Regression")
    predict_ranger_reg(object, new_data, type)
}

predict_ranger_reg <- function(
  object,
  new_data,
  all_trees = FALSE,
  std_error = FALSE,
  others = NULL) {

  # make sure others works

  if (all_trees && std_error)
    stop("These don't work at the same time")

  if (!all_trees && !std_error) {
    pred_obj <- predict(object, new_data, type = "response")
    return(as_pred_tibble(pred_obj$predictions))
  }

  if (std_error) {
    pred_obj <- predict(object, new_data, type = "response")
    pred <- as_pred_tibble(pred_obj$predictions)
    pred$.std_error <- predict(object, new_data, type = "se")$predictions
    return(pred)
  }

  if (all_trees) {
    pred_obj <- predict(object, new_data, type = "response", predict.all = TRUE)
    pred_mat <- pred_obj$predictions
    pred <- as_pred_tibble(pred_mat, 1:pred_obj$num.trees)
    # TODO: is tree going to be character here? should be numeric
    gather(pred, tree, .pred)
  }
}

predict_ranger_class <- function(
  object,
  new_data,
  all_trees = FALSE,
  others = NULL) {

  # make sure others works

  if (!all_trees) {
    pred_obj <- predict(object, new_data, type = "response")
    return(as_pred_tibble(pred_obj$predictions))
  }

  if (all_trees) {
    pred_obj <- predict(object, new_data, type = "response", predict.all = TRUE)
    pred_mat <- pred_obj$predictions
    pred <- as_pred_tibble(pred_mat, 1:pred_obj$num.trees)
    # TODO: is tree going to be character here? should be numeric
    gather(pred, tree, .pred)
  }
}

predict_ranger_prob <- function(
  object,
  new_data,
  all_trees = FALSE) {


}


reg_rf <- ranger(Sepal.Width ~ ., data = iris)
reg_rf2 <- ranger(Sepal.Width ~ ., data = iris, keep.inbag = TRUE)

predict_ranger_reg(reg_rf, iris)
predict_ranger_reg(reg_rf2, iris, std_error = TRUE)

# why do there two disagree?
predict_ranger_reg(reg_rf, iris, all_trees = TRUE)
predict_ranger_reg(reg_rf2, iris, all_trees = TRUE)

# this should error, and ask for Sepal.Width to be a factor
reg_prob_rf <- ranger(Sepal.Width ~ ., data = iris, probability = TRUE)
predict(reg_prob_rf, iris, predict.all = TRUE)$predictions %>% dim()

# For classification and predict.all = TRUE, a factor levels are returned as numerics. To retrieve the corresponding factor levels, use rf$forest$levels, if rf is the ranger object.


class_rf <- ranger(Species ~ ., data = iris, keep.inbag = TRUE)
class_prob_rf <- ranger(Species ~ ., data = iris, probability = TRUE)



str(class_prob_rf, 2)
str(reg_prob_rf, 2)

# regression only
predict(reg_rf, iris, type = "se")$predictions


reg_rf <- ranger(Sepal.Width ~ ., data = iris, keep.inbag = TRUE, quantreg = TRUE)

predict(reg_rf, iris, type = "quantiles")$predictions
predict(reg_rf, iris, type = "se")$predictions
# ranger prediction cases:
# - regression
#   - aggregate predictions
#   - predictions by tree
# - classification
#   - aggregate predictions
#   - aggregate class probs
#   - class probs by tree

# param_pred: number of trees to use

# response types:
#   - response
#   - se
#   - terminalNodes
#   - quantiles

# vector of quantiles to use in quantile prediction

# treetype: probability estimation

# punt on survival for the moment

str(predict(fit, iris, predict.all = TRUE))

fit <- ranger(Species ~ ., data = iris, probability = TRUE)
str(predict(fit, iris))
str(predict(fit, iris, predict.all = TRUE))


str(predict(fit, iris, predict.all = TRUE))

fit2 <- ranger(Sepal.Length ~ ., data = iris)
str(fit2, 2)


# treetype: regression
rf <- ranger(mpg ~ ., mtcars[1:26, ], quantreg = TRUE)
str(rf, 2)



pred <- predict(rf, mtcars[27:32, ], type = "quantiles")
pred$predictions

library(survival)
rg.veteran <- ranger(Surv(time, status) ~ ., data = veteran)
str(predict(rg.veteran, veteran), 2)

## Alternative interface
fit4 <- ranger(dependent.variable.name = "Species", data = iris)

