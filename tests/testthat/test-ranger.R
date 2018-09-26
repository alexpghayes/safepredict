context("test-ranger")

reg_rf <- ranger(Sepal.Width ~ ., data = iris)
reg_rf2 <- ranger(Sepal.Width ~ ., data = iris, keep.inbag = TRUE)

predict_ranger_helper(reg_rf, iris)
predict_ranger_helper(reg_rf2, iris, std_error = TRUE)

# why do there two disagree?
predict_ranger_helper(reg_rf, iris, all_trees = TRUE)
predict_ranger_helper(reg_rf2, iris, all_trees = TRUE)

class_rf <- ranger(Species ~ ., data = iris)
class_rf2 <- ranger(Species ~ ., data = iris, keep.inbag = TRUE)

predict_ranger_helper(class_rf, iris)
predict_ranger_helper(class_rf2, iris)

predict_ranger_helper(class_rf, iris, all_trees = TRUE)
predict_ranger_helper(class_rf2, iris, all_trees = TRUE)


class_prob_rf <- ranger(Species ~ ., data = iris, probability = TRUE)
predict(class_prob_rf, iris, type = "response")$predictions
str()
str(predict(class_prob_rf, iris, predict.all = TRUE))


# this should error, and ask for Sepal.Width to be a factor
reg_prob_rf <- ranger(Sepal.Width ~ ., data = iris, probability = TRUE)
predict(reg_prob_rf, iris, predict.all = TRUE)$predictions %>% dim()

# For classification and predict.all = TRUE, a factor levels are returned as numerics. To retrieve the corresponding factor levels, use rf$forest$levels, if rf is the ranger object.




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
