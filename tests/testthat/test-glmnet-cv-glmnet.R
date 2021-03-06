# context("test-glmnet-cv-glmnet")
#
# library(tibble)
# library(glmnet)
#
# x <- matrix(rnorm(100 * 5), 100, 5)
# y <-  x[, 1] * 3 + rnorm(100)
# fit <- cv.glmnet(x, y)
#
# test <- matrix(rnorm(10 * 5), 10, 5)
# diag(test) <- NA
#
# safe_predict(fit, test)
# safe_predict(fit, test, type = "response")
# safe_predict(fit, test, type = "link")
#
# safe_predict(fit, test, penalty = "1-se")
# safe_predict(fit, test, penalty = "min")
#
# safe_predict(fit, test, type = "class")
# safe_predict(fit, test, type = "prob")
#
# # long format, need to change to nested
# multi_predict(fit, test, params = 1:5)
#
# # this should error
# multi_predict(fit, test, params = 1:5, type = "prob")
#
# # this shouldn't error
# multi_predict(fit, test, params = 1:5, type = "link")
#
# y2 <- as.factor(rep(LETTERS[1:2], each = 50))
#
# x2 <-  rbind(
#   matrix(rnorm(50 * 3), 50, 3),
#   matrix(rnorm(50 * 3, 10), 50, 3)
# )
#
# test2 <- head(x2, 10)
# diag(test2) <- NA
#
# fit2 <- cv.glmnet(x2,  y2, family = "binomial")
#
# safe_predict(fit2, test2, type = "class")
# safe_predict(fit2, test2, type = "link")
#
# safe_predict(fit2, test2, penalty = "1-se")
# safe_predict(fit2, test2, penalty = "min")
#
# safe_predict(fit2, test2, type = "class")
# safe_predict(fit2, test2, type = "prob")
#
# multi_predict(fit2, test2, params = 1:5)
#
# multi_predict(fit2, test2, params = 1:5, type = "prob")
#
# multi_predict(fit2, test2, params = 1:5, type = "link")
#
#
# y3 <-  sample(LETTERS[1:5], 100, replace = TRUE)
# fit3 <- cv.glmnet(x2, y3, family = "multinomial")
#
# safe_predict(fit3, test2, type = "class")
# safe_predict(fit3, test2, type = "link")
#
# safe_predict(fit3, test2, penalty = "1-se")
# safe_predict(fit3, test2, penalty = "min")
#
# safe_predict(fit3, test2, type = "prob")
#
# # this should error for glmnet only cases
# safe_predict(fit3, test2, penalty = NULL)
#
# # this should error because we should be using multi_predict instead
# safe_predict(fit3, test2, penalty = 1:4)
#
# y4 <-  rpois(100, round(x[, 1]) + 10)
# fit4 <- cv.glmnet(x, y4, family = "poisson")
#
# safe_predict(fit4, test, type = "class")
# safe_predict(fit4, test, type = "link")
#
# safe_predict(fit4, test, penalty = "1-se")
# safe_predict(fit4, test, penalty = "min")
#
#
# # this should error for glmnet only cases
# safe_predict(fit4, test, penalty = NULL)
#
# # this should error because we should be using multi_predict instead
# safe_predict(fit4, test, penalty = 1:4)
#
#
# x <- matrix(rnorm(100 * 5), 100, 5)
# y5 <-  cbind(3 * x[, 1], 5 * x[, 2] + 4 * x[, 3], 0.5 * x[, 1])
# fit5 <- cv.glmnet(x, y5, family = "mgaussian")
#
# safe_predict(fit5, test, type = "class")
# safe_predict(fit5, test, type = "link")
#
# safe_predict(fit5, test, penalty = "1-se")
# safe_predict(fit5, test, penalty = "min")
#
#
# # this should error for glmnet only cases
# safe_predict(fit5, test, penalty = NULL)
#
# # this should error because we should be using multi_predict instead
# safe_predict(fit5, test, penalty = 1:4)
