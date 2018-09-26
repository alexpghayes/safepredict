context("test-glmnet-cv-glmnet-multi")

library(tibble)
library(glmnet)

x <- matrix(rnorm(100 * 5), 100, 5)
y <-  x[, 1] * 3 + rnorm(100)
fit <- cv.glmnet(x, y)

test <- matrix(rnorm(10 * 5), 10, 5)
diag(test) <- NA

safe_predict(fit, test, params = "1-se")
safe_predict(fit, test, params = "min")
safe_predict(fit, test, params = 0.1)

grid <- tibble(lambda = 1:10)

safe_predict(fit, test, type = "param_pred", params = grid)

y2 <- as.factor(rep(LETTERS[1:2], each = 50))

x2 <-  rbind(
  matrix(rnorm(50 * 3), 50, 3),
  matrix(rnorm(50 * 3, 10), 50, 3)
)

test2 <- head(x2, 10)
diag(test2) <- NA

fit2 <- cv.glmnet(x2,  y2, family = "binomial")

safe_predict(fit2, test2, type = "param_pred", params = grid)

y3 <-  sample(LETTERS[1:5], 100, replace = TRUE)
fit3 <- cv.glmnet(x, y3, family = "multinomial")

safe_predict(fit3, test, type = "param_pred", params = grid)
