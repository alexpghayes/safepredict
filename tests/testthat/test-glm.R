context("test-glm")

library(tibble)

df <- data.frame(
  y = as.factor(c(rep("A", 50), rep("B", 50))),
  x = c(rnorm(50, 1), rnorm(50, 3))
)

fit <- glm(y ~ x, df, family = binomial)

safe_predict(fit, df)

pred <- predict(fit, type = "response")
pred

mf <- model.frame(fit)
mr <- model.response(mf)

if (!is.factor(mr))
  stop("safe_predict only works when outcome has been specified as a factor")

lvl <- levels(mr)  # first element is reference level
                   # second element is "positive" level


tibble::tibble(
  !!paste0(".pred_", lvl[1]) := 1 - pred,
  !!paste0(".pred_", lvl[2]) := pred
)



library(parsnip)

log_spec <- logistic_reg(regularization = 0, mixture = 0)




test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
