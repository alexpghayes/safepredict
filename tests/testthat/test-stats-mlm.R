context("test-stats-mlm")

fit <- lm(cbind(Girth, Height) ~ Volume, data = trees)

## use the first two rows as prediction dataset
predict(fit, newdata = trees)
predict(fit, trees, interval = "confidence")

fit <- lm(cbind(hp, mpg) ~ ., mtcars)
#'
#' safe_predict(fit, mtcars)
#'
#' mt2 <- mtcars
#' diag(mt2) <- NA
#'
#' safe_predict(fit, mt2)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
