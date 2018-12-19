context("test-stats-mlm")

fit <- lm(cbind(Girth, Height) ~ Volume, data = trees)

## use the first two rows as prediction dataset
predict(fit, newdata = trees)
predict(fit, trees, interval = "confidence")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
