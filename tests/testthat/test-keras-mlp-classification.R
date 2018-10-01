context("test-keras-mlp-classification")

library(testthat)
context("simple neural network execution with keras")
library(parsnip)
library(tibble)

###################################################################

num_pred <- names(iris)[1:4]

iris_keras <- mlp(mode = "classification", hidden_units = 2)



test_that('keras classification prediction', {

  skip_if_not_installed("keras")
  library(keras)

  xy_fit <- parsnip::fit_xy(
    iris_keras,
    x = iris[, num_pred],
    y = iris$Species,
    engine = "keras",
    control = ctrl
  )

  xy_pred <- predict_classes(xy_fit$fit, x = as.matrix(iris[1:8, num_pred]))
  xy_pred <- factor(levels(iris$Species)[xy_pred + 1], levels = levels(iris$Species))
  expect_equal(xy_pred, predict_class(xy_fit, new_data = iris[1:8, num_pred]))

  keras::backend()$clear_session()

  form_fit <- parsnip::fit(
    iris_keras,
    Species ~ .,
    data = iris,
    engine = "keras",
    control = ctrl
  )

  form_pred <- predict_classes(form_fit$fit, x = as.matrix(iris[1:8, num_pred]))
  form_pred <- factor(levels(iris$Species)[form_pred + 1], levels = levels(iris$Species))
  expect_equal(form_pred, predict_class(form_fit, new_data = iris[1:8, num_pred]))

  keras::backend()$clear_session()
})


test_that('keras classification probabilities', {

  skip_if_not_installed("keras")

  xy_fit <- parsnip::fit_xy(
    iris_keras,
    x = iris[, num_pred],
    y = iris$Species,
    engine = "keras",
    control = ctrl
  )

  xy_pred <- predict_proba(xy_fit$fit, x = as.matrix(iris[1:8, num_pred]))
  xy_pred <- as_tibble(xy_pred)
  colnames(xy_pred) <- levels(iris$Species)
  expect_equal(xy_pred, predict_classprob(xy_fit, new_data = iris[1:8, num_pred]))

  keras::backend()$clear_session()

  form_fit <- parsnip::fit(
    iris_keras,
    Species ~ .,
    data = iris,
    engine = "keras",
    control = ctrl
  )

  form_pred <- predict_proba(form_fit$fit, x = as.matrix(iris[1:8, num_pred]))
  form_pred <- as_tibble(form_pred)
  colnames(form_pred) <- levels(iris$Species)
  expect_equal(form_pred, predict_classprob(form_fit, new_data = iris[1:8, num_pred]))

  keras::backend()$clear_session()
})

