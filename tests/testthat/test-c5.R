context("test-c5")

test_that('C5.0 prediction', {

  skip_if_not_installed("C50")

  classes_xy <- fit_xy(
    lc_basic,
    x = lending_club[, num_pred],
    y = lending_club$Class,
    engine = "C5.0",
    control = ctrl
  )

  xy_pred <- predict(classes_xy$fit, newdata = lending_club[1:7, num_pred])
  expect_equal(xy_pred, predict_class(classes_xy, lending_club[1:7, num_pred]))

})

test_that('C5.0 probabilities', {

  skip_if_not_installed("C50")

  classes_xy <- fit_xy(
    lc_basic,
    x = lending_club[, num_pred],
    y = lending_club$Class,
    engine = "C5.0",
    control = ctrl
  )

  xy_pred <- predict(classes_xy$fit, newdata = as.data.frame(lending_club[1:7, num_pred]), type = "prob")
  xy_pred <- as_tibble(xy_pred)
  expect_equal(xy_pred, predict_classprob(classes_xy, lending_club[1:7, num_pred]))

  one_row <- predict_classprob(classes_xy, lending_club[1, num_pred])
  expect_equal(xy_pred[1,], one_row)

})

