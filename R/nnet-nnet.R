
nn_dat <- read.csv("nnet_test.txt")

test_that('multivariate nnet formula', {

  skip_if_not_installed("keras")

  nnet_form <-
    mlp(
      mode = "regression",
      hidden_units = 3,
      penalty = 0.01
    ) %>%
    parsnip::fit(
      cbind(V1, V2, V3) ~ .,
      data = nn_dat[-(1:5),],
      engine = "keras"
    )
  expect_equal(length(unlist(keras::get_weights(nnet_form$fit))), 24)
  nnet_form_pred <- predict_num(nnet_form, new_data = nn_dat[1:5, -(1:3)])
  expect_equal(ncol(nnet_form_pred), 3)
  expect_equal(nrow(nnet_form_pred), 5)
  expect_equal(names(nnet_form_pred), c("V1", "V2", "V3"))

  keras::backend()$clear_session()

  nnet_xy <-
    mlp(
      mode = "regression",
      hidden_units = 3,
      penalty = 0.01
    ) %>%
    parsnip::fit_xy(
      x = nn_dat[-(1:5), -(1:3)],
      y = nn_dat[-(1:5),   1:3 ],
      engine = "keras"
    )
  expect_equal(length(unlist(keras::get_weights(nnet_xy$fit))), 24)
  nnet_form_xy <- predict_num(nnet_xy, new_data = nn_dat[1:5, -(1:3)])
  expect_equal(ncol(nnet_form_xy), 3)
  expect_equal(nrow(nnet_form_xy), 5)
  expect_equal(names(nnet_form_xy), c("V1", "V2", "V3"))

  keras::backend()$clear_session()
})
