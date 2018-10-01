context("test-rstanarm-logistic-reg")


test_that('stan_glm prediction', {

  skip_if_not_installed("rstanarm")
  library(rstanarm)

  xy_fit <- fit_xy(
    logistic_reg(others = list(seed =  11, chains = 1)),
    engine = "stan",
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  xy_pred <-
    predict(xy_fit$fit,
            newdata = lending_club[1:7, num_pred])
  xy_pred <- xy_fit$fit$family$linkinv(xy_pred)
  xy_pred <- ifelse(xy_pred >= 0.5, "good", "bad")
  xy_pred <- factor(xy_pred, levels = levels(lending_club$Class))
  xy_pred <- unname(xy_pred)

  expect_equal(xy_pred, predict_class(xy_fit, lending_club[1:7, num_pred]))

  res_form <- fit(
    logistic_reg(others = list(seed =  11, chains = 1)),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    engine = "stan",
    control = ctrl
  )

  form_pred <-
    predict(res_form$fit,
            newdata = lending_club[1:7, c("funded_amnt", "int_rate")])
  form_pred <- xy_fit$fit$family$linkinv(form_pred)
  form_pred <- unname(form_pred)
  form_pred <- ifelse(form_pred >= 0.5, "good", "bad")
  form_pred <- factor(form_pred, levels = levels(lending_club$Class))
  expect_equal(form_pred, predict_class(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]))
})



test_that('stan_glm probability', {

  skip_if_not_installed("rstanarm")

  xy_fit <- fit_xy(
    logistic_reg(others = list(seed =  11, chains = 1)),
    engine = "stan",
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  xy_pred <-
    predict(xy_fit$fit,
            newdata = lending_club[1:7, num_pred])
  xy_pred <- xy_fit$fit$family$linkinv(xy_pred)
  xy_pred <- tibble(bad = 1 - xy_pred, good = xy_pred)

  expect_equal(xy_pred, predict_classprob(xy_fit, lending_club[1:7, num_pred]))

  res_form <- fit(
    logistic_reg(others = list(seed =  11, chains = 1)),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    engine = "stan",
    control = ctrl
  )

  form_pred <-
    predict(res_form$fit,
            newdata = lending_club[1:7, c("funded_amnt", "int_rate")])
  form_pred <- xy_fit$fit$family$linkinv(form_pred)
  form_pred <- tibble(bad = 1 - form_pred, good = form_pred)
  expect_equal(form_pred, predict_classprob(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]))
})


test_that('stan intervals', {
  skip_if_not_installed("rstanarm")
  library(rstanarm)

  res_form <- fit(
    logistic_reg(others = list(seed =  11, chains = 1)),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    engine = "stan",
    control = ctrl
  )

  set.seed(555)
  confidence_parsnip <-
    predict(res_form,
            new_data = lending_club[1:5,],
            type = "conf_int",
            level = 0.93,
            std_error = TRUE)

  set.seed(555)
  prediction_parsnip <-
    predict(res_form,
            new_data = lending_club[1:5,],
            type = "pred_int",
            level = 0.93,
            std_error = TRUE)

  stan_post <-
    posterior_linpred(res_form$fit, newdata = lending_club[1:5, ], seed = 13,
                      prob = 0.93, transform = TRUE)

  stan_lower <- apply(stan_post, 2, quantile, prob = 0.035)
  stan_upper <- apply(stan_post, 2, quantile, prob = 0.965)
  stan_std  <- apply(stan_post, 2, sd)

  expect_equivalent(confidence_parsnip$.pred_lower, stan_lower)
  expect_equivalent(confidence_parsnip$.pred_upper, stan_upper)
  expect_equivalent(confidence_parsnip$.std_error, stan_std)

  stan_pred_post <-
    posterior_predict(res_form$fit, newdata = lending_club[1:5, ], seed = 13,
                      prob = 0.93)

  stan_pred_lower <- apply(stan_pred_post, 2, quantile, prob = 0.035)
  stan_pred_upper <- apply(stan_pred_post, 2, quantile, prob = 0.965)
  stan_pred_std  <- apply(stan_pred_post, 2, sd)

  expect_equivalent(prediction_parsnip$.pred_lower, stan_pred_lower)
  expect_equivalent(prediction_parsnip$.pred_upper, stan_pred_upper)
  expect_equivalent(prediction_parsnip$.std_error, stan_pred_std, tolerance = 0.1)
})
