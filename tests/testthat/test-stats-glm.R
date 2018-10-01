context("test-glm")

library(tibble)
library(modeltests)

test_that("arguments", {
  check_predict_arguments(safe_predict.glm)
})

binomial_df <- tibble(
  y = as.factor(c(rep("A", 50), rep("B", 50))),
  x = c(rnorm(50, 1), rnorm(50, 3))
)

binomial_fit <- glm(y ~ x, binomial_df, family = binomial)
quasibinomial_fit <- glm(y ~ x, binomial_df, family = quasibinomial)

gaussian_df <- tibble(y = rnorm(100), x = rnorm(100))
gaussian_fit <- glm(y ~ x, gaussian_df, family = gaussian)

gamma_df <- tibble(y = rnorm(100, 50), x = rnorm(100))
gamma_fit <- glm(y ~ x, gamma_df, family = Gamma)

inv_gaussian_fit <- glm(y ~ x, gamma_df, family = inverse.gaussian)

poisson_df <- tibble(y = rpois(100, 3), x = rnorm(100))
poisson_fit <- glm(y ~ x, poisson_df, family = poisson)

quasipoisson_fit <- glm(y ~ x, poisson_df, family = quasipoisson)

quasi_df <- tibble(x = rnorm(100), y = rpois(100, exp(1 + x)))
quasi_fit <- glm(
  y ~ x,
  quasi_df,
  family = quasi(variance = "mu", link = "logit"),
  start = c(0, 1)
)

fit_list <- list(
  binomial_fit,
  quasibinomial_fit,
  gaussian_fit,
  gamma_fit,
  inv_gaussian_fit,
  poisson_fit,
  quasipoisson_fit,
  quasi_fit
)

data_list <- list(
  binomial_df,
  binomial_df,
  gaussian_df,
  gamma_df,
  gamma_df,
  poisson_df,
  poisson_df,
  quasi_df
)

type_list <- list(
  c("class", "prob"),
  c("class", "prob"),
  "response",
  "response",
  "response",
  "response",
  "response",
  "response"
)

type_list <- purrr::map(type_list, ~c("link", "conf_int", .x))

check_fit <- function(fit, data, types) {
  check_predict(
    predict_method = safe_predict.glm,
    object = fit,
    new_data = data,
    outcome = "y",
    types = types
  )
}

test_that("predictions", {
  purrr::pmap(
    list(fit_list, data_list, type_list),
    ~check_fit(..1, ..2, ..3)
  )
})



test_that('glm prediction', {
  classes_xy <- fit_xy(
    lc_basic,
    x = lending_club[, num_pred],
    y = lending_club$Class,
    engine = "glm",
    control = ctrl
  )

  xy_pred <- predict(classes_xy$fit, newdata = lending_club[1:7, num_pred], type = "response")
  xy_pred <- ifelse(xy_pred >= 0.5, "good", "bad")
  xy_pred <- factor(xy_pred, levels = levels(lending_club$Class))
  xy_pred <- unname(xy_pred)
  expect_equal(xy_pred, predict_class(classes_xy, lending_club[1:7, num_pred]))

})

test_that('glm probabilities', {
  classes_xy <- fit_xy(
    lc_basic,
    x = lending_club[, num_pred],
    y = lending_club$Class,
    engine = "glm",
    control = ctrl
  )

  xy_pred <- predict(classes_xy$fit, newdata = lending_club[1:7, num_pred], type = "response")
  xy_pred <- tibble(bad = 1 - xy_pred, good = xy_pred)
  expect_equal(xy_pred, predict_classprob(classes_xy, lending_club[1:7, num_pred]))

  one_row <- predict_classprob(classes_xy, lending_club[1, num_pred])
  expect_equal(xy_pred[1,], one_row)

})



test_that('glm intervals', {
  stats_glm <- glm(Class ~ log(funded_amnt) + int_rate, data = lending_club,
                   family = binomial)
  pred_glm <- predict(stats_glm, newdata = lending_club[1:5, ], se.fit = TRUE)
  t_val <- qt(0.035, df = stats_glm$df.residual, lower.tail = FALSE)
  lower_glm <- pred_glm$fit - t_val * pred_glm$se.fit
  upper_glm <- pred_glm$fit + t_val * pred_glm$se.fit

  lower_glm <- stats_glm$family$linkinv(lower_glm)
  upper_glm <- stats_glm$family$linkinv(upper_glm)

  res <- fit(
    logistic_reg(),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    engine = "glm",
    control = ctrl
  )

  confidence_parsnip <-
    predict(res,
            new_data = lending_club[1:5,],
            type = "conf_int",
            level = 0.93,
            std_error = TRUE)

  expect_equivalent(confidence_parsnip$.pred_lower, lower_glm)
  expect_equivalent(confidence_parsnip$.pred_upper, upper_glm)
  expect_equivalent(confidence_parsnip$.std_error, pred_glm$se.fit)

})

