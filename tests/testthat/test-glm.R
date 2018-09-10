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
