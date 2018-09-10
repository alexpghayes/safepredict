# context("test-glm")
#
# library(tibble)
#
# all <- c("link", "conf_int")
#
# type_by_family <- list(
#   binomial = c("class", "prob"),
#   gaussian = "response",
#   Gamma = "response",
#   inv_gaussian = "response",
#   poisson = "response",
#   quasi = "response",
#   quasibinomial = c("class", "prob"),
#   quasipoisson = "response"
# )
#
# binomial_df <- data.frame(
#   y = as.factor(c(rep("A", 50), rep("B", 50))),
#   x = c(rnorm(50, 1), rnorm(50, 3))
# )
#
# binomial_fit <- glm(y ~ x, binomial_df, family = binomial)
# quasibinomial_fit <- glm(y ~ x, binomial_df, family = quasibinomial)
#
# gaussian_df <- tibble(y = rnorm(100), x = rnorm(100))
# gaussian_fit <- glm(y ~ x, gaussian_df, family = gaussian)
#
# gamma_df <- tibble(y = rnorm(100, 50), x = rnorm(100))
# gamma_fit <- glm(y ~ x, gamma_df, family = Gamma)
#
# inv_gaussian_fit <- glm(y ~ x, gamma_df, family = inverse.gaussian)
#
# poisson_df <- tibble(y = rpois(100, 3), x = rnorm(100))
# poisson_fit <- glm(y ~ x, poisson_df, family = poisson)
#
# quasipoisson_fit <- glm(y ~ x, poisson_df, family = quasipoisson)
#
# quasi_df <- tibble(x = rnorm(100), y = rpois(100, exp(1 + x)))
# quasi_fit <- glm(
#   y ~ x,
#   quasi_df,
#   family = quasi(variance = "mu", link = "logit"),
#   start = c(0, 1)
# )
#
# all_fits <- list(
#
#   binomial_fit,
#   quasibinomial_fit,
#   gaussian_fit,
#   inv_gaussian_fit,
#   poisson_fit,
#   quasipoisson_fit,
#   quasi_fit
# )
#
# test_that("type = link", {
#
#   for (fit in all_fits) {
#
#   }
# })
