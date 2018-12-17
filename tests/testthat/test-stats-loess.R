context("test-stats-loess")

#
# fit <- loess(mpg ~ wt, mtcars)
# predict(fit, tibble(wt = 1:10), se = TRUE)
#
# safe_predict(fit, mtcars)
# safe_predict(fit, mtcars, std_error = TRUE)
#
# safe_predict(fit, mtcars, type = "conf_int")
# safe_predict(fit, mtcars, type = "pred_int") %>%
#   bind_cols(mtcars) %>%
#   ggplot(aes(wt, mpg)) +
#   geom_point() +
#   geom_ribbon(aes(wt, ymin = .pred_lower, ymax = .pred_upper), alpha = 0.3)
#
# # extrapolation
# # to get extrapolation
# fit2 <- loess(mpg ~ wt, mtcars, control = loess.control(surface = "direct"))
# predict(fit2, tibble(wt = 1:10), se = TRUE)
