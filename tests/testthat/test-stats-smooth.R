context("test-stats-smooth")

# test for a good error on missing data

fit <- smooth.spline(mtcars$mpg, mtcars$hwy, cv = TRUE)
predict(fit, c(30, NA, 40))


