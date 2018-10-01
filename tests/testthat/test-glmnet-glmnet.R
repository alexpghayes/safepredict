context("test-glmnet-glmnet")

# this is exactly the same as the cv.glmnet tests except
# now the penalty has to specified

library(tibble)
library(glmnet)

x <- matrix(rnorm(100 * 5), 100, 5)
y <-  x[, 1] * 3 + rnorm(100)
fit <- glmnet(x, y)

test <- matrix(rnorm(10 * 5), 10, 5)
diag(test) <- NA

safe_predict(fit, test)
safe_predict(fit, test, penalty = 0.1)


# long format, need to change to nested
n <- multi_predict(fit, test, params = 1:5) %>%
  nest(-id)

# this should error
multi_predict(fit, test, params = 1:5, type = "prob")

# this shouldn't error
multi_predict(fit, test, params = 1:5, type = "link")

y2 <- as.factor(rep(LETTERS[1:2], each = 50))

x2 <-  rbind(
  matrix(rnorm(50 * 3), 50, 3),
  matrix(rnorm(50 * 3, 10), 50, 3)
)

test2 <- head(x2, 10)
diag(test2) <- NA

fit2 <- glmnet(x2,  y2, family = "binomial")

safe_predict(fit2, test2, type = "class", penalty = 0.1)
safe_predict(fit2, test2, type = "link")

y3 <-  sample(LETTERS[1:5], 100, replace = TRUE)
fit3 <- glmnet(x2, y3, family = "multinomial")

safe_predict(fit3, test2, type = "class")
safe_predict(fit3, test2, type = "link")

safe_predict(fit3, test2, penalty = 0.1)
safe_predict(fit3, test2, penalty = "min")

# NOTE: small penalties result more NA predictions because more variables are
# in the model

y4 <-  rpois(100, round(x[, 1]) + 10)
fit4 <- glmnet(x, y4, family = "poisson")

safe_predict(fit4, test)
safe_predict(fit4, test, penalty = 0.1)

x <- matrix(rnorm(100 * 5), 100, 5)
y5 <-  cbind(3 * x[, 1], 5 * x[, 2] + 4 * x[, 3], 0.5 * x[, 1])
fit5 <- glmnet(x, y5, family = "mgaussian")

safe_predict(fit5, test)
safe_predict(fit5, test, penalty = 0.1)

# this should error
safe_predict(fit5, test, penalty = NULL)

# this should error because we should be using multi_predict instead
safe_predict(fit5, test, penalty = 1:4)




test_that('glmnet prediction, one lambda', {

  skip_if_not_installed("glmnet")

  xy_fit <- fit_xy(
    logistic_reg(penalty = 0.1),
    engine = "glmnet",
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  uni_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            s = xy_fit$spec$args$penalty, type = "response")
  uni_pred <- predict(xy_fit$fit, newx = as.matrix(lending_club[1:7, num_pred]), type = "response")
  uni_pred <- ifelse(uni_pred >= 0.5, "good", "bad")
  uni_pred <- factor(uni_pred, levels = levels(lending_club$Class))
  uni_pred <- unname(uni_pred)

  expect_equal(uni_pred, predict_class(xy_fit, lending_club[1:7, num_pred]))

  res_form <- fit(
    logistic_reg(penalty = 0.1),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    engine = "glmnet",
    control = ctrl
  )

  form_mat <- model.matrix(Class ~ log(funded_amnt) + int_rate, data = lending_club)
  form_mat <- form_mat[1:7, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_mat,
            s = res_form$spec$args$penalty)
  form_pred <- ifelse(form_pred >= 0.5, "good", "bad")
  form_pred <- factor(form_pred, levels = levels(lending_club$Class))
  form_pred <- unname(form_pred)
  expect_equal(form_pred, predict_class(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]))

})


test_that('glmnet prediction, mulitiple lambda', {

  skip_if_not_installed("glmnet")

  xy_fit <- fit_xy(
    logistic_reg(penalty = c(0.01, 0.1)),
    engine = "glmnet",
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  mult_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            s = xy_fit$spec$args$penalty, type = "response")
  mult_pred <- stack(as.data.frame(mult_pred))
  mult_pred$values <- ifelse(mult_pred$values >= 0.5, "good", "bad")
  mult_pred$values <- factor(mult_pred$values, levels = levels(lending_club$Class))
  mult_pred$lambda <- rep(xy_fit$spec$args$penalty, each = 7)
  mult_pred <- mult_pred[, -2]

  expect_equal(mult_pred, predict_class(xy_fit, lending_club[1:7, num_pred]))

  res_form <- fit(
    logistic_reg(penalty = c(0.01, 0.1)),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    engine = "glmnet",
    control = ctrl
  )

  form_mat <- model.matrix(Class ~ log(funded_amnt) + int_rate, data = lending_club)
  form_mat <- form_mat[1:7, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_mat,
            type = "response")
  form_pred <- stack(as.data.frame(form_pred))
  form_pred$values <- ifelse(form_pred$values >= 0.5, "good", "bad")
  form_pred$values <- factor(form_pred$values, levels = levels(lending_club$Class))
  form_pred$lambda <- rep(res_form$spec$args$penalty, each = 7)
  form_pred <- form_pred[, -2]
  expect_equal(form_pred, predict_class(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]))

})

test_that('glmnet prediction, no lambda', {

  skip_if_not_installed("glmnet")

  xy_fit <- fit_xy(
    logistic_reg(others = list(nlambda =  11)),
    engine = "glmnet",
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  mult_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            s = xy_fit$spec$args$penalty, type = "response")
  mult_pred <- stack(as.data.frame(mult_pred))
  mult_pred$values <- ifelse(mult_pred$values >= 0.5, "good", "bad")
  mult_pred$values <- factor(mult_pred$values, levels = levels(lending_club$Class))
  mult_pred$lambda <- rep(xy_fit$fit$lambda, each = 7)
  mult_pred <- mult_pred[, -2]

  expect_equal(mult_pred, predict_class(xy_fit, lending_club[1:7, num_pred]))

  res_form <- fit(
    logistic_reg(others = list(nlambda =  11)),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    engine = "glmnet",
    control = ctrl
  )

  form_mat <- model.matrix(Class ~ log(funded_amnt) + int_rate, data = lending_club)
  form_mat <- form_mat[1:7, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_mat,
            type = "response")
  form_pred <- stack(as.data.frame(form_pred))
  form_pred$values <- ifelse(form_pred$values >= 0.5, "good", "bad")
  form_pred$values <- factor(form_pred$values, levels = levels(lending_club$Class))
  form_pred$lambda <- rep(res_form$fit$lambda, each = 7)
  form_pred <- form_pred[, -2]
  expect_equal(form_pred, predict_class(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]))

})


test_that('glmnet probabilities, one lambda', {

  skip_if_not_installed("glmnet")

  xy_fit <- fit_xy(
    logistic_reg(penalty = 0.1),
    engine = "glmnet",
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  uni_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            s = xy_fit$spec$args$penalty, type = "response")[,1]
  uni_pred <- tibble(bad = 1 - uni_pred, good = uni_pred)

  expect_equal(uni_pred, predict_classprob(xy_fit, lending_club[1:7, num_pred]))

  res_form <- fit(
    logistic_reg(penalty = 0.1),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    engine = "glmnet",
    control = ctrl
  )

  form_mat <- model.matrix(Class ~ log(funded_amnt) + int_rate, data = lending_club)
  form_mat <- form_mat[1:7, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_mat,
            s = res_form$spec$args$penalty, type = "response")[, 1]
  form_pred <- tibble(bad = 1 - form_pred, good = form_pred)
  expect_equal(form_pred, predict_classprob(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]))

  one_row <- predict_classprob(res_form, lending_club[1, c("funded_amnt", "int_rate")])
  expect_equal(form_pred[1,], one_row)

})

test_that('glmnet probabilities, mulitiple lambda', {

  skip_if_not_installed("glmnet")

  xy_fit <- fit_xy(
    logistic_reg(penalty = c(0.01, 0.1)),
    engine = "glmnet",
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  mult_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            s = xy_fit$spec$args$penalty, type = "response")
  mult_pred <- stack(as.data.frame(mult_pred))
  mult_pred <- tibble(bad = 1 - mult_pred$values, good = mult_pred$values)
  mult_pred$lambda <- rep(xy_fit$spec$args$penalty, each = 7)

  expect_equal(mult_pred, predict_classprob(xy_fit, lending_club[1:7, num_pred]))

  res_form <- fit(
    logistic_reg(penalty = c(0.01, 0.1)),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    engine = "glmnet",
    control = ctrl
  )

  form_mat <- model.matrix(Class ~ log(funded_amnt) + int_rate, data = lending_club)
  form_mat <- form_mat[1:7, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_mat,
            s = res_form$spec$args$penalty, type = "response")
  form_pred <- stack(as.data.frame(form_pred))
  form_pred <- tibble(bad = 1 - form_pred$values, good = form_pred$values)
  form_pred$lambda <- rep(res_form$spec$args$penalty, each = 7)

  expect_equal(form_pred, predict_classprob(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]))

})


test_that('glmnet probabilities, no lambda', {

  skip_if_not_installed("glmnet")

  xy_fit <- fit_xy(
    logistic_reg(),
    engine = "glmnet",
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  mult_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            type = "response")
  mult_pred <- stack(as.data.frame(mult_pred))
  mult_pred <- tibble(bad = 1 - mult_pred$values, good = mult_pred$values)
  mult_pred$lambda <- rep(xy_fit$fit$lambda, each = 7)

  expect_equal(mult_pred, predict_classprob(xy_fit, lending_club[1:7, num_pred]))

  res_form <- fit(
    logistic_reg(),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    engine = "glmnet",
    control = ctrl
  )

  form_mat <- model.matrix(Class ~ log(funded_amnt) + int_rate, data = lending_club)
  form_mat <- form_mat[1:7, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_mat,
            type = "response")
  form_pred <- stack(as.data.frame(form_pred))
  form_pred <- tibble(bad = 1 - form_pred$values, good = form_pred$values)
  form_pred$lambda <- rep(res_form$fit$lambda, each = 7)

  expect_equal(form_pred, predict_classprob(res_form, lending_club[1:7, c("funded_amnt", "int_rate")]))

})


test_that('submodel prediction', {

  skip_if_not_installed("glmnet")

  vars <- c("female", "tenure", "total_charges", "phone_service", "monthly_charges")
  class_fit <-
    logistic_reg() %>%
    fit(churn ~ .,
        data = wa_churn[-(1:4), c("churn", vars)],
        engine = "glmnet")

  pred_glmn <- predict(class_fit$fit, as.matrix(wa_churn[1:4, vars]), s = .1, type = "response")

  mp_res <- multi_predict(class_fit, new_data = wa_churn[1:4, vars], penalty = .1, type = "prob")
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred_No"]], unname(pred_glmn[,1]))
})


test_that('glmnet prediction, one lambda', {

  skip_if_not_installed("glmnet")

  xy_fit <- fit_xy(
    multinom_reg(penalty = 0.1),
    engine = "glmnet",
    control = ctrl,
    x = iris[, 1:4],
    y = iris$Species
  )

  uni_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(iris[rows, 1:4]),
            s = xy_fit$spec$args$penalty, type = "class")
  uni_pred <- factor(uni_pred[,1], levels = levels(iris$Species))
  uni_pred <- unname(uni_pred)

  expect_equal(uni_pred, predict_class(xy_fit, iris[rows, 1:4]))
  expect_equal(uni_pred, predict(xy_fit, iris[rows, 1:4], type = "class")$.pred_class)

  res_form <- fit(
    multinom_reg(penalty = 0.1),
    Species ~ log(Sepal.Width) + Petal.Width,
    data = iris,
    engine = "glmnet",
    control = ctrl
  )

  form_mat <- model.matrix(Species ~ log(Sepal.Width) + Petal.Width, data = iris)
  form_mat <- form_mat[rows, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_mat,
            s = res_form$spec$args$penalty,
            type = "class")
  form_pred <- factor(form_pred[,1], levels = levels(iris$Species))
  expect_equal(form_pred, predict_class(res_form, iris[rows, c("Sepal.Width", "Petal.Width")]))
  expect_equal(form_pred, predict(res_form, iris[rows, c("Sepal.Width", "Petal.Width")], type = "class")$.pred_class)

})


test_that('glmnet probabilities, mulitiple lambda', {

  skip_if_not_installed("glmnet")

  xy_fit <- fit_xy(
    multinom_reg(penalty = c(0.01, 0.1)),
    engine = "glmnet",
    control = ctrl,
    x = iris[, 1:4],
    y = iris$Species
  )

  expect_error(predict(xy_fit, iris[rows, 1:4], type = "class"))
  expect_error(predict(xy_fit, iris[rows, 1:4], type = "prob"))

  mult_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(iris[rows, 1:4]),
            s = xy_fit$spec$args$penalty, type = "response")
  mult_pred <- apply(mult_pred, 3, as_tibble)
  mult_pred <- dplyr:::bind_rows(mult_pred)
  mult_probs <- mult_pred
  names(mult_pred) <- paste0(".pred_", names(mult_pred))
  mult_pred$penalty <- rep(xy_fit$spec$args$penalty, each = 3)
  mult_pred$row <- rep(1:3, 2)
  mult_pred <- mult_pred[order(mult_pred$row, mult_pred$penalty),]
  mult_pred <- split(mult_pred[, -5], mult_pred$row)
  names(mult_pred) <- NULL
  mult_pred <- tibble(.pred = mult_pred)

  expect_equal(
    mult_pred$.pred,
    multi_predict(xy_fit, iris[rows, 1:4], penalty = xy_fit$spec$args$penalty, type = "prob")$.pred
  )

  mult_class <- names(mult_probs)[apply(mult_probs, 1, which.max)]
  mult_class <- tibble(
    .pred = mult_class,
    penalty = rep(xy_fit$spec$args$penalty, each = 3),
    row = rep(1:3, 2)
  )
  mult_class <- mult_class[order(mult_class$row, mult_class$penalty),]
  mult_class <- split(mult_class[, -3], mult_class$row)
  names(mult_class) <- NULL
  mult_class <- tibble(.pred = mult_class)

  expect_equal(
    mult_class$.pred,
    multi_predict(xy_fit, iris[rows, 1:4], penalty = xy_fit$spec$args$penalty)$.pred
  )
})

