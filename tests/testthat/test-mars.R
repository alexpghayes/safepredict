context("test-mars")


test_that('mars prediction', {

  skip_if_not_installed("earth")
  library(earth)

  uni_mars <- earth(Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length, data = iris)
  uni_pred <- unname(predict(uni_mars, newdata = iris[1:5, ])[,1])
  inl_mars <- earth(Sepal.Length ~ log(Sepal.Width) + Species, data = iris)
  inl_pred <- unname(predict(inl_mars, newdata = iris[1:5, ])[,1])
  mv_mars <- earth(cbind(Sepal.Width, Petal.Width) ~ ., data = iris)
  mv_pred <- as.data.frame(predict(mv_mars, newdata = iris[1:5, ]))

  res_xy <- fit_xy(
    iris_basic,
    x = iris[, num_pred],
    y = iris$Sepal.Length,
    engine = "earth",
    control = ctrl
  )

  expect_equal(uni_pred, predict_num(res_xy, iris[1:5, num_pred]))

  res_form <- fit(
    iris_basic,
    Sepal.Length ~ log(Sepal.Width) + Species,
    data = iris,
    engine = "earth",
    control = ctrl
  )
  expect_equal(inl_pred, predict_num(res_form, iris[1:5, ]))

  res_mv <- fit(
    iris_basic,
    cbind(Sepal.Width, Petal.Width) ~ .,
    data = iris,
    control = ctrl,
    engine = "earth"
  )
  expect_equal(mv_pred, predict_num(res_mv, iris[1:5,]))
})


test_that('submodel prediction', {

  skip_if_not_installed("earth")
  library(earth)

  reg_fit <-
    mars(
      num_terms = 20,
      prune_method = "none",
      mode = "regression",
      others = list(keepxy = TRUE)
    ) %>%
    fit(mpg ~ ., data = mtcars[-(1:4), ], engine = "earth")

  pruned_fit <- update(reg_fit$fit, nprune = 5)
  pruned_pred <- predict(pruned_fit, mtcars[1:4, -1])[,1]

  mp_res <- multi_predict(reg_fit, new_data = mtcars[1:4, -1], num_terms = 5)
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred"]], pruned_pred)

  vars <- c("female", "tenure", "total_charges", "phone_service", "monthly_charges")
  class_fit <-
    mars(mode = "classification", prune_method = "none", others = list(keepxy = TRUE)) %>%
    fit(churn ~ .,
        data = wa_churn[-(1:4), c("churn", vars)],
        engine = "earth")

  pruned_fit <- update(class_fit$fit, nprune = 5)
  pruned_pred <- predict(pruned_fit, wa_churn[1:4, vars], type = "response")[,1]

  mp_res <- multi_predict(class_fit, new_data = wa_churn[1:4, vars], num_terms = 5, type = "prob")
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred_No"]], pruned_pred)
})


###################################################################

data("lending_club")

test_that('classification', {

  skip_if_not_installed("earth")

  expect_error(
    glm_mars <- mars(mode = "classification") %>%
      fit(Class ~ ., data = lending_club[-(1:5),], engine = "earth"),
    regexp = NA
  )
  expect_true(!is.null(glm_mars$fit$glm.list))
  parsnip_pred <- predict_classprob(glm_mars, new_data = lending_club[1:5, -ncol(lending_club)])

  library(earth)
  earth_fit <- earth(Class ~ ., data = lending_club[-(1:5),],
                     glm = list(family = binomial))
  earth_pred <-
    predict(
      earth_fit,
      newdata = lending_club[1:5, -ncol(lending_club)],
      type = "response"
    )

  expect_equal(parsnip_pred[["good"]], earth_pred[,1])
})
