context("test-xgboost-multi")


test_that('submodel prediction', {

  skip_if_not_installed("xgboost")
  library(xgboost)

  reg_fit <-
    boost_tree(
      trees = 20,
      mode = "regression"
    ) %>%
    fit(mpg ~ ., data = mtcars[-(1:4), ], engine = "xgboost")

  x <-  xgboost::xgb.DMatrix(as.matrix(mtcars[1:4, -1]))

  pruned_pred <- predict(reg_fit$fit, x, ntreelimit = 5)

  mp_res <- multi_predict(reg_fit, new_data = mtcars[1:4, -1], trees = 5)
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred"]], pruned_pred)


  vars <- c("female", "tenure", "total_charges", "phone_service", "monthly_charges")
  class_fit <-
    boost_tree(trees = 20, mode = "classification") %>%
    fit(churn ~ .,
        data = wa_churn[-(1:4), c("churn", vars)],
        engine = "xgboost")

  x <-  xgboost::xgb.DMatrix(as.matrix(wa_churn[1:4, vars]))

  pred_class <- predict(class_fit$fit, x, ntreelimit = 5)

  mp_res <- multi_predict(class_fit, new_data = wa_churn[1:4, vars], trees = 5, type = "prob")
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred_No"]], pred_class)
})
