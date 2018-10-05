context("test-c5-multi")

#
# test_that('submodel prediction', {
#
#   skip_if_not_installed("C50")
#   library(C50)
#
#   vars <- c("female", "tenure", "total_charges", "phone_service", "monthly_charges")
#   class_fit <-
#     boost_tree(trees = 20, mode = "classification",
#                others = list(control = C5.0Control(earlyStopping = FALSE))) %>%
#     fit(churn ~ .,
#         data = wa_churn[-(1:4), c("churn", vars)],
#         engine = "C5.0")
#
#   pred_class <- predict(class_fit$fit, wa_churn[1:4, vars], trials = 5, type = "prob")
#
#   mp_res <- multi_predict(class_fit, new_data = wa_churn[1:4, vars], trees = 5, type = "prob")
#   mp_res <- do.call("rbind", mp_res$.pred)
#   expect_equal(mp_res[[".pred_No"]], unname(pred_class[, "No"]))
# })
#
