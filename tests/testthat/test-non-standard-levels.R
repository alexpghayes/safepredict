# context("test-non-standard-levels")
#
# class_dat2 <- airquality[complete.cases(airquality),]
# class_dat2$Ozone <- factor(ifelse(class_dat2$Ozone >= 31, "high+values", "2low"))
#
# lr_fit_2 <-
#   logistic_reg() %>%
#   fit(Ozone ~ ., data = class_dat2, engine = "glm")
#
# test_that('non-standard levels', {
#   expect_true(is_tibble(predict(lr_fit, new_data = class_dat[1:5,-1])))
#   expect_true(is.factor(predict_class(lr_fit, new_data = class_dat[1:5,-1])))
#   expect_equal(names(predict(lr_fit, new_data = class_dat[1:5,-1])), ".pred_class")
#
#   expect_true(is_tibble(predict(lr_fit_2, new_data = class_dat2[1:5,-1], type = "prob")))
#   expect_true(is_tibble(predict_classprob(lr_fit_2, new_data = class_dat2[1:5,-1])))
#   expect_equal(names(predict(lr_fit_2, new_data = class_dat2[1:5,-1], type = "prob")),
#                c(".pred_2low", ".pred_high+values"))
#   expect_equal(names(predict_classprob(lr_fit_2, new_data = class_dat2[1:5,-1])),
#                c("2low", "high+values"))
# })
