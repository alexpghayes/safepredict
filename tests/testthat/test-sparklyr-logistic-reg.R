context("test-sparklyr-logistic-reg")


expect_error(
  spark_class_pred <- predict(spark_class_fit, churn_logit_te),
  regexp = NA
)

expect_error(
  spark_class_pred_class <- predict_class(spark_class_fit, churn_logit_te),
  regexp = NA
)

expect_equal(colnames(spark_class_pred), "pred_class")

expect_equal(
  as.data.frame(spark_class_pred)$pred_class,
  as.data.frame(spark_class_pred_class)$pred_class
)

expect_error(
  spark_class_prob <- predict(spark_class_fit, churn_logit_te, type = "prob"),
  regexp = NA
)

expect_error(
  spark_class_prob_classprob <- predict_classprob(spark_class_fit, churn_logit_te),
  regexp = NA
)

expect_equal(colnames(spark_class_prob), c("pred_No", "pred_Yes"))

expect_equivalent(
  as.data.frame(spark_class_prob),
  as.data.frame(spark_class_prob_classprob)
)
