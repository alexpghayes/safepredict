library(testthat)

check_predict_basic <- function(
  predictions,
  passed_data) {

  expect_true(tibble::is_tibble(predictions))

  expect_equal(
    nrow(predictions), nrow(passed_data),
    info = "Prediction tibble must have same number of rows as `new_data`."
  )
}

# does not check if column names are the same!
expect_same_content <- function(df_like_1, df_like_2) {
  df1 <- as.data.frame(df_like_1)
  df2 <- as.data.frame(df_like_2)
  colnames(df2) <- colnames(df1)
  expect_equivalent(df1, df2)
}
