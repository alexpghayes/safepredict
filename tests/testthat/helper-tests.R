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

check_predict <- function(
  predict_method,
  object,
  new_data = NULL,
  outcome = NULL,
  types = NULL) {

  new_data_passed <- !is.null(new_data)

  if (is.null(new_data))
    stop(
      "Must pass data to `check_predict()` via `new_data` argument.",
      call. = FALSE
    )

  n <- nrow(new_data)
  if (n < 5)
    stop("`new_data` must have at least five rows.", call. = FALSE)

  if (is.null(outcome))
    stop("Must specify model outcomes via `outcome` argument.", call. = FALSE)

  expect_error(
    predict_method(object),
    info = "Must specify data via `new_data` argument."
  )

  if (is.null(types)) {
    arglist <- as.list(formals(predict_method))
    types <- eval(arglist$type)
  }

  no_outcome <- select(new_data, -one_of(outcome))
  no_outcome <- select(new_data, -one_of(outcome))

  extra_cols <- mutate(
    new_data,
    .apples = rnorm(n),
    .oranges = rnorm(n),
    .bananas = rnorm(n),
    SnaKeCaseISbaDfoRtHeSOul = rnorm(n)
  )

  one_row <- new_data[1, , drop = FALSE]

  expect_silent({
    ## outcome not required for new_data
    predict_method(object, no_outcome)

    ## but having the outcome and extra columns shouldn't break anything
    predict_method(object, extra_cols)

    ## and things should work with only one row
    predict_method(object, one_row)
  })

  ## same deal but explicitly specify type argument

  for (type in types) {

    expect_silent({
      no_outcome_preds <- predict_method(object, no_outcome, type = type)
    })

    check_predict_output(
      predictions = no_outcome_preds,
      passed_data = no_outcome,
      type = type
    )

    expect_silent({
      extra_cols_preds <- predict_method(object, extra_cols, type = type)
    })

    check_predict_output(
      predictions = extra_cols_preds,
      passed_data = extra_cols,
      type = type
    )

    expect_silent({
      one_row_preds <- predict_method(object, one_row, type = type)
    })

    check_predict_output(
      predictions = one_row_preds,
      passed_data = one_row,
      type = type
    )
  }

  ## TODO: check that se_fit does something
}

check_predict_output <- function(
  predictions,
  passed_data,
  type) {

  ## what should be in the returned prediction tibble

  passed_cols <- colnames(passed_data)
  pred_cols <- colnames(predictions)
  pred_classes <- sapply(predictions, class)

  expect_false(
    any(passed_cols %in% pred_cols),
    info = "Columns from `new_data` must not appear in prediction tibble."
  )

  expect_equal(
    nrow(predictions), nrow(passed_data),
    info = "Prediction tibble must have same number of rows as `new_data`."
  )

  ## column presence

  expect_.pred_column <- c("response", "class", "link", "conf_int", "pred_int")
  expect_.pred_level_columns <- "prob"
  expect_.pred_interval_cols <- c("conf_int", "pred_int")

  if (type %in% expect_.pred_column)
    expect_true(".pred" %in% pred_cols)

  if (type %in% expect_.pred_level_columns)
    expect_true(all(stringr::str_detect(pred_cols, ".pred_")))

  if (type %in% expect_.pred_interval_cols)
    expect_true(all(c(".pred_lower", ".pred_upper") %in% pred_cols))

  ## column type

  expect_numeric <- c("response", "prob", "link", "conf_int", "pred_int")
  expect_factor <- "class"

  if (type %in% expect_numeric)
    expect_true(all(pred_classes == "numeric"))

  if (type %in% expect_factor)
    expect_true(all(pred_classes == "factor"))


  ## check for presence of interval attributes
  if (type == "conf_int")
    expect_equal(attr(predictions, "interval"), "confidence")

  if (type == "pred_int")
    expect_equal(attr(predictions, "interval"), "prediction")

  if (type %in% c("conf_int", "pred_int"))
    expect_false(is.null(attr(predictions, "level")))

  ## class probabilities must add to one
  if (type == "class")
    expect_equivalent(rowSums(predictions), 1)

  ## standard error column should be present

  # if (std_error) {
  #   expect_true(".pred_str_error" %in% pred_cols)
  #   expect_true(all(pred_class == "numeric"))
  # }
}

