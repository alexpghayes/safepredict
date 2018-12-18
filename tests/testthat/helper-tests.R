library(testthat)

# does not check if column names are the same!
expect_same_content <- function(df_like_1, df_like_2) {
  df1 <- as.data.frame(df_like_1)
  df2 <- as.data.frame(df_like_2)
  colnames(df2) <- colnames(df1)
  expect_equivalent(df1, df2)
}

method <- safe_predict.loess

check_safepredict_signature <- function(object) {

  act <- quasi_label(rlang::enquo(object))

  allowed_args <- c(
    "object",
    "new_data",
    "type",
    "...",
    "std_error",
    "level"
  )

  arglist <- as.list(formals(act$val))
  args <- names(arglist)
  not_allowed <- setdiff(args, allowed_args)

  expect(
    length(not_allowed) == 0,
    glue(
      "Invalid {act$lab} method signature\n",
      "The following arguments are not allowed: {not_allowed}"
    ),
  )

  # if you have intervals, you must have a level argument, and it
  # must default to 0.95

  interval <- any(c("conf_int", "pred_int") %in% eval(arglist$type))

  if (interval) {
    expect(
      "level" %in% args,
      glue(
        "Invalid {act$lab} method signature\n",
        "If predictions of type \"conf_int\" or \"pred_int\" are allowed",
        "you must provide a `level` argument to specify the confidence level."
      )
    )

    expect(
      "level" %in% args,
      glue(
        "Invalid {act$lab} method signature\n",
        "The `level` argument must default to 0.95, not {eval(arglist$level)}"
      )
    )
  }

  # if you have a standard error argument, it must default to FALSE

  std_error <- "std_error" %in% args

  if (std_error) {
    expect(
      arglist$std_error == FALSE,
      glue(
        "Invalid {act$lab} method signature\n",
        "The `std_error` argument must default to `FALSE`."
      )
    )
  }

  # QUESTION: should we check the order of the arguments, and that
  # std_error and level always come after the dots?

  # invisibly return value to allow chaining
  invisible(act$val)
}

#' Title
#'
#' @param method A `safe_predict()` method such as `safe_predict.loess`.
#' @param object A model object to use to calculate predictions.
#' @param new_data A data frame or tibble of predictors.
#' @param outcome A character vectoring specifying the column of the response
#'  variable in `new_data`.
#'
#' @return
#' @export
check_predict_response <- function(method, object, new_data, outcome, expected) {

  n <- nrow(new_data)
  if (n < 5)
    stop("`new_data` must have at least five rows.", call. = FALSE)

  expect_error(
    method(object),
    info = "Must specify data via `new_data` argument."
  )

  no_outcome <- select(new_data, -one_of(outcome))

  extra_cols <- mutate(
    new_data,
    .apples = rnorm(n),
    SnaKeCaseISbaDfoRtHeSOul = rnorm(n)
  )

  one_row <- new_data[1, , drop = FALSE]


  # use quasi-quotation for better reporting of errors
  check_predict_output_response <- function(predictions, passed_data) {

    check_predict_output(predictions, passed_data)

    # presence of the correct columns

    # class of the prediction columns
  }

  expect_silent({
    no_outcome_preds <- method(object, no_outcome, type = type)
  })

  check_predict_output(
    predictions = no_outcome_preds,
    passed_data = no_outcome,
    type = type
  )

  expect_silent({
    extra_cols_preds <- method(object, extra_cols, type = type)
  })

  check_predict_output(
    predictions = extra_cols_preds,
    passed_data = extra_cols,
    type = type
  )

  expect_silent({
    one_row_preds <- method(object, one_row, type = type)
  })

  check_predict_output(
    predictions = one_row_preds,
    passed_data = one_row,
    type = type
  )

  ## TODO: check that se_fit does something
}


#' Basic checks on prediction output
#'
#' Doesn't do any `type` specific checking
#'
#' @param object Tibble of predictions returned from safe_predict method.
#' @param passed_data The predictors used, as a data-frame like object.
#'
#' @return
#' @export
#'
#' @examples
check_predict_output <- function(object, passed_data) {

  act <- quasi_label(rlang::enquo(object))

  pred_cols <- colnames(act$val)
  passed_cols <- colnames(passed_data)

  expect(
    tibble::is_tibble(act$val) == TRUE,
    glue(
      "The predictions in {act$lab} were returned as a {class(act$val)[1]}",
      "but should be returned in a tibble."
    )
  )

  expect(
    any(passed_cols %in% pred_cols) == FALSE,
    glue(
      "Columns from `new_data` should not appear in the prediction tibble",
      "{act$lab}."
    )
  )

  expect(
    NROW(predictions) == NROW(passed_data),
    glue(
      "The prediction tibble {act$lab} must have the same number of rows",
      "as `new_data`."
    )
  )

  invisible(act$val)
}



# ## column presence
#
# expect_pred_column <- c("response", "class", "link", "conf_int", "pred_int")
# expect_pred_level_column <- "prob"
# expect_pred_interval_cols <- c("conf_int", "pred_int")
#
# if (type %in% expect_pred_column)
#   expect_true(".pred" %in% pred_cols)
#
# if (type %in% expect_pred_level_column)
#   expect_true(all(stringr::str_detect(pred_cols, ".pred_")))
#
# if (type %in% expect_pred_interval_cols)
#   expect_true(all(c(".pred_lower", ".pred_upper") %in% pred_cols))
#
# ## column type
#
# expect_numeric <- c("response", "prob", "link", "conf_int", "pred_int")
# expect_factor <- "class"
#
# if (type %in% expect_numeric)
#   expect_true(all(pred_classes == "numeric"))
#
# if (type %in% expect_factor)
#   expect_true(all(pred_classes == "factor"))
#
#
# ## check for presence of interval attributes
# if (type == "conf_int")
#   expect_equal(attr(predictions, "interval"), "confidence")
#
# if (type == "pred_int")
#   expect_equal(attr(predictions, "interval"), "prediction")
#
# if (type %in% c("conf_int", "pred_int"))
#   expect_false(is.null(attr(predictions, "level")))
#
# ## class probabilities must add to one
# if (type == "class")
#   expect_equivalent(rowSums(predictions), 1)
#
# ## standard error column should be present
#
# # if (std_error) {
# #   expect_true(".pred_str_error" %in% pred_cols)
# #   expect_true(all(pred_class == "numeric"))
# # }

# TODO: check what happens when type isn't specified

