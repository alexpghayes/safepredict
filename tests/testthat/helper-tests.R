library(testthat)

# does not check if column names are the same!
expect_same_content <- function(df_like_1, df_like_2) {
  df1 <- as.data.frame(df_like_1)
  df2 <- as.data.frame(df_like_2)
  colnames(df2) <- colnames(df1)
  expect_equivalent(df1, df2)
}

check_safepredict_signature <- function(object) {

  act <- quasi_label(rlang::enquo(object))

  # TODO: must appear in this order
  allowed_args <- c(
    "object",
    "new_data",
    "type",
    "...",
    "std_error",
    "level",
    "threshold"
  )

  arglist <- as.list(formals(act$val))
  args <- names(arglist)
  not_allowed_args <- setdiff(args, allowed_args)

  expect(
    length(not_allowed_args) == 0,
    glue(
      "Invalid {act$lab} method signature\n",
      "The following arguments are not allowed: {not_allowed_args}"
    ),
  )

  # TODO: probs should always come before class (i.e. probs
  # should be the default). and probs instead of prob. i.e.
  # include the s

  allowed_types <- c(
    "response",
    "class",
    "prob",
    "link",
    "conf_int",
    "pred_int"
  )

  not_allowed_types <- setdiff(eval(arglist$type), allowed_types)

  expect(
    length(not_allowed_types) == 0,
    glue(
      "Invalid {act$lab} method signature\n",
      "The following elements of `type` are not allowed: {not_allowed_types}"
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

  # TODO: if you allow type = prob, you must have a threshold arg

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
#'  variable in `new_data`. This is necessary so we can check that formula
#'  interfaces don't explode when the response column isn't present in the
#'  predictor data frame.
#'
#' @return
#' @export
check_predict <- function(method, object, new_data, outcome, type) {

  expect_error(
    method(object),
    info = "Must specify data via `new_data` argument."
  )

  ## construct a bunch of possible input data edge cases

  n <- NROW(new_data)

  extra_cols <- dplyr::mutate(
    new_data,
    .apples = rnorm(n),
    SnaKeCaseISbaDfoRtHeSOul = rnorm(n)
  )

  single_row <- new_data[1, , drop = FALSE]
  triple_obs <- dplyr::bind_rows(single_row, single_row, single_row)

  na_on_diag <- new_data
  diag(na_on_diag) <- NA
  na_on_diag[n + 1, ] <- NA

  empty_rows <- data.frame()

  ## get predictions for each of these

  extra_cols_preds <- method(object, extra_cols, type)
  single_row_preds <- method(object, single_row, type)
  triple_obs_preds <- method(object, triple_obs, type)
  na_on_diag_preds <- method(object, na_on_diag, type)

  ## sanity check the predictions

  check_predict_output(extra_cols_preds, extra_cols, type)
  check_predict_output(single_row_preds, single_row, type)
  check_predict_output(triple_obs_preds, triple_obs, type)
  check_predict_output(na_on_diag_preds, na_on_diag, type)

  ## condition check: deals with missing outcome

  if (outcome %in% colnames(new_data)) {
    no_outcome <- dplyr::select(new_data, -dplyr::one_of(outcome))
    no_outcome_preds <- method(object, no_outcome, type)
    check_predict_output(no_outcome_preds, no_outcome, type)
  }

  ## TODO: check that se_fit does something
  ## TODO: check commutativity
  ## TODO: check empty inputs

  # try()
  #
  # expect_equal(
  #   empty_rows_preds,
  #   tibble(),
  #   info = "Predictions on empty input should return an empty tibble."
  # )


}

check_predict_output <- function(object, passed_data, type) {

  act <- quasi_label(rlang::enquo(object))

  # perform general checks
  check_predict_output_basic(act$val, passed_data, act$lab)

  # perform type-specific checks
  switch(
    type,
    "response" = check_predict_output_response(object, passed_data),
    "class" = check_predict_output_class(object, passed_data),
    "prob" = check_predict_output_prob(object, passed_data),
    "link" = check_predict_output_link(object, passed_data),
    "conf_int" = check_predict_output_interval(object, passed_data, type),
    "pred_int" = check_predict_output_interval(object, passed_data, type)
  )
}

#' Check the output of predictions of type "response"
#'
#' TODO: add support for std_error
#'
#' @param object
#' @param passed_data
#'
#' @return
#' @export
#'
#' @examples
check_predict_output_response <- function(object, passed_data) {

  act <- quasi_label(rlang::enquo(object))

  # presence of correct columns
  expect(
    ".pred" %in% colnames(act$val),
    glue("The predictions tibble {act$lab} did not have a `.pred` column.")
  )

  # class of the present columns
  expect(
    is.numeric(act$val$.pred),
    glue("The `.pred` column in {act$lab} must be a *numeric* vector.")
  )

  # no additional columns
  expect(
    NCOL(act$val) == 1,
    glue("{act$lab} did not have exactly one column.")
  )
}

check_predict_output_prob <- function(object, passed_data) {
}

check_predict_output_class <- function(object, passed_data) {
}

check_predict_output_interval <- function(object, passed_data) {

  act <- quasi_label(rlang::enquo(object))

  # presence of correct columns
  expect(
    ".pred" %in% colnames(act$val),
    glue("The predictions tibble {act$lab} did not have a `.pred` column.")
  )

  # class of the present columns
  expect(
    is.numeric(act$val$.pred),
    glue("The `.pred` column in {act$lab} must be a *numeric* vector.")
  )

  # no additional columns
  expect(
    NCOL(act$val) == 1,
    glue("{act$lab} did not have exactly one column.")
  )
}

check_predict_output_interval <- function(object, passed_data, type) {

  interval <- if (type == "conf_int") "confidence" else "prediction"

  act <- quasi_label(rlang::enquo(object))

  # presence of correct columns
  expect(
    all(c(".pred", ".pred_lower", ".pred_upper") %in% colnames(act$val)),
    glue("The predictions tibble {act$lab} did not have a `.pred` column.")
  )

  # class of the present columns
  expect(
    all(vapply(act$val, is.numeric, logical(1))),
    glue("The `.pred` column in {act$lab} must be a *numeric* vector.")
  )

  # no additional columns
  expect(
    NCOL(act$val) == 3,
    glue("{act$lab} did not have exactly three columns.")
  )

  # has interval attribute with value "prediction"
  expect(
    attr(act$val, "interval") == interval,
    glue(
    "{act$lab} did not have an `interval` attribute equal to {interval}."
    )
  )

  expect(
    0 < attr(act$val, "level") && attr(act$val, "level") < 1,
    glue("{act$lab} did not have an `level` attribute between 0 and 1.")
  )
}


#' Basic checks on prediction output
#'
#' Doesn't do any `type` specific checking. A building block for the
#' check_predict_output_* family that does include type specific checking
#' as well.
#'
#' @param object Tibble of predictions returned from safe_predict method.
#' @param passed_data The predictors used, as a data-frame like object.
#'
#' @return
#' @export
#'
#' @examples
check_predict_output_basic <- function(object, passed_data, label) {

  act <- quasi_label(rlang::enquo(object), label)

  pred_cols <- colnames(act$val)
  passed_cols <- colnames(passed_data)

  expect(
    tibble::is_tibble(act$val) == TRUE,
    glue(
      "The predictions in {act$lab} were returned as a {class(act$val)[1]} ",
      "but should be returned in a tibble."
    )
  )

  expect(
    any(passed_cols %in% pred_cols) == FALSE,
    glue(
      "Columns from `new_data` should not appear in the prediction tibble ",
      "{act$lab}."
    )
  )

  expect(
    NROW(act$val) == NROW(passed_data),
    glue(
      "The prediction tibble {act$lab} had {NROW(act$val)} rows but must ",
      " have the same number of rows as `new_data` ({NROW(passed_data})."
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


# - level and interval attributes for type = conf_int and type = pred_int

