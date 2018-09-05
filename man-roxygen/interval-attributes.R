# cases


#' @return A [tibble::tibble()] with one row for each row of `newdata`.
#'   Predictions for observations with missing data will be `NA`.
#'   Specific cases:

- For univariate, numeric point estimates, the column should be named `.pred`. For multivariate numeric predictions (excluding probabilities), the columns should be named `.pred_{outcome name}`.

- Class predictions should be factors with the same levels as the original outcome and named `.pred_class`.

- For class probability predictions, the columns should be named the same as the factor levels, e.g., `.pred_{level}`.

- If interval estimates are produced (e.g. prediction/confidence/credible), the column names should be `.pred_lower` and `.pred_upper`. If a standard error is produced, it should be named `.std_error`.

- For predictions that are not simple scalars, such as distributions or non-rectangular structures, the `.pred` column should be a list-column [{note}](#list-cols)
#'   are contained in the
#'   boilerplate about safe_predict_guarantees. ATTRIBUTE DETAILS. When `type` is such that you get an interval
