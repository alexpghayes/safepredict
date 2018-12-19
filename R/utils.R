

#' Predict at a single observation or return NA
#'
#' A painful and inefficient hack to bring missing value support to the most
#' desperate cases.
#'
#' @param object Object to calculate predictins from.
#' @param obs A single observation to get a prediction at.
#'
#' @return A numeric vector of length one containing a prediction, or `NA` if
#'  prediction is not successful.
#'
#' @examples
#'
#' # doesn't support prediction at missing values
#' fit <- smooth.spline(mtcars$mpg, mtcars$hwy, cv = TRUE)
#'
#' has_missing <- c(30, NA, 40)
#'
#' # this fails
#' \dontrun{
#' predict(fit, has_missing)
#' }
#'
#' # and this is the hacky solution
#' vapply(has_missing, function(x) predict_or_na(fit, x)$y, numeric(1))
#'
#' # this also works on data frames
#' fit2 <- lm(mpg ~ ., mtcars)
#'
#' # add in some missing values
#' mt2 <- mtcars
#' diag(mt2) <- NA
#'
#' apply(mt2, 1, predict_or_na, object = fit2)
#'
#' purrr::map_dbl(mt2, ~predict_or_na(fit2, .x))
#'
predict_or_na <- function(object, obs) {

  # print(obs)
  # print(NROW(obs))
  # print(class(obs))
  # print(dim(obs))
  #
  pred <- tryCatch({
    predict(object, obs)
  }, error = function(cnd) {
    NA
  })

  if (!is.numeric(pred) || !(length(pred) == 1))
    pred <- NA

  pred
}


use_suggested_package <- function(pkg_name) {
  if (!requireNamespace(pkg_name, quietly = TRUE))
    glubort("Must install the {pkg_name} packge in order to use this function.")
}

#' @export
add_id_column <- function(data) {
  tibble::add_column(data, .id = 1:nrow(data), .before = TRUE)
}

`%notin%` <- Negate(`%in%`)

#' TODO
#'
#' @param raw A numeric vector of probabilities.
#' @param levels A two-element character vector of class names. The first
#'   element is the name of the baseline class, and the second element is
#'   the name of the positive event. Probabilities specified in `raw` are
#'   assumed to be for the positive event.
#' @param type TODO
#' @param threshold TODO
#'
#' @importFrom rlang ":="
binomial_helper <- function(
  raw,
  levels,
  type = c("prob", "class"),
  threshold = 0.5) {

  # MK the thresholding part we are going to add to `probably`

  # TODO: this should really just create an appropriate probability
  # matrix and pass the work off to multinomial_helper

  stopifnot(length(levels) == 2)

  type <- arg_match(type)
  validate_probability(threshold)
  raw <- as.vector(raw)

  if (type == "prob") {
    tibble(
      !!paste0(".pred_", levels[1]) := 1 - raw,
      !!paste0(".pred_", levels[2]) := raw
    )
  } else if (type == "class") {
    tibble(
      .pred_class = as.factor(
        dplyr::if_else(raw > threshold, levels[2], levels[1])
      )
    )
  }
}

multinomial_helper <- function(
  raw,
  levels,
  type = c("prob", "class")) {

  type <- arg_match(type)
  probs <- as_tibble(raw)
  colnames(probs) <- paste0(".pred_", levels)

  if (type == "prob")
    return(probs)

  # use as.numeric to coerce integer() elements of the resulting list to NA
  argmax_idx <- as.numeric(apply(raw, 1, which.max))
  tibble(.pred_class = as.factor(levels[argmax_idx]))
}

# MK it's always the second level. We might want to tie this in with the
# MK global variable defined by `yardstick` to be consistent?


# convention: std_error: logical argument, se: numeric vector of standard errors
# adds level attribute but not interval attribute
safe_confint <- function(mean, se, level, df = NULL) {

  # level != alpha, so parentheses here are key!
  #   (1 - 0.95) / 2 = 0.025
  #   1 - 0.95 / 2   = 0.525  -- very wrong and overconfident interval!

  alpha <- (1 - level) / 2
  crit_val <- if (is.null(df)) qnorm(alpha) else qt(alpha, df)

  pred <- tibble(
    .pred = mean,
    .pred_lower = .pred - crit_val * se,
    .pred_upper = .pred + crit_val * se
  )

  attr(pred, "level") <- level
  pred
}


# two columns: param, type
# param needs to be a character vector with a single element
# type is a character vector of all allowed types, many elements okay

check_type_by_param <- function(type_param_table, type, param, all = NULL) {

  allowed_types <- dplyr::filter(type_param_table, param == !!param)
  allowed_types <- dplyr::pull(allowed_types)[[1]]
  allowed_types <- c(allowed_types, all)

  if (type %notin% allowed_types)
    abort(
      "`type` must be one of the following: ",
      paste(allowed_types, collapse = ", "), ". You entered: ", type, "."
    )
}
