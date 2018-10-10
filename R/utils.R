use_suggested_package <- function(pkg_name) {
  if (!requireNamespace(pkg_name, quietly = TRUE))
    stop(
      "Must install package `", pkg_name, "` in order to use this function",
      call. = FALSE
    )
}

safe_tibble <- function(df) {
  # try to coerce to a tibble
  # give an informative error on failure


  if (!inherits(df, "data.frame"))
    stop("`newdata` argument must be a data frame.")
  df

}

# use this instead of mutate because it puts the column first in the dataframe
add_id_column <- function(data) {
  tibble::add_column(data, .id = 1:nrow(data), .before = TRUE)
}

# MK Use `.id` instead or `.row`


validate_logical <- function(x) {
  arg_name <- as.character(substitute(x))
  if (!is.logical(x) || length(x) != 1)
    stop(
      "Argument `", arg_name, "` must be a logical vector with one element.",
      call. = FALSE
    )
}

validate_probability <- function(x) {
  arg_name <- as.character(substitute(x))
  if (!is.numeric(x) || length(x) != 1 || x < 0 || x > 1)
    stop(
      "Argument `", arg_name, "` must be a vector with one element strictly",
      "between 0 and 1.",
      call. = FALSE
    )
}

validate_positive <- function(x) {
  arg_name <- as.character(substitute(x))
  if (!is.numeric(x) || x <= 0)
    stop(
      "Argument `", arg_name, "` must be a positive vector with one positive",
      "element", call. = FALSE
    )
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

  type <- match.arg(type)
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

# TODO: make this more informative
could_not_dispatch_error <- function()
  stop("There's no method for the given object and type.")

multinomial_helper <- function(
  raw,
  levels,
  type = c("prob", "class")) {

  type <- match.arg(type)
  probs <- as_tibble(raw)
  colnames(probs) <- paste0(".pred_", levels)

  if (type == "prob")
    return(probs)

  # use as.numeric to coerce integer() elements of the resulting list to NA
  argmax_idx <- as.numeric(apply(raw, 1, which.max))
  tibble(.pred_class = as.factor(levels[argmax_idx]))
}


#' Determine the positive class of a logistic regression fit with `glm`
#'
#' @param object TODO
#'
#' @return TODO
#' @export
positive_class <- function(object) {
  stopifnot(inherits(object, "glm"))
  stopifnot(family(object)$family == "binomial")

  # TODO
}

# MK it's always the second level. We might want to tie this in with the
# MK global variable defined by `yardstick` to be consistent?


pred_se_to_confint <- function(pred_se, level, se_fit) {
  crit_val <- qnorm(1 - level / 2)

  pred <- dplyr::mutate(
    pred_se,
    .pred_lower = fit - crit_val * .std_error,
    .pred_upper = fit + crit_val * .std_error
  )

  if (!se_fit)
    pred <- dplyr::select(pred, -.std_error)

  attr(pred, "interval") <- "confidence"
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
    stop(
      "`type` must be one of the following: ",
      paste(allowed_types, collapse = ", "), ". You entered: ", type, ".",
      call. = FALSE
    )
}
