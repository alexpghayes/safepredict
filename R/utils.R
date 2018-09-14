safe_tibble <- function(df) {
  # try to coerce to a tibble
  # give an informative error on failure


  if (!inherits(df, "data.frame"))
    stop("`newdata` argument must be a data frame.")
  df

}

# assumes table_like has a single column
as_pred_tibble <- function(table_like, names = ".pred") {
  purrr::set_names(as_tibble(table_like), names)
}

# use this instead of mutate because it puts the column first in the dataframe
add_id_column <- function(data) {
  tibble::add_column(data, id = 1:nrow(data), .before = TRUE)
}

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

multinomial_helper <- function(
  raw,
  levels,
  type = c("prob", "class")) {

  type <- match.arg(type)
  probs <- as_tibble(raw)
  colnames(probs) <- paste0(".pred_", levels)

  if (type == "prob")
    return(probs)

  argmax_idx <- apply(raw, 1, which.max)
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
