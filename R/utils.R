safe_tibble <- function(df) {
  # try to coerce to a tibble
  # give an informative error on failure


  if (!inherits(df, "data.frame"))
    stop("`newdata` argument must be a data frame.")
  df

}

f <- function(cat) {
  validate_logical(cat)
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

`%notin%` <- Negate(`%in%`)


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
