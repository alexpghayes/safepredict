## cases:
#  - vector output:
#    - factor / class predictions
#    - numeric / regression outcome
#    - numeric / probability for positive class in binary problem
#  - matrix output:
#    - no column names
#    - column names
#    - multiple responses (class probabilities, multiple numeric outcomes, etc)
#  - array output
#    - standards unclear if any
#  - data frame output
#    - probably similar to matrix output

# assumes x is a thing with column names
add_pred_prefix <- function(x) {
  colnames(x) <- paste0(".pred_", colnames(x))
  x
}

as_pred_tibble <- function(x, ...)
  UseMethod("as_pred_tibble")

# this is in the case where the matrix has more than one column
# assumes colnames are correct, or that names is specified if this is not
# the case
#
# if there's a single column dispatch to the single vector situation
multi_column_table_like <- function(x, names) {

  if (NCOL(x) == 1)
    as_pred_tibble(as.vector(x))

  if (!is.null(names))
    colnames(x) <- names

  x <- add_pred_prefix(x)
  as_tibble(x)
}

as_pred_tibble.matrix <- function(x, names = NULL, ...)
  multi_column_table_like(x, names)

as_pred_tibble.data.frame <- function(x, names = NULL, ...)
  multi_column_table_like(x, names)

as_pred_tibble.tbl_df <- function(x, names = NULL, ...)
  multi_column_table_like(x, names)

## vector return situation

as_pred_tibble.numeric <- function(x, ...)
  tibble(.pred = x)

as_pred_tibble.factor <- function(x, ...)
  tibble(.pred_class = x)

as_pred_tibble.character <- function(x, ...)
  as_pred_tibble(as.factor(x))

as_pred_tibble.logical <- function(x, ...)
  as_pred_tibble(as.factor(x))


## vector return: binomial class probability

class_prob_helper <- function(
  raw,
  levels,
  type = c("prob", "class"),
  threshold = 0.5) {

  if (is.vector(raw) || (ncol(raw) == 1))
    binomial_helper(as.vector(raw), levels, type, threshold)
  else
    multinomial_helper(raw, levels, type)
}

#' TODO: this might not have the right factor levels? or multinomial might not
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

  # TODO: this should really just create an appropriate probability
  # matrix and pass the work off to multinomial_helper

  stopifnot(length(levels) == 2)
  stopifnot(is.factor(levels) || is.character(levels) || is.logical(levels))

  type <- match.arg(type)
  validate_probability(threshold)
  raw <- as.vector(raw)

  if (type == "prob") {
    tibble(
      !!paste0(".pred_", levels[1]) := 1 - raw,
      !!paste0(".pred_", levels[2]) := raw
    )
  } else if (type == "class") {
    class_vec <- dplyr::if_else(raw > threshold, levels[2], levels[1])
    as_pred_tibble(class_vec)
  }
}

multinomial_helper <- function(
  raw,
  levels,
  type = c("prob", "class")) {

  stopifnot(is.factor(levels) || is.character(levels))

  type <- match.arg(type)
  probs <- as_pred_tibble(raw)

  if (type == "prob")
    return(probs)

  # use as.numeric to coerce integer() elements of the resulting list to NA
  argmax_idx <- as.numeric(apply(raw, 1, which.max))
  as_pred_tibble(levels[argmax_idx])
}


