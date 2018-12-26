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

