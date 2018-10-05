#
# as_pred_tibble <- function(messy_preds, ...)
#   UseMethod("")

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

# assumes table_like has a single column
#' @export
as_pred_tibble <- function(table_like, names = ".pred") {

  if (is.matrix(table_like))
    colnames(table_like) <- names

  if (is.null(names))
    as_tibble(table_like)

  purrr::set_names(as_tibble(table_like), names)
}
