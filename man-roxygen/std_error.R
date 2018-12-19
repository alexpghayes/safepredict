#' @param std_error Logical indicating whether or not calculate standard
#'  errors for the fit at each point. Not available for all models, and can
#'  be computationally expensive to compute. The standard error is *always*
#'  the standard error for the mean, and *never* the standard error for
#'  predictions. Standard errors are returned in a column called `.std_error`.
#'  Defaults to `FALSE`.
