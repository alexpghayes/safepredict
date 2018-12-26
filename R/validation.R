glubort <- function(..., .sep = "", .envir = parent.frame()) {
  abort(glue(..., .sep = .sep, .envir = .envir))
}

could_not_dispatch_error <- function()
  abort("There's no method for the given object and type.")

no_method_for_type_error <- function()
  abort("There's no method for the given object and type.")

validate_new_data <- function(new_data) {

  # what to do on zero length data? return a zero length tibble()?

  # must be present
  # try to coerce to a tibble
  # give an informative error on failure

  # TODO: what to do when this isn't present?

  new_data

}

validate_logical <- function(x) {
  arg_name <- as.character(substitute(x))
  if (!is.logical(x) || length(x) != 1)
    glubort("`{arg_name}` must be a logical vector with one element.")
}

validate_probability <- function(x) {
  arg_name <- as.character(substitute(x))
  if (!is.numeric(x) || length(x) != 1 || x <= 0 || x >= 1)
    glubort(
      "`{arg_name}` must be a vector with one element strictly between 0 and 1."
    )
}

validate_positive <- function(x) {
  arg_name <- as.character(substitute(x))
  if (!is.numeric(x) || x <= 0)
    glubort("`{arg_name}` must be a positive vector with one positive element.")
}
