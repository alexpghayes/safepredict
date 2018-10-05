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

could_not_dispatch_error <- function()
  stop("There's no `safe_predict` method for the given object and type.")

