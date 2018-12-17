#' @keywords internal
#'
#' @importFrom stats family model.frame model.response na.pass predict qnorm
#' @importFrom tibble as_tibble tibble
#' @importFrom rlang arg_match
"_PACKAGE"

#' Safely predict from a model object
#'
#' @param object TODO
#' @param new_data TODO
#' @param type
#' @param opts
#' @param level
#' @param std_error
#' @param ... Unused.
#'
#' @return TODO
#' @export
safe_predict <- function(
  object,
  new_data,
  type = NULL,
  opts = list(),
  level = 0.95,
  std_error = FALSE,
  ...) {

  if (!is.null(type) && type != "raw" && length(opts) > 0)
    warning("`opts` is only used with `type = 'raw'` and was ignored.")

  if (!is.null(type) && type == "raw") {
    call <- quote(predict(object, new_data))
    call <- rlang::call_modify(call, !!!opts)
    return(rlang::eval_tidy(call))
  }

  # TODO: don't pass `opts` to individual methods
  UseMethod("safe_predict")
}

safe_predict.default <- function(object, ...) {
  stop(
    "No safe_predict method has been defined for objects of class",
    class(object)[1], call. = FALSE
  )
}

# MK doesn't `safepredict` need to add all of these packages to suggests? Does
# MK it end up attaching these packages?

# Define a generic to make multiple predictions for the same model object ------

#' Model predictions across many sub-models
#'
#' For some models, predictions can be made on sub-models in the model object.
#' @param object A `model_fit` object.
#' @param ... Optional arguments to pass to `predict.model_fit(type = "raw")`
#'  such as `type`.
#' @return A tibble with the same number of rows as the data being predicted.
#'  Mostly likely, there is a list-column named `.pred` that is a tibble with
#'  multiple rows per sub-model.
#' @keywords internal
#' @export
multi_predict <- function(object, ...)
  UseMethod("multi_predict")

#' @keywords internal
#' @export
#' @rdname multi_predict
multi_predict.default <- function(object, ...)
  stop ("No `multi_predict` method exists for objects with classes ",
        paste0("'", class(object), "'", collapse = ", "), call. = FALSE)

