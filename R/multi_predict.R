
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
        paste0("'", class(), "'", collapse = ", "), call. = FALSE)
