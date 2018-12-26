#' Safe predictions from a C5 classification tree
#'
#' @param object A `C5.0` object produced by a call to [C50::C5.0()].
#' @param new_data TODO
#' @param type TODO
#' @template unused_dots
#' @template threshold
#'
#' @export
safe_predict.C5.0 <- function(
  object,
  new_data,
  type = c(
    "prob",
    "class"
  ),
  ...,
  threshold = 0.5) {

  type <- arg_match(type)
  validate_probability(threshold)

  if (type == "prob")

  # TODO: use the threshold

  pred <- predict(object, new_data, type = type, na.action = na.pass)
  as_pred_tibble(pred)
}


