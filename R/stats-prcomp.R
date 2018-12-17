pca <- prcomp(USArrests, scale = TRUE)
class(predict(pca))

as_tibble(scale(USArrests))
as_tibble(pca$rotation %*% diag(pca$sdev^2) %*% t(pca$rotation))
cor(USArrests)

?prcomp

pca$sdev

#' Safe predictions from a prcomp object
#'
#' @param object A `prcomp` object returned from a call to [stats::prcomp()].
#'
#' @param type What kind of predictions to return. There is only one option:
#'   - `"response"` (default): Standard predictions from non-linear regression.
#' @param transformation
#'
#'   - `"rotate_and_scale"` (default)
#'   - `"rotate"`
#'
#' @export
#' @examples
#' @family unsupervised
#'
#' @details
#'
#' @return What columns you get back, and the details of the computation
#'
safe_predict.prcomp <- function(
  object,
  new_data,
  type = "response",
  ...) {

  new_data <- safe_tibble(new_data)
  type <- arg_match(type)

  # sanity check that na.pass actually works here
  pred_mat <- predict(object, new_data, na.action = na.pass)
  as_tibble(pred_mat, row)


  tibble(.pred = )
}

