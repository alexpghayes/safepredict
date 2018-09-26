
#' @export
multi_predict._C5.0 <-
  function(object, new_data, type = NULL, trees = NULL, ...) {
    if (is.null(trees))
      trees <- min(object$fit$trials)
    trees <- sort(trees)

    if (is.null(type))
      type <- "class"

    res <-
      map_df(trees, C50_by_tree, object = object,
             new_data = new_data, type = type, ...)
    res <- arrange(res, .row, trees)
    res <- split(res[, -1], res$.row)
    names(res) <- NULL
    tibble(.pred = res)
  }

C50_by_tree <- function(tree, object, new_data, type, ...) {
  pred <- predict(object$fit, newdata = new_data, trials = tree, type = type)

  # switch based on prediction type
  if (type == "class") {
    pred <- tibble(.pred = factor(pred, levels = object$lvl))
  } else {
    pred <- as_tibble(pred)
    names(pred) <- paste0(".pred_", names(pred))
  }
  nms <- names(pred)
  pred[["trees"]] <- tree
  pred[[".row"]] <- 1:nrow(new_data)
  pred[, c(".row", "trees", nms)]
}

,
classes = list(
  pre = NULL,
  post = NULL,
  func = c(fun = "predict"),
  args = list(
    object = quote(object$fit),
    newdata = quote(new_data)
  )
),
prob = list(
  pre = NULL,
  post = function(x, object) {
    as_tibble(x)
  },
  func = c(fun = "predict"),
  args =
    list(
      object = quote(object$fit),
      newdata = quote(new_data),
      type = "prob"
    )
),
raw = list(
  pre = NULL,
  func = c(fun = "predict"),
  args = list(
    object = quote(object$fit),
    newdata = quote(new_data)
  )
)
