safe_predict.C5.0 <- function(
  object,
  new_data,
  type = c(
    "response",
    "class",
    "prob"
  ),
  ...) {

  new_data <- safe_tibble(new_data)
  type <- match.arg(type)

  if (type == "response")
    predict_c5_response(object, new_data)
  else if (type == "class")
    predict_c5_class(object, new_data)
  else if (type == "prob")
    predict_c5_prob(object, new_data)
  else
    no_method_for_type_error()

  pred
}

predict_c5_response <- function(object, new_data) {
  predict(object, new_data)
}

predict_c5_class <- function(object, new_data) {
  predict(object, new_data)
}

predict_c5_prob <- function(object, new_data) {
  predict(object, new_data, type = "prob")
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

# ------------------------------------------------------------------------------

multi_predict.C5.0 <-
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
