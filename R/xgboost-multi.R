
#' @importFrom purrr map_df
#' @export
multi_predict._xgb.Booster <-
  function(object, new_data, type = NULL, trees = NULL, ...) {
    if (is.null(trees))
      trees <- object$fit$nIter
    trees <- sort(trees)

    if (is.null(type)) {
      if (object$spec$mode == "classification")
        type <- "class"
      else
        type <- "numeric"
    }

    res <-
      map_df(trees, xgb_by_tree, object = object,
             new_data = new_data, type = type, ...)
    res <- arrange(res, .row, trees)
    res <- split(res[, -1], res$.row)
    names(res) <- NULL
    tibble(.pred = res)
  }

xgb_by_tree <- function(tree, object, new_data, type, ...) {
  # pred <- xgb_pred(object$fit, newdata = new_data, ntreelimit = tree)
  #
  # # switch based on prediction type
  # if(object$spec$mode == "regression") {
  #   pred <- tibble(.pred = pred)
  #   nms <- names(pred)
  # } else {
  #   if (type == "class") {
  #     pred <- boost_tree_xgboost_data$classes$post(pred, object)
  #     pred <- tibble(.pred = factor(pred, levels = object$lvl))
  #   } else {
  #     pred <- boost_tree_xgboost_data$prob$post(pred, object)
  #     pred <- as_tibble(pred)
  #     names(pred) <- paste0(".pred_", names(pred))
  #   }
  #   nms <- names(pred)
  # }
  # pred[["trees"]] <- tree
  # pred[[".id"]] <- 1:nrow(new_data)
  # # MK I like `.row` but in other code you use `id`. Is this inconsistent or am
  # # MK I reading it wrong?
  # # AH This is just copy-pasted from parsnip and I haven't touched it yet.
  # # AH Rolling with .id as a default for now, but .row also makes sense.
  # pred[, c(".id", "trees", nms)]
}
