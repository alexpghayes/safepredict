
#' @importFrom stats binomial
xgb_pred <- function(object, newdata, ...) {
  if (!inherits(newdata, "xgb.DMatrix")) {
    newdata <- as.matrix(newdata)
    newdata <- xgboost::xgb.DMatrix(data = newdata, missing = NA)
  }

  res <- predict(object, newdata, ...)

  x = switch(
    object$params$objective,
    "reg:linear" =, "reg:logistic" =, "binary:logistic" = res,
    "binary:logitraw" = stats::binomial()$linkinv(res),
    "multi:softprob" = matrix(res, ncol = object$params$num_class, byrow = TRUE),
    res
  )
  x
}

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
  pred <- xgb_pred(object$fit, newdata = new_data, ntreelimit = tree)

  # switch based on prediction type
  if(object$spec$mode == "regression") {
    pred <- tibble(.pred = pred)
    nms <- names(pred)
  } else {
    if (type == "class") {
      pred <- boost_tree_xgboost_data$classes$post(pred, object)
      pred <- tibble(.pred = factor(pred, levels = object$lvl))
    } else {
      pred <- boost_tree_xgboost_data$prob$post(pred, object)
      pred <- as_tibble(pred)
      names(pred) <- paste0(".pred_", names(pred))
    }
    nms <- names(pred)
  }
  pred[["trees"]] <- tree
  pred[[".row"]] <- 1:nrow(new_data)
  pred[, c(".row", "trees", nms)]
}

,
pred = list(
  pre = NULL,
  post = NULL,
  func = c(fun = "xgb_pred"),
  args =
    list(
      object = quote(object$fit),
      newdata = quote(new_data)
    )
),
classes = list(
  pre = NULL,
  post = function(x, object) {
    if (is.vector(x)) {
      x <- ifelse(x >= 0.5, object$lvl[2], object$lvl[1])
    } else {
      x <- object$lvl[apply(x, 1, which.max)]
    }
    x
  },
  func = c(pkg = NULL, fun = "xgb_pred"),
  args =
    list(
      object = quote(object$fit),
      newdata = quote(new_data)
    )
),
prob = list(
  pre = NULL,
  post = function(x, object) {
    if (is.vector(x)) {
      x <- tibble(v1 = 1 - x, v2 = x)
    } else {
      x <- as_tibble(x)
    }
    colnames(x) <- object$lvl
    x
  },
  func = c(pkg = NULL, fun = "xgb_pred"),
  args =
    list(
      object = quote(object$fit),
      newdata = quote(new_data)
    )
),
raw = list(
  pre = NULL,
  func = c(fun = "xgb_pred"),
  args =
    list(
      object = quote(object$fit),
      newdata = quote(new_data)
    )
)
