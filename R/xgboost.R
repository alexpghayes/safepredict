safe_predict.xgb.Booster <- function(
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

  ## TODO: dispatch on type
  if (type == "response")
    predict_xgb_response(object, new_data)
  else if (type == "class")
    predict_xgb_class(object, new_data)
  else if (type == "prob")
    predict_xgb_prob(object, new_data)
  else
    no_method_for_type_error()

  pred
}


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

predict_xgb_response <- function(object, new_data, ...) {
  pred <- predict(object, newdata = new_data)
}

predict_xgb_class <- function(object, new_data, ...) {
  pred <- xgb_pred(object, newdata = new_data, type = "response")

  post = function(x, object) {
    if (is.vector(x)) {
      x <- ifelse(x >= 0.5, object$lvl[2], object$lvl[1])
    } else {
      x <- object$lvl[apply(x, 1, which.max)]
    }
    x
  }
}

predict_xgb_prob <- function(object, new_data, ...) {
  pred <- xgb_pred(object, newdata = new_data, type = "response")

  post = function(x, object) {
    if (is.vector(x)) {
      x <- tibble(v1 = 1 - x, v2 = x)
    } else {
      x <- as_tibble(x)
    }
    colnames(x) <- object$lvl
    x
  }
}
