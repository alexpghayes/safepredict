,
classes = list(
  pre = check_glmnet_lambda,
  post = organize_multnet_class,
  func = c(fun = "predict"),
  args =
    list(
      object = quote(object$fit),
      newx = quote(as.matrix(new_data)),
      type = "class",
      s = quote(object$spec$args$penalty)
    )
),
prob = list(
  pre = check_glmnet_lambda,
  post = organize_multnet_prob,
  func = c(fun = "predict"),
  args =
    list(
      object = quote(object$fit),
      newx = quote(as.matrix(new_data)),
      type = "response",
      s = quote(object$spec$args$penalty)
    )
),
raw = list(
  pre = NULL,
  func = c(fun = "predict"),
  args =
    list(
      object = quote(object$fit),
      newx = quote(as.matrix(new_data))
    )
)
