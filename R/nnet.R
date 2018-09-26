,
pred = list(
  pre = NULL,
  post = maybe_multivariate,
  func = c(fun = "predict"),
  args =
    list(
      object = quote(object$fit),
      newdata = quote(new_data),
      type = "raw"
    )
),
classes = list(
  pre = NULL,
  post = NULL,
  func = c(fun = "predict"),
  args =
    list(
      object = quote(object$fit),
      newdata = quote(new_data),
      type = "class"
    )
),
raw = list(
  pre = NULL,
  func = c(fun = "predict"),
  args =
    list(
      object = quote(object$fit),
      newdata = quote(new_data)
    )
)
