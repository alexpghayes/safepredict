,
pred = list(
  pre = NULL,
  post = NULL,
  func = c(fun = "predict"),
  args =
    list(
      object = quote(object$fit),
      newdata = quote(new_data)
    )
),
classes = list(
  pre = NULL,
  post = NULL,
  func = c(fun = "predict"),
  args =
    list(
      object = quote(object$fit),
      newdata = quote(new_data)
    )
),
prob = list(
  pre = NULL,
  post = function(x, object) {
    as_tibble(as.data.frame(x))
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
  args =
    list(
      object = quote(object$fit),
      newdata = quote(new_data)
    )
)
