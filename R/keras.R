,
pred = list(
  pre = NULL,
  post = maybe_multivariate,
  func = c(fun = "predict"),
  args =
    list(
      object = quote(object$fit),
      x = quote(as.matrix(new_data))
    )
),
classes = list(
  pre = NULL,
  post = function(x, object) {
    object$lvl[x + 1]
  },
  func = c(fun = "predict_classes"),
  args =
    list(
      object = quote(object$fit),
      x = quote(as.matrix(new_data))
    )
),
prob = list(
  pre = NULL,
  post = function(x, object) {
    x <- as_tibble(x)
    colnames(x) <- object$lvl
    x
  },
  func = c(fun = "predict_proba"),
  args =
    list(
      object = quote(object$fit),
      x = quote(as.matrix(new_data))
    )
),
raw = list(
  pre = NULL,
  func = c(fun = "predict"),
  args =
    list(
      object = quote(object$fit),
      x = quote(as.matrix(new_data))
    )
)
