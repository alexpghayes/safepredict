,
classes = list(
  pre = NULL,
  post = format_spark_class,
  func = c(pkg = "sparklyr", fun = "ml_predict"),
  args =
    list(
      x = quote(object$fit),
      dataset = quote(new_data)
    )
),
prob = list(
  pre = NULL,
  post = format_spark_probs,
  func = c(pkg = "sparklyr", fun = "ml_predict"),
  args =
    list(
      x = quote(object$fit),
      dataset = quote(new_data)
    )
)
