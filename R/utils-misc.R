
`%notin%` <- Negate(`%in%`)

pred_se_to_confint <- function(pred_se, level, se_fit) {
  crit_val <- qnorm(1 - level / 2)

  pred <- dplyr::mutate(
    pred_se,
    .pred_lower = .pred - crit_val * .std_error,
    .pred_upper = .pred + crit_val * .std_error
  )

  if (!se_fit)
    pred <- dplyr::select(pred, -.std_error)

  attr(pred, "interval") <- "confidence"
  attr(pred, "level") <- level

  pred
}
