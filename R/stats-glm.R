#' Safe predictions from a generalized linear model
#'
#' @param object A `glm` object returned from a call to [stats::glm()].
#' @param threshold A number between `0` and `1` to use as a threshold for
#'   classification. When the class probability for the class corresponding
#'   to a positive event is greater than the threshold, the event will be
#'   classified as positive. Defaults to `0.5`. See [positive_class()] for
#'   assistance determining which class is considered the positive class.
#' @param type What kind of predictions to return. Which predictions are
#'   available depends on the family of `object`.
#'
#'   `"link"` and `"conf_int"` are available for all families. `"link"`
#'   produces numeric predictions on the linear predictor scale. `"conf_int"`
#'   produces numeric predictions on the response scale and corresponding
#'   confidence bounds.
#'
#'   - `"response"` results in a numeric prediction on the response scale
#'     and is available for families:
#'       - `gaussian`
#'       - `Gamma`
#'       - `inverse.gaussian`
#'       - `poisson`
#'       - `quasipoisson`
#'       - `quasi`
#'
#'   - `"class"` results in hard class predictions and is only available for
#'     `binomial` and `quasibinomial` families
#'
#'   - `"prob"` results in class predictions for each class and is only
#'     available for `binomial` and `quasibinomial` families
#'
#'   Default is `"link"`.
#'
#' @template boilerplate
#'
#' @details For GLMs, standard errors can only be calculated when
#'   `type = "link"`.
#'
#' @export
safe_predict.glm <- function(
  object,
  new_data,
  type = c(
    "link",
    "conf_int",
    "response",
    "class",
    "prob"
  ),
  std_error = FALSE,
  level = 0.95,
  threshold = 0.5,
  ...) {

  ## input validation

  type <- arg_match(type)

  validate_logical(std_error)
  validate_probability(level)
  validate_probability(threshold)

  fam <- family(object)$family

  type_by_family <- tibble::tribble(
    ~ param, ~ type,
    "binomial", c("class", "prob"),
    "gaussian", "response",
    "Gamma", "response",
    "inverse.gaussian", "response",
    "poisson", "response",
    "quasi", "response",
    "quasibinomial", c("class", "prob"),
    "quasipoisson", "response"
  )

  check_type_by_param(type_by_family, type, fam, all = c("link", "conf_int"))

  if (std_error && type != "link")
    stop(
      "Standard errors cannot be calculated unless `type = link`.",
      call. = FALSE
    )

  if (type == "link")
    predict_glm_link(object, new_data, std_error)
  else if (type == "conf_int")
    predict_glm_confint(object, new_data, level, std_error)
  else if (type == "response")
    predict_glm_response(object, new_data)
  else if (type %in% c("class", "prob") && fam == "binomial")
    predict_glm_binomial(object, new_data, type, threshold)
  else
    could_not_dispatch_error()
}

predict_glm_link <- function(object, new_data, std_error) {
  if (!std_error) {
    pred <- tibble(
      .pred = predict(object, new_data, na.action = na.pass, type = "link")
    )
  } else {

    pred_list <- predict(
      object,
      new_data,
      se.fit = TRUE,
      na.action = na.pass,
      type = "link"
    )

    pred <- tibble(
      .pred = pred_list$fit,
      .std_error = pred_list$se.fit
    )
  }

  pred
}

predict_glm_confint <- function(object, new_data, level, std_error) {

  # NOTE: this calculates a confidence interval for the linear predictors,
  # then applies the inverse link to transform this confidence interval to
  # response scale.

  # TODO: pass df for better intervals in the low-data case

  pred_link <- predict_glm_link(object, new_data, std_error = TRUE)
  pred <- safe_confint(pred_link$.pred, pred_link$.std_error, level)
  pred <- dplyr::mutate_all(pred, object$family$linkinv)
  pred
}

predict_glm_response <- function(object, new_data) {
  tibble(
    .pred = predict(object, new_data, na.action = na.pass, type = "response")
  )
}

predict_glm_binomial <- function(object, new_data, type, threshold) {

  mf <- model.frame(object)
  mr <- model.response(mf)

  if (!is.factor(mr))
    stop("safe_predict only works when outcome has been specified as a factor")

  # first element is reference level, second element is "positive" level
  lvls <- levels(mr)

  raw <- predict(object, new_data, na.action = na.pass, type = "response")

  # use probably::make_two_class_pred() here
  # and use probably::make_class_pred() in the multinomial case
  binomial_helper(
    raw = raw,
    levels = lvls,
    type = type,
    threshold = threshold
  )
}

