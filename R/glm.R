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
  se_fit = FALSE,
  level = 0.95,
  threshold = 0.5,
  ...) {

  ## input validation

  new_data <- safe_tibble(new_data)
  type <- match.arg(type)

  validate_logical(se_fit)
  validate_probability(level)
  validate_probability(threshold)

  if (se_fit && type != "link")
    stop(
      "Standard errors cannot be calculated unless `type = link`.",
      call. = FALSE
    )

  fam <- family(object)$family
  all <- c("link", "conf_int")

  type_by_family <- list(
    binomial = c("class", "prob"),
    gaussian = "response",
    Gamma = "response",
    inv_gaussian = "response",
    poisson = "response",
    quasi = "response",
    quasibinomial = c("class", "prob"),
    quasipoisson = "response"
  )

  type_by_family <- purrr::map(type_by_family, ~c(all, .x))
  allowed_types <- type_by_family[[fam]]

  if (type %notin% allowed_types)
    stop(
      paste0("For GLMs with family `", fam, "`, `type` must be one of: "),
      paste(allowed_types, collapse = ", "), ". You entered: ", type, ".",
      call. = FALSE
    )

  if (type == "link")
    predict_glm_link(object, new_data, se_fit)
  else if (type == "conf_int")
    predict_glm_confint(object, new_data, level)
  else if (type == "response")
    predict_glm_response(object, new_data)
  else if (type %in% c("class", "prob") && fam == "binomial")
    predict_glm_binomial(object, new_data, type, threshold)
  else
    stop("This shouldn't happen.")
}

predict_glm_link <- function(object, new_data, se_fit) {
  if (!se_fit) {
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
      .pred_std_error = pred_list$se.fit
    )
  }

  pred
}

predict_glm_confint <- function(object, new_data, level) {

  # NOTE: this calculates a confidence interval for the linear predictors,
  # then applies the inverse link to transform this confidence interval to
  # response scale.

  pred_list <- predict(
    object,
    new_data,
    type = "link",
    se.fit = TRUE,
    na.action = na.pass
  )

  crit_val <- qnorm(1 - level / 2)

  pred <- with(
    pred_list,
    tibble(
      .pred = fit,
      .pred_lower = fit - crit_val * se.fit,
      .pred_upper = fit + crit_val * se.fit
    )
  )

  pred <- dplyr::mutate_all(pred, object$family$linkinv)

  attr(pred, "interval") <- "confidence"
  attr(pred, "level") <- level

  pred
}

predict_glm_response <- function(object, new_data) {
  tibble(
    .pred = predict(object, new_data, na.action = na.pass, type = "response")
  )
}

#' @importFrom rlang ":="
predict_glm_binomial <- function(object, new_data, type, threshold) {

  # special case handler for logistic regression:
  #   - class probabilities
  #   - hard predictions

  mf <- model.frame(object)
  mr <- model.response(mf)

  if (!is.factor(mr))
    stop("safe_predict only works when outcome has been specified as a factor")

  lvl <- levels(mr)  # first element is reference level
  # second element is "positive" level

  raw <- predict(object, new_data, na.action = na.pass, type = "response")

  if (type == "prob") {
    pred <- tibble(
      !!paste0(".pred_", lvl[1]) := 1 - raw,
      !!paste0(".pred_", lvl[2]) := raw
    )
  } else if (type == "class") {
    pred <- tibble(
      .pred_class = as.factor(dplyr::if_else(raw > threshold, lvl[2], lvl[1]))
    )
  }

  pred
}

