#' Safe predictions from a generalized linear model
#'
#' @param object A `glm` object returned from a call to [stats::glm()].
#' @param newdata A dataset as a [data.frame] object. In theory this should
#'   work with any object accepted by the [stats::predict.glm()] `newdata`
#'   argument that can be reasonably coerced into a tibble.
#' @param type What kind of predictions to return. Options are:
#'   - `"response"` (default): Predictions on the response scale.
#'   - `"link"`: Linear predictions before transformation.
#'   - `"conf_int"`: TODO? separate interval argument?
#' @param se_fit Logical indicating whether or not to also calculate standard
#'   errors for the fit at each point. These standard errors do not including
#'   the residual variance. That is, they can be used to calculate confidence
#'   intervals but not prediction intervals.
#' @param level A number between 0 and 1 to use as the confidence level to
#'   use when calculating confidence and prediction intervals.
#' @param ... Unused. TODO: boilerplate for this.
#'
#' @details *What is the difference between confidence intervals and prediction
#'   intervals*? TODO.
#'
#' @return A [tibble::tibble()] with one row for each row of `newdata`.
#'   predictions for observations with missing data will be `NA`. Predictions
#'   are contained in the
#'   boilerplate about safe_predict_guarantees. ATTRIBUTE DETAILS.
#' @export
#'
#' @details Do not use on model objects that only subclass `lm`. Will error.
#'
#' @examples
#'
#' fit <- glm(I(Species == setosa) ~ ., iris, family = binomial)
#'
#' safe_predict(fit, mtcars)
#'
#' mt2 <- mtcars
#' diag(mt2) <- NA  # overly aggressive
#'
#' safe_predict(fit, mt2, se_fit = TRUE)
#' safe_predict(fit, mt2, type = "pred_int", level = 0.9)
#'
safe_predict.glm <- function(
  object,
  newdata,
  type = c(
    "response",
    "class",
    "prob",
    "link",
    "conf_int"
  ),
  se_fit = FALSE,
  level = 0.95,
  ...) {

  ## input validation

  newdata <- safe_tibble(newdata)
  type <- match.arg(type)

  validate_logical(se_fit)
  validate_probability(level)

  if (se_fit && type != link)
    stop("Standard errors cannot be calculated unless `type = link`.")

  fam <- family(object)$family

  TODO <- character(0)
  all <- c("link", "conf_int")

  type_by_family <- list(
    binomial = c("class", "prob"),
    gaussian = "response",
    Gamma= "response",
    inv_gaussian_types = TODO,
    poisson_types = "response",
    quasi = TODO,
    quasibinomial = TODO,
    quasipoisson = TODO
  )

  type_by_family <- purrr::map(type_by_family, ~c(all, .x))
  allowed_types <- type_by_family[[fam]]

  if (type %notin% allowed_types)
    stop(
      paste0("For GLMs with family `", fam, "`, `type` must be one of: "),
      paste(allowed_types, collapse = ", "), ".",
      call. = FALSE
    )

  if (type == "link")
    predict_glm_link(object, newdata, se_fit)
  else if (type == "conf_int")
    predict_glm_confint(object, newdata, level)
  else if (type == "response")
    predict_glm_response(object, newdata)
  else if (type %in% c("class", "prob") && fam == "binomial")
    predict_glm_binomial(object, newdata, type)
  else
    stop("This shouldn't happen.")
}

predict_glm_link <- function(object, newdata, se_fit) {
  if (!se_fit) {
    pred <- tibble(
      .pred = predict(object, newdata, na.action = na.pass, type = "link")
    )
  } else {

    pred_list <- predict(
      object,
      newdata,
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

predict_glm_confint <- function(object, newdata, level) {

  # NOTE: this calculates a confidence interval for the linear predictors,
  # then applies the inverse link to transform this confidence interval to
  # response scale.

  pred_list <- predict(
    object,
    newdata,
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

  pred <- mutate_all(pred, object$family$linkinv)

  attr(pred, "interval") <- "confidence"
  attr(pred, "level") <- level

  pred
}

predict_glm_response <- function(object, newdata) {
  tibble(
    .pred = predict(object, newdata, na.action = na.pass, type = "response")
  )
}

predict_glm_binomial <- function(object, newdata, type) {

  # special case handler for logistic regression:
  #   - class probabilities
  #   - hard predictions

  mf <- model.frame(fit)
  mr <- model.response(mf)

  if (!is.factor(mr))
    stop("safe_predict only works when outcome has been specified as a factor")

  lvl <- levels(mr)  # first element is reference level
  # second element is "positive" level

  raw <- predict(object, newdata, na.action = na.pass, type = "response")

  if (type == "prob") {
    pred <- tibble::tibble(
      !!paste0(".pred_", lvl[1]) := 1 - raw,
      !!paste0(".pred_", lvl[2]) := raw
    )
  } else if (type == "class") {
    pred <- tibble(
      .pred_class = as.factor(if_else(raw > 0.5, lvl[2], lvl[1]))
    )
  }

  pred
}

