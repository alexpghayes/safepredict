

#' @importFrom purrr map_dfr
earth_submodel_pred <- function(object, new_data, terms = 2:3, ...) {
  map_dfr(terms, earth_reg_updater, object = object, newdata = new_data, ...)
}

#' @importFrom tibble as_tibble tibble
#' @importFrom stats update
earth_reg_updater <- function(num, object, new_data, ...) {
  object <- update(object, nprune = num)
  pred <- predict(object, new_data, ...)
  if (ncol(pred) == 1) {
    res <- tibble::tibble(.pred = pred[, 1], nprune = num)
  } else {
    res <- tibble::as_tibble(res)
    names(res) <- paste0(".pred_", names(res))
    res$nprune <- num
  }
  res
}


# earth helpers ----------------------------------------------------------------

#' @importFrom purrr map_df
#' @importFrom dplyr arrange
#' @export
multi_predict._earth <-
  function(object, new_data, type = NULL, num_terms = NULL, ...) {
    if (is.null(num_terms))
      num_terms <- object$fit$selected.terms[-1]

    num_terms <- sort(num_terms)

    msg <-
      paste("Please use `keepxy = TRUE` as an option to enable submodel",
            "predictions with `earth`.")
    if (any(names(object$spec$others) == "keepxy")) {
      if(!object$spec$others$keepxy)
        stop (msg, call. = FALSE)
    } else
      stop (msg, call. = FALSE)

    if (!exists("earth"))
      suppressPackageStartupMessages(attachNamespace("earth"))

    if (is.null(type)) {
      if (object$spec$mode == "classification")
        type <- "class"
      else
        type <- "numeric"
    }

    res <-
      map_df(num_terms, earth_by_terms, object = object,
             new_data = new_data, type = type, ...)
    res <- arrange(res, .row, num_terms)
    res <- split(res[, -1], res$.row)
    names(res) <- NULL
    tibble(.pred = res)
  }

earth_by_terms <- function(num_terms, object, new_data, type, ...) {
  object$fit <- update(object$fit, nprune = num_terms)
  pred <- predict(object, new_data = new_data, type = type)
  nms <- names(pred)
  pred[["num_terms"]] <- num_terms
  pred[[".row"]] <- 1:nrow(new_data)
  pred[, c(".row", "num_terms", nms)]
}
