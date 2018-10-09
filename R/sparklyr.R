#' @export
safe_predict.ml_model <- function(
  object,
  new_data,
  type = c(
    "response",
    "class",
    "prob"
  ),
  ...) {

  # MK um this shouldn't convert a spark object to a tibble, right? 
  new_data <- safe_tibble(new_data)
  type <- match.arg(type)

  # MK This makes me think that you will have to attach this package (and have it
  # MK installed). I don't see anything that checks to see if the package is 
  # MK installed and loads it. 
  pred <- ml_predict(object, dataset = new_data)

  if (type == "response")
    pred <- format_spark_num(pred)
  else if (type == "class")
    pred <- format_spark_class(pred)
  else if (type == "prob")
    pred <- format_spark_prob(pred)
  else
    no_method_for_type_error()

  pred
}

#' @importFrom dplyr starts_with rename rename_at vars funs
format_spark_probs <- function(results, object) {
  results <- dplyr::select(results, starts_with("probability_"))
  results <- dplyr::rename_at(
    results,
    vars(starts_with("probability_")),
    funs(gsub("probability", "pred", .))
  )
  results
}

format_spark_class <- function(results, object) {
  results <- dplyr::select(results, predicted_label)
  results <- dplyr::rename(results, pred_class = predicted_label)
  results
}

format_spark_num <- function(results, object) {
  results <- dplyr::select(results, prediction)
  results <- dplyr::rename(results, pred = prediction)
  results
}
