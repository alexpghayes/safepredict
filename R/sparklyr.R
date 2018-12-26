#'
#' # NOTE: input and output should both be Spark DFs, not R DFs. document!
#' #' @export
#' safe_predict.ml_model <- function(
#'   object,
#'   new_data,
#'   type = c(
#'     "response",
#'     "class",
#'     "prob"
#'   ),
#'   ...) {
#'
#'   type <- arg_match(type)
#'
#'   use_suggested_package("sparklyr")
#'   pred <- sparklyr::ml_predict(object, dataset = new_data)
#'
#'   switch(type,
#'     "response" = format_spark_num(pred),
#'     "class" = format_spark_class(pred),
#'     "prob" = format_spark_prob(pred),
#'     no_method_for_type_error()
#'   )
#'
#'   pred
#' }
#'
#' #' @importFrom dplyr starts_with rename rename_at vars funs
#' format_spark_probs <- function(results, object) {
#'   results <- dplyr::select(results, starts_with("probability_"))
#'   results <- dplyr::rename_at(
#'     results,
#'     vars(starts_with("probability_")),
#'     funs(gsub("probability", "pred", .))
#'   )
#'   results
#' }
#'
#' format_spark_class <- function(results, object) {
#'   results <- dplyr::select(results, predicted_label)
#'   results <- dplyr::rename(results, pred_class = predicted_label)
#'   results
#' }
#'
#' format_spark_num <- function(results, object) {
#'   results <- dplyr::select(results, prediction)
#'   results <- dplyr::rename(results, pred = prediction)
#'   results
#' }
