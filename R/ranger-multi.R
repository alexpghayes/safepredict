
## IMPORTANT: think about making cross-validation easy:
##   - `param` grid should always behave the same way
##   - `param` grid takes columns of hyperparameters
##   - if a value **does not effect** the estimation process, it doesn't go
##     in `param`. since num_trees affects esatimation process, it can go
##     in params.
##   - param can either be a tibble, in which case we go row by row
##     or it can be a list, in which we take the cartesian product

# num_trees: predictions **by tree**
# params$num_trees: aggregated predictions on n trees using various values of n

multi_predict.ranger  <- function(
  object,
  new_data,
  params = NULL,
  num_trees = NULL,
  verbose = FALSE,
  seed = sample.int(10^5, 1),
  ...) {

  mode <- object$treetype

  if (!is.null(params) && !is.null(num_trees))
    stop("You must specify only one of `params` and `num_trees`")



  # check for mode/type compatibility

  # warn when trying to get quantile and probability predictions
  # from objects fit with the wrong stuff

  # TODO: make verbose and seed actually work

  if (mode == "Probability estimation")
    multi_predict_ranger_prob(object, new_data, type)
  else if (mode %in% c("Classification", "Regression"))
    multi_predict_ranger_helper(object, new_data)
  else
    stop("No predict method for ranger tree with treetype:", mode)
}

multi_predict_ranger_helper <- function(
  object,
  new_data,
  params) {

  # this parameter name should agree with the acceptable parameter names
  # as outlined in the model implementation document
  # params$num_tree

  pred_obj <- predict(object, new_data, type = "response", num.trees = num_trees)
  pred_mat <- pred_obj$predictions
  pred <- as_pred_tibble(pred_mat, 1:pred_obj$num.trees)

  # TODO: is tree going to be character here? should be numeric
  gather(pred, tree, .pred)
}

multi_predict_ranger_prob <- function(
  object,
  new_data,
  params) {

  pred_obj <- predict(object, new_data, type = "response", predict.all = TRUE)
  pred_array <- pred_obj$predictions

  pred_list <- apply(pred_array, 3, predict_ranger_prob)
  pred_list <- do.call(bind_rows, pred_list, .id = ".tree")

  pred <- as_pred_tibble(pred_mat, 1:pred_obj$num.trees)
  # TODO: is tree going to be character here? should be numeric
  gather(pred, tree, .pred)
}
