#' @section Factors and novel factor levels:
#'
#'  We recommend using the
#'  [recipes][recipes::`recipes-package`] to consistently handle categorical
#'  (factor) predictors. For details see:
#'
#'  ```
#'  vignette("novel-factor-levels", package = "safepredict")
#'  ```
#'
#'  Currently we do not have a robust way to check for novel factor levels
#'  from within `safepredict`. In practice this would require storing
#'  information about predictors in the model object at fitting time,
#'  and `safepredict` is largely at the mercy of package writers in
#'  that regard. Using `recipes` to preprocess your data should take care
#'  of the issue.
#'
