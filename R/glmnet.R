#' Safe predictions from a linear model
#'
#' @param object An `lm` object returned from a call to [stats::lm()].
#'
#' @param type What kind of predictions to return. Options are:
#'   - `"response"` (default): Standard predictions from linear regression.
#'   - `"conf_int"`: Fitted values plus a confidence interval for the fit.
#'   - `"pred_int"`: Predictions with accompanying prediction interval.
#' @template boilerplate
#'
#' @details Do not use on model objects that only subclass `lm`. This will result
#'   in an error.
#'
#' @section Confidence intervals versus predictions intervals:
#'
#' TODO
#'
#' @export
#' @examples
#'
#'
type = c("response", "class", "prob", "param_pred")

safe_predict.cv.glmnet <- function(object, new_data, type)
object <- fit

family <- object$call$family
family <- if (is.null(family)) "gaussian" else family

# Gaussian
x=matrix(rnorm(100*20),100,20)
y=rnorm(100)
fit1=cv.glmnet(x,y)

predict_glmnet_gaussian <- function(object, new_data) {

}

predict_glmnet_mgaussian <- function(object, new_data) {

}

predict_glmnet_binomial <- function(object, new_data) {

}

predict_glmnet_multinomial <- function(object, new_data) {

}

predict_glmnet_poisson <- function(object, new_data) {

}

predict_glmnet_cox <- function(object, new_data) {
  .NotYetImplemented()
}

test <- x[1:10, ]

predict(fit1,newx=x[1:10,],s=c(0.01,0.005)) # make predictions

#multivariate gaussian
y=matrix(rnorm(100*3),100,3)
fit1m=cv.glmnet(x,y,family="mgaussian")

pred <- predict(fit1m, newx=x[1:10,])  # all the predictions are the same?

class(pred)  # 3d array

fit1m$call$family

#binomial
g2 = as.factor(sample(1:2,100,replace=TRUE))
fit2=cv.glmnet(x,g2,family="binomial")

# one column matrix
predict(fit2, test)

# fit2$glmnet.fit$classnames

#multinomial
g4=sample(1:4,100,replace=TRUE)
fit3=glmnet(x,g4,family="multinomial")
fit3a=glmnet(x,g4,family="multinomial",type.multinomial="grouped")
#poisson
N=500; p=20
nzc=5
x=matrix(rnorm(N*p),N,p)
beta=rnorm(nzc)
f = x[,seq(nzc)]%*%beta
mu=exp(f)
y=rpois(N,mu)
fit=glmnet(x,y,family="poisson")
plot(fit)
pfit = predict(fit,x,s=0.001,type="response")
plot(pfit,y)



#Cox
set.seed(10101)
N=1000;p=30
nzc=p/3
x=matrix(rnorm(N*p),N,p)
beta=rnorm(nzc)
fx=x[,seq(nzc)]%*%beta/3
hx=exp(fx)
ty=rexp(N,hx)
tcens=rbinom(n=N,prob=.3,size=1)# censoring indicator
y=cbind(time=ty,status=1-tcens) # y=Surv(ty,1-tcens) with library(survival)
fit=glmnet(x,y,family="cox")
plot(fit)

# Sparse
n=10000;p=200
nzc=trunc(p/10)
x=matrix(rnorm(n*p),n,p)
iz=sample(1:(n*p),size=n*p*.85,replace=FALSE)
x[iz]=0
sx=Matrix(x,sparse=TRUE)
inherits(sx,"sparseMatrix")#confirm that it is sparse
beta=rnorm(nzc)
fx=x[,seq(nzc)]%*%beta
eps=rnorm(n)
y=fx+eps
px=exp(fx)
px=px/(1+px)
ly=rbinom(n=length(px),prob=px,size=1)
system.time(fit1<-glmnet(sx,y))
system.time(fit2n<-glmnet(x,y))


binomial_df <- tibble(
  y = as.factor(c(rep("A", 50), rep("B", 50))),
  x = c(rnorm(50, 1), rnorm(50, 3))
)

X <- model.matrix()

fit2 <- cv.glmnet( family = "binomial")

library(glmnet)

fit <- cv.glmnet(X, y)

fit$glmnet.fit$call

safe_predict.cv.glmnet <- function(
  object,
  new_data,
  type = c(
    "response",
    "conf_int",
    "pred_int"
  ),
  se_fit = FALSE,
  level = 0.95,
  ...) {

  ## input validation

  # if (length(class(object)) > 1)
  #   stop(
  #     paste0(
  #       "The `safe_predict` lm method is not intended to be used with objects",
  #       "that subclass lm.",
  #       call. = FALSE
  #     )
  #   )

  new_data <- safe_tibble(new_data)
  type <- match.arg(type)

  validate_logical(se_fit)
  validate_probability(level)

  ## dispatch on type

  # NOTE: predict.lm deals with missing data nicely.
  # in other circumstances, passing na.action = na.pass may be necessary
  # or you may need to set rownames on a data.frame, see which rownames
  # are retained in the predictions and join on these rownames

  if (type == "response")
    pred <- predict_lm_response(object, new_data, se_fit)
  else
    pred <- predict_lm_interval(object, new_data, type, level)

  pred
}

predict_lm_response <- function(object, new_data, se_fit) {
  if (!se_fit) {
    pred <- tibble(.pred = predict(object, new_data, na.action = na.pass))
  } else{
    pred_list <- predict(object, new_data, se.fit = TRUE, na.action = na.pass)
    pred <- tibble(.pred = pred_list$fit, .pred_std_error = pred_list$se.fit)
  }

  pred
}

predict_lm_interval <- function(object, new_data, type, level) {

  interval <- if (type == "conf_int") "confidence" else "prediction"

  pred_mat <- predict(
    object,
    new_data,
    interval = interval,
    level = level,
    na.action = na.pass
  )

  pred <- dplyr::rename(
    as_tibble(pred_mat),
    ".pred" = "fit",
    ".pred_lower" = "lwr",
    ".pred_upper" = "upr"
  )

  attr(pred, "interval") <- interval
  attr(pred, "level") <- level

  pred
}
