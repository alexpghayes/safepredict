

# glmnet handles NA okay, thank god

library(glmnet)
# Gaussian
x=matrix(rnorm(100*20),100,20)
y= x[, 1] * 3 + rnorm(100)
fit1=cv.glmnet(x,y)

test1 <- head(x)
test2 <- head(x, 50)
diag(test2) <- NA

grid <- tibble::tibble(
  lambda = 1:10
)

predict_cv_glmnet_response(fit1, test1, "response", "lambda.min")
predict_cv_glmnet_response(fit1, test2, "response", "lambda.min")

predict_cv_glmnet_response(fit1, test2, "response", params = 0.1)

predict_cv_glmnet_response(fit1, test2, "param_pred", params = grid)


#multivariate gaussian
y=matrix(rnorm(100*3),100,3)
fit1m=cv.glmnet(x,y,family="mgaussian")


predict_cv_glmnet_mgaussian(fit1m, test1, "lambda.min")
predict_cv_glmnet_mgaussian(fit1m, test2, "lambda.min")


g2 = as.factor(rep(1:2, each = 50))

x2 = rbind(
  matrix(rnorm(50 * 3), 50, 3),
  matrix(rnorm(50 * 3, 10), 50, 3)
)
fit2=cv.glmnet(x2,g2,family="binomial")

predict_cv_glmnet_binomial(fit2, x2, "class", "lambda.1se", 0.5)
predict_cv_glmnet_binomial(fit2, x2, "prob", "lambda.1se", 0.5)

predict_cv_glmnet_binomial(fit2, x2, "param_pred", grid)


#multinomial
g4 = sample(1:4,100,replace=TRUE)
fit3 = cv.glmnet(x,g4,family="multinomial")
fit3a = cv.glmnet(x,g4,family="multinomial",type.multinomial="grouped")


predict_cv_glmnet_multinomial(fit3, x, "prob", "lambda.1se")
predict_cv_glmnet_multinomial(fit3, x, "class", "lambda.1se")
predict_cv_glmnet_multinomial(fit3a, x, "prob", "lambda.1se")
predict_cv_glmnet_multinomial(fit3a, x, "class", "lambda.1se")


predict_cv_glmnet_multinomial(fit3a, x, "class", 1)

predict_cv_glmnet_multinomial(fit3a, x, "param_pred", grid)



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
system.time(fit1<-cv.glmnet(sx,y))
system.time(fit2n<-cv.glmnet(x,y))

safe_predict(fit1, sx)
safe_predict(fit1, x)
