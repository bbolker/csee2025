library(Matrix)
library(scam)
library(mgcv)
library(MASS)
ifun <- function(x, ...) {
  image(Matrix(x), ...)
}


set.seed(4)
n <- 200
x1 <- runif(n)*6-3
f1 <- 3*exp(-x1^2) # unconstrained term
y <- f1+rnorm(n)*.5
dat <- data.frame(x1=x1,x2=x2,y=y)
## fit model, get results, and plot...
b <- scam(y~s(x2,bs="cv"),data=dat)
summary(b)
plot(b,pages=1,shade=TRUE)

pp <- predict(b, se.fit=TRUE)
with(pp,
     matplot())



## want to use Vp (no Vc available)
## predict.scam uses: sqrt(rowSums((X %*% object$Vp.t) * X)) for se.fit
## pick a scam example ...
## must be transformed variant ... back-transform?
Sigma <- mgcv::vcov.gam(m_scam_tedecv)
ifun(Sigma)
beta <- drop(coef(m_scam_tedecv))


## object$coefficients -- raw (untransformed)
## object$coefficients.t -- transformed
## object$p.ident -- whether to transform or not

## maybe that's all I need? don't need to hack covariance matrix because I'm going to mvrnorm anyway
## replace object$coefficients, object$coefficients.t (how different?)
##
debug(scam:::predict.scam)
predict(m_scam_tedecv)
