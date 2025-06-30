library(Matrix)
ifun <- function(x, ...) {
  image(Matrix(x), ...)
}

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
