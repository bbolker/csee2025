library(Matrix)
library(scam)
library(mgcv)
library(MASS)
source("funs.R")

ifun <- function(x, ...) {
  image(Matrix(x), ...)
}


## simplified example from ?scam
set.seed(4)
n <- 200
x1 <- runif(n)*6-3
f1 <- 3*exp(-x1^2) # unconstrained term
y <- f1+rnorm(n)*.5
dat <- data.frame(x1, y)
dat <- dat[order(x1),]
## fit model, get results, and plot...
b <- scam(y~s(x1,bs="cv"),data=dat)

pdat <- data.frame(x1 = seq(-3, 3, by = 0.05))
pp <- predict(b, se.fit=TRUE, newdata = pdat)
qq <- qnorm((1+0.95)/2)
with(pp,
     matplot(pdat$x1,
             cbind(fit, fit-qq*se.fit, fit + qq*se.fit),
             lty = c(1, 2, 2),
             col = 1,
             type = "l"))

##
Sigma <- b$Vp.t
mu <- b$coefficients.t
m <- MASS::mvrnorm(1000, mu, Sigma)

preds <- apply(m, 1, predhack.scam, x = b, pred.args = list(newdata = pdat))
with(pp,
     matplot(pdat$x1,
             cbind(fit, fit-qq*se.fit, fit + qq*se.fit),
             lty = c(1, 2, 2),
             col = 1,
             type = "l",
             ylab = "y", xlab = "x"))
matlines(pdat$x1, preds, lty = 1, type = "l",
         col = adjustcolor("black", alpha.f = 0.01))
## use encapsulated version
bootci <- predCI(b, pdat = pdat)
matlines(pdat$x1, bootci, col = 2)

## predict.scam uses: sqrt(rowSums((X %*% object$Vp.t) * X)) for se.fit

## object$coefficients -- raw (untransformed)
## object$coefficients.t -- transformed
## object$p.ident -- whether to transform or not

maxCI <- predCI(b, PFUN = max, pdat = pdat)
## x clashes with args
## (limited resolution)
maxlocCI <- predCI(b, PFUN = \(y,xx) xx[which.max(y)],
                pdat = pdat, xx = pdat$x1)
