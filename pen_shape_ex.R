## fit monotone increasing curve to non-monotone data

dd <- data.frame(x = 1:26, y = c(1:10, 9:7, 8:20))


## using scam
library(scam)
f1 <- scam(y ~ s(x, bs = "mpi"), data = dd)
plot(y~x, data = dd)
lines(dd$x, predict(f1))

## using new functionality from gam
library(mgcv)
f2 <- scasm(y ~ s(x, bs="sc", xt="m+"), data = dd)
lines(dd$x, predict(f2), col = 2)

## Now try to replicate in RTMB ...
coef(f2)

sm0 <- smoothCon(s(x, bs = "sc"), absorb.cons = TRUE, dd)[[1]]
sm <- smoothCon(s(x, bs = "sc", xt = "m+"), absorb.cons = TRUE, dd)[[1]]
dim(smoothCon(s(x, bs = "sc"), absorb.cons = FALSE, dd)[[1]]$X)
## don't know how lower-rank X with 'absorbed constraints' (sum-to-zero)
## corresponds to required monotonicity constraints; for now, add sum-to-zero
## should (?) be able to  work out the transformation?

sm1 <- smoothCon(s(x, bs = "sc", xt = "m+"), absorb.cons = FALSE, dd)[[1]]

## specifying xt doesn't affect X, S ...
all.equal(sm0$X, sm$X)
all.equal(sm0$S, sm$S)

## waldo::compare(sm0, sm)
## added Ain, bin, bin0
## Ain is the constraint matrix

pospenfun0 <- function(x, rho = 20) { 1/rho * log(1+exp(-rho*x)) }
pospenfun <- function(x, rho = 20) { 1/rho * logspace_add(0, -rho*x) }
curve(pospenfun(x), from = -5, to = 5)
curve(pospenfun(x), from = -5, to = 5, log="y")
curve(pospenfun0(x), add = TRUE, col = 2, lwd = 3)

zpenfun <- function(x, rho = 20) { exp(rho*x) }


library(RTMB)
f <- function(data) {
  function(par) {
    getAll(data, par)
    pen_constr <- sum(pospenfun(Ain %*% beta)) ## is this correct?
    pen_smooth <- exp(logtau)*(beta %*% S %*% beta)
    pen_zero <- zpenfun(C %*% beta)
    ypred <- b0 + X %*% beta
    nll <-  sum(y - ypred)^2
    nll + pen_constr + pen_smooth
  }
}

TMBdat <- c(list(S = sm1$S[[1]]), sm1[c("Ain", "X", "C")], dd)
f0 <- f(TMBdat)

par0 <- list(b0=10, beta = rep(0, ncol(sm1$X)), logtau = 0)
par1 <- list(b0=10, beta = rep(0.1, ncol(sm1$X)), logtau = 1)
f0(par0)
f0(par1)

fwrap <- function(x) {
  f0(relist(x, skeleton = par0))
}
     
opt1 <-optim(par = unlist(par0), fn = fwrap, control = list(maxit = 100000))

lines(dd$x, with(relist(opt1$par, par1), b0 + sm1$X %*% beta))

## NO random effects yet ...
f1 <- MakeADFun(f0, par0)
f1$fn(unlist(par0))
f1$gr(unlist(par0))

opt2 <- optim(par = unlist(par0), fn = f1$fn, control = list(maxit = 100000))
opt3 <- optim(par = unlist(par1), fn = f1$fn, method = "BFGS")  ## beta values are zero?

with(f1, nlminb(start = par, objective = fn, gradient = gr))


