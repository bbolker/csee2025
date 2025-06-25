#' expand a binomial data set into a bernoulli data et
#' (scam doesn't handle binomial (N>1) data properly:
#' see scam_binom_test.R)
#' @param dd data frame containing a binomial (N>1 response variable
#' @param response name of response variable
#' @param size (integer) numeric value or (character) name of size column
expand_bern <- function(dd, response = "y1", size = 20) {
    dd[["..size"]] <- if (is.numeric(size)) size else dd[[size]]
    L <- apply(dd, 1,
               function(x) {
                   k <- x[[response]]
                   data.frame(response = rep(c(0,1), times = c(x[["..size"]]-k, k)),
                              rbind(x[!names(x) %in% c(response, "..size")]))
          })
    ret <- do.call(rbind, L)
    names(ret)[names(ret)=="response"] <- response
    return(ret)
}

#' negative log-likelihood function for a univariate spline model, intended for RTMB
#' @details `data` list (in environment) should include (at least?)
#' * `p.ident` positions of parameters to be exponentiated
#' * `X` model matrix
#' * `S` penalty matrix
#' * `size` binomial denominator vector
#' @param data
#' @param parms parameters
#' @param random smooth variable(s)
#' @param silent for MakeADFun
#' @param family GLM family
#' @param ... passed to MakeADFun
mk_mpd_fun <- function(data, parms, random = "b1", silent = TRUE,
                       family = "gaussian", ...) {
    ## can't use %~% format if we want to add a penalty
    f <- function(parms) {
      getAll(data, parms)
      OBS(y)
      b_pos <- b1
      b_pos[p.ident] <- exp(b1[p.ident])

      ## compute penalty first, for simulation purposes
      ## translate from:
      ##   lambda = 1/sigma_sm^2
      ##   MVgauss NLL = (1/2) (n*log(2pi) + log(det(Sigma)) + bT Sigma^{-1} b)
      ##   Sigma = sd^2 S^{-1}
      ##   log(det(Sigma)) = 2*logsd - log(det(S))
      ##   MVNLL = C + logsd + 1/(2*sd^2) (bT S b)
      ##   (ignoring C = constant log(det(S)) + f(pi))
      pen <- (exp(-2*log_smSD) * (t(b_pos) %*% S %*% b_pos) + 2*log_smSD)/2

      eta <- drop(b0 + X %*% b_pos)
      nll <- 0
      for (i in 1:length(y)) {
        if (!is.na(y[i])) {
          nll <- nll  +
            switch(family,
                   gaussian = -1*dnorm(y[i], eta[i], exp(log_rSD), log = TRUE),
                   binomial = -1*dbinom_robust(y[i], logit_p = eta[i],
                                               size = size[i], log = TRUE))
        }
      }
      ## for reporting purposes
      mu <- switch(family,
                   gaussian = eta,
                   binomial = plogis(eta),
                   stop("unimplemented family"))

      REPORT(mu)
      ADREPORT(mu)
      REPORT(eta)
      ADREPORT(eta)
      nll + pen
    }
  ## n.b. neem to name all args so update/MakeADFun will work ...
  ## FIXME: better to use args(MakeADFun) instead?
  ret <- MakeADFun(func = f, parameters = parms, random=random, silent = silent, ...)
  class(ret) <- c("TMB", class(ret))
  return(ret)
}

## commented out, this is too clever: would like to use update(),
##   but too much environment hacking is needed
#' wrapper for RTMB::MakeADFun that includes a call component
#' (to enable use of `update()`)
#' @param ... arguments to pass to MakeADFun
## MakeADFun <- function(...) {
##   cc <- match.call()
##   res <- eval.parent(RTMB::MakeADFun(...))
##   L <- list(...)
##   for (x in names(L)) assign(x, get(x, L), environment(MakeADFun))
##   res$call <- cc
##   res
## }
  
#' @param data data frame including response variable ('y' by default) and predictor/x variable ('x')
#' @param response name of response variable/data column
#' @param xvar name of predictor variable/data column
#' @param form smooth term (not including ~)
#' @param size denominator/number of trials term for binomial models
#' @param parms starting parameter values
#' @param knots knot locations for smooth
#' @param predict (logical) predict new responses?
#' @param family GLM family
#' @param random smooth variable(s)
#' @param silent for `MakeADFun`
#' @param ... passed to `mk_mpd_fun`, thence to `MakeADFun`
fit_mpd_fun <- function(data,
                        response = "y",
                        xvar = "x",
                        form = s(x, bs = "mpd"), 
                        size = numeric(0),
                        parms = NULL,
                        knots = NULL,
                        predict = FALSE,
                        family = "gaussian",
                        random = "b1",
                        silent = TRUE,
                        opt = "nlminb",
                        se.fit = TRUE,
                        nlopt.args = list(),
                        lower = -Inf,
                        ...) {
    form$term <- xvar
    ## if predicting, make sure to pass old knots so basis is constructed properly
    sm1 <- smoothCon(form, data = data, absorb.cons = TRUE, knots = knots)[[1]]
    def_parms <- list(b0 = 0,
                      b1 = rep(0, ncol(sm1$X)),
                      log_smSD = 2)
    if (!is.null(parms)) {
      for (nm in setdiff(names(parms), "log_rSD")) def_parms[[nm]] <- parms[[nm]]
    }
    if (family == "gaussian") {
      def_parms[["log_rSD"]] <- parms[["log_rSD"]] %||% 0
    }
    parms <- def_parms

    data$y <- data[[response]]
    tmbdat <- c(as.list(data),
                list(size = size, family = family),
                list(p.ident = sm1$"p.ident", S = sm1$S[[1]], X = sm1$X))
    obj <- mk_mpd_fun(data = tmbdat, parms = parms,
                      random = random, 
                      silent = silent, ...)
    ## p0 <- parms[names(parms) != "b1"]
    ## optim(par = p0, fn = obj$fn, control = list(maxit = 2000))
    if (predict) {
        ## shouldn't need to map() b since we are using best-fit  if random = NULL ?
        if (!is.null(random)) parms <- parms[setdiff(names(parms), random)]
        obj$fn(unlist(parms))
        if (se.fit) {
            sdr <- sdreport(obj)
            return(with(sdr,
                        data.frame(nm = names(value), value, sd)))
        } else {
          stop("not implemented")
        }
    }
    res <- with(obj,
                switch(opt,
                       nlminb =  try(nlminb(par, fn, gr, lower = lower, control = c(list(eval.max = 1000, iter.max = 1000), nlopt.args))),
                       BFGS = try(optim(par, fn, gr, method = "BFGS", control = list(maxit =1000))),
                       stop("unknown optimizer ", opt))
                )
    ret <- list(fit = res, obj = obj, mu = obj$report()$mu, eta = obj$report()$eta)

    ## TRY HARDER HERE? i.e., map these parameters to a small (i.e. large-magnitude negative) value
    ##  and re-fit?
    zero_pars <- which(tmbdat$p.ident & exp(obj$env$parList()[["b1"]]) < 1e-6)
    
    class(ret) <- c("RTMB", "list")
    return(ret)
}


predict.RTMB <- function(x, type = "link", ...)  {
    switch(type,
           link = x$eta,
           response = x$mu)
}

mk_holling2_rtmb_fun <- function(data) {
    f <- function(parms) {
        getAll(data, parms)
        a <- exp(loga)
        prob <- a/(1+a*exp(logh)*Initial)
        logitprob <- qlogis(prob) ## for CIs on logit scale
        nll <- -1*sum(dbinom(Killed, prob = prob, size = Initial, log = TRUE))
        REPORT(prob)
        ADREPORT(logitprob)
        return(nll)
    }
    return(f)
}

fit_RTMB_holling2 <- function(data, parms = list(loga=log(0.5), logh = log(0.01))) {
    obj <- MakeADFun(mk_holling2_rtmb_fun(data), parameters = parms,
                     silent = TRUE)
    fit <- with(obj, nlminb(par, fn, gr))
    sdr <- sdreport(obj)
    return(list(obj = obj, fit = fit, pred = data.frame(Initial = data$Initial,
                                                        prob = obj$report()$prob,
                                                        logitprob = sdr$value,
                                                        logitprob_sd = sdr$sd)))
}

predict_RTMB_holling2 <- function(data, newdata, parms, response = "Killed",
                               qq = qnorm(0.975)) {
    newdata[[response]] <- NA_integer_
    n_new <- nrow(newdata)
    data <- rbind(newdata, data)
    ## don't think I have to worry about mapping b parameters here ...
    ## no random coef here
    newobj <- MakeADFun(mk_holling2_rtmb_fun(data), parameters = as.list(parms), silent = TRUE)
    newobj$fn(parms)
    sdr <- sdreport(newobj)    
    pframe <- with(sdr,
                   data.frame(Initial = data$Initial,
                              prob = newobj$report()$prob,
                              logitprob = value,
                              logitprob_sd = sd,
                              lwr = plogis(value-qq*sd),
                              upr = plogis(value+qq*sd))
                   )
    pframe <- pframe[seq(n_new), ]
    return(pframe)
}
    

## attempt to get predictions directly from X matrix ...
## newpred <- function(data = data.frame(Initial = 5:100),
##                     olddata = dd,
##                     fit = m_RTMB2_mpd,
##                     xvar = "Initial",
##                     form = s(x, bs = "mpd"), 
##                     size = numeric(0),
##                     parms = NULL,
##                     knots = NULL) {
##     browser()
##     form$term <- xvar
##     k <- data.frame(Initial = smoothCon(s(Initial, bs="mpd"), data = olddata, absorb.cons = TRUE)[[1]]$knots)
##     sm1 <- smoothCon(form, data = data, absorb.cons = TRUE, knots = k)[[1]]
##     X <- cbind(1, sm1$X)
##     p <- fit$obj$env$last.par.best
##     p <- p[grepl("^b", names(p))]
##     pos.pars <- names(p) == "b1"
##     p[pos.pars] <- exp(p[pos.pars])
##     plogis(X %*% p)
##     sdr <- sdreport(fit$obj, getJointPrec = TRUE)
##     V <- vcov(fit)
## }

## newpred()
##
## apropos("smooth.construct")
## ?smooth.construct.miso.smooth.spec
## scam smooth codes:
##  m = monotonic
##  p = p-spline
##  i/d = increasing/decreasing
##  cv = concavity
## te = tensor
## d = double
## de = 'decreasing' (why not md?)

library(scam)
scam_pos <- match("package:scam", search())
aa <- apropos("smooth.construct", where = TRUE)
scam_smooths <- unname(aa[names(aa) == scam_pos]) |>
    gsub(pattern = "[.]?smooth\\.(construct|spec)[.]?", replacement = "")

## tensors only
grep("^te", scam_smooths, value = TRUE)

s_help <- function(s) {
    help(sprintf("smooth.construct.%s.smooth.spec", s))
}

get_info <- function(fit, newdata = dd, init_dens = "N", killed = "killed", predresp = "mu") {
    if (inherits(fit, "scam")) {
        df <- attr(logLik(fit), "df")
        pp <- drop(predict(fit, newdata = newdata, type = "response"))
        nll <- -sum(dbinom(newdata[[killed]], size = newdata[[init_dens]], prob = pp, log = TRUE))
        AIC <- 2*(nll + df)
    } else if (inherits(fit, "RTMB")) {
        pp <- fit[[predresp]]
        nll <- -sum(dbinom(newdata[[killed]], size = newdata[[init_dens]], prob = pp, log = TRUE))
        df <- NA
        AIC <- NA
    } else {
        nll <- -logLik(fit)
        df <- attr(nll, "df")
        nll <- c(nll)
        AIC <- AIC(fit)
    }
    tibble(AIC, nll, df)
}
