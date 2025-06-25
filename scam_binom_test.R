## NOTES:
## headed down a bit of a rabbit hole right now.
## Fairly straightforward to show that scam with bs = "mpd" and
##  binomial (N>1) data doesn't actually respect the constraints,
##  but RTMB with "tp" basis (which should be OK) is not behaving at the moment ...

#' Nothing obviously relevant in  https://cran.r-project.org/web/packages/scam/ChangeLog

## everything below is also slightly in disarray as I try to modularize/DRY everything

## check glmmTMB vs RTMB
##  glmmTMB converts smooth2random (maybe not possible for scam bases?)
##  

library(RTMB)
library(glmmTMB)
library(scam)
library(mgcv)
source("funs.R")

if (!interactive()) pdf("scam_binom_test.pdf")

set.seed(101)
dd <- data.frame(x=seq(-5, 5, length = 101))
dd <- within(dd, {
  p <- plogis(1 + x - x^3/4)
  ## Bernoulli outcomes
  y0 <- rbinom(nrow(dd), size = 1, prob = p)
  ## binomial (N==20) outcomes
  y1 <- rbinom(nrow(dd), size = 20, prob = p)
})

par(las = 1, bty = "l") ## cosmetic
colvec <- c(2,4:6, 8:9)
ltyvec <- 1:5

#' @param response response (y0 = Bernoulli, y1 = binomial)
fit_all <- function(response = c("bernoulli", "binomial"), basis = "tp", data = dd, v = NULL,
                    log_smSD = 1, skip_RTMB = FALSE) {
  response <- match.arg(response)
  if (response == "bernoulli") {
    size <- rep(1, nrow(dd))
    v <- v %||% "y0"
    RTMBresp <- gamresp <- v
  } else {
    size <- rep(20, nrow(data))
    RTMBresp <- "y1"
    gamresp <- "cbind(y1, 20-y1)"
  }
  gamform <- reformulate("s(x, bs = 'tp')", response = gamresp)
  scamform <- reformulate(sprintf("s(x, bs = '%s')", basis), response = gamresp)

  if (!skip_RTMB) {
    m_RTMB <- suppressWarnings(
      fit_mpd_fun(data,
                  ## evaluation of form is weird, try it this way ...
                  form = if (basis == "tp") s(x, bs = "tp") else s(x, bs = "mpd"),
                  size = size, 
                  family = "binomial",
                  response = RTMBresp,
                  random = "b1",
                  parms = list(b0 = 0, log_smSD = log_smSD, b1 = rep(0, 9)))
    )
  }
  m_gam_gcv <-  gam(gamform, family = binomial, data = data, method = "GCV.Cp")
  m_gam_reml <- gam(gamform, family = binomial, data = data, method = "REML")
  ## scam produces "non-integer #successes in a binomial glm!" warnings (!!!!) with binomial ...
  m_scam <-     scam(scamform, family = binomial, data = data)
  m_glmmTMB <-  glmmTMB(gamform, family = binomial, data = data, REML = TRUE)
  nm0 <- ls(pattern = "m_.*")
  nm <- nm0 |> gsub(pattern = "^m_", replacement = "")
  predmat <- mget(nm0) |> lapply(predict) |> do.call(what=cbind)
  colnames(predmat) <- nm
  return(predmat)
}

leg_names <- function(basis = "tp", skip_RTMB = FALSE) {
  nm <- c("gam/GCV/tp", "gam/REML/tp", "glmmTMB/REML/tp", sprintf(c("RTMB/REML/%s", "scam/UBRE/%s"), basis))
  if (skip_RTMB) nm <- grepv("RTMB", nm, invert = TRUE)
  nm
}
add_true <- function() lines(dd$x, qlogis(dd$p), lwd = 2)
add_rug <- function() {
  rug(dd$x[dd$y0==0], ticksize = 0.1, side = 1)
  rug(dd$x[dd$y0==1], ticksize = 0.1, side = 3)
}
add_legend <- function(leg_pos, ...) {
  legend(lwd = 2, lty = ltyvec, col = colvec, legend = leg_names(...), x = leg_pos)
}
plotfun <- function(predmat, ...,
                    truth = TRUE, legend = TRUE, rug = TRUE,
                    leg_pos = "right", data = dd,
                    skip_RTMB = FALSE,
                    basis = "tp",
                    title = "") {
  matplot(data$x, predmat, lty = ltyvec, col = colvec, type = "l", lwd = 2, ...)
  if (truth) add_true()
  if (legend) add_legend(leg_pos, skip_RTMB = skip_RTMB, basis = basis)
  if (rug) add_rug()
  title(title)
}

## RTMB fit is wonky with bs = "tp" (why????)
## DOES depend on starting value? see RTMB_tp_test.R ...
## all others agree, nearly
predmat_bern_tp <- fit_all()
plotfun(predmat_bern_tp, ylim = c(-10, 10), leg_pos = "bottomleft",
        title = "Bernoulli, all tp bases")

## scam, RTMB results reasonable (but different) with "mpd"
predmat_bern_mpd <- fit_all(basis = "mpd")
plotfun(predmat_bern_mpd, ylim =c(-10, 10), leg_pos = "bottomleft",
        basis = "mpd",
        title = "Bernoulli, tp/mpd bases")

## zoom in so we can confirm that RTMB, scam do in fact preserve monotonicity,
## and are very similar:
plotfun(predmat_bern_mpd, ylim = c(-1, 3), leg_pos = "bottomleft",
        title = "Bernoulli, tp/mpd bases, zoom",
        basis = "mpd")

## scam and RTMB are both a bit wonky
predmat_binom_tp <- fit_all(response = "binomial")
plotfun(predmat_binom_tp, ylim = c(-10, 10), leg_pos = "bottomleft",
        title = "Binomial, tp")

predmat_binom_mpd <- fit_all(response = "binomial", basis = "mpd")
plotfun(predmat_binom_mpd, ylim = c(-10, 10), leg_pos = "bottomleft",
        basis = "mpd",
        title = "Binomial, tp/md")
## zoom in: RTMB, scam give different answers, but neither is horribly wrong
plotfun(predmat_binom_mpd, ylim = c(-1, 2), leg_pos = "bottomleft",
        title = "Binomial, tp/md (zoom)",
        basis = "mpd")

dd_expand <- expand_bern(dd)
## ugh, error
predmat_bernbinom_mpd <- fit_all(response = "bernoulli", basis = "mpd", data = dd_expand, v = "y1",
                                 skip_RTMB = TRUE)
plotfun(predmat_bernbinom_mpd, data = dd_expand, ylim = c(-10, 10), skip_RTMB = TRUE, leg_pos = "bottomleft",
        title = "Expanded Bernoulli, tpd/md",
        basis = "mpd")


scam_comp <- cbind(binomial = predmat_binom_mpd[,"scam"],
                   RTMB = predmat_binom_mpd[,"RTMB"],
                   expanded = predmat_bernbinom_mpd[!duplicated(dd_expand$x), "scam"])
matplot(scam_comp, type = "l", col = colvec, lwd = 2,
        ylim = c(-10, 10))
legend("bottomleft", c("scam_binom", "RTMB_binom", "scam_expand"),
       col = colvec, lty = 1:3)
title("scam binom vs scam binom-expanded")

sessionInfo()

if (!interactive()) dev.off()

