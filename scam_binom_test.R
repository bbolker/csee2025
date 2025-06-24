## NOTES:
## headed down a bit of a rabbit hole right now.
## Fairly straightforward to show that scam with bs = "mpd" and
##  binomial (N>1) data doesn't actually respect the constraints,
##  but RTMB with "tp" basis (which should be OK) is not behaving at the moment ...

## everything below is also slightly in disarray as I try to modularize/DRY everything

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
fit_all <- function(response = c("bernoulli", "binomial"),
                    basis = "tp") {
  response <- match.arg(response)
  if (response == "bernoulli") {
    size <- rep(1, nrow(dd))
    RTMBresp <- gamresp <- "y0"
  } else {
    size <- rep(20, nrow(dd))
    RTMBresp <- "y1"
    gamresp <- "cbind(y1, 20-y1)"
  }
  gamform <- reformulate("s(x, bs = 'tp')", response = gamresp)
  scamform <- reformulate(sprintf("s(x, bs = '%s')", basis), response = gamresp)

  m_RTMB <- suppressWarnings(
    fit_mpd_fun(dd,
                ## evaluation of form is weird, try it this way ...
                form = if (basis == "tp") s(x, bs = "tp") else s(x, bs = "mpd"),
                size = size, 
                family = "binomial",
                response = RTMBresp,
                random = "b1",
                parms = list(b0 = 0, log_smSD = 3, b1 = rep(0, 9)))
  )
  m_gam_gcv <-  gam(gamform, family = binomial, data = dd, method = "GCV.Cp")
  m_gam_reml <- gam(gamform, family = binomial, data = dd, method = "REML")
  m_scam <-     scam(scamform, family = binomial, data = dd)
  m_glmmTMB <-  glmmTMB(gamform, family = binomial, data = dd, REML = TRUE)
  nm0 <- ls(pattern = "m_.*")
  nm <- nm0 |> gsub(pattern = "^m_", replacement = "")
  predmat <- mget(nm0) |> lapply(predict) |> do.call(what=cbind)
  colnames(predmat) <- nm
  return(predmat)
}

leg_names <- function(basis = "tp") {
  c("gam/GCV/tp", "gam/REML/tp", "glmmTMB/REML/tp", sprintf(c("RTMB/REML/%s", "scam/UBRE/%s"), basis))
}
add_true <- function() lines(dd$x, qlogis(dd$p), lwd = 2)
add_rug <- function() {
  rug(dd$x[dd$y0==0], ticksize = 0.1, side = 1)
  rug(dd$x[dd$y0==1], ticksize = 0.1, side = 3)
}

predmat_bern_tp <- fit_all()
matplot(dd$x, predmat_bern_tp, lty = ltyvec, col = colvec, type = "l", lwd = 2)
## RTMB way overfits -- undersmoothed??
legend(lwd = 2,
       lty = ltyvec,
       col = colvec,
       "right",
       legend = leg_names())


## results are heavily smoothed relative to true prob, but not surprising given
## v. low-resolution data

## zoom in so we can confirm that RTMB, scam do in fact preserve monotonicity:
matplot(dd$x, predmat_bern, lty = 1:4, col = colvec, type = "l", lwd = 2, xlim = c(-3, 3), ylim = c(-5, 5))
lines(dd$x, qlogis(dd$p), lwd = 2)

m_binom_gam_gcv <- gam(cbind(y1, 20-y1) ~ s(x, bs = "tp"), family = binomial, data = dd,
                      method = "GCV.Cp")
m_binom_gam_reml <- gam(cbind(y1, 20-y1) ~ s(x, bs = "tp"), family = binomial, data = dd,
                       method = "REML")
m_binom_scam_2col <- scam(cbind(y1, 20-y1) ~ s(x, bs = "tp"), family = binomial, data = dd)
## NOTE warnings() about non-integer # successes (due to weights issues)
m_binom_scam_wts <- scam(y1/20 ~ s(x, bs = "tp"), weights = rep(20, nrow(dd)),
                                                             family = binomial, data = dd)
m_binom_glmmTMB <- glmmTMB(cbind(y1, 20-y1) ~ s(x, bs = "tp"), family = binomial, data = dd,
                           REML = TRUE)

dd_expand <- expand_bern(dd)
m_binom_scam_expand <- scam(y1 ~ s(x, bs = "tp"),
                            family = binomial, data = dd_expand)

nm0 <- ls(pattern = "m_binom_.*")
nm <- nm0 |> gsub(pattern = "^m_", replacement = "")

## RTMB silently ignores newdata. no-op for the rest, only relevant for expanded data
predmat_binom <- (mget(nm0)
    |> lapply(predict, newdata = dd)
    |> as.data.frame()
    |> setNames(nm)
)

matplot(dd$x, predmat_binom, lty = 1:4, col = colvec, ylim = c(-25,25))
lines(dd$x, qlogis(dd$p), lwd = 2)
## squeeze obs probs in slightly so we can look at logit scale
points(dd$x, qlogis((dd$y1+0.25)/20.5))
legend("topright", lty = 1, lwd = 2,
       col = colvec,
       legend = gsub("_", "/", gsub("binom_", "", nm))
       )

## note, RTMB is mpd, not tp; FIXME ...

m_binom_RTMB_mpd <- fit_mpd_fun(dd, size = rep(20, nrow(dd)), family = "binomial", response = "y1",
                            random = "b1",
                            parms = list(b0 = 0, log_smSD = 3, b1 = rep(0, 9)))
m_binom_scam_expand_mpd <- scam(y1 ~ s(x, bs = "mpd"),
                            family = binomial, data = dd_expand)

nm0 <- ls(pattern = "m_.*_mpd")
nm <- nm0 |> gsub(pattern = "^m_", replacement = "")

## RTMB silently ignores newdata. no-op for the rest, only relevant for expanded data
predmat_mpd <- (mget(nm0)
    |> lapply(predict, newdata = dd)
    |> as.data.frame()
    |> setNames(nm)
)

matplot(dd$x, predmat_mpd, lty = 1:2, type = "l", col = colvec, ylim = c(-25,25))
lines(dd$x, qlogis(dd$p), lwd = 2)
## squeeze obs probs in slightly so we can look at logit scale
points(dd$x, qlogis((dd$y1+0.25)/20.5))
legend("topright", lty = 1, lwd = 2,
       col = colvec,
       legend = gsub("_", "/", gsub("binom_", "", nm))
       )

## close, not identical. RTMB actually looks slightly more sensible (ML vs GCV difference?)

sessionInfo()

if (!interactive()) dev.off()
