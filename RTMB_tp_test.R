library(RTMB)
library(glmmTMB)
library(scam)
library(mgcv)
source("funs.R")

if (!interactive()) pdf("RTMB_tp_test.pdf")

set.seed(101)
dd <- data.frame(x=seq(-5, 5, length = 101))
dd <- within(dd, {
  p <- plogis(1 + x - x^3/4)
  ## Bernoulli outcomes
  y0 <- rbinom(nrow(dd), size = 1, prob = p)
  ## binomial (N==20) outcomes
  y1 <- rbinom(nrow(dd), size = 20, prob = p)
})


mpd_start_fun <- function(log_smSD = 0, ret = "predict") {
  m_RTMB <- suppressWarnings(
    fit_mpd_fun(dd,
                ## evaluation of form is weird, try it this way ...
                form = s(x, bs = "tp"),
                size = rep(1, nrow(dd)), 
                family = "binomial",
                response = "y0",
                random = "b1",
                parms = list(b0 = 0, log_smSD = log_smSD, b1 = rep(0, 9)))
  )
  if (ret == "predict") predict(m_RTMB) else m_RTMB
}
obj <- mpd_start_fun(log_smSD = -2, ret = "obj")

update(obj$obj, map = list(log_smSD = factor(NA)
TMB::tmbprofile(obj$obj,  name = "log_smSD", parm.range = c(-3, 3))
log_smSD_vec <- seq(-3, 3, by = 0.5)
mpd_start_fit <- lapply(log_smSD_vec, mpd_start_fun) |> do.call(what=cbind)
matplot(dd$x, mpd_start_fit, ylim = c(-10, 10), type = "b")
add_true()
add_rug()
