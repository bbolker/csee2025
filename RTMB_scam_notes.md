## Shape-constrained fits with RTMB

Continuing frustration with the fragility/wonkiness of RTMB fits using `scam`. I think the fundamental problem is that scam effectively requires an increasing sequence of parameters, which is achieved here by computing the cumulative sum of a vector of *positive* (non-negative?) values, which in `scam` etc are constrained positive by exponentiating the raw parameters.

For small data sets, many of these parameters (which are "random effects", i.e. Laplace-applied, in the TMB world) appear to hit their lower bound - `scam` reports values around -11 to -15 (2e-5 to 1e-6), while RTMB + nlminb gets much lower (-200 or so). Getting to this lower bound tends to break the inner optimization step in (R)TMB ... was using `maxit=1` in inner.control (a *single* Newton step!), but I now think that's crazy. I don't think there's any *easy* way to introduce lower bounds into the inner optimization step ...

Not sure what `scam` is doing differently/why it succeeds in getting small, but not breakingly small, values for the almost-zero parameters. Best guess is that it's at the end of section 3.2, where it discusses how to use SVD in the algorithm to auto-drop unidentifiable parameters and use a pseudo-inverse ... (does that lead to weakly identifiable parameters stopping at their 'last identifiable' points?)

* is it setting a lower bound somewhere? (I don't see anything in the code ...)
* is the difference between REML and GCV/UBRE leading to different smoothing penalties and hence to problems with TMB but not scam?
* not sure why the radical difference between 'included' (beta < -100) and 'unincluded' (beta > -3) latent variables isn't being mitigated

If we fit without Laplace, for a small data set, the penalty term blows up (RE std dev → 0) and all the latent variables collapse to a mean value

### Possible solutions

* implement bounds in inner opt? (If I was going to go that far I could even work on the non-exponential scale and set lower bounds to 0 rather than (say) some reasonably small log-value like -14 ...)
* regularize the Hessian in the inner optimization?
* if there's an interaction with the variance parameter, try fixing the variance/penalty parameter? (Extract from `scam` fit?)
* do a second pass where the apparently-zero `b` parameters are mapped to small values? (That could lead to reasonable fits but would mess up uncertainty calculations ... ??)
* some other way to implement the ordering constraint (that also works with autodiff)? See https://groups.google.com/g/tmb-users/c/UCwY5YyzJxc (Hans Skaug points out that `nloptr` should be able to impose inequality constraints, but this won't apply in the inner optimization ...)

What is the difference, really, between including and not including the Laplace approximation, i.e. between a simply 'penalized' regression and a mixed model? (What does mgcv say/do about this?) Is there any difference once we condition on the penalty parameter?  What if we fix the RE variance and then fit without Laplace? (If this works, how do we decide on a good value for logSD?) 

## RTMB consistency with glmmTMB

Even for `bs="tp"` (boring old thin plate spline, none of the issues above apply) I'm still getting some wonky results with my RTMB code. `glmmTMB` seems to handle this case fine, so ... ?? (Not that important, except if diagnosing this problem/pulling on this loose thread leads to some other more general bug-fixes/insights/solutions ...)
