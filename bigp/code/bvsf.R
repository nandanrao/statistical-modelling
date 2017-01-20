################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
################################################################################
# Course : Statistical Modelling and Inference
# Title  : Functions for BVS Practical Workshop
# Date   : 2017.01.13
################################################################################
# source('/Users/Miquel/Dropbox/bvsworkshop/workshop/code/bvsf.R')
################################################################################

################################################################################
postProb. <- function(ms, dec = 5, ...) {
# Wrapper for mombf::postProb to faster read its outcome
################################################################################
  if (class(ms) != 'msfit') {
    aux <- ms
  } else {
    aux <- mombf::postProb(ms, ...)
  }
  out <- cbind.data.frame(aux[, 1:2], round(aux[, 3], dec))
  colnames(out) <- names(aux)
  rownames(out) <- NULL
  return(out)
}

################################################################################
looCV.mle <- function(y, x) {
# Author: David Rossell
# Description: Least-squares Leave-one-out cross-validation
# Input
# - y: outcome
# - x: covariates
# Output
# - pred: cross-validated predictions for y
# - ssr: cross-validated sum of squared residuals
################################################################################
  x <- data.frame(x)
  pred <- double(nrow(x))
  for (i in 1:nrow(x)) {
    fit <- lm(y[-i] ~ ., data = x[-i, , drop = FALSE])
    pred[i] <- predict(fit, newdata = x[i, , drop = FALSE])
  }
  return(list(pred = pred, ssr = sum((pred - y) ** 2, na.rm = TRUE)))
}

################################################################################
looCV.bma <- function(y, x, type = 'bma', priorCoef,
                      priorDelta = modelbbprior(alpha.p = 1, beta.p = 1),
                      priorVar = igprior(alpha = 0.01, lambda = 0.01),
                      niter = 5000, niter2 = niter, pp = 'norm',
                      center = TRUE, scale = TRUE, mc.cores = 1, ...) {
# Description: BMA leave-one-out cross-validation
# Author: David Rossell
# Input
# - y: outcome
# - x: covariates
# - priorCoef: prior on coefficients, e.g. momprior(tau=.348)
# - priorDelta: prior on model space
# - priorVar: prior on residual variance
# - ...: other arguments to be passed on to modelSelection
# Output
# - pred: cross-validated predictions for y
# - ssr: cross-validated sum of squared residuals
################################################################################
  pred <- double(nrow(x))
  y <- y - mean(y)
  if (scale == TRUE) { y <- y / sd(y) }
  x <- scale(x, center = center, scale = scale)

  f <- function(i, ...) {
    ms <- modelSelection(y = y[-i], x = x[-i, , drop = FALSE], center = FALSE,
                         scale = FALSE, niter = niter, priorCoef = priorCoef,
                         priorDelta = priorDelta, priorVar = priorVar,
                         verbose = FALSE, ...)
    thpost <- rnlp(y = y[-i], x = x[-i, , drop = FALSE], msfit = ms,
                   priorCoef = priorCoef, priorVar = priorVar, niter = niter2)
    cat('.')
    extra <- sum(colMeans(thpost[, -ncol(thpost)]) * t(x[i, , drop = FALSE]))
    return(mean(y[-i]) + extra)
  }

  fmedian <- function(i, threshold, ...) {
    ms <- modelSelection(y = y[-i], x = x[-i, , drop = FALSE], center = FALSE,
                         scale = FALSE, niter = niter, priorCoef = priorCoef,
                         priorDelta = priorDelta, priorVar = priorVar,
                         verbose = FALSE, ...)
    sel <- ms$margpp > threshold
    thpost <- rnlp(y = y[-i], x = x[-i, sel, drop = FALSE],
                   priorCoef = priorCoef, priorVar = priorVar, niter = niter2)
    extra <- colMeans(thpost[, -ncol(thpost)]) * t(x[i, sel, drop = FALSE])
    cat('.')
    return(mean(y[-i]) + sum(extra))
  }

  if (type == 'bma') {
    pred <- unlist(lapply(1:nrow(x), f, ...))
  } else if (type == 'median') {
    pred <- unlist(lapply(1:nrow(x), fmedian, threshold = 0.5, ...))
  } else { stop('Invalid argument "type"') }
  return(list(pred = pred, ssr = sum((pred - y) ** 2, na.rm = TRUE)))
}
# END OF SCRIPT
