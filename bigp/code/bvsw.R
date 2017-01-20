################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
################################################################################
# Course : Statistical Modelling and Inference
# Title  : BVS Practical Workshop
# Author : Miquel Torrens
# Date   : 2017.01.13
################################################################################
# source('/Users/Miquel/Dropbox/bvsworkshop/bvsw/code/bvsw.R')
################################################################################

################################################################################
# Preliminaries
################################################################################
# Path (adapt it to your local path)
PATH <- '../'

# Global parameters
force <- TRUE    # Turn to TRUE to force installation of missing packages
compute <- FALSE  # Turn to TRUE to make all computations

# Load functions
source(paste(PATH, 'code/bvsf.R', sep = ''))

# Dependencies
pkgs <- c('mombf', 'ncvreg', 'devtools', 'screening', 'parallel', 'monomvn')
for (pkg in pkgs) {
  if (! require(pkg, character.only = TRUE)) {
    if (force == TRUE) {
      if (pkg == 'screening') {
        devtools::install_github('wwrechard/screening')
      } else {
        install.packages(pkg, repos = 'https://cloud.r-project.org')
      }    
      if (! require(pkg, character.only = TRUE)) {
        warning('Package "', pkg, '" could not be installed.', sep = '')
      }
    }
  }
}
cat('Loaded dependencies:', paste(pkgs, collapse = ', '), '\n')

# Load auxiliary data (Gene expression data)
file <- paste(PATH, 'data/tgfb.txt', sep = '')
tgfb <- read.table(file, header = TRUE, sep = '\t')
cat('Loaded file:', file, '\n')
################################################################################

################################################################################
# Small number of predictors: full enumeration
################################################################################
# Generate random data
set.seed(666)
n <- 100  # Observations: 100
p <- 5    # Predictors: 5
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
w <- matrix(c(1, 0.5, 0, 2, 0), ncol = 1)
y <- X %*% w + rnorm(n)  # Assumption of normal residuals with q = 1

# Prior especification
prior.w <- mombf::imomprior(tau = 0.131)  # iMOM prior (tau = .131)
prior.M <- mombf::modelbbprior(alpha.p = 1, beta.p = 1)  # BetaBin(1, 1)
prior.q <- mombf::igprior(1e-3, 1e-3)  # IG prior (a = b = 0.001)

# Model selection (Full enumeration: enumerate = TRUE, FALSE does Gibbs)
ms00 <- modelSelection(y = y, x = X, priorCoef = prior.w, priorDelta = prior.M,
                       priorVar = prior.q, enumerate =  TRUE)

# Results
ms00[['postMode']]  # Posterior mode model (most frequented model)
ms00[['margpp']]    # Marginal posterior probs. of inclusion
pp00 <- postProb(ms00)

# Pretty outcomes
round(ms00[['margpp']], 4)
head(postProb.(pp00))

# R^2 Comparison
summary(lm(y ~ X[, 1] + X[, 2] + X[, 3] + X[, 4] + X[, 5]))$adj.r.squared
summary(lm(y ~ X[, 1] + X[, 2] + X[, 4]))$adj.r.squared

# Estimated coefficients
wh00 <- rnlp(y = y[, 1], x = X, msfit = ms00, priorCoef = prior.w, niter = 1e4)
bma00 <- apply(wh00, 2, mean)

# Comparing BMA vs. Frequentist point coefficients
round(bma00, 4)  # BMA
round(c(coef(summary(lm(y ~ X)))[2:(1 + p), 1], summary(lm(y ~ X))$sigma), 4)

# Comparing Bayesian vs. Frequentist dispersion of coefficients
round(apply(wh00, 2, sd), 4)  # Irrelevant coefficients shrunk to zero!
round(coef(summary(lm(y ~ X)))[2:(1 + p), 2], 4)

# Probability with which you use every model
w00 <- apply(wh00[, 1:p] != 0, 1, function(z) {
  paste(which(z), collapse = ',')
})
table(w00)
round(table(w00) / sum(table(w00)), 5)  # Proportions of Gibbs visits

# Results for specific model, e.g. HPM
whpm00 <- apply(wh00[w00 == pp00[1, 1], ], 2, mean)
bma00  # Compare to BMA

# BMA prediction example (for the last row)
(pr00 <- as.numeric(X[n, ] %*% matrix(bma00[1:p])))  # Predicted
(ob00 <- y[n, 1])  # Observed
################################################################################

################################################################################
# Comparing Full Enumeration vs. Gibbs sampler
################################################################################
# Use colon cancer data
y <- scale(tgfb[, 'tgfb'])
X <- scale(tgfb[, -1])[, 1:20]  # Pick first 20 predictors (+1M models)
n <- nrow(X)

# Prior especifications
prior.w <- zellnerprior(tau = n)  # Unit Information Prior (parameters)
prior.U <- modelunifprior()       # Uniform prior (model space)
prior.B <- modelbbprior()         # Beta-binomial prior (model space)

# Model under uniform prior (with full enumeration and with Gibbs)
ms01 <- modelSelection(y = y, x = X, priorCoef = prior.w,
                       priorDelta = prior.U, enumerate = TRUE)
ms02 <- modelSelection(y = y, x = X, priorCoef = prior.w,
                       priorDelta = prior.U, niter = 1e5)

# Model under beta-binomial prior (with full enumeration and with Gibbs)
ms03 <- modelSelection(y = y, x = X, priorCoef = prior.w,
                       priorDelta = prior.B, enumerate = TRUE)
ms04 <- modelSelection(y = y, x = X, priorCoef = prior.w,
                       priorDelta = prior.B, niter = 1e5)

# Posterior probabilities
pp01 <- postProb(ms01)
pp02 <- postProb(ms02)
pp03 <- postProb(ms03)
pp04 <- postProb(ms04)

# Best models in each case (based on posterior probabilities)
head(postProb.(pp01))
head(postProb.(pp02))  # Gibbs approximates well full enumeration
head(postProb.(pp03))
head(postProb.(pp04))  # Especially, it preserves model ordering

# Number of variables (under full enumeration)
vs01 <- sapply(strsplit(as.character(pp01[, 1]), split = ','), length)
vs03 <- sapply(strsplit(as.character(pp03[, 1]), split = ','), length)
(mn01 <- round(tapply(pp01[, 'pp'], vs01, sum), 4))
(mn03 <- round(tapply(pp03[, 'pp'], vs03, sum), 4))

# Best candidates
as.vector(which(ms01[['postMode']] == 1))  # HPM
as.vector(which(ms03[['postMode']] == 1))
as.vector(which(ms01[['margpp']] > 0.50))  # Ones with larger Marg. PP
as.vector(which(ms03[['margpp']] > 0.50))

# Compare Beta-Binomial against Uniform
plot(names(mn01), mn01, type = 'l', xlab = 'Number of variables',
     ylab = 'Posterior probability', ylim = c(0, 0.3), col = 'red')
lines(names(mn03), mn03, col = 'blue')
legend('topright', c('Uniform','Beta-Binomial(1, 1)'), lty = 1,
       col = c('red', 'blue'))

# Estimated BMA coefficients
wh01 <- rnlp(y = y[, 1], x = X, msfit = ms01, priorCoef = prior.w, niter = 1e4)
wh03 <- rnlp(y = y[, 1], x = X, msfit = ms03, priorCoef = prior.w, niter = 1e4)
(bma01 <- round(apply(wh01, 2, mean), 4))
(bma03 <- round(apply(wh03, 2, mean), 4))

# Comparison of the estimates
plot(bma01, pch = 16, col = 'red', xlab = 'Predictor #', ylab = 'BMA')
points(bma03, col = 'darkblue', pch = 16)
title('Coefficient estimates under different priors')
legend('topleft', c('Uniform','Beta-Bin'), pch = 16, col = c('red', 'darkblue'))
################################################################################

################################################################################
# Comparing local and non-local priors
################################################################################
# Full enumeration: off the table
X <- scale(tgfb[, -1])  # Dimension: 262 x 172
p <- ncol(X)

# Various priors
prior.q <- igprior(1e-3, 1e-3)     # IG prior (alpha = 0.001 = beta)
prior.M <- modelbbprior()          # BetaBin(1, 1)
prior.L <- zellnerprior(tau = n)   # Unit Information Prior
prior.N <- imomprior(tau = 0.131)  # iMOM prior (tau = .131)

# Compute models
file5 <- paste(PATH, 'data/ms05.RData', sep = '')
file6 <- paste(PATH, 'data/ms06.RData', sep = '')
if (compute == TRUE) {
  # Gibbs on a local prior
  ms05 <- modelSelection(y = y, x = X, priorCoef = prior.L,
                         priorDelta = prior.B, priorVar = prior.q, niter = 1e5)

  # Gibbs on a non-local prior (careful! Takes a lot of time)
  ms06 <- modelSelection(y = y, x = X, priorCoef = prior.N,
                         priorDelta = prior.B, priorVar = prior.q, niter = 1e5)

  # Save results
  save(ms05, file = file5); cat('Saved file:', file5, '\n')
  save(ms06, file = file6); cat('Saved file:', file6, '\n')
} else {
  # Load results
  ms05 <- get(load(file = file5)); cat('Loaded file:', file5, '\n')
  ms06 <- get(load(file = file6)); cat('Loaded file:', file6, '\n')
}

# Posterior probabilities
pp05 <- postProb(ms05)
pp06 <- postProb(ms06)
head(postProb.(pp05))
head(postProb.(pp06))
as.vector(which(ms05[['postMode']] == 1))  # HPM
as.vector(which(ms06[['postMode']] == 1))
as.vector(which(ms05[['margpp']] > 0.05))  # Ones with larger Marg. PP
as.vector(which(ms06[['margpp']] > 0.05))

# Number of variables
vs05 <- sapply(strsplit(as.character(pp05[, 1]), split = ','), length)
vs06 <- sapply(strsplit(as.character(pp06[, 1]), split = ','), length)
(mn05 <- round(tapply(pp05[, 'pp'], vs05, sum), 4))
(mn06 <- round(tapply(pp06[, 'pp'], vs06, sum), 4))

# Compare Local and Non-local priors
plot(names(mn05), mn05, type = 'l', xlab = 'Number of variables',
     ylab = 'Posterior probability', ylim = c(0, 0.45), col = 'red')
lines(names(mn06), mn06, col = 'blue')
legend('topright', c('Local prior','Non-local prior'), lty = 1,
       col = c('red', 'blue'))

# What model is chosen Bayesian vs. Frequentist?
(ols <- summary(lm(y ~ X)))
postProb.(pp05)[1, ]  # e.g. HPM

# Estimated BMA coefficients
wh05 <- rnlp(y = y[, 1], x = X, msfit = ms05, priorCoef = prior.w, niter = 1e4)
wh06 <- rnlp(y = y[, 1], x = X, msfit = ms06, priorCoef = prior.w, niter = 1e4)
bma05 <- apply(wh05, 2, mean)
bma06 <- apply(wh06, 2, mean)
wols06 <- coef(ols)[, 1]

# Comparing BMA vs. Frequentist point estimations
plot(wols06, col = 'darkgreen', pch = 1, xlab = 'Predictor #', ylab = 'Pred. w')
points(bma06[1:p], col = 'darkblue', pch = 16)
points(bma05[1:p], pch = 16, col = 'red')
title('Coefficient estimates under local and non-local priors')
legend('topleft', c('Local','Non-local', 'OLS'), pch = c(16, 16, 1),
       col = c('red', 'darkblue', 'darkgreen'))
################################################################################

################################################################################
# Gains in high dimensions
################################################################################
# Generate random data
set.seed(666)
n <- 500
p <- 200
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
w <- matrix(rep(0, p), ncol = 1)
w[seq(50, p, 50)] <- sample(1:length(seq(50, p, 50)), replace = FALSE)
y <- X %*% w + rnorm(n, sd = 3)  # Assumption of normal residuals

# Prior especification
prior.w <- mombf::imomprior(tau = 0.131)  # iMOM prior (tau = .131)
prior.M <- mombf::modelbbprior(alpha.p = 1, beta.p = 1)  # Beta-binomial(1, 1)
prior.q <- mombf::igprior(1e-3, 1e-3)  # IG prior (alpha = 0.001 = beta)

# Compute models
file <- paste(PATH, 'data/ms07.RData', sep = '')
if (compute == TRUE) {
  # Model selection (true model has p / 50 active features)
  ms07 <- modelSelection(y = y, x = X, priorCoef = prior.w,
                         priorDelta = prior.M, priorVar = prior.q, niter = 1e5)
  # Save results
  save(ms07, file = file); cat('Saved file:', file, '\n')
} else {
  # Load results
  ms07 <- get(load(file = file)); cat('Loaded file:', file, '\n')
}

# Results
pm07 <- ms07[['postMode']]  # Posterior mode inclusion
mp07 <- ms07[['margpp']]    # Marginal posterior probs. of inclusion
pp07 <- postProb(ms07)      # Posterior model probabilities

# Best predictors
margpp07 <- round(sort(mp07, decreasing = TRUE), 5)
names(margpp07) <- order(mp07, decreasing = TRUE)
head(margpp07, 20)

# Best models
head(postProb.(pp07))
as.vector(which(ms07[['postMode']] == 1))
as.vector(which(ms07[['margpp']] > 0.05))

# Model choice compared to lm
summary(lm(y ~ X))    # A lot of significant predictors...
postProb.(ms07)[1, ]  # :)

# Comparing BMA vs. Frequentist
wh07 <- rnlp(y = y[, 1], x = X, msfit = ms07, priorCoef = prior.w, niter = 1e4)
bma07 <- apply(wh07, 2, mean)
wols07 <- coef(summary(lm(y ~ X)))[2:(1 + p), 1]
plot(wols07, col = 'red', pch = 16, xlab = 'Predictor #', ylab = 'Pred. w')
points(bma07[1:p], col = 'darkblue', pch = 16)
abline(h = 1:p, lty = 2)
legend('topleft', c('BVS', 'OLS'), pch = 16, col = c('darkblue', 'red'))

################################################################################
# When p >> n
set.seed(666)
n <- 100
p <- 500
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
w <- matrix(rep(0, p), ncol = 1)
w[seq(50, p, 50)] <- sample(1:length(seq(50, p, 50)), replace = FALSE)
y <- X %*% w + rnorm(n, sd = 3)  # Assumption of normal residuals

# Compute models
file <- paste(PATH, 'data/ms08.RData', sep = '')
if (compute == TRUE) {
  # Model selection (Full enumeration: enumerate = TRUE)
  ms08 <- modelSelection(y = y, x = X, priorCoef = prior.w,
                         priorDelta = prior.M, priorVar = prior.q, niter = 1e5)
  # Save results
  save(ms08, file = file); cat('Saved file:', file, '\n')
} else {
  # Load results
  ms08 <- get(load(file = file)); cat('Loaded file:', file, '\n')
}

# Results
pm08 <- ms08[['postMode']]  # Posterior mode inclusion
mp08 <- ms08[['margpp']]    # Marginal posterior probs. of inclusion
pp08 <- postProb(ms08)      # Posterior model probabilities

# Best predictors
margpp08 <- round(sort(ms08[['margpp']], decreasing = TRUE), 5)
names(margpp08) <- order(ms08[['margpp']], decreasing = TRUE)
head(margpp08, 15)

# Best models
head(postProb.(pp08))

# Comparison to usual approach
summary(lm(y ~ X))    # Can't be computed!
postProb.(pp08)[1, ]  # Really close!

# Comparing BMA vs. Frequentist point estimations
wh08 <- rnlp(y = y[, 1], x = X, msfit = ms08, priorCoef = prior.w, niter = 1e4)
bma08 <- apply(wh08, 2, mean)[1:p]
sort(round(bma08, 2), decreasing = TRUE)[1:30]
plot(bma08, col = 'darkblue', pch = 16, xlab = 'Predictor #', ylab = 'BMA')
abline(h = 1:p, lty = 2)
################################################################################

################################################################################
# Comparing BMA vs. Median probability model
################################################################################
# Example with p = 20
y <- scale(tgfb[, 'tgfb'])
X <- scale(tgfb[, -1])[, 1:20]  # Pick 20 features
prior.w <- zellnerprior(tau = nrow(X))  # Unit information prior

# Compute models
file <- paste(PATH, 'data/loo_fits.RData', sep = '')
if (compute == TRUE) {
  # Least squares estimator
  fit.mle <- looCV.mle(y = y, x = X)

  # BMA
  fit.bma <- looCV.bma(y = y, x = X, type = 'bma', priorCoef = prior.w,
                       priorDelta = prior.M, niter = 1e4)

  # Median probability model
  fit.med <- looCV.bma(y = y, x = X, type = 'median', priorCoef = prior.w,
                       priorDelta = prior.M, niter = 1e4)

  # Save results
  save(fit.mle, fit.bma, fit.med, file = file)
  cat('Saved file:', file, '\n')
} else {
  aux <- load(file = file); cat('Loaded file:', file, '\n')
  fit.mle <- get(aux[1])
  fit.bma <- get(aux[2])
  fit.med <- get(aux[3])
}

# Compare predictions for different methods
nomx <- 'Bayesian model averaging'
nomy1 <- 'Median probability model'
nomy2 <- 'Least squares'
par(mfrow = c(1, 2))
plot(fit.bma[['pred']], fit.med[['pred']], xlab = nomx, ylab = nomy1)
abline(0,1)
plot(fit.bma[['pred']], fit.mle[['pred']], xlab = nomx, ylab = nomy2)
abline(0,1)
################################################################################

################################################################################
# Heuristics
################################################################################
# Recover TGFB data
y <- scale(tgfb[, 1])
X <- scale(tgfb[, -1])  # Dimension: 262 x 172
Z <- tgfb
colnames(Z) <- c('y', paste('x', 1:(ncol(tgfb) - 1), sep = ''))

################################################################################
# Forward / backward / stepwise regression
m0 <- lm(y ~ 1, data = Z)  # Smallest model
mf <- lm(y ~ ., data = Z)  # Largest model

# Run regressions
fwd.m <- step(m0, direction = 'forward', scope = formula(mf))
bwd.m <- step(mf, direction = 'backward', data = Z)
hyb.m <- step(m0, scope = list(upper = mf), data = Z, direction = 'both')

# Normal "lm" objects
summary(fwd.m)
summary(bwd.m)
summary(hyb.m)

################################################################################
# Lasso / Scad / Bayesian Lasso
# Run model
lasso.m01 <- ncvreg(X, y, penalty = 'lasso')
scad.m01 <- ncvreg(X, y, penalty = 'SCAD')

# Need to specify some lambda
summary(lasso.m01, lambda = 0.1)
summary(scad.m01, lambda = 0.1)

# Choose the one minimising loss
cl <- makeCluster(detectCores() - 1)
lasso.m02 <- cv.ncvreg(X, y, cluster = cl, nfolds = 10, penalty = 'lasso')
#lasso.m03 <- cv.ncvreg(X, y, cluster = cl, nfolds = nrow(X), penalty = 'lasso')
scad.m02 <- cv.ncvreg(X, y, cluster = cl, nfolds = 10, penalty = 'SCAD')
#scad.m03 <- cv.ncvreg(X, y, cluster = cl, nfolds = nrow(X), penalty = 'SCAD')

# Results
summary(lasso.m02)
#summary(lasso.m03)
summary(scad.m02)
#summary(scad.m03)

# Plot of the paths and cv-scores
par(mfrow = c(1, 2))
plot(lasso.m02)
plot(lasso.m02[['fit']])
par(mfrow = c(1, 2))
plot(scad.m02)
plot(scad.m02[['fit']])

# Model coefficients
plot(coef(lasso.m02), col = 'darkblue', pch = 16,
     xlab = 'Predictor', ylab = 'w_hat', main = 'Lasso and Scad')
grid()
points(coef(scad.m02), col = 'red', pch = 16)
legend('topleft', c('Lasso', 'Scad'), pch = 16, col = c('darkblue', 'red'))

# To do Bayesian Lasso (really intensive)
if (compute == TRUE) {
  blasso.m01 <- monomvn::blasso(X, y, T = 1e3)
  summary(blasso.m01)
  plot(blasso.m01)
}

################################################################################
# Prescreening: SIS and HOLP
sis.m01 <- screening(X, y, method = 'sis', num.select = 50)
sis.m02 <- screening(X, y, method = 'sis', num.select = 10)
sis.m03 <- screening(X, y, method = 'sis', num.select = 5)
holp.m01 <- screening(X, y, method = 'holp', num.select = 50)
holp.m02 <- screening(X, y, method = 'holp', num.select = 10)
holp.m03 <- screening(X, y, method = 'holp', num.select = 5)

# Results
sis.m01[['screen']]
sis.m02[['screen']]
sis.m03[['screen']]
holp.m01[['screen']]
holp.m02[['screen']]
holp.m03[['screen']]

# Are results similar?
sort(sis.m02[['screen']])
sort(holp.m02[['screen']])

################################################################################
# Block-diagonality
# CASE 1: Block-orthogonal design
set.seed(666)
p <- 200
n <- 210
X <- scale(matrix(rnorm(n * p), nrow = n, ncol = p))
e <- eigen(cov(X))
X <- t(t(X %*% e[['vectors']]) / sqrt(e[['values']]))
w <- c(rep(0, p - 3), c(0.5, 0.75, 1))
q <- 1
y <- X %*% matrix(w, ncol = 1) + rnorm(n, sd = sqrt(q))

# Priors
prior.M <- modelbinomprior(p = 1 / p)
prior.q <- igprior(1e-3, 1e-3)
prior.w1 <- zellnerprior(tau = n)
prior.w2 <- momprior(tau = 0.348)

# Algorithm
bd.m01 <- postModeOrtho(y, x = X, priorCoef = prior.w1, priorDelta = prior.M,
                        priorVar = prior.q, bma = TRUE)
bd.m02 <- postModeOrtho(y, x = X, priorCoef = prior.w2, priorDelta = prior.M,
                        priorVar = prior.q, bma = TRUE)

# Results
head(bd.m01[['models']])
head(bd.m02[['models']])
tail(round(bd.m01[['bma']], 2))
tail(round(bd.m02[['bma']], 2))

# Coefficient BMA estimates
par(mar = c(5, 5, 1, 1))
ols <- (t(X) %*% y) / colSums(X ** 2)
plot(ols, bd.m01[['bma']][, 'coef'], xlab = 'Least squares estimate',
     ylab = expression(paste('E(', beta[j], '| y)')), cex.lab = 1.5,
     cex.axis = 1.2, col = 'blue', pch = 0)
points(ols, bd.m02[['bma']][, 'coef'], pch = 1, col = 'red')
legend('topleft', c('Zellner', 'MOM'), pch = c(0, 1), col = c('blue', 'red'))

# CASE 2: Block-diagonal design
set.seed(666)
p <- 100
n <- 110
bsize <- 10
blocks <- rep(1:(p / bsize), each = bsize)
X <- scale(matrix(rnorm(n * p), nrow = n, ncol = p))
e <- eigen(cov(X))
X <- t(t(X %*% e[['vectors']]) / sqrt(e[['values']]))

# Build block-diagonal matrix
Sb <- diag(bsize)
Sb[upper.tri(Sb)] <- Sb[lower.tri(Sb)] <- 0.5
vv <- eigen(Sb)[['vectors']]
sqSb <- vv %*% diag(sqrt(eigen(Sb)[['values']])) %*% t(vv)
for (i in 1:(p / bsize)) {
  X[, blocks == i] <- X[, blocks == i] %*% sqSb
}

# Parametrize
q <- 1
w <- rep(0, p)
w[blocks == 1] <- c(rep(0, bsize - 3), c(0.5, 0.75, 1))
w[blocks == 2] <- c(rep(0, bsize - 2), c(0.75, -1))
y <- X %*% matrix(w, ncol = 1) + rnorm(n, sd = sqrt(q))

# Run model
bd.m03 <- postModeBlockDiag(y = y, x = X, blocks = blocks, priorCoef = prior.w1,
                            priorDelta = prior.M, priorVar = prior.q,
                            bma = TRUE)

# Results
(aux <- head(bd.m03[['models']][, 1:3], 10))
(aux <- cbind.data.frame(aux[, 1:2], round(aux[, 3], 4)))
round(bd.m03[['bma']][1:30, ], 2)
head(bd.m03[['postmean.model']], 10)
bd.m03[['postmean.model']][6, ]  # True model
################################################################################
# END OF SCRIPT
