library(rstan)
library(MASS)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load("synth_reg.rda")


##########################################################
## ROBUST REGRESSION
##########################################################

# --------------------- MCMC
model_robust <- stan("model_robust.stan", data=synth_reg)
loglik.robust <- function (params, X, t, nu = 10) {
    q = 1
    N <- nrow(X)
    M <- ncol(X)
    W <- head(params, -N)
    Eta <- tail(params, N)

    mean <- X %*% W

    s <- 0
    for (i in 1:N) {
        sd <- sqrt(1/(Eta[i]))
        likelihood <- dnorm(t[i], mean[i], sd, log = TRUE)
        latent <- dgamma(Eta[i], nu/2, nu/2 - 1, log = TRUE)
        s <- s + likelihood + latent
    }
    -s
}

# --------------------- Conjugate Gradient Descent
params <- c(rep(0, synth_reg$M + 1), rep(1,synth_reg$N))
X <- cbind(rep(1, nrow(synth_reg$Phy)), synth_reg$Phy)
o.robust <- optim(params, fn = loglik.robust, method=c("CG"), X = X, t = synth_reg$t, nu = 10)
weights.mle.robust <- o.robust$par[1:31]
weights.mle.robust


##########################################################
## RANDOM EFFECTS
##########################################################

# --------------------- Generate Data
generate.data <- function (N, M, nu = .5) {
    # This is overcomplicated, but makes it feel more realistic?
    # No need for them to be from a distribution as we treat them as IID!
    X <- mvrnorm(N, runif(M, 0, 3), diag(runif(M, 0, 3)))

    # weights are uniform <-- change to play with regularization!
    weights <- runif(M, -2, 2)

    t.pre <- X %*% weights
    Z <- rnorm(N, 0, nu^2)

    Lambda <- exp(t.pre + Z)

    t <- sapply(Lambda, function (l) rpois(1, l))
    list(N = N, M = M, nu = nu, t = t, X = X, weights = weights)
}

set.seed(19)
data <- generate.data(75, 10, .5)


# --------------------- MCMC
model.random.effects <- stan("model_random_effects.stan", data=data)

# --------------------- Conjugate Gradient Descent
loglik.random <- function (params, X, t) {
    N <- dim(X)[1]
    W <- head(params, -N)
    Z <- tail(params, N)

    lambda <- X %*% W

    likelihood <- dpois(t, exp(lambda + Z), log = TRUE)
    latent <- sapply(Z, function (zet) dnorm(zet, 0, .5^2, log = TRUE))

    -sum(likelihood + latent)
}

o.random <- optim(rnorm(85,0,1), fn = loglik.random, method=c("CG"), X = data$X, t = data$t)
weights.mle.random <- o.random$par[1:31]
weights.mle.random
