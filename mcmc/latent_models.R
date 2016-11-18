library(rstan)
library(MASS)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load("synth_reg.rda")


##########################################################
## ROBUST REGRESSION
##########################################################
model_robust <- stan("model_robust.stan", data=synth_reg)


##########################################################
## RANDOM EFFECTS
##########################################################
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

model.random.effects <- stan("model_random_effects.stan", data=data)
