

################### LOAD ####################################
load("synth_reg.rda")
library(dplyr)
library(ggplot2)
library(MASS)



em <- function (T, X, iter = 10, nu = 10, tol=10**-3) {

    # Initialize our theta variables
    q <- 1
    w <- rep(0, ncol(X))
    ll <- c()
    i <- 1
    flatlined <- FALSE

    while (!flatlined && i <= iter) {
        Z <- estep(T, X, q, w, nu)
        Theta <- mstep(T, X, Z)
        q <- head(Theta, 1)
        w <- tail(Theta, -1)
        ll <- c(ll, likelihood(T, X, Z, q, w, nu))

        flatlined <- abs(tail(ll, 2) %*% c(-1, 1)) < tol
        i <- i + 1
    }

    # Calculate our standard errors!
    se <- std.err(X, q, Z)

    # Return everything.
    list(Z = Z, q = q, w = w, Z = Z, se = se, ll = ll)
}

std.err <- function (X, q, Z) {
    sqrt(diag(solve(t(X) %*% diag(Z) %*% X)))
}


likelihood <- function (T, X, Z, q, w, nu = 10, sum = TRUE, latent = TRUE) {
    N <- nrow(X)
    s <- c()

    for (i in 1:N) {
        sd <- sqrt(1/(q*Z[i]))
        likelihood <- dnorm(T[i], t(X[i,]) %*% w, sd, log=TRUE)
        lat <- 0
        if (latent) {
            lat <- dgamma(Z[i], nu/2, nu/2 - 1, log=TRUE)
        }
        s <- c(s, likelihood + lat)
    }
    if (sum) {
        s <- sum(s)
    }
    s
}

estep <- function (T, X, q, w, nu = 10) {
    N <- nrow(X)
    sapply(1:N, function (i) {
        (nu + 1)/(nu + q * (T[i] - t(X[i,]) %*% w)**2 - 2)
    })
}

mstep <- function (T, X, Z) {
    N <- nrow(X)
    w = solve((t(X) %*% diag(Z) %*% X ), t(X) %*% diag(Z) %*% T)
    q = 1/(1/N * t(T - X %*% w) %*% diag(Z) %*% (T - X %*% w))
    c(q, w)
}

simulate.dev <- function (Mu, X, Z, q, w, N = 50, quan = .99, latent=TRUE) {
    sim <- rmvnorm(N, Mu, diag(1/(q * Z)))
    dev <- sapply(1:N, function (i) {
        -2 * likelihood(sim[i,], X, Z, q, w, 10, FALSE, latent)
    })
    quantile(dev, quan)
}


# Run our algorithm!
X <- cbind(rep(1, nrow(synth_reg$Phy)), synth_reg$Phy)
res <- em(synth_reg$t, X, iter=100, tol = -Inf)

# MLE MODEL (LEAST SQUARES)
lm.model<- lm(t ~ Phy, synth_reg)
lm.coef <- coefficients(lm.model)
lm.Mu <- X %*% coef.lm
lm.q <- 1
lm.Z <- 1/rep(sd(X), nrow(X))


# Weights with errors
rbind(
    data.frame(w = res$w, se = res$se, model = "Robust Regression"),
    data.frame(w = lm.coef, se = coef(summary(lm.model))[, 2], model = "Least Squares")
) %>%
    group_by(model) %>%
    mutate(index = row_number()) %>%
    ggplot(aes(x = index, y = w)) +
    facet_grid(model ~ .) +
    geom_point() +
    geom_errorbar(aes(ymin = w - se, ymax = w + se )) +
    labs(x = "Parameter", y = "Coefficient", title = "Regression Coefficients with 1.96 SE")




# SIMULATIONS FOR 99%
lm.sim.q <- simulate.dev(lm.Mu, X, lm.Z, lm.q, lm.coef, latent = FALSE)
robust.sim.q <- simulate.dev(X %*% res$w, X, res$Z, res$q, res$w)

# DEVIANCES
dev.robust <- -2 * likelihood(T, X, res$Z, res$q, res$w, 10, FALSE)
dev.lm <- -2 * likelihood(T, X, 1/rep(sd(X), nrow(X)), 1, lm.coef, 10, FALSE, FALSE)


# Plotting Deviances
rbind(
    data.frame(residuals = dev.robust, model = "Robust Regression", quant = robust.sim.q),
    data.frame(residuals = dev.lm, model = "Least Squares", quant = lm.sim.q)
    ) %>%
    group_by(model) %>%
    mutate(index = row_number()) %>%
    ggplot(aes(x = index, y = residuals)) +
    facet_grid(model ~ ., scales="free") +
    geom_point() +
    geom_hline(aes(yintercept = quant, color = factor(round(quant, 3)))) +
    scale_colour_discrete(name = "99% Quantile") +
    labs(x = "Index", y = "Deviance", title = "Deviance Residuals with 99% Quantile Expectation from Simulation")


res.2 <- em(synth_reg$t, X, iter=20, nu=10)

data.frame(Likelihood = res.2$ll) %>%
    mutate(Iteration = row_number(), Change = c(0, diff(res.2$ll))) %>%
    ggplot(aes(x = Iteration, y = Likelihood)) +
    geom_point() +
    geom_line() +
    geom_vline(aes(xintercept = Iteration, color = factor(round(Change, 5)))) +
    scale_colour_discrete(name = "Change from Previous Iteration") +
    labs(title = "Log-Likelihood of EM Results Over Iterations")


#######################################
# CROSS VALIDATE!!
#######################################

my.split <- function (y, n) {

    # support for matrix or vector
    l <- ifelse(class(y) == 'matrix', nrow(y), length(y))
    size <- floor(l/n)
    lapply(1:n, function (i) {
        tail(head(y, i*size), size)
    })
}


cross.validation <- function (y, X, em, nus) {

    # hardcode number of folds
    n <- 5
    sets.y <- my.split(y, n)
    sets.X <- my.split(X, n)
    RSS <- c()

    for (nu in nus) {
        temp <- 0
        for (i in 1:n) {
            test.y <- sets.y[[i]]
            test.X <- sets.X[[i]]

                                        # ugliness for removing list
            train.y <- unlist(sets.y[setdiff(1:n, i)])
            train.X <- do.call(rbind, sets.X[setdiff(1:n, i)])

            beta <- em(train.y, train.X, iter=20, nu = nu)$w

                                        # We want the average of the RSS for all, so sum their fraction
            temp <- temp + sum((test.y - test.X %*% beta)**2)/n
        }
        RSS <- c(RSS, temp)
    }

    RSS
}

cv <- cross.validation(synth_reg$t, X, em, 3:30)


###############################
# STAN
###############################

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
model_robust <- stan("model_robust.stan", data=synth_reg)



################################
# MIX
################################

X.nas <- data.frame(z = NA, X)
X.mix <- prelim.mix(X.nas, 0)
