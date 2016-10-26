###########################################################
## PROSTATE
##########################################################

library(dplyr)
library(lasso2)
data(Prostate)

set.seed(2)

# In the book they normalized, so we do the same ehre.
cols <- colnames(Prostate)
p.scaled <- Prostate %>% mutate_each_(funs(scale), vars = cols[which(cols != "lpsa")])

# We want a 67/30 split of for training/test data.
sample.indices <- sample(nrow(p.scaled), size = 67)
p.train <- p.scaled[sample.indices, ]
p.test <- p.scaled[-sample.indices, ]


# manually create hat matrix. why not.
X <- cbind(rep(1, nrow(p.train)), data.matrix(subset(p.train, select = -c(lpsa))))
hat <- X %*% solve(t(X) %*% X) %*% t(X)

# model
model <- lm(lpsa ~ ., p.train)
summary(model)



# manually create hat matrix. why not.
X.raw <- data.matrix(subset(Prostate, select = -c(lpsa)))
hat.raw <- X.raw %*% solve(t(X.raw) %*% X.raw) %*% t(X.raw)

# model
model.raw <- lm(lpsa ~ ., Prostate)
summary(model.raw)


###########################################################
## Synthetic Regression
##########################################################

library(stats4)
library(ggplot2)
library(dplyr)

dat.raw <- read.csv("synthetic_regression.txt", sep=" ", nrows=300)
dat.trim <- dat.raw[, 0:31]
dat.model <- lm(t ~ ., dat.trim)

# Plot parameter estimates and confidence intervals
se <- coef(summary(dat.model))[, 2]
ce <- dat.model$coefficients
dat.estimates <- data.frame(cbind(ce, ce - 1.96 * se, ce + 1.96 * se))
colnames(dat.estimates) <- c("mean", "lower.bound", "upper.bound")
dat.estimates$coefficient = 0:(dim(dat.estimates)[1] - 1)
dat.estimates %>%
    ggplot(aes(x = coefficient, y = mean)) +
    geom_point() +
    geom_pointrange(aes(ymin = lower.bound, ymax = upper.bound))


# Plot leverage points
dat.scaled.residuals <- scale(residuals(dat.model))
dat.hats <- influence(dat.model)$hat
threshold <- 1.5*31/300

mod <- data.frame(fitted = dat.model$fitted, scaled.residuals = dat.scaled.residuals) %>%
    mutate(leverage = dat.hats, high.leverage = leverage > threshold )

ggplot(mod, aes(fitted, scaled.residuals)) +
    geom_point(aes(colour = leverage)) 


# plot quantiles of residuals vs standard normal
data.frame(residuals = dat.scaled.residuals) %>%
    ggplot(aes(sample = residuals)) +
    stat_qq(distribution = qnorm) +
    geom_abline(colour = "#33FF00")



###############################################################################
## Explorations with numerical optimizations
##############################################################################

# Analytical
model.an <- solve(t(dat.trim.X) %*% dat.trim.X) %*% t(dat.trim.X) %*% dat.trim.Y

# Analytical scaled
model.an.scale <- solve(t(dat.scaled.X) %*% dat.scaled.X) %*% t(dat.scaled.X) %*% dat.scaled.Y

# gaussian noise mle function
cols <- colnames(dat.trim)
dat.scaled <- dat.trim %>% mutate_each_(funs(scale), vars = cols[which(cols != "t")])
dat.scaled.X <- model.matrix(lm(t ~ ., data = dat.scaled))
dat.scaled.Y <- dat.scaled[, "t"]

dat.trim.X <- model.matrix(lm(t ~ ., data = dat.trim))
dat.trim.Y <- dat.trim[, "t"]

theta.start <- setNames(rep(0, 31), colnames(dat.scaled.X))
theta.start.sigma <- setNames(c(rep(0, 31), 10), c(colnames(dat.scaled.X), "sigma"))

ll.1 <- function (Theta) {
    # not pure, relies on dat.scaled in scope!
    params <- head(Theta, -1)
    sigma <- tail(Theta, 1)
    y.hat <- dat.scaled.X %*% params
    resids <- y.hat - dat.scaled.Y
    -sum(log(dnorm(resids, 0, sigma)))
}

# Try and determin sigma with dat.trim
ll.2 <- function (Theta) {
    params <- head(Theta, -1)
    sigma <- tail(Theta, 1)
    y.hat <- dat.trim.X %*% params
    resids <- y.hat - dat.trim.Y
    -sum(log(dnorm(resids, 0, sigma)))
}

# Do not try and determine sigma, keep it at a fixed amount
ll.2.simple <- function (Theta) {
    y.hat <- dat.trim.X %*% Theta
    resids <- y.hat - dat.trim.Y
    -sum(log(dnorm(resids, 0, 2)))
}

# Minimize sum of squares numerically
ll.3 <- function (Theta) {
    y.hat <- dat.trim.X %*% Theta
    resids <- y.hat - dat.trim.Y
    sum(resids**2)
}

# optim(theta.start.sigma, ll.1, method = c("Nelder-Mead"), control = list(maxit = 50000))
