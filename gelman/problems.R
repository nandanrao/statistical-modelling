
mode <- density(run10$age, na.rm = TRUE)$x[which.max(density(run10$age, na.rm=TRUE)$y)]

run10 %>%
    mutate(
        m = mean(age, na.rm = TRUE),
        sd = sd(age)
    ) %>%
    ggplot(aes(age)) +
    geom_density() +
    stat_function(fun = dnorm, colour = "red", args = list(mean = mean(run10$age, na.rm = TRUE), sd = sd(run10$age, na.rm = TRUE)))

ggplot(data.frame(x = c(0,1)),aes(x)) +
    stat_function(fun = function (x) x*log(x))

ggplot(data.frame(x = c(-1,1)),aes(x)) +
    stat_function(fun = dnorm, args = list(sd = .25))

ggplot(data.frame(x = c(0,5)),aes(x)) +
    stat_function(fun = dgamma, args = list(shape = 1)) +
    stat_function(fun = dgamma, args = list(shape = 2)) +
    stat_function(fun = dgamma, args = list(shape = 1, scale = 2))


ggplot(data.frame(x = c(-1,1)),aes(x)) +
    stat_function(fun = dt, args = list(df = 1))

ggplot(data.frame(x = c(0,1)),aes(x)) +
    stat_function(fun = function (x) -x*log(x))



###############
# 3.2 -- estimate probability of positive shift to bush post debate

library(dplyr)
library(ggplot2)
library(gtools)


# Testing example
data.frame(x = rdirichlet(10000, c(728, 584, 138))) %>%
    mutate(diff = x.1 - x.2) %>%
    ggplot(aes(diff)) +
    geom_histogram()

data.frame(x = rbinom(10000, size = 728+584, prob = .523)) %>%
    mutate(total = 728 + 524, y = total - x) %>%
    mutate(diff = x/total - y/total) %>%
    ggplot(aes(diff)) +
    geom_histogram()


# problem
draws <- data.frame(pre = rdirichlet(10000, c(294, 307, 38)), post = rdirichlet(10000, c(288, 332, 19)))
diff <- draws$post.1 - draws$pre.1


draws %>%
    mutate(diff = post.1 - pre.1) %>%
    ggplot(aes(diff)) +
    geom_histogram(binwidth = .005)


# CHANCE OF POSITIVE SHIFT!
pdf <- density(diff)
mode <- pdf$x[which.max(pdf$y)]
cdf <- approxfun(pdf$x, pdf$y, yleft = 0, yright=0)
integrate(cdf, 0, Inf)



###########################################
# Chickens

sample.means <- function (n, mu, sigma) {
    alpha <- 1
    beta <- 1
    k <- 1
    precisions <- rgamma(10000, shape = alpha + n/2, rate = beta + (sigma^2)/2)
    means <- sapply(precisions, function (p) rnorm(1, mean = 1.013, sd = (p^-2)/k))
}

sample.means.an <- function (n, mu, sigma, nu, k = 1) {
    X <- rt(1000, df = nu)
    X*sigma/k + mu
}


plot.post <- function (means, binwidth = .001) {
    data.frame(x = means) %>%
        ggplot(aes(x))+
        geom_histogram(binwidth = binwidth)    
}


means.c <- sample.means(32, 1.013, 0.24)
means.t <- sample.means(36, 1.173, 0.20)


means.c.an <- sample.means.an(32, 1.013, 0.24, nu = 10, k = 1)
means.t.an <- sample.means.an(36, 1.173, 0.20, nu = 10, k = 1)

plot.post(means.c.an - means.t.an, binwidth=.01)
quantile(means.c.an - means.t.an, probs=c(.025, .975))

plot.post(means.c)

plot.post(means.t)

plot.post(means.c - means.t)
quantile(means.c - means.t, probs = c(.025, .975))
