library(ggplot)
library(dplyr)

poly <- function (x, M) {
    sapply(0:M, function (n) x**n)
}

g.mus <- function (M) {
    sapply(0:(M-1), function (n) n/(M-1))
}

g.exp <- function (x, mu) {
    exp(-(x-mu)**2/.2)
}

gauss <- function (x, M) {
    terms <- sapply(g.mus(M), function (mu) g.exp(x, mu))
    c(1, terms)
}

phix <- function (x, M, type) {
    if (type == "poly") return(poly(x, M))
    if (type == "gauss") return(gauss(x, M))
}

makephi <- function (X, fn, M, type) {
    t(sapply(X, function (p) fn(p, M, type)))
}

post.params <- function (t, X, M, type, fn, delta, q) {

    # Create precision matrix from hyperparameters
    D <- delta/q * diag(M+1)

    # Transform inputs into features
    phi <- makephi(X, fn, M, type)

    # Plugin to analytical form!
    Q <- q * (D + t(phi) %*% phi)
    w <- solve(D + t(phi) %*% phi) %*% t(phi) %*% t
    
    list(w=w,Q=Q)
}

bayes.estimator <- function (w, Q, type) {
    M <- length(w) - 1
    
    function (X) {
        makephi(X, phix, M, type) %*% w
    }
}

dat.raw <- read.csv("curve_data.txt", sep=" ")
params.poly <- post.params(as.vector(dat.raw$t), dat.raw$x, 9, "poly", phix, 2, 100)
params.gauss <- post.params(as.vector(dat.raw$t), dat.raw$x, 9, "gauss", phix, 2, 100)

fn.poly <- bayes.estimator(params.poly$w, params.poly$Q, "poly")
fn.gauss <- bayes.estimator(params.gauss$w, params.gauss$Q, "gauss")

dat.raw %>%
    ggplot(aes(x, t)) +
    geom_point()


dat.raw %>%
    ggplot(aes(x, t)) +
    geom_point() +
    stat_function(fun = fn.poly, aes(colour="poly")) +
    stat_function(fun = fn.gauss, aes(colour="gauss")) 
