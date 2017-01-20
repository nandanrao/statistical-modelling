library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)

dat.1.raw <- read.csv("data/exam16_data1.txt", sep=" ")
dat.2.raw <- read.csv("data/exam16_data2.txt", sep=" ")
dat.3.raw <- read.csv("data/exam16_data3.txt", sep=" ")
dat.4.raw <- read.csv("data/exam16_data4.txt", sep=" ", stringsasfactors=FALSE)
dat.5.raw <- read.csv("data/exam16_data5.txt", sep=" ")

##########################################################
# 1. FWER AND FDR
bhfdr <- function(vec, alpha = .05) {
    sorted <- sort(vec)
    m <- length(vec)
    accepted = c()
    for (i in 1:m){
        if (vec[i] > (i/m) * alpha) {
            break;
        }
        accepted[i] <- vec[i]
    }
    accepted
}

bhfdr(dat.1.raw$pvalue)

#################################################################
# 2.1 --> average two observations, garauntees sd of exactly our target.
# On the other hand, just taking y_1 will be an unbiased estimator, with smaller variance...
a <- rnorm(1000000, mean = 0, sd = 2)
b <- rnorm(1000000, mean = 0, sd = 3)
c <- (a+b)/2
pdf <- density(c)
cdf <- approxfun(pdf$x, pdf$y, yleft = 0, yright=0)
target.sd <- sqrt((2^2+3^2)/4)
integrate(cdf, -target.sd, target.sd)



#3.1

colnames(dat.3.raw) <- c("price", "m", "bed", "floor", "collab")
dat.3 <- data.frame(dat.3.raw)



dat.3 %>%
    do(fit = lm(.$price ~ .$m)) %>%
    augment(fit) %>%
    rename(price = ..price, m = ..m) %>%
    ungroup() %>%
    mutate(hat.range = cut(.hat, 4)) %>%
    ggplot(aes(y = price, x = m)) +
    geom_point(aes(colour = hat.range)) +
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) + 
    scale_colour_discrete(name = "Leverage")

dat.3 %>%
    filter(m < 400) %>%
    ggplot(aes(y = price, x = m, colour = factor(collab))) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x)


# remove outlier
dat.3 %>%
    filter(m < 400) %>%
    ggplot(aes(y = price, x = m, colour = factor(collab))) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x)


base.model <- lm(price ~ ., data = dat.3)
summary(base.model)



cheating.effect <- coef(base.model)["collab"]
cheating.int <- 9086.34*1.96
cheating.lower <- cheating.effect - cheating.int
cheating.upper <- cheating.effect + cheating.int
cheating.lower
cheating.upper


base.model.small <- lm(price ~ ., data = dat.3 %>% filter(m < 400))
summary(base.model.small)

dat.3.0 <- filter(dat.3, collab == 0)
dat.3.1 <- filter(dat.3, collab == 1)


summary(lm(price ~ . - collab, data = dat.3.0))
summary(lm(price ~ . - collab, data = dat.3.1))


find.cheaters <- function(n) {
    
    # wish I had the internet so I could google how to get std error
    # out of this stupid R model object instead hardcoding them...
    sim.cheating <- rnorm(n, mean = 183281, sd = 17706)
    sim.straight <- rnorm(n, mean = 127612, sd = 8248)
    diff <- sim.cheating - sim.straight
    pdf <- density(diff)
    mode <- pdf$x[which.max(pdf$y)]
    cdf <- approxfun(pdf$x, pdf$y, yleft = 0, yright=0)
    interval <- quantile(diff, probs = c(.025, .975))
    list(mode = mode, cdf = cdf, pdf = pdf, interval = interval)
}




summary(lm(price ~ m, data = dat.3.0))
summary(lm(price ~ m, data = dat.3.1))

dat.3.filtered <- filter(dat.3, m < 400)

summary(lm(price ~ m, data = dat.3.filtered %>% filter(collab == 0)))



summary(lm(price ~ m, data = dat.3.filtered %>% filter(collab == 1)))

summary(lm(price ~ ., data = dat.3.filtered))



summary(lm(price ~ m + collab, data = dat.3.filtered))

######################################################
# 4 - GENE DATA

library(lars)

y <- dat.4.raw[, 1]
X <- dat.4.raw[, -2]
gene.lasso.model <- lars(as.matrix(X), y, use.Gram=FALSE, normalize=FALSE)
plot(gene.lasso.model)



# R2 as we increase through the steps --> look for elbow!
data.frame(step = 0:60, R2 = gene.lasso.model$R2) %>% ggplot(aes(x = step, y = R2)) + geom_point() + geom_line()


# we can see that no element was dropped in the first couple dozen steps
# so we only have additions, which makes it easy to filter: 
flipped.features <- data.frame(t(dat.4.raw[, -2]), stringsAsFactors=FALSE)
filtered <- flipped.features %>%
    mutate(
        entry = gene.lasso.model$entry,
        gene = rownames(.)
    ) %>%
    filter(entry <= 3) %>%
    filter(entry > 0) %>%
    mutate(entry = NULL)

# make crazy rerighted dataframe with filtered cols
genes <- filtered$gene
rerighted <- filtered %>%
    mutate(gene = NULL) %>%
    t() %>%
    data.frame(stringsAsFactors=FALSE)

colnames(rerighted) <- genes
rerighted <- rerighted[-50, ]
rownames(rerighted) <- NULL

# regress!
sparse.genes <- rerighted %>% mutate(y = y)
lm.model <- glm(y ~ ., family=binomial(link = "logit"), data = sparse.genes)
summary(lm.model)

leave.one.out <- function(dat) {
    rows <- dim(dat)[1]
    success <- c()
    for (i in 1:rows) {
        model <- glm(y ~ ., family=binomial(link = "logit"), data = dat[-i, ])
        if (predict(lm.model, newdata = sparse.genes[i, ]) > 0) {
            prediction <- 1
        } else {
            prediction <- 0
        } if (prediction == dat[i, "y"]){
            success[i] <- TRUE
        }else {
            success[i] <- FALSE
        }
    }
    success
}




small.lasso.model <- lars(y = sparse.genes$y, x = sparse.genes %>% mutate(y = NULL) %>% as.matrix(), use.Gram = FALSE, normalize=FALSE)

# FDR!!! 






##############################################################
# 5 - airline pricing
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

colnames(dat.5.raw) <- c("price", "available", "purchased")
dat.5 <- dat.5.raw

airlines <- list(
    N = dim(dat.5)[1],
    price = dat.5$price,
    available = dat.5$available,
    purchased = dat.5$purchased
)

model_airlines <- stan("airlines.stan", data = airlines)

model_airlines


#simulate and plot
data.frame(x = rpois(10000, 14*-8.77 + 139.20)) %>% ggplot(aes(x)) + geom_histogram(binwidth=1)

# plot our data at this price point
dat.5 %>% filter(price == 14) %>% ggplot(aes(purchased)) + geom_histogram(binwidth=1)
