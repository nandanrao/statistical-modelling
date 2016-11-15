library(distr)
library(ggplot2)
library(dplyr)

r.laplace <- function (n, mean = 0, v = 1) {
    r(DExp(1/v))(n) + mean
}


create.data <- function (n, sd, dist = rnorm) {
    a <- data.frame(x = rnorm(n,5,sd), y=dist(n,5,sd), class='a')
    b <- data.frame(x = rnorm(n,-5,sd), y=dist(n,-5,sd), class='b')
    rbind(a,b)
}




plot.classifier <- function (data, mdl) {

    # TODO: does this generalize?????????????????
    data.wh <- cbind(data, hat = influence(mdl)$hat)
    slope <- coef(mdl)[2]/(-coef(mdl)[3])
    intercept <- coef(mdl)[1]/(-coef(mdl)[3])

    data.wh  %>%
        mutate(hat.range = cut(data.wh$hat, 7)) %>%
        ggplot(aes(x, y)) +
        geom_point(aes(color = hat.range, shape = class)) +
        geom_abline(aes(slope= slope, intercept = intercept)) +
        geom_abline(aes(slope= -1, intercept = 0), linetype='3313', colour='grey') +
        xlim(-20, 20) + ylim(-20,20)
}


set.seed(1)
data.la <- create.data(250, 3, r.laplace)
mdl.la <- glm(class ~.,family=binomial(link=make.link("logit")),data=data.la)
plot.classifier(data.la, mdl.la)


data.la.wh <- cbind(data.la, hat = influence(mdl.la)$hat)
data.la.wh  %>%
    mutate(hat.range = cut(data.la.wh$hat, 5)) %>%
    ggplot(aes(x,y)) +
    geom_point(aes(color = hat.range))



data <- create.data(200, 5)
mdl <- glm(class ~.,family=binomial(link='logit'),data=data)
plot.classifier(data, mdl)


data.wh <- cbind(data, hat = influence(mdl)$hat)
data.wh  %>%
    mutate(hat.range = cut(data.wh$hat, 6)) %>%
    ggplot(aes(x,y)) +
    geom_point(aes(color = hat.range))
