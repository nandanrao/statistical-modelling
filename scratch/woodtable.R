wood.raw <- read.csv("woodtable2.csv")

remove.nas <- function (x) !all(is.na(x))
                                
wood.clean <- subset(Filter(remove.nas, wood.raw), select = -c(X))

wood.data <- subset(wood.clean, select = -c(LS, LMS))

# remove artifically added "dirty" rows
wood.data.og <- wood.data[-c(4, 6, 8, 19), ]
wood.data.dirty <- wood.data
wood.data.lowlev.1 <- wood.data[-c(8), ]
wood.data.lowlev.2 <- wood.data[-c(4, 6, 8), ]
wood.data.lowlev.alt <- wood.data[-c(19), ]

lm(y. ~ ., wood.data.og)
lm(y. ~ ., wood.data.dirty)
lm(y. ~ ., wood.data.lowlev.1)
lm(y. ~ ., wood.data.lowlev.2)
lm(y. ~ ., wood.data.lowlev.alt)

# Why are these not correlated??? I understand that the basic diagnotistics is looking
# for points with high leverage AND high residuals (outliers), but why are they not
# correlated? How is leverage really related to the residual? And how do they both
# relate to "variance"??
layout(1:2)
plot(abs(influence(lm(y. ~ ., wood.data.dirty))$hat))
plot(abs(influence(lm(y. ~ ., wood.data.dirty))$wt.res))
plot(resid(lm(y. ~ ., wood.data.dirty)))

# calc hat
X <- data.matrix(subset(wood.data.dirty, select = -c(y.)))
hat <- X %*% solve(t(X) %*% X) %*% t(X)
head(hat)
influence(lm(y. ~ ., wood.data.dirty))$hat
