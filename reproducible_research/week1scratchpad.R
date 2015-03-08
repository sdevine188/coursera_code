install.packages("kernlab")
library(kernlab)
data(spam)
?data
str(spam[ , 1:5])
names(spam)
set.seed(3435)
train.indicator <- rbinom(4601, size = 1, prob = .5)
x <- rbinom(1, size = 10, prob = .05)
?rbinom
x

table(train.indicator)
train.spam <- spam[train.indicator == 1, ]
test.spam <- spam[train.indicator == 0, ]
names(train.spam)
table(train.spam$type)
plot(train.spam$type, train.spam$capitalAve)
?plot
table(train.spam$capitalAve)

costFunction <- function(x, y) sum(x != (y > .5))
cvError <- rep(NA, 55)
library(boot)
for (i in 1:55)
        lmFormula <- reformulate(names(train.spam)[i], 
                                 response <- "numType")
        glmFit <- 
