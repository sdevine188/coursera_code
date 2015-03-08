data <- mtcars
names(data)

fit1 <- lm(mpg ~ am, data = data)
summary(fit1)

fit2 <- lm(mpg ~ am + wt, data = data)
summary(fit2)

fit3 <- lm(mpg ~ am + wt + qsec, data = data)
summary(fit3)

fit4 <- lm(mpg ~ am + wt + qsec + drat, data = data)
summary(fit4)

anova(fit1, fit2, fit3, fit4)


library(swirl)
install.packages("swirl")
swirl()
