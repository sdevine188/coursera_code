x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~ x)
fit
summary(fit)
summary(fit)$coefficients

cars <- mtcars
attach(cars)
fit1 <- lm(cars$mpg ~ cars$wt)
fit2 <- lm(mpg ~ wt)
names(cars)
summary(fit1)
37.2851 -5.3445
3.046*2

mean(cars$wt)
mean(wt)
car_avg_wt <- data.frame(wt = 3.21725)
predict(fit2, car_avg_wt, interval = "predict")

confint(fit1, "cars$wt", level=0.95)
37.285 + (3.21725*-5.344)
20.09051 - (2*0.5591)

confint(fit1, cars, level=0.95)
confint(fit1)

?mtcars
y = 37.2851 + -5.3445x + e
y = 37.2851 + -5.3445*3


car_avg_wt2 <- data.frame(wt = 3)
predict(fit2, car_avg_wt2, interval = "predict")

fit1_resid <- sum(resid(fit1)^2)
fit3 <- lm(mpg ~)
summary(fit3)

sum(resid(fit1))

fit3 <- lm(mpg ~ wt, data = mtcars)
fit4 <- lm(mpg ~ 1, data = mtcars)
1 - summary(fit3)$r.squared

attach(faithful)     # attach the data frame 
eruption.lm = lm(eruptions ~ waiting)
newdata = data.frame(waiting=80)
predict(eruption.lm, newdata, interval="predict") 

