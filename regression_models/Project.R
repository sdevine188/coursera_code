
library(ggplot2)
library(stargazer)

data <- mtcars

descrip_stats <- stargazer(data, type = "text", title="Descriptive statistics", digits=1, out="table1.htm",
                           covariate.labels=c("Miles/(US)gallon","No. of cylinders","Displacement (cu.in.)",
                                              "Gross horsepower","Rear axle ratio","Weight (lb/1000)",
                                              "1/4 mile time","V/S","Transmission (0=auto, 1=manual)",
                                              "Number of forward gears","Number of carburetors"))

am_scatter <- ggplot(data, aes(x = am, y = mpg, group = 1)) + geom_point(shape = 1) + 
        geom_smooth(method = lm) + ggtitle("MPG and Transmission") + xlab("Transmission") +
        ylab("MPG") 

pairs(data)

fit1 <- lm(formula = mpg ~ am, data = data)
summary(fit1)

fit3 <- lm(formula = mpg ~ ., data = data)
summary(fit3)

fit2 <- lm(formula = mpg ~ am + hp + wt, data = data)
summary(fit2)
par(mfrow=c(2,2))
plot(fit2)

reg_table <- stargazer(fit1, fit2, fit3, type="html",
          dep.var.labels="Miles/(US) gallon",
          covariate.labels=c("Type of transmission (manual=1)", "Gross horsepower", "Weight", 
          "No. of cylinders","Displacement (cu.in.)",
          "Rear axle ratio","1/4 mile time","V/S",
          "Number of forward gears", "Number of carburetors", out="models.htm"))
reg_table