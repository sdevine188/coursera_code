?influence.measures
mtcars
data <- mtcars
names(data)
str(data$am)
data$am <- factor(data$am)

library(ggplot2)
am_scatter <- ggplot(data, aes(x = am, y = mpg, group = 1)) + geom_point(shape = 1) + 
        geom_smooth(method = lm) + ggtitle("MPG and Transmission") + xlab("Transmission") +
        ylab("MPG") 

weight_scatter <- ggplot(data, aes(x = wt, y = mpg, color = am)) + geom_point(shape = 1) + 
        geom_smooth(method = lm) + ggtitle("MPG and Weight, by Transmission") + xlab("Weight") +
        ylab("MPG") + scale_color_discrete(name = "Transmission", labels = c("Automatic", "Manual"))

cyl_scatter <- ggplot(data, aes(x = cyl, y = mpg, color = am)) + geom_point(shape = 1) + 
        geom_smooth(method = lm) + ggtitle("MPG and Cylinders, by Transmission") + xlab("Cylinders") +
        ylab("MPG") + scale_color_discrete(name = "Transmission", labels = c("Automatic", "Manual"))

disp_scatter <- ggplot(data, aes(x = disp, y = mpg, color = am)) + geom_point(shape = 1) + 
        geom_smooth(method = lm) + ggtitle("MPG and Displacement, by Transmission") + xlab("Displacement") +
        ylab("MPG") + scale_color_discrete(name = "Transmission", labels = c("Automatic", "Manual"))

drat_scatter <- ggplot(data, aes(x = drat, y = mpg, color = am)) + geom_point(shape = 1) + 
        geom_smooth(method = lm) + ggtitle("MPG and Rear Axel Ratio, by Transmission") + xlab("Rear Axel Ratio") +
        ylab("MPG") + scale_color_discrete(name = "Transmission", labels = c("Automatic", "Manual"))

qsec_scatter <- ggplot(data, aes(x = qsec, y = mpg, color = am)) + geom_point(shape = 1) + 
        geom_smooth(method = lm) + ggtitle("MPG and 1/4 Mile Time, by Transmission") + xlab("1/4 Mile Time") +
        ylab("MPG") + scale_color_discrete(name = "Transmission", labels = c("Automatic", "Manual"))

vs_scatter <- ggplot(data, aes(x = vs, y = mpg, color = am)) + geom_point(shape = 1) + 
        geom_smooth(method = lm) + ggtitle("MPG and V/S, by Transmission") + xlab("V/S") +
        ylab("MPG") + scale_color_discrete(name = "Transmission", labels = c("Automatic", "Manual"))

gear_scatter <- ggplot(data, aes(x = gear, y = mpg, color = am)) + geom_point(shape = 1) + 
        geom_smooth(method = lm) + ggtitle("MPG and Gear, by Transmission") + xlab("Gears") +
        ylab("MPG") + scale_color_discrete(name = "Transmission", labels = c("Automatic", "Manual"))

carb_scatter <- ggplot(data, aes(x = carb, y = mpg, color = am)) + geom_point(shape = 1) + 
        geom_smooth(method = lm) + ggtitle("MPG and Carburetor, by Transmission") + xlab("Carburetor") +
        ylab("MPG") + scale_color_discrete(name = "Transmission", labels = c("Automatic", "Manual"))

hp_scatter <- ggplot(data, aes(x = hp, y = mpg, color = am)) + geom_point(shape = 1) + 
        geom_smooth(method = lm) + ggtitle("MPG and Horsepower, by Transmission") + xlab("Horsepower") +
        ylab("MPG") + scale_color_discrete(name = "Transmission", labels = c("Automatic", "Manual"))

fit1 <- lm(formula = mpg ~ am, data = data)
summary(fit1)

pairs(data)

fit2 <- lm(formula = mpg ~ am + hp + wt, data = data)
summary(fit2)

fit3 <- lm(formula = mpg ~ ., data = data)
summary(fit3)

fit4 <- lm(formula = mpg ~ am + hp + wt + gear*am, data = data)
summary(fit4)

install.packages("stargazer")
library(stargazer)

descrip_stats <- stargazer(data, type = "html", title="Descriptive statistics", digits=1, out="table1.htm",
          covariate.labels=c("Miles/(US)gallon","No. of cylinders","Displacement (cu.in.)",
                             "Gross horsepower","Rear axle ratio","Weight (lb/1000)",
                             "1/4 mile time","V/S","Transmission (0=auto, 1=manual)",
                             "Number of forward gears","Number of carburetors"))
descrip_stats

reg_test <- stargazer(fit1, fit2, fit3, type="text",
          dep.var.labels="Miles/(US) gallon",
          out="models.htm")

reg_table <- stargazer(fit1, fit2, fit3, type="text",
                       dep.var.labels="Miles/(US) gallon",
                       covariate.labels=c("Type of transmission (manual=1)", "Gross horsepower", "Weight (lb/1000", 
                                          "No. of cylinders","Displacement (cu.in.)",
                                          "Rear axle ratio","1/4 mile time","V/S",
                                          "Number of forward gears", "Number of carburetors"))
reg_table

reg_table2 <- stargazer(fit1, fit2, fit3, type="html",
                       dep.var.labels="Miles/(US) gallon", 
                       out="models.htm")

reg_table3 <- stargazer(fit1, fit2, fit3, type="html",
                        dep.var.labels="Miles/(US) gallon", covariate.labels=c("No. of cylinders", "Displacement (cu.in.)",
                                        "Type of transmission (manual=1)", "Number of forward gears", "Number of carburetors",
                                        "Gross horsepower", "Rear axle ratio", "Weight (lb/1000)", "1/4 mile time", "V/S"), 
                                        out="models.htm")

reg_table4 <- stargazer(fit 1, fit2, fit3, type="text",
                        dep.var.labels="Miles/(US) gallon", covariate.labels=c("No. of cylinders", "Displacement (cu.in.)",
                        "Gross horsepower", "Rear axle ratio", "Weight (lb/1000", "1/4 mile time", "V/S", 
                        "Type of transmission (manual=1)", "Number of forward gears", "Number of carburetors"), 
                        out="models.txt")

## work around, change dataframe column names to full names, then remove covariate label option

data$cyl <- factor(data$cyl)
fit4 <- lm(formula = mpg ~ cyl + wt, data = data)
summary(fit4)

fit5 <- lm(formula = mpg ~ cyl, data = data)
summary(fit5)

install.packages("qualityTools")
library(qualityTools)
plot(fit2)[1]
outlierTest(fit2)
qqPlot(fit2, main = "QQ Plot")

cutoff <- 4/((nrow(mtcars)-length(fit2$coefficients)-2)) 
plot(fit2, which=4, cook.levels=cutoff)

install.packages("car")
library(car)
influencePlot(fit2, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance")

vif(fit2)

fit2$coefficients
fit2$residuals
qqplot(fit2$residuals)

plot(fit2)

plot(fit2, which = 5)
