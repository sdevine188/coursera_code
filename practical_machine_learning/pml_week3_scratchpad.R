## quiz from regression models

library(MASS)
data(shuttle)
data <- shuttle
head(data)
str(data)

auto1 <- which(data$use == "auto")
data$auto <- 0
data$auto[auto1] <- 1
length(which(data$use == "auto"))
length(which(data$auto == 1))


data$head <- 0
head <- which(data$wind == "head")
data$head[head] <- 1
length(which(data$wind == "head"))
length(which(data$head == 1))

fit1 <- glm(auto ~ wind, data = data, family = "binomial")
fit1

fit2 <- glm(auto ~ head, data = data, family = "binomial")
fit2

exp(1.5)
log(4.48)
log_odds <- fit2$coefficients[2] + fit2$coefficients[1]
log_odds
exp(log_odds)

## not getting answer right, so trying this???
log_odds_tail <- fit2$coefficients[1]
log_odds_headtail <- log_odds/log_odds_tail
exp(log_odds_headtail)

data$mag <- 0
fit3 <- glm(auto ~ head + magn, data = data, family = "binomial")
fit3
exp(fit3$coefficients[2])
unique(data$magn)

str(data)
data$not_auto <- 0
not_auto <- which(data$use == "noauto")
data$not_auto[not_auto] <- 1
length(which(data$use == "noauto"))
length(which(data$not_auto == 1))

fit4 <- glm(not_auto ~ head, data = data, family = "binomial")
fit4


data(InsectSprays)
is <- InsectSprays
head(is)
str(is)

## need to relevel spray so that sprayB is the reference cateogry, not the default sprayA 
fit5 <- glm(count ~ spray, data = is, family = "poisson")
fit5

fit6 <- glm(count ~ relevel(spray, ref = "B"), data = is, family = "poisson")
fit6
exp(fit6$coefficients[2])


## pml week 3
library(ggplot2)
library(caret)

data <- iris
head(iris)
names(iris)
table(iris$Species)

## seperate into training and test
in_train <- createDataPartition(data$Species, p = .7, list = FALSE)
training <- data[in_train, ]
testing <- data[-in_train, ]
str(training)
str(testing)

ggplot(data = training, aes(x = Petal.Width, y = Sepal.Width, color = Species)) + geom_point()

mod_fit1 <- train(Species ~ ., method = "rpart", data = training)
mod_fit1
mod_fit1$finalModel

plot(mod_fit1$finalModel, uniform = TRUE, main = "Classification Tree")
text(mod_fit1$finalModel, use.n = TRUE, all = TRUE, cex = .8)

install.packages("rattle")
library(rattle)
fancyRpartPlot(mod_fit1$finalModel)

predict(mod_fit1, newdata = testing)
