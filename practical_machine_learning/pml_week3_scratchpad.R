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
library(rpart)
library(rpart.plot)

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

## bagging lecture - boostrap aggregating
## bagging estimates will have lower variable but similiar bias than individual estimates

## ordering ozone (outcome variable) just to show clearly how bagging works
library(ElemStatLearn)
data(ozone, package = "ElemStatLearn")
ozone <- ozone[order(ozone$ozone), ]
head(ozone)

## create empty ll matrix to fill in for loop
ll <- matrix(NA, nrow = 10, ncol = 155)

## bootstrap resample with replacement of ozone rows, in ascending order,
## then fit a loess model of temperature on each ozone sample dataset
## then predict that model on newdata which is just a column 1:155 representing range of ozone values
for(i in 1:10){
        ss <- sample(1:dim(ozone)[1], replace = T)
        ozone0 <- ozone[ss, ]
        ozone0 <- ozone0[order(ozone$ozone), ]
        loess0 <- loess(temperature ~ ozone, data = ozone0, span = .2)
        ll[i, ] <- predict(loess0, newdata = data.frame(ozone = 1:155))
}

## can do bagging automatically in "train" function of caret package
## specify method = bagEarth, or treebag, or bagFDA

## now plot ozone and temperature
## then add in grey loess line for each individual model fit, independent 1:155 ozone value on x axis, and predicted temp on y axis
## then add in red loess line that is the mean of the predicted temperature for each 1:155 ozone value
plot(ozone$ozone, ozone$temperature, pch = 19, cex = .5)
for(i in 1:10){
        lines(1:155, ll[i, ], col = "grey", lwd = 2)
        lines(1:155, apply(ll, 2, mean), col = "red", lwd = 2)
}


## random forests lecture
## random forests and boosting are two most accurate algorithms.  downside of rf is that it can be tough to interpret
## random forests is extension of bagging - you also bootstrap resample your training data, predict the model, then avg the models
## but the difference is that on each resampling of the training data, you sample only a random subset of the variables
## this produces many random regression trees, and the final model is an avg of these many tree model predictions
data(iris)
library(ggplot2)
library(caret)
inTrain <- createDataPartition(y = iris$Species, p = .7, list = FALSE)
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]

mod_fit <- train(Species ~ ., data = training, method = "rf", prox = TRUE)
# another option for rf model that uses k-fold cross-validation instead of default bootstrap resampling
# also limits k to 3, instead of default 10
# this can speed up rf model processing time (I think)
# for more, see http://topepo.github.io/caret/training.html#control
# mod_fit <- train(Species ~ ., data = training, method = "rf", trControl = trainControl(method = "cv"), number = 3)
mod_fit
## in mod_fit output, the "mtry" column of the tuning parameters is the index number of the random tree it built (i think)
## can look at specific tree, for instance the second tree (mtry = 2)
getTree(mod_fit$finalModel, k = 2) 

## can plot the "class centers" of outcome variable on an x-y plot of two predictor variables
irisP <- classCenter(training[ , c(3, 4)], training$Species, mod_fit$finalModel$prox)
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, color = Species, data = training)
p + geom_point(aes(x = Petal.Width, y = Petal.Length, color = Species), size = 5, shape = 4, data = irisP)

## can predict on the testing set
pred <- predict(mod_fit, testing)
testing$pred_right <- pred == testing$Species
table(pred, testing$Species)

## can see which predictions you missed
qplot(Petal.Width, Petal.Length, color = pred_right, data = testing)
 
## boosting lecture
## idea is to start with any old weak classifiers, calculate accuracy, then upweighting the incorrectly classified/predicted values
## then re-iterate using another classifier (example is horizontal line, then vertical line, etc)
## the algorithm then aims to correctly classify the upweighted points
## when finished, algo adds together the weighted accuracy of all models, which is then the final model
## several algorithms for boosting: gbm, mboost, ada, gamBoost
## in this example, using gbm - boosting with trees

library(ISLR)
data(Wage)
library(gbm)
wage <- subset(Wage, select = -c(logwage))
InTrain <- createDataPartition(wage$wage, p = .7, list = FALSE)
training <- wage[inTrain, ]
testing <- wage[-inTrain, ]

## verbose = false criteria just prevents the model from including a lot of under the hood output
mod_fit <- train(wage ~ ., method = "gbm", data = training, verbose = FALSE)
mod_fit

qplot(predict(mod_fit, testing), wage, data = testing)

## model based prediction
## examples focus on linear discriminant analysis and naive bayes 
## lda - draw lines through dataspace where probability becomes more likely of other being class
## nb - 
data(iris)
table(iris$Species)

inTrain <- createDataPartition(iris$Species, p = .7, list = FALSE)
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]

modlda <- train(Species ~ ., method = "lda", data = training)
modnb <- train(Species ~., method = "nb", data = training)
plda <- predict(modlda, testing)
pnb <- predict(modnb, testing)
table(plda, pnb)

equal_pred <- plda == pnb
qplot(Petal.Width, Sepal.Width, color = equal_pred, data = testing)

## quiz
## Q1 
library(AppliedPredictiveModeling)
data(segmentationOriginal)
data <- segmentationOriginal
head(data)
names(data)

inTrain <- createDataPartition(data$Class, p = .7, list = FALSE)
training <- data[inTrain, ]
testing <- data[-inTrain, ]
set.seed(125)

mod_fit_rpart <- train(Class ~ ., method = "rpart", data = training)
mod_fit_rpart$finalModel

plot(mod_fit_rpart$finalModel, uniform = TRUE, main = "Classification Tree")
text(mod_fit_rpart$finalModel, use.n = TRUE, all = TRUE, cex = .8)
fancyRpartPlot(mod_fit_rpart$finalModel)

pred_rpart <- predict(mod_fit_rpart, newdata = testing)
testing$pred <- pred_rpart
testing$pred_right <- testing$pred == testing$Class
table(testing$pred_right)

## Q2
install.packages("pgmm")
library(pgmm)
data(olive)
olive <- olive[,-1]

mod_fit_rpart <- train(Area ~ ., method = "rpart", data = olive)
mod_fit_rpart$finalModel

fancyRpartPlot(mod_fit_rpart$finalModel)

newdata = as.data.frame(t(colMeans(olive)))
pred <- predict(mod_fit_rpart, newdata)

## Q3
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
mod_logit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, method = "glm", family = "binomial", data = trainSA)
mod_logit$finalModel

pred <- predict(mod_logit, testSA)
pred2 <- predict(mod_logit, trainSA)

missClass <- function(values,prediction){
        sum(((prediction > 0.5)*1) != values)/length(values)
}

missClass(testSA$chd, pred)
missClass(trainSA$chd, pred2)


## Q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
data <- vowel.test
data$y <- as.factor(data$y)
str(data)

inTrain <- createDataPartition(data$y, p = .7, list = FALSE)
training <- data[inTrain, ]
testing <- data[-inTrain, ]

set.seed(33833)
mod_fit_rf <- train(y ~ ., method = "rf", data = training, prox = TRUE)
mod_fit_rf$finalModel

pred <- predict(mod_fit_rf, newdata = testing)
testing$pred <- pred
testing$pred_right <- testing$pred == testing$y
table(testing$pred_right)

varImp(mod_fit_rf)
varImp(mod_fit_rf, scale = FALSE)


