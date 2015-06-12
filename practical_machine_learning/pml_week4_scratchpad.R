## pml week 4

## regularized regression
## if you have two predictors that are highly correlated, you can drop one, but multiple its coefficient times the colinear coeff.
## for instance, if in y = b0 + b1X + b2Y b1 x and y are collinear, then y = b0 + b1 * b2Y can be better
library(ElemStatLearn)
str(prostate)


## model stacking, eg. a way to ensembled/average different model classifiers together
library(ISLR)
library(ggplot2)
library(caret)
data(Wage)

## create validation set, and then the usual training and testing
in_build <- createDataPartition(y = Wage$wage, p = .7, list = FALSE)
validation <- Wage[-in_build, ]
build_data <- Wage[in_build, ]

in_train <- createDataPartition(y = build_data$wage, p = .7, list = FALSE)
training <- build_data[in_train, ]
testing <- build_data[-in_train, ]

str(validation)
str(training)
str(testing)

## fit two models, glm and rf
mod1 <- train(wage ~ ., method = "glm", data = training)
mod2 <- train(wage ~ ., method = "rf", data = training, trControl = trainControl(method = "cv"), number = 3)
# just to test rf function without the trControl argument, which uses cross-validation instead of default bootstrap
# and also uses k-fold number = 3, instead of default k/re-sample number = 10
# I'm assuming that rf model without number = 3 will take longer to run, but will be more accurate.  We will see
# update: definitely takes longer to run, about twice? as long
mod3 <- train(wage ~ ., method = "rf", data = training)

## predict each model on the testing data, see that they are in close alignment, but not perfect
pred1 <- predict(mod1, testing)
pred2 <- predict(mod2, testing)
# for fun to see mod3
pred3 <- predict(mod3, testing)

qplot(pred1, pred2)
# to see similarity of mod2 and mod3
qplot(pred2, pred3)

## then fit a combined model using the predictions from mod1 and mo2 to predict wage
pred_df <- data.frame(pred1, pred2, wage = testing$wage)
comb_modfit <- train(wage ~ ., method = "gam", data = pred_df)
comb_pred <- predict(comb_modfit, pred_df)

# evaluate how individual and combined model fit testing$wage 
# not great comparison though, since we trained comb_modfit on testing$wage data - will need to see fit on validation set to be sure
sqrt(sum((pred1 - testing$wage)^2))
sqrt(sum((pred2 - testing$wage)^2))
# mod3 with 10 bootstrap reamplings had only very, very slightly less error than the cv with number = 3
sqrt(sum((pred3 - testing$wage)^2))
sqrt(sum((comb_pred - testing$wage)^2))

# predict all three models on validation set
pred1v <- predict(mod1, validation)
pred2v <- predict(mod2, validation)
# mod 3
pred3v <- predict(mod3, validation)
predv_df <- data.frame(pred1 = pred1v, pred2 = pred2v)
comb_predv <- predict(comb_modfit, predv_df)

## to evaluate models
sqrt(sum((pred1v - validation$wage)^2))
sqrt(sum((pred2v - validation$wage)^2))
# mod 3, again only very slight decrease in error over cv k = 3 rf model
sqrt(sum((pred3v - validation$wage)^2))
sqrt(sum((comb_predv - validation$wage)^2))

## forecasting
install.packages("quantmod")
library(quantmod)
from.dat <- as.Date("01/01/08", format = "%m/%d/%y")
to.dat <- as.Date("12/31/13", format = "%m/%d/%y")
getSymbols("GOOG", src = "google", from = from.dat, to = to.dat)
head(GOOG)

mGoog <-to.monthly(GOOG) 
googOpen <- Op(mGoog)

# quiz

# q1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
head(vowel.train)
str(vowel.train)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)

# predict with rf model
rf_mod <- train(y ~ ., method = "rf", data = vowel.train)
rf_mod
rf_pred <- predict(rf_mod, vowel.test)

# predict with gbm boosting model
library(gbm)
boost_mod <- train(y ~ ., method = "gbm", data = vowel.train, verbose = FALSE)
boost_mod
boost_pred <- predict(boost_mod, vowel.test)

# compare accuracy of rf and boosting model
# rf accuracy
confusionMatrix(rf_pred, vowel.test$y)
# boosting accuracy
confusionMatrix(boost_pred, vowel.test$y)
# accuracy agreement
pred <- data.frame("rf_pred" = rf_pred, "boost_pred" = boost_pred, "y" = vowel.test$y)
agreement <- subset(pred, pred$rf_pred == pred$boost_pred)
confusionMatrix(agreement$rf_pred, agreement$y)

# q2
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
ad <- data.frame(diagnosis, predictors)
in_train <- createDataPartition(ad$diagnosis, p = 3/4)[[1]]
training <- ad[in_train, ]
testing <- ad[-in_train, ]
set.seed(62433)

# rf model
rf_mod <- train(diagnosis ~ ., method = "rf", data = training)
rf_mod
rf_pred <- predict(rf_mod, testing)
confusionMatrix(rf_pred, testing$diagnosis)

# boosted model
boost_mod <- train(diagnosis ~ ., method = "gbm", data = training, verbose = FALSE)
boost_mod
boost_pred <- predict(boost_mod, testing)
confusionMatrix(boost_pred, testing$diagnosis)

# linear discriminant analysis model
lda_mod <- train(diagnosis ~ ., method = "lda", data = training)
lda_mod
lda_pred <- predict(lda_mod, testing)
confusionMatrix(lda_pred, testing$diagnosis)

# stack models
# it's odd that the quiz doesn't specify to use a validation set
# since we're training the stack_rf_mod on the testing$diagnosis, so you'd assume its prediction accuracy will be higher than 
# the out-of-sample predictions from the individual models
# you'd think you'd want to have a validation set, like in the video lecture example above??
pred_df <- data.frame(diagnosis = testing$diagnosis, rf_pred = rf_pred, boost_pred = boost_pred, lda_pred = lda_pred)
stack_rf_mod <- train(diagnosis ~ ., method = "rf", data = pred_df)
stack_rf_pred <- predict(stack_rf_mod, pred_df)
confusionMatrix(stack_rf_pred, testing$diagnosis)

# maybe i should stack the model predictions when predicting based on training data??



# q3
# not sure how to use plot.enet, so can't answer question
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
in_train = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ in_train, ]
testing = concrete[-in_train, ]

# lasso model
set.seed(233)
lasso_mod <- train(CompressiveStrength ~ ., method = "lasso", data = training)
lasso_mod
lasso_pred <- predict(lasso_mod, testing)
lasso_rmse <- sqrt(mean((lasso_pred - testing$CompressiveStrength)^2)) 

qplot(lasso_pred, testing$CompressiveStrength)
plot.enet()

# total aside, just want to see RMSE for lm model compared to lasso
lm_mod <- train(CompressiveStrength ~ ., method = "lm", data = training)
lm_mod
summary(lm_mod)
lm_pred <- predict(lm_mod, testing)
lm_rmse <- sqrt(mean((lm_pred - testing$CompressiveStrength)^2)) 

# q4
# don't know bats function, can't complete
library(lubridate)  # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

# q5
install.packages("e1071")
library(e1071)
# support vector machine info http://www.r-tutor.com/gpu-computing/svm/rpusvm-1 ???
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
