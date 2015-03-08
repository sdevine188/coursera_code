Week 2

install.packages('caret', dependencies = TRUE)
library(caret)
library(kernlab)
data(spam)
head(spam)
summary(spam)
str(spam)

?createDataPartition
inTrain <- createDataPartition(y = spam$type, p = .75, list = FALSE)
head(inTrain)
tail(inTrain)
str(inTrain)
inTrain

training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
head(training, 50)
tail(training, 50)
dim(training)

set.seed(32343)
modelFit <- train(type ~ ., data = training, method = "glm")
warnings()
modelFit
modelFit$finalModel

predictions <- predict(modelFit, newdata = testing)
predictions

confusionMatrix(predictions, testing$type)

set.seed(32323)
folds <- createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = TRUE)
head(folds)
sapply(folds, length)
nrow(spam)
folds[[1]][1:10]
folds[[2]][1:10]
folds[[3]][1:10]
folds[[4]][1:10]
folds[[5]][1:10]


folds <- createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = FALSE)
sapply(folds, length)
folds[[1]][1:10]
folds[[2]][1:10]
folds[[3]][1:10]

set.seed(32323)
folds <- createResample(y = spam$type, times = 10, list = TRUE)
sapply(folds, length)
folds[[1]][1:10]
folds[[2]][1:10]
folds[[3]][1:10]

set.seed(32323)
time <- 1:1000
folds <- createTimeSlices(y = time, initialWindow = 20, horizon = 10)
names(folds)
folds$train[1]
folds$train[2]
str(folds)
str(folds$train)
str(folds$test)

female <- c(67, 68, 69, 69, 74, 55, 62, 64, 70, 69)
mean(female)
male <- c(70, 71, 73, 75, 66, 68, 73, 80, 69, 75, 74, 73, 75, 80, 66, 68, 70, 73, 75, 76)
mean(male)
length(male)
length(female)
10/20
all <- c(female, male)
all
length(all)
mean(all)
w_male <- male*.5
w_male
mean(w_male)
w_all <- c(w_male, female)
mean(w_all)
sum(w_all)
sum(w_male)
sum(female)
725+667
sum(male)
1450/2
1392/20


install.packages("ISLR")
library(ISLR)
library(ggplot2)
library(caret)
data(Wage)
head(Wage)
summary(Wage)

inTrain <- createDataPartition(y = Wage$wage, p = .7, list = FALSE)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]
str(training)
str(testing)
names(training)

featurePlot(x = training[ , c("age", "education", "jobclass")], y = training$wage, plot = "pairs")

ggplot(training, aes(x = age, y = wage, color = jobclass)) + geom_point(shape = 1)
ggplot(training, aes(x = age, y = wage, color = jobclass, shape = education)) + geom_point()
ggplot(training, aes(x = age, y = wage, color = education)) + geom_point() + 
        geom_smooth(method = lm)

install.packages("Hmisc")
library(Hmisc)
cutWage <- cut2(training$wage, g = 3)
table(cutWage)
str(cutWage)
head(cutWage)
names(training)
length(cutWage)
training$cutWage <- cutWage

ggplot(training, aes(x = cutWage, y = age, fill = cutWage)) + geom_boxplot()
ggplot(training, aes(x = cutWage, y = age, fill = cutWage)) + geom_boxplot() + geom_jitter()

t1 <- table(cutWage, training$jobclass)
prop.table(t1, 1)
prop.table(t1, 2)

ggplot(training, aes(x = wage, color = education)) + geom_density()

names(training)
ggplot(training, aes(x = capitalAve)) + geom_density()
mean(testing$capitalAve)
sd(testing$capitalAve)

## need to normalize the testing set values using the mean and sd of the training set values
capitalAve <- training$capitalAve
testCapitalAve <- testing$capitalAve
testCapitalAveS <- (testCapitalAve - mean(capitalAve))/sd(capitalAve)
mean(testCapitalAveS)
sd(testCapitalAveS)

set.seed(32343)
modelFit <- train(type ~ ., data = training, preProcess = c("center", "scale"), method = "glm")

## imputing data using k nearest neighbors
## make some values NA
set.seed(13343)
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size = 1, prob = .05) == 1
training$capAve[selectNA] <- NA
names(training)
dim(training)

## imput and standardize
preObj <- preProcess(training[ , -58], method = "knnImpute")
capAve <- predict(preObj, training[ , -58])$capAve

inTrain <- createDataPartition(y = Wage$wage, p = .7, list = FALSE)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]

## creating covariates

## creating dummies
dummies <- dummyVars(wage ~ jobclass, data = training)
head(predict(dummies, newdata = training))

## getting rid of near-zero variables with little variation
nsv <- nearZeroVar(training, saveMetrics = TRUE)
nsv

## adding polynomial terms to linear model
library(splines)
bsBasis <- bs(training$age, df = 3)
bsBasis

lm1 <- lm(wage ~ bsBasis, data = training)
plot(training$age, training$wage, pch = 19, cex = .5)
points(training$age, predict(lm1, newdata = training), col = "red", pch = 19, cex = .5)

## Principal Components Analysis pre-processing
library(kern)
data(spam)
inTrain <- createDataPartition(y = spam$type, p = .75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

head(training)
M <- abs(cor(training[ ,-58]))
diag(M) <- 0
which(M > .8, arr.ind = T)
names(spam[c(32, 34)])    
ggplot(training, aes(x = num857, y = num415)) + geom_point()
head(training)

## example of idea quasi-similar to PCA/SVD
## .71 is derived from actual PCA rotation table, see below
x <- .71 * training$num415 + .71 * training$num857
y <- .71 * training$num415 - .71 * training$num857
ggplot(training, aes(x = x, y = y)) + geom_point(shape = 1)
## you can see in the plot that the x variable captures most of the variation, not the y variable, so using x might
## be a useful way to combine the two highly-correlated measures, while retaining info, to use for prediction

## now using real PCA
small_spam <- spam[ , c(32, 34)]
pc_small_spam <- prcomp(small_spam)
pc_small_spam
plot(pc_small_spam$x[ , 1], pc_small_spam$x[ , 2])
## this .71 is used in quasi-PCA calculation above
## so, PC1 is .7061 * num857 and .7080 * num 415, etc
pc_small_spam$rotation

type_color <- ((spam$type == "spam")*1 + 1)
## conduct PCA on entire dataset, needed to conduct log10 transform and +1 
## to make some skewed variables more normally distributed, this is common with PCA
pr_comp <- prcomp(log10(spam[ , -58] +1))
plot(pr_comp$x[ , 1], pr_comp$x[ , 2], col = type_color, xlab = "PCA1", ylab = "PCA2")


## PCA using caret package
pre_proc <- preProcess(log10(spam[ , -58] + 1), method = "pca",
                       pcaComp = 2)
spam_pc <- predict(pre_proc, log10(spam[ , -58] + 1))
plot(spam_pc[ , 1], spam_pc[ , 2], col = type_color)



## more pre-processing with PCA and caret
pre_proc <- preProcess(log10(training[ , -58] + 1), method = "pca",
                       pcaComp = 2)
train_pc <- predict(pre_proc, log10(training[ , -58] + 1))
model_fit <- train(training$type ~ ., method = "glm", data = train_pc)

## now use the pre_proc and model_fit from training data on the testing data
test_pc <- predict(pre_proc, log10(testing[ , -58] + 1))
confusionMatrix(testing$type, predict(model_fit, test_pc))


pca_train <- prcomp(log10(training[ , -58] + 1), scale. = TRUE)
pca_train
summary(pca_train)
pca_train$rotation
pca_train$sdev
screeplot(pca_train, type = "line")
biplot(pca_train)

## non-coursera test of PCA
## http://gastonsanchez.com/blog/how-to/2012/06/17/PCA-in-R.html
?USArrests
arrests <- USArrests
?princomp
summary(arrests)
cor(arrests)
pca <- princomp(arrests, scores = TRUE, cor = TRUE)
summary(pca)
loadings(pca)
pca$loadings
?screeplot
screeplot(pca, type = "line")
plot(pca)


## better to use prcomp, princomp is old and historic
pca1 <- prcomp(arrests, scale. = TRUE)
pca1
pca1$sdev
pca1$rotation
pca1$sdev^2
head(pca1$x)
pca1$x
screeplot(pca1, type = "line")
biplot(pca1)


pca2 <- prcomp(log10(arrests + 1), scale. = TRUE)

## predicting with regression
data(faithful)
set.seed(333)
head(faithful)
?faithful
?createDataPartition
head(in_train)
str(faithful)
str(in_train)
faithful[1:20, ]
length(in_train)
in_train <- createDataPartition(y = faithful$waiting, p = .5, list = FALSE)
train_faith <- faithful[ in_train , ]
test_faith <- faithful[ -in_train, ]

lm1 <- lm(eruptions ~ waiting, data = train_faith)
summary(lm1)
lm1$coefficients
lm1$coefficients[1]
lm1$fitted
ggplot(train_faith, aes(x = waiting, y = eruptions)) + geom_smooth(method = "lm") + geom_point(shape = 1)
ggplot(train_faith, aes(x = waiting, y = eruptions)) + geom_smooth() + geom_point(shape = 1)
ggplot(train_faith, aes(x = waiting, y = eruptions)) + geom_abline(intercept = lm1$coefficients[1], 
                                                                   slope = lm1$coefficients[2]) + geom_point(shape = 2)

plot(train_faith$waiting, train_faith$eruptions)
lines(train_faith$waiting, lm2$fitted)

ggplot(test_faith, aes(x = waiting, y = eruptions)) + geom_smooth(method = "lm") + geom_point(shape = 1)

newdata <- data.frame(waiting = 80)
predict(lm1, newdata)

train_rmse <- sqrt(sum((lm1$fitted - train_faith$eruptions)^2))
test_rmse <- sqrt(sum((predict(lm1, newdata = test_faith) - test_faith$eruptions)^2))
train_rmse
test_rmse

## same analysis using caret
fit2 <- train(eruptions ~ waiting, data = train_faith, method = "lm")
fit2
fit2$finalModel
summary(fit2)

## the scatterplot with loess curve looks like there is a polynomial relationship...update: it is not
train_faith$waiting2 <- train_faith$waiting^2
train_faith$waiting3 <- train_faith$waiting^3

names(train_faith)
lm2 <- lm(eruptions ~ waiting + waiting2, data = train_faith)
summary(lm2)
lm2$coefficients
lm2$coefficients[1]
lm2$fitted
ggplot(train_faith, aes(x = waiting, y = eruptions)) + geom_abline(intercept = lm2$coefficients[1], 
        slope = lm2$coefficients[2]) + geom_point(shape = 2)

## prediction using multiple regression
library(ISLR)
data(Wage)
names(Wage)
which(names(Wage) == "logwage")
wage <- Wage[ , -11]
names(wage)
summary(wage)

?createDataPartition
in_train <- createDataPartition(y = wage$wage, p = .7, list = FALSE)
wage_train <- wage[in_train, ]
wage_test <- wage[-in_train, ]
str(wage_train)
str(wage_test)
featurePlot(x = wage_train[ , c("age", "education", "jobclass")], y = wage_train$wage, plot = "pairs")

fit3 <- train(wage ~ age + education + jobclass, method = "lm", data = wage_train)
fit3
final_mod <- fit3$finalModel
summary(fit3)
plot(final_mod)

str(final_mod$fitted)
fitted_df <- data.frame(final_mod$fitted)
head(fitted_df)
str(fitted_df)
residuals_df <- data.frame(final_mod$residuals)
head(residuals_df)
str(residuals_df)
fit_df <- data.frame(fitted_df, residuals_df)
names(fit_df) <- c("fitted", "residuals")
head(fit_df)

ggplot(fit_df, aes(x = fitted, y = residuals)) + geom_point(shape = 1)
final_mod
final_mod$fitted
final_mod$residuals
str(final_mod)
summary(final_mod)

## quiz
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
head(training)
tail(training)
nrow(training)
str(training)
training$row <- 1:774 
names(training)
summary(training)


cut_cement <- cut2(training$Cement, g = 3)
cut_bfs <- cut2(training$BlastFurnaceSlag, g = 3)
cut_flyash <- cut2(training$FlyAsh, g = 4)
cut_age <- cut2(training$Age, g = 4)
ggplot(training, aes(x = row, y = CompressiveStrength, color = cut_flyash)) + geom_point(shape = 2)

cutWage <- cut2(training$wage, g = 3)


library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

head(training)
ggplot(training, aes(x = Superplasticizer)) + geom_histogram(binwidth = .0001)


library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

head(training)
names(training)
il_df <- training[ , 58:69]
head(il_df)
pca1 <- prcomp(il_df, scale. = TRUE)
?prcomp
summary(pca1)
str(il_df)
pca1$rotation
pca1$x
pca1$sdev
pca1$sdev^2
screeplot(pca1, type = "lines")
biplot(pca1)

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

names(training)
il_df <- training[ , 58:69]
il_df$diagnosis <- training[ , "diagnosis"]
names(il_df)
head(il_df)
summary(il_df)

?train
fit1 <- train(diagnosis ~ ., method = "glm", data = il_df)
summary(fit1)
fit1_final_model <- fit1$finalModel
fit1_final_model

pca1 <- prcomp(il_df[ , -13], scale. = TRUE)
summary(pca1)
pca1$rotation
pca1$x
pca1$sdev
pca1$sdev^2
screeplot(pca1, type = "lines")
biplot(pca1)

pre_proc <- preProcess(il_df[ , -13], method = "pca", pcaComp = 7)
ildf_pc <- predict(pre_proc, il_df[ , -13])
head(ildf_pc)
pc_model_fit <- train(il_df$diagnosis ~ ., method = "glm", data = ildf_pc)
summary(pc_model_fit)
pc_final_model <- pc_model_fit$finalModel
pc_model_fit$finalModel

head(il_df_test)
il_df_test <- testing[ , 58:69]
il_df_test$diagnosis <- testing[ , "diagnosis"]
pc_test <- predict(pre_proc, il_df_test[ , -13])
confusionMatrix(il_df_test$diagnosis, predict(pc_model_fit, pc_test))

fit1_predictions <- predict(fit1, newdata = il_df_test[ , -13])
confusionMatrix(fit1_predictions, il_df_test$diagnosis)


## for reference

set.seed(32343)
modelFit <- train(type ~ ., data = training, method = "glm")
warnings()
modelFit
modelFit$finalModel
predictions <- predict(modelFit, newdata = testing)
predictions

confusionMatrix(predictions, testing$type)

## more pre-processing with PCA and caret
pre_proc <- preProcess(log10(training[ , -58] + 1), method = "pca",
                       pcaComp = 2)
train_pc <- predict(pre_proc, log10(training[ , -58] + 1))
model_fit <- train(training$type ~ ., method = "glm", data = train_pc)

## now use the pre_proc and model_fit from training data on the testing data
test_pc <- predict(pre_proc, log10(testing[ , -58] + 1))
confusionMatrix(testing$type, predict(model_fit, test_pc))