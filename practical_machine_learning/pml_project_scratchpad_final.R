library(caret)

# read in data
setwd("C:/Users/Steve/Desktop/Coursera/Practical Machine Learning")
full_training <- read.csv("pml-training.csv")
full_testing <- read.csv("pml-testing.csv")

# split full_training into training and testing
in_train <- createDataPartition(full_training$classe, p = .7, list = FALSE)
training <- full_training[in_train, ]
testing <- full_training[-in_train, ]

# remove non-predictors
training1 <- training[ , -1]
training1 <- training1[ , -c(2:6)]
testing1 <- testing[ , -1]
testing1 <- testing1[ , -c(2:6)]

# find columns with NAs
# confirm that columns with NA are same for training and testing data 
training_na <- lapply(training1, function(x) length(which(is.na(x))))
training_na
length(which(training_na == 0))
length(which(training_na != 0))

testing_na <- lapply(testing1, function(x) length(which(is.na(x))))
testing_na
length(which(testing_na == 0))
length(which(testing_na != 0))

training_actual_na <- as.data.frame(which(training_na != 0))
training_actual_na <- as.data.frame(t(training_actual_na))
names(training_actual_na)

testing_actual_na <- as.data.frame(which(testing_na != 0))
testing_actual_na <- as.data.frame(t(testing_actual_na))
names(testing_actual_na)
length(which(names(training_actual_na) %in% names(testing_actual_na)))

# remove columns with NA
training1 <- training1[ , colSums(is.na(training1)) == 0]
testing1 <- testing1[ , colSums(is.na(testing1)) == 0]

# remove rows with #DIV/0!
training2 <- training1
rownames(training2) <- NULL
remove <- rowSums(training2 == "#DIV/0!")
remove2 <- which(remove != 0)
training3 <- training2[-remove2, ]

testing2 <- testing1
rownames(testing2) <- NULL
remove <- rowSums(testing2 == "#DIV/0!")
remove2 <- which(remove != 0)
testing3 <- testing2[-remove2, ]

# convert numeric-looking factors to numeric
training4 <- training3
training4 <- as.data.frame(lapply(training4[ , -c(1, 87)], function(x) as.numeric(as.character(x))))
training4 <- cbind(training3[ , 1], training4)
training4 <- cbind(training4, training3[ , 87])
names(training4)[87] <- "classe"
names(training4)[1] <- "user_name"

testing4 <- testing3
testing4 <- as.data.frame(lapply(testing4[ , -c(1, 87)], function(x) as.numeric(as.character(x))))
testing4 <- cbind(testing3[ , 1], testing4)
testing4 <- cbind(testing4, testing3[ , 87])
names(testing4)[87] <- "classe"
names(testing4)[1] <- "user_name"

# remove columns that now consist only of na after #DIV/0! rows removed
training5 <- training4
na <- colSums(is.na(training5))
remove3 <- which(na != 0)
training5 <- training5[ , -remove3]

testing5 <- testing4
na <- colSums(is.na(testing5))
remove3 <- which(na != 0)
testing5 <- testing5[ , -remove3]

# rf model on whole dataset
rf_mod <- train(classe ~ ., method = "rf", data = training5)
rf_mod
head(getTree(rf_mod$finalModel, k = 29, labelVar = TRUE))
rf_pred <- predict(rf_mod, testing)
confusionMatrix(rf_pred, testing$classe)
importance(rf_mod$finalModel)
varImpPlot(rf_mod$finalModel)
partialPlot(rf_mod, testing, roll_belt)
save(rf_mod, file = "pml_project_rf_mod.RData")

# try gbm
gbm_mod <- train(classe ~ ., method = "gbm", data = training5, verbose = FALSE)
gbm_pred <- predict(gbm_mod, testing)
confusionMatrix(gbm_pred, testing$classe)

# stack rf and gbm models