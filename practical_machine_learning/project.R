## read in data
full_training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
full_testing <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

## alternate
## read in data
setwd("C:/Users/Steve/Desktop/Coursera/Practical Machine Learning")
full_training <- read.csv("pml-training.csv")
full_testing <- read.csv("pml-testing.csv")

## split full_training into training and testing
in_train <- createDataPartition(full_training$classe, p = .7, list = FALSE)
training <- full_training[in_train, ]
testing <- full_training[-in_train, ]

## remove non-measurement variables
training1 <- training[ , -c(1:7)]

## remove variables with mostly NA values
missing <- lapply(training1, function(x) length(which(is.na(x))))
no_missing <- which(missing == 0)
training2 <- training1[ , no_missing]

## remove variables with mostly blanks
blanks <- lapply(training2, function(x) "" %in% x)
no_blanks <- which(blanks == FALSE)
training3 <- training2[ , no_blanks]

## convert integers and factor-numbers to numeric
class <- sapply(training3, class)
yes_integer <- which(class == "integer")
training4 <- training3
training4[ , yes_integer] <- sapply(training3[ , yes_integer], as.numeric)

## histogram of classe counts
ggplot(data = training, aes(x = classe)) + geom_histogram()

## rpart tree
mod_fit_rpart <- train(classe ~ ., method = "rpart", data = training4)
mod_fit_rpart$finalModel
fancyRpartPlot(mod_fit_rpart$finalModel)

pred <- predict(mod_fit_rpart, testing)
confusionMatrix(pred, testing$classe)

## predict on full_testing dataset
pred2 <- predict(mod_fit_rpart, full_testing)

