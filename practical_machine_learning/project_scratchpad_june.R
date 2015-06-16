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

# find NAs
missing <- lapply(training1, function(x) length(which(is.na(x))))
missing <- lapply(testing1, function(x) length(which(is.na(x))))

# convert NAs to blanks
training2 <- training1
training2[is.na(training2)] <- ""
testing2 <- testing1
testing2[is.na(testing2)] <- ""

# convert #DIV/0! to blanks
# all the data remains accurate after conversion, but all variables are inexplicably turned into factors
training3 <- training2
training3 <- as.data.frame(lapply(training3, function(x) str_replace(x, "#DIV/0!", "")))

# turn factors into numeric variables to speed processing time
# all variable columns except 1 and 154 can be converted to numeric
# column 1 is user_name, column 154 is classe
# but it converts blanks to NA, so we'll need to reconvert NAs to blanks again
training4 <- training3
# more efficient version of as.numeric(as.character(x))
# http://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
# training4 <- as.data.frame(lapply(training4, function(x) as.numeric(as.character(x))))
training4 <- as.data.frame(lapply(training4[ , -c(1, 154)], function(x) as.numeric(levels(x))[x]))
# re-add columns 1 and 154
training4 <- cbind(training3[ , 1], training4)
training4 <- cbind(training4, training3[ , 154])
names(training4)[154] <- "classe"

# reconvert NAs to blanks
training5 <- training4
training5[is.na(training5)] <- ""

# small dataset to try rf model
sample_index <- sample(1:nrow(training5), 500)
sample <- training3[sample_index, ]
str(sample)
rf_mod <- train(classe ~ ., method = "rf", data = sample2)
# i think this is a slower version bc it limits number of k in k-fold cv to 3, instead of default 10
rf_mod <- train(classe ~ ., method = "rf", data = sample, trControl = trainControl(method = "cv"), number = 3)
# try gbm
gbm_mod <- train(classe ~ ., method = "gbm", data = sample)


