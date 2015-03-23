## read in data
full_training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
final_testing <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

## split full_training into training and testing
in_train <- createDataPartition(full_training$classe, p = .7, list = FALSE)
training <- full_training[in_train, ]
testing <- full_training[-in_train, ]

## exploratory data analysis
head(training)
str(training)
names(training)
str(testing)
dim(training)[1] + dim(testing)[1]
dim(full_training)

## remove non-measurement variables
training1 <- training[ , -c(1:7)]
str(training1)

## find NA values
missing <- lapply(training1, function(x) length(which(is.na(x))))
str(missing)

## several variables have 13456 NAs out of 13737 total obs, so let's remove these variables
no_missing <- which(missing == 0)
length(no_missing)
yes_missing <- which(missing != 0)
length(yes_missing)

training2 <- training1[ , no_missing]

## inspect blanks
blanks <- lapply(training2, function(x) "" %in% x)
no_blanks <- which(blanks == FALSE)
length(no_blanks)
yes_blanks <- which(blanks == TRUE)
length(yes_blanks)

training3 <- training2[ , no_blanks]

## convert integers and factor-numbers to numeric
str(training3)
names(training3)
head(training3)

class <- sapply(training3, class)
yes_integer <- which(class == "integer")
length(yes_integer)
no_integer <- which(class != "integer")
length(no_integer)

training4 <- training3
training4[ , yes_integer] <- sapply(training3[ , yes_integer], as.numeric)
str(training4)

## histogram of classe counts
ggplot(data = training3, aes(x = classe)) + geom_histogram()

## rpart tree
mod_fit_rpart <- train(classe ~ ., method = "rpart", data = training4)
mod_fit_rpart$finalModel
fancyRpartPlot(mod_fit_rpart$finalModel)

pred <- predict(mod_fit_rpart, testing)
pred_df <- data.frame(pred, testing$classe)
names(pred_df)[2] <- "classe"
pred_df$pred_right <- pred_df$pred == pred_df$classe

confusionMatrix(pred, testing$classe)

## predict on full_testing dataset
pred2 <- predict(mod_fit_rpart, full_testing)
confusionMatrix(pred, testing$classe)

## pca
## kept getting error trying to do log10 transform
## add constant to values to remove negative numbers
training5 <- training4[ , -53] + 1
training6 <- log(training5[ , -53])
train_pca <- prcomp(log(training5[ , -53]))
train_pca <- prcomp(training4[ , -53], scale. = TRUE)

summary(train_pca)
train_pca$rotation
screeplot(train_pca, type = "line")
biplot(train_pca)
## elbow in screeplot is after PCA7, so keep first 7 PCAs

pre_proc <- preProcess(training4[ , -53], method = "pca", pcaComp = 7)
train_pred <- predict(pre_proc, training4[ , -53])
model_fit <- train(training4$classe ~ ., method = "glm", data = train_pred)








## examples from pml_week2

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





pre_proc <- preProcess(log10(training[ , -58] + 1), method = "pca",
                       pcaComp = 2)
train_pc <- predict(pre_proc, log10(training[ , -58] + 1))
model_fit <- train(training$type ~ ., method = "glm", data = train_pc)

## now use the pre_proc and model_fit from training data on the testing data
test_pc <- predict(pre_proc, log10(testing[ , -58] + 1))
confusionMatrix(testing$type, predict(model_fit, test_pc))


## how to submit txt files for prediction on full_testing
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(pred2)
