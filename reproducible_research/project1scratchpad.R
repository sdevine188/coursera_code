## import data

data <- read.csv("activity.csv")
summary(data)
str(data)
head(data)
?as.POSIXct
data$date <- as.POSIXlt(data$date)
str(data$date)

## mean total number of steps taken per day
## make histogram of total steps per day
library(ggplot2)
data.agg <- aggregate(data$steps, list(date = data$date), sum)
names(data.agg) <- c("date", "steps")
hist <- ggplot(data.agg, aes(x = steps)) + geom_histogram()
print(hist)

mean(data.agg$steps, na.rm = TRUE)
median(data.agg$steps, na.rm = TRUE)

day2.time <- subset(data, data$date == "2012-10-02")
data$date[1]
2355/1440
60*24

## create line graph of average steps by interval

data.agg2 <- aggregate(data$steps, list(interval = data$interval), mean, na.rm = TRUE)
names(data.agg2) <- c("interval", "steps")
line <- ggplot(data = data.agg2, aes(x = interval, y = steps)) + geom_line()
print(line)

summary(data$steps)
?aggregate
mean(data$steps, na.rm = TRUE)

## find interval with maximum average steps per day

max.interval <- which(data.agg2$steps == max(data.agg2$steps))
print(data.agg2$interval[max.interval])


max.interval <- subset(data.agg2, steps > 206.1 & steps < 206.3)

max.interval <- which(data.agg2$steps == max(data.agg2$steps))
x <- subset(data.agg2, steps == 206.1698)
sorted <- sort(data.agg2$steps)
sorted[288]
data.agg2[104, ]
data.agg2$steps.char <- as.character(data.agg2$steps)
str(data.agg2)
?which
?subset
summary(data.agg2$steps)
str(data.agg2$steps)

## calculate number of missing values

summary(data)

## impute missing values using mean for interval

which(data == NA)
is.na(data)
str(data)
head(steps.na)
steps.na <- which(is.na(data$steps) == TRUE)
head(data.agg2)
str(data.agg2)
which(data.agg2$interval == 05)

steps.na <- which(is.na(data$steps) == TRUE)
data2 <- data
for(i in steps.na) {
        interval <- data$interval[i]
        row <- which(data.agg2$interval == interval)
        mean <- data.agg2$steps[row]
        data2$steps[i] <- mean
}

str(data2)
names(data2)
summary(data2)

## create histogram of total number of steps taken each day using imputed data

data.agg3 <- aggregate(data2$steps, list(date = data$date), sum)
names(data.agg3) <- c("date", "steps")
hist2 <- ggplot(data.agg3, aes(x = steps)) + geom_histogram()
print(hist2)

## calculate mean and median total steps per day using imputed data

mean(data.agg3$steps)
median(data.agg3$steps)

## what is the impact of imputing missing data?

print("The impact of imputing 2,304 missing steps data points, using the mean number of steps for the 
        associated interval, is that the median of the new dataset is now equal to the mean.  
        The mean of the new dataset is unchanged from the mean of the old dataset.  This creates values for those
      days which previously had no values in the old dataset and were just NA for each interval.")

head(data.agg3)
head(data.agg4)
data.agg4 <- aggregate(data$steps, list(date = data$date), sum, na.rm = TRUE)
names(data.agg4) <- c("date", "steps")
data.agg4$set <- "non-imputed"
data.agg3$set <- "imputed"
data.agg3
?rbind
data.agg5 <- rbind(data.agg4, data.agg3)
day1 <- subset(data$steps, data$date == "2012-10-01")
head(data)
str(data)
which(data$date == "2012-10-01")


line2 <- ggplot(data.agg5, aes(x = date, y = steps, group = set, color = set)) + geom_line() + geom_point()

head(data.agg5)
data.compare <- cbind(data.agg4, data.agg3$steps)
names(data.compare)[4] <- "steps.i"
data.compare$diff <- data.compare$steps - data.compare$steps.i
data.compare

## create new factor variables for weekday and weekend

data2$day <- data$date$wday + 1
days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
         "Friday", "Saturday")
data2$day <- days[data$day]

weekday.index <- ifelse(data2$day == "Monday" | data$day == "Tuesday" | data$day == "Wednesday" | 
                data$day == "Thursday" | data$day == "Friday", "weekday", "weekend")
data2$weekday <- weekday.index

## create two line graphs showing average steps per interval for weekdays and for weekends

data.agg6 <- aggregate(data2$steps, list(interval = data2$interval, weekday = data2$weekday), mean)
names(data.agg6)[3] <- "steps"

line2 <- ggplot(data.agg6, aes(x = interval, y = steps)) + geom_line() 
line2 + facet_grid(. ~ weekday)

head(data.agg6)
str(data.agg6)
str(data2)

iris
install.packages("GGally")
library(GGally)
ggpairs(iris, colour='Species', alpha=0.4)
ggpairs(data = iris, color = iris$Species)
mtcars
pairs(mtcars$cyl, mtcars$mpg, mtcars$hp, mtcars$gear)

ggpairs(iris, color = "Species")
iris <- iris[ , 1:5]
str(iris)
iris
head(iris)
which(iris$Species == "setosa")
iris$x <- NA
setosa.index <- ifelse(iris$Species == c("setosa" | "virginica"), 1, 0)
iris$setosa <- setosa.index
iris$Species <- as.character(iris$Species)
if(iris$Species == "setosa") {
        iris$x <- 1
} 


?ifelse()
?if()
summary(iris$x)

install.packages("knitr")
library(knitr)
knit2html("PA1_template.Rmd")
