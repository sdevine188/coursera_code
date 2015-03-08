
scc <- readRDS("Source_Classification_Code.rds")
mydata <- readRDS("summarySCC_PM25.rds")
str(mydata)
str(scc)
head(scc)
head(mydata)

mtcars
plot(mtcars$cyl, mtcars$mpg, type = "l")
?plot
NEI <- readRDS("summarySCC_PM25.rds")

names(NEI)
aggdata <- aggregate(NEI$Emissions, list(year = NEI$year), sum)

data <- ChickWeight
head(data)
length(unique(data$Chick))
length(unique(data$Time))
aggdata <- aggregate(data$weight, list(year = data$Time), mean)
aggdata
plot(aggdata$year, aggdata$x, type = "b")
boxplot(aggdata$x)
plot(density(aggdata$x))
hist(aggdata$x)
?barplot
barplot(aggdata$x, names = aggdata$year)
summary(aggdata$weight)
is.na(aggdata$x)
boxplot(mtcars$mpg)

library(ggplot2)
aggdata$weight)
aggdata
library(ggplot)
plot1 <- ggplot(data = aggdata, aes(y = aggdata$x, x = aggdata$year)) + 
        geom_bar(stat = "identity")
plot1

chick1 <- data[which(data$Chick == 1), ]
chick1 <- data[which(data$Chick == 1 | 2 | 3, ]
chicklist <- subset(data, data$Chick < 5)
z <- which(data$Chick == 8 | data$Chick == 9)
chicklist2 <-  
str(data)
str(data$Chick)
data$Chick <- as.numeric(data$Chick)
?as.numeric
unique(data$Chick)
head(airmiles)
airmiles

str(data)
c1 <- subset(data, data$Chick < "10")
c1
data <- data.frame(data)
str(mtcars)
mtcars
mtcars2 <- subset(mtcars, mtcars$mpg > 1 & mtcars$mpg < 20)
data2 <- subset(data, data$Chick > 0 & data$Chick < 10)
g <- ggplot(data = c1, aes(x = Time, y = weight, 
        color = Chick)) + geom_point() + geom_line()
g + facet_grid(Diet ~ .)
barplot(c1$weight)
mtcars
mtcars$type <- rownames(mtcars)
rownames(mtcars)
ggplot(data = subset(mtcars, mtcars$type == "Valiant" | mtcars$type == "Fiat 128"), 
       aes(x = cyl, y = mpg)) + geom_bar(stat = "identity") + facet_grid(type ~ .)
str(mtcars$type)
test <- subset(mtcars, mtcars$type == "Valiant" | mtcars$type == "Fiat 128")
test
aggdata2 <- aggregate(data$weight, list(year = data$Time, chick = data$Chick, ))