mydata <- read.csv("data/avgpm25.csv", colClasses = c("numeric", "character", "factor", "numeric", "numeric"))
head(mydata)
summary(mydata$pm25)
boxplot(mydata$pm25, col = "blue")
abline(h = 12)
hist(mydata$pm25, breaks = 100, col = "yellow")
abline(v = 12)
abline(v = median(mydata$pm25), col = "green", lwd = 4)
rug(mydata$pm25)
table(mydata$region)
barplot(table(mydata$region), main = "Bar Graph of Region", col = "pink")

boxplot(pm25 ~ region, data = mydata, col = "red")
hist(subset(mydata, region == "east")$pm25, col = "blue")
hist(subset(mydata, region == "west")$pm25, col = "green")
with(mydata, plot(latitude, pm25))
abline(h = 12, lwd = 2, lty = 3)
plot(mydata$latitude, mydata$pm25, col = mydata$region)

par(mfrow = c(1, 2))
plot(subset(mydata, region == "east")$latitude, subset(mydata, region == "east")$pm25, main = "East")
plot(subset(mydata, region == "west")$latitude, subset(mydata, region == "west")$pm25, main = "West")

head(cars)
cars[10,]
cars
plot(cars$speed, cars$dist)

par(mfrow = c(1, 1))
library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 1))

install.packages("ggplot2")
library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)
qplot(mpg$displ, mpg$hwy)
head(mpg)

head(airquality)
with(airquality, plot(Ozone, Wind))
plot(airquality$Ozone, airquality$Wind)
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))

plot(airquality$Wind, airquality$Ozone, main = "Ozone and Wind in New York City", type = "n")
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("blue", "green"), legend = c("May", "Other Months"))
model <- lm(airquality$Ozone ~ airquality$Wind)
abline(model, lwd = 4, lty = 3)

par(mfrow = c(1, 2), oma = c(0,0,2,0))
plot(airquality$Ozone, airquality$Wind, main = "Ozone and Wind")
plot(airquality$Ozone, airquality$Wind, main = "Ozone and Wind")
mtext("Major Title", outer = TRUE)

pchShow()
example(points)

par(mfrow = c(1, 1))

pdf(file = "myplot.pdf")
plot(airquality$Ozone, airquality$Wind)
dev.off()

png(file = "mypng.png")
plot(airquality$Ozone, airquality$Wind)
dev.off()


//////////////////////

        
mydata <- read.table("household_power_consumption.txt", sep = ";")
mydata[3, ]
mydata[1, ]
names2 <- lapply(names, FUN = toString)
names2 <- as.list(names)
names3 <- toString(names2)
names <- mydata[1, ]
colnames(mydata) <- names2
colnames(mydata)
ncol(mydata)
names(mydata)
str(mydata)
mydata$Date[1:3]
mydata <- as.date(mydata)
data <- mydata[Date == ]
head(mydata)
mydata <- mydata[-1, ]
date1 <- mydata[2, 1]
date1 <- as.Date(date1, format = '%d/%m/%Y')
mydata$Date <- as.Date(mydata$Date, format = '%d/%m/%Y')
lapply(mydata$Date, function(x) as.Date(x, format = '%d/%m/%Y'))
data <- mydata[mydata$Date == "2007-02-01" | mydata$Date == "2007-02-02", ]
str(data)
head(data)


test <- sapply(mtcars, function(x) x+1)
test

hist(data$global.active.power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
data$Global_active_power <- as.numeric(data$Global_active_power)
data$global.active.power.kw <- data$Global_active_power/1000
str(data)
summary(data$Date)

png(plot1.png)
hist(data$global.active.power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
dev.off()

mydata$Global_active_power[1:3]

names(data)
date1 <- data$Date[1]
weekdays(date1)
data$weekday <- weekdays(data$Date)
data$weekday[2000:2220]
summary(data$weekday)

time1 <- mydata$Time[1]
time2 <- as.Date(time1, format = "%H/%M/%S")
time2 <- as.character(time1)
time2
time3 <- as.POSIXct(time2, format = "%H:%M:%S")

volt1 <- data$Voltage[2]
volt1
str(volt1)
volt2 <- as.character(volt1)
as.numeric(volt2)

data$Voltage[1:10]
data$voltage[1:10]
data$voltage <- as.numeric(as.character(data$Voltage))
data$voltage <- as.numeric(data$Voltage)

data$Global_reactive_power[1:10]
data$global.reactive.power[1:10]

par(mfrow = c(1, 1))
dev.new(width = 10, height = 10)
