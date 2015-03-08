download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", destfile = "acs.csv")
acs <- read.csv("acs.csv")
head(acs)
str(acs)
names(acs)
names.list <- names(acs)
split.names.list <- strsplit(names.list, "wgtp")
split.names.list[123]

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", destfile = "gdp.csv")
gdp.data <- read.csv("gdp.csv")
names(gdp.data)
head(gdp.data)
gdp.subset <- gdp.data$X.3[5:235]
gdp.subset
is.na(gdp.subset)
gdp.subset <- gsub(",", "", gdp.subset)
mean(gdp.subset))
length(which(is.na(gdp.subset)))

library(stringr)
gdp.subset <- str_trim(gdp.subset)
gdp.subset <- as.numeric(gdp.subset)
gdp.subset2 <- gdp.subset[!is.na(gdp.subset)]
length(gdp.subset2)
mean(gdp.subset2)
mean(gdp.subset2[1:190])

gdp.data$X.2
head(gdp.data)
countries <- gdp.data$X.2[5:194]
countries
united <- grep("^united", countries, ignore.case = TRUE, value = TRUE)
united

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", destfile = "gdp2.csv")
gdp2 <- read.csv("gdp2.csv")
head(gdp2)


download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv", destfile = "educ.csv")
educ <- read.csv("educ.csv")

head(educ)
names(educ)
names(gdp2)
educ$Special.Notes
merged <- merge(educ, gdp2, by = "ID")
educ$ID <- educ$Short.Name
gdp2$ID <- gdp2$X.2
names(merged)
head(merged)
x <- grep("Fiscal year end: June", educ$Special.Notes, ignore.case = TRUE, value = TRUE)
x

install.packages("quantmod")
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn) 
names(amzn)
head(amzn)
names(sampleTimes)
sampleTimes[1:6]
nrow(amzn)
amzn$ID <- 1:1963
nrow(amzn)
length(sampleTimes)
amzn$times3 <- times
amzn$times2
times <- as.POSIXlt(sampleTimes)
length(times)
amzn2012 <- subset(amzn.df, amzn.df$times > "2012-01-01" & amzn.df$times < "2012-12-31")
amzn.df <- data.frame(amzn$AMZN.Open, amzn$AMZN.High, amzn$AMZN.Low, amzn$AMZN.Close, amzn$AMZN.Volume, amzn$AMZN.Adjusted, amzn$times3)
names(amzn.df)
head(amzn.df)
amzn.df$times[1:6]
amzn.df$times <- times
which(names(amzn.df) == "times3")
amzn.df <- amzn.df[ , -7]
names(amzn2012)
head(amzn2012)
nrow(amzn2012)
row.names(amzn)
weekdays(times[1:10])
amzn2012$days <- weekdays(amzn2012$times)
head(amzn2012$days)
mondays <- subset(amzn2012, amzn2012$days == "Monday")
mondays
nrow(mondays)
