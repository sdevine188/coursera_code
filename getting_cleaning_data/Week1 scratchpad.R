
if(!file.exists("data")) {
        dir.create("data")
}
getwd()
setwd("C:/Users/Steve/Desktop/Coursera/Getting and Cleaning Data/data")
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "cameras.csv")
dateDownloaded <- date()
cameraData <- read.table("cameras.csv", sep = ",", header = TRUE)
head(cameraData)
dateDownloaded
library(XLSX)
install.packages("xlsx")
install.packages("XML")
library(XML)
fileUrl <- "http://www.w3schools.com/xml/simple.xml"
doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)
rootNode[[1]]
rootNode[[1]] [[1]]
xmlSApply(rootNode, xmlValue)
xpathSApply(rootNode, "//name", xmlValue)
xpathSApply(rootNode, "//price", xmlValue)
fileUrl <- "http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens"
doc <- htmlTreeParse(fileUrl, useInternal = TRUE)
scores <- xpathSApply(doc, "//li[@class='score']", xmlValue)
teams <- xpathSApply(doc, "//li[@class='team-name']", xmlValue)
scores
teams


fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, dest = "acsData.csv")
dateDownloaded <- date()
acsdata <- read.table("acsData.csv", sep = ",", header = TRUE)
head(acsdata)
acsdata[1, ]
nrow(acsdata)
acsdata$VAL[1:5]
sum(acsdata$VAL == 24, na.rm = TRUE)


fileurl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileurl, dest = "ngap3.xlsx", mode = "wb")
datedownloaded <- date()
library(xlsx)
ngapdata <- read.xlsx("ngap3.xlsx", sheetIndex = 1, header = TRUE)
getwd()
str(ngapdata)
datrows <- ngapdata[18:23, ]
datcols <- ngapdata[ , 7:15]
colIndex = 7:15
rowIndex = 18:23
dat <- read.xlsx("ngap3.xlsx", sheetIndex = 1, header = TRUE, colIndex = colIndex, rowIndex = rowIndex)

library(XML)
fileurl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc <- xmlTreeParse(fileurl, useInternal = TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)
rootNode[[1]]
rootNode[[1]] [[1]]
zipcode <- xpathSApply(rootNode, "//zipcode", xmlValue)
zipcode
sum(zipcode = "21231")
str(zipcode)
zipcode[1326]
sum(zipcode == "21231")

fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileurl, dest = "acsdata2")
acsdata2 <- read.csv("acsdata2")
acsdata2[1, ]


install.packages("data.table")
library(data.table)
DT = data.table(x = rnorm(9), y = rep(c("a", "b", "c"), each = 3), z = rnorm(9))
tables()
DT[2, ]
DT[DT$y=="a", ]
DT[c(2, 3), ]
DT[ , w:= z+2]
DT2 <- copy(DT)
DT[ , v:= w+2]
DT[ , u:= x > 0]
DT[ , mean(w), by = u]
DT[ , m:= {tmp <- (v + 1); tmp + 2}]
setkey(DT, y)
DT['a']
dt1 <- data.table(x = c("a", "a", "b", "dt1"), y = 1:4)
dt2 <- data.table(x = c("a", "b", "c", "dt2"), z = 5:7)

id <- c(1, 2, 3, 3, 4)
age <- c(30, 40, 50, 60, 70)
salary <- c(100, 200, 300, 400,)
salary2 <- c(400, 500, 600)
x <- data.frame(id, age, salary)
y <- data.frame(id, age, salary2)
z <- merge(x, y, by = "id")


fileurl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileurl, dest = "acsdata2.csv")
dt <- fread("acsdata2.csv")
dt[ , mean(pwgtp15)]

df <- data.frame(DT)
df


list.files("C:/Users/Steve/Desktop/Coursera/Getting and Cleaning Data/data")


fileurl2 <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileurl2, dest = "ngap2.xlsx")
library(xlsx)
ngapdata2 <- read.xlsx("ngap2.xlsx", sheetIndex = 1, header = TRUE)


library(XML)
fileUrl <- "view-source:http://www.project-syndicate.org/commentary/j--bradford-delong-assesses-the-ict-revolution-s-ongoing-impact-on-the-middle-class"
doc <- xmlTreeParse(fileUrl, useInternal = TRUE, isHTML = TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)
rootNode[[4]]
xmlSApply(rootNode, xmlValue)
<div class="body" itemprop="articleBody">


