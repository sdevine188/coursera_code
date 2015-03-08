test <- read.table("./project/test/X_test.txt")
testy <- read.table("./project/test/y_test.txt")
features <- read.table("./project/test/features.txt")

library(tm)
my.path <- "C:\\Users\\Steve\\Desktop\\Coursera" 
Corpus(DirSource(my.path), readerControl = list(reader=readPDF))

getwd()

fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "./cameras.csv")
cameraData <- read.csv("./cameras.csv")
names(cameraData)
str(cameraData)
cameraData[3, ]
tolower(names(cameraData))
splitNames <- strsplit(names(cameraData), "\\.")
splitNames


fileUrl1 <- "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 <- "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1, destfile = "./reviews.csv")
download.file(fileUrl2, destfile = "./solutions.csv")
reviews <- read.csv("./reviews.csv")
solutions <- read.csv("./solutions.csv")

gsub("_", "", names(reviews))
names(reviews)


