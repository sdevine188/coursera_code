ucscDb <- dbConnect(MySQL(), user = "genome", 
                    host = "genome-mysql.cse.ucsc.edu");

result <- dbGetQuery(ucscDb, "show databases;"); 
result;

dbDisconnect(ucscDb);

hg19 <- dbConnect(MySQL(), user = "genome", db = "hg19", 
                  host = "genome-mysql.cse.ucsc.edu")
allTables <- dbListTables(hg19)
length(allTables)
allTables[1:5]
dbListFields(hg19, "affyU133Plus2")
dbGetQuery(hg19, "select count(*) from affyU133Plus2")
affyData <- dbReadTable(hg19, "affyU133Plus2")
head(affyData)
query <- dbSendQuery(hg19, "select * from affyU133Plus2 where misMatches between 1 and 3")
affyMis <- fetch(query); quantile(affyMis$misMatches)
affyMisSmall <- fetch(query, n=10); dbClearResult(query)
dim(affyMisSmall)
dbDisconnect(hg19)

## more MySQL commands at pantz.org, and RMySQL vignette at cran, and r-bloggers 



##HDF5

source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
library(rhdf5)
created = h5createFile("example.h5")
created
created = h5createGroup("example.h5", "foo")
created = h5createGroup("example.h5", "baa")
created = h5createGroup("example.h5", "foo/foobaa")
h5ls("example.h5")


A = matrix(1:10, nr = 5, nc = 2)
h5write(A, "example.h5", "foo/A")
B = array(seq(0.1, 2.0, by = 0.1), dim = c(5,2,2))
attr(B, "scale") <- "liter"
h5write(B, "example.h5", "foo/foobaa/B")
h5ls("example.h5")

df = data.frame(1L:5L, seq(0, 1, length.out = 5),
                c("ab", "cde", "fghi", "a", "s"), stringsAsFactors = FALSE)
h5write(df, "example.h5", "df")
h5ls("example.h5")

readA = h5read("example.h5", "foo/A")
readB = h5read("example.h5", "foo.foobaa/B")
readdf = h5read("example.h5", "df")
readA


h5write(c(12, 13, 14), "example.h5", "foo/A", index = list(1:3, 1))
h5read("example.h5", "foo/A")


## getting data off web

con = url("http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
htmlCode = readLines(con)
close(con)
htmlCode

library(XML)
url <- "http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
html <- htmlTreeParse(url, useInternalNodes = TRUE)
xpathSApply(html, "//title", xmlValue)
xpathSApply(html, "//td[@id='col-citedby']", xmlValue)


library(httr)
html2 = GET(url)
content2 = content(html2, as = "text")
parsedHtml = htmlParse(content2, asText = TRUE)
xpathSApply(parsedHtml, "//title", xmlValue)

pg1 = GET("http://httpbin.org/basic-auth/user/passwd")
pg1
pg2 = GET("http://httpbin.org/basic-auth/user/passwd", 
          authenticate("user", "passwd"))
pg2
names(pg2)

testHandle = handle("http://httpbin.org/basic-auth/user/passwd")
pg3 = GET("http://httpbin.org/basic-auth/user/passwd", 
          authenticate("user", "passwd"), handle = testHandle)
pg3
pg4 = GET("http://httpbin.org/basic-auth/user/passwd", handle = testHandle)
pg4



## API

## twitter name is finnegantest123
library(httr)

myapp = oauth_app("twitter", 
                  key = "IAGwo6qbhKHWIavxlQvJPvh0B", secret = "qp8W4dbNxviTNeGA0ubaMhgrkfAdhmK9DiVCVXFcfjfihYftU2")
sig = sign_oauth1.0(myapp, token = "956590380-Em2O3ERM5u6zAjqhLmajZYiAwX8auJ0FmN1SJtjp", 
                    token_secret = "IPEKFjGnku7Agf5dQ1aP2qmlnXnv75Q1Tm2fWBDAXX4el")
homeTL = GET("htpps://api.twitter.com/1.1/statuses/home_timeline.json", sig)

install.packages(RJSONIO)



library(httr)

myapp = oauth_app("github", 
                  key = "9b638deb8f618d0f50cf", secret = "e9753252a4a3bbdf012c081ecc1c03d5d7a8fd3b")
sig = sign_oauth1.0(myapp, token = "850ead243be2ffa3a3b37052550b502f3ec653a1") 
                   
homeTL = GET("https://api.github.com/users/jtleek/repos", sig)

content2 = content(homeTL, as = "text")
parsedHtml = htmlParse(content2, asText = TRUE)
parsedText <- xpathSApply(parsedHtml, "//title", xmlValue)


library(httr)

oauth_endpoints("github")

myapp <- oauth_app("github", "9b638deb8f618d0f50cf", secret = 
                           "e9753252a4a3bbdf012c081ecc1c03d5d7a8fd3b")

github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

gtoken <- config(token = github_token)
req <- GET("https://github.com/hadley/httr/blob/master/demo/oauth2-github.r", gtoken)
stop_for_status(req)
content(req)

req <- with_config(gtoken, GET("https://github.com/hadley/httr/blob/master/demo/oauth2-github.r"))
stop_for_status(req)
content(req)


response <- GET("https://api.github.com/repos/jtleek/datasharing")
responsefile <- file(response)

fileConn <- file("test.txt")
writeLines(response, fileConn)
close(fileConn)

install.packages(sqldf)
library(sqldf)

acs <- read.csv("acs.csv")
head(acs)

sqldf("select * from acs where AGEP < 50")

library(XML)
url <- "http://biostat.jhsph.edu/~jleek/contact.html"
doc <- htmlTreeParse(url, useInternal = TRUE)
saveXML(doc, "textfile.txt")
textfile <- file("textfile.txt")
text <- readLines(textfile, n = 100)
lines <- readLines(doc, n=100)



line100 <- text[100]
l100 <- nchar(line100)
line10 <- text[10]
l10 <- nchar(line10)
line20 <- text[20]
l20 <- nchar(line20)
line30 <- text[30]
l30 <- nchar(line30)
answer <- c(l10, l20, l30, l100)
answer

test <- text[10]
result <- sub(" ", "", test)
nchar(result)

x10 <- '<meta name="Distribution" content="Global" />'
l10 <- nchar(x10)
x20 <- '<script type="text/javascript">'
l20 <- nchar(x20)
x30 <- '})();'
l30 <- nchar(x30)
x100 <- '<ul class="sidemenu">'
l100 <- nchar(x100)
answer <- c(l10, l20, l30, l100)
answer


x <- read.fwf("test.for", widths = 5)
y <- readLines("test.for")


mtcars$mpg[3:5]
mtcars[1:5, ]

dir.create("./week3data")
fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "./week3data/restaurants.csv")
restData <- read.csv("./week3data/restaurants.csv")

restData[2, ]
table(restData$zipCode)
table(mtcars$cyl, useNA = "ifany")
table(mtcars$cyl, mtcars$mpg)
colSums(is.na(restData))
all(colSums(is.na(restData)) == 0)
table(restData$zipCode %in% c("21212", "21213"))
restData[restData$zipCode %in% c("21212", "21213"), ]

x <- data(UCBAdmissions)
x[3, ]
str(x)
df = as.data.frame(UCBAdmissions)
df[3, ]
xt <- xtabs(Freq ~ Gender + Admit, data = df)

restData$nearMe <- restData$neighborhood %in% c("Roland Park", "Homeland")
mtcars$foursix <- mtcars$cyl %in% c(4, 6)
mtcars

install.packages("plyr")
library(reshape2)
rownames(mtcars)
mtcars$carname <- rownames(mtcars)
carMelt <- melt(mtcars, id=c("carname", "gear", "cyl"), measure.vars = c("mpg", "hp"))
mtcars[1:3, ]
cylData <- dcast(carMelt, cyl ~ variable)
cylData <- dcast(carMelt, cyl ~ variable, mean)
spCyl <- split(mtcars$carname, mtcars$cyl)
spCyl
str(spCyl$'8')
str(spCyl)
x <- unlist(spCyl)
str(x)
y <- as.data.frame(x)
str(y)



fileUrl1 <- "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 <- "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1, destfile = "./week3data/reviews.csv")
download.file(fileUrl2, destfile = "./week3data/solutions.csv")
reviews = read.csv("./week3data/reviews.csv")
solutions <- read.csv("./week3data/solutions.csv")
head(reviews, 2)
head(solutions, 2)
names(reviews)
names(solutions)
mergedData <- merge(reviews, solutions, by.x = "solution_id", by.y = "id", all = TRUE)
head(mergedData, 4)
intersect(names(reviews), names(solutions))

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile = "./week3data/acs.csv")
acs <- read.csv("./week3data/acs.csv")
acs[1:5, ]

agricultureLogical <- acs$ACR == 3 & acs$AGS == 6
agricultureLogical[1:5]
agricultureLogical <- acs$acr > 2 & acs$ags > 5
names(acs)
which(agricultureLogical)
x <- acs[c(125, 238, 262), ]

install.packages("jpeg")
library(jpeg)
img <- readJPEG("https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg")
readJPEG("https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg", native = TRUE)

readJPEG("./week3data/manualleek.jpeg")

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(fileUrl, destfile = "./week3data/leek.jpg")
x <- file("./week3data/leek.jpg")

link = "http://29.media.tumblr.com/tumblr_m0q2g8mhGK1qk6uvyo1_500.png"
download.file(link, destfile = "./week3data/testpic.png")
x <- file("./week3data/testpic.png")

?readJPEG

names(mtcars)[2] <- "Cylindercol"

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileUrl, destfile = "./week3data/gdp.csv")
gdp <- read.csv("./week3data/gdp.csv", stringsAsFactors = FALSE)
head(gdp)
names(gdp)

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileUrl, destfile = "./week3data/educ.csv")
educ <- read.csv("./week3data/educ.csv")
head(educ)
names(educ)

b <- gdp$X %in% educ$CountryCode
sum(is.na(b))
sum(b)

mergedData <- merge(gdp, educ, by.x = "X", by.y = "CountryCode")
head(mergedData)
write.csv(mergedData, file = "./week3data/mergedData.csv")




gdp$X[1:7]
x <- names(gdp)
x[1:3]
y <- unlist(gdp$X)
y[1:3]
str(y)
z <- as.vector(y)
z[1:15]
q <- as.character(y)
q[1:15]
gdp[gdp$X == "CHN", "X.3"]
names(gdp)
mergedData[mergedData$X == "CHN", "X.3"]
x <- mergedData$X
y <- mergedData$X.3
z <- gsub(" ", "", y)
z <- gsub(",", "", z)
p <- as.numeric(z)
df <- data.frame(x, p)
df[df$x == "CHN", "p"]
dfOrder <- df[order(df$p), ]
head(dfOrder, 15)
dfOrder[13, ]
gdp[gdp$X == "KNA", ]

mergedData <- merge(gdp, educ, by.x = "X", by.y = "CountryCode")
head(mergedData)
write.csv(mergedData, file = "./week3data/mergedData.csv")
sort(mergedData)
mergedData$X.3[1:15]
names(mergedData)[5] <- "GDP"
mergedData$X
z <- data.frame(mergedData[mergedData$X == "CHN", ])
z$Currency.Unit
mergedData$GDP
mergedData$gdp
str(mergedData)
x <- mergedData$GDP
y <- mergedData$X
df <- data.frame(y, x)
df[order(df$y), ]
as.numeric(df$x)
as.numeric(df[ , 2])

mergedData$gdp <- as.numeric(mergedData$GDP)
sortData <- mergedData[order(mergedData$gdp), ]
names(sortData)
sortData[1:20, ]
sortData$GDP
str(df$x)
e <- as.numeric(x)
q <- (as.character(x))
p <- gsub(" ", "", q)
p <- gsub(",", "", p)
p <- as.numeric(p)
w <- as.numeric(p)
gdp$X.3


p <- as.numeric(levels(x))[x]
str(gdp$X.3)
p[1]
as.numeric(p[1])
e <- gsub(",", "", p)
e[1]
as.numeric(e[1])

sort(x)
sort.int(x)
str(x)
l <- levels(x)
str(l)




names(mergedData)
names(mergedData)[2] <- "gdpRank"
unique(mergedData$Income.Group)
df <- data.frame(mergedData$X, mergedData$gdpRank, mergedData$X.3, mergedData$Income.Group)
df
oecd <- df[df$mergedData.Income.Group == "High income: OECD", "mergedData.gdpRank"]
nonOecd <- df[df$mergedData.Income.Group == "High income: nonOECD", "mergedData.gdpRank"]
charOecd <- as.character(oecd)
charNonOecd <- as.character(nonOecd)
numOecd <- as.numeric(charOecd)
numNonOecd <- as.numeric(charNonOecd)
mean(numOecd, na.rm = TRUE)
mean(numNonOecd, na.rm = TRUE)


mergedData$gdpRank <- as.character(mergedData$gdpRank)
mergedData$gdpRank <- as.numeric(mergedData$gdpRank)
x <- mergedData$gdpRank
sort(x)


quantile(df$mergedData.gdpRank, c(.2, .4, .6, .8, 1.0), na.rm = TRUE)
dfc$q1 <- dfc[(dfc$mergedData.gdpRank <= 38), ]
dfc$q2 <- dfc[(dfc$mergedData.gdpRank > 38 & dfc$mergedData.gdpRank <= 76), ]
dfc$q3 <- dfc[(dfc$mergedData.gdpRank > 76 & dfc$mergedData.gdpRank <= 113), ]
dfc$q4 <- dfc[(dfc$mergedData.gdpRank > 113 & dfc$mergedData.gdpRank <= 152), ]
dfc$q5 <- dfc[(dfc$mergedData.gdpRank > 152 & dfc$mergedData.gdpRank <= 190), ]
nrow(dfc)
dfc$q <- c()
quantcol <- function(df) {
        for(i in 1:nrow(df)) {
                if(dfc[i, dfc$mergedData.gdpRank] <= 38) {
                        dfc[i, dfc$q] == 1
                }
                if(dfc[i, dfc$mergedData.gdpRank] > 38 & dfc$mergedData.gdpRank <= 76) {
                        dfc[i, dfc$q] == 2
                }
                if(dfc[i, dfc$mergedData.gdpRank] > 76 & dfc$mergedData.gdpRank <= 113) {
                        dfc[i, dfc$q] == 3
                }   
                if(dfc[i, dfc$mergedData.gdpRank] > 113 & dfc$mergedData.gdpRank <= 152) {
                        dfc[i, dfc$q] == 4
                }   
                if(dfc[i, dfc$mergedData.gdpRank] > 152 & dfc$mergedData.gdpRank <= 190) {
                        dfc[i, dfc$q] == 5
                }  
        }
}


quantcol <- function(df) {
        for(i in 1:nrow(df)) {
                if(dfc[i, "mergedData.gdpRank"] <= 38) {
                        dfc[i, dfc$q] == 1
                }
                if(dfc[i, "mergedData.gdpRank"] > 38 & dfc[i, "mergedData.gdpRank"] <= 76) {
                        dfc[i, dfc$q] == 2
                }
                if(dfc[i, "mergedData.gdpRank"] > 76 & dfc[i, "mergedData.gdpRank"] <= 113) {
                        dfc[i, dfc$q] == 3
                }   
                if(dfc[i, "mergedData.gdpRank"] > 113 & dfc[i, "mergedData.gdpRank"] <= 152) {
                        dfc[i, dfc$q] == 4
                }   
                if(dfc[i, "mergedData.gdpRank"] > 152 & dfc[i, "mergedData.gdpRank"] <= 190) {
                        dfc[i, dfc$q] == 5
                }  
        }
}

if(dfc[6, "mergedData.gdpRank"] <= 38) {
        dfc[i, dfc$q] == 1
        }

lmi <- dfc[dfc$mergedData.Income.Group == "Lower middle income", ]
table(dfc$mergedData.Income.Group, dfc$q1)

dfc[6, "mergedData.gdpRank"]
dfc$q1 <- dfc$mergedData.gdpRank <= 38
dfc$q2 <- dfc$mergedData.gdpRank > 38 & dfc$mergedData.gdpRank <= 76
dfc$q3 <- dfc$mergedData.gdpRank > 76 & dfc$mergedData.gdpRank <= 113
dfc$q4 <- dfc$mergedData.gdpRank > 113 & dfc$mergedData.gdpRank <= 152
dfc$q5 <- dfc$mergedData.gdpRank > 152 & dfc$mergedData.gdpRank <= 190
qdf <- data.frame(q1best, q2, q3, q4, q5)


complete.cases(df)
df$mergedData.gdpRank <- as.numeric(as.character(df$mergedData.gdpRank))
df
str(df$mergedData.gdpRank)
colSums(is.na(df))
dfc <- df[complete.cases(df), ]
colSums(is.na(dfc))
