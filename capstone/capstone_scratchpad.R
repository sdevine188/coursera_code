# read in chunks of data using file connection
# note that readLines will not "replace" the lines already read, so if you call readLines(con, 1) twice in a row,
# you'll read successive lines; you need to close the connection and re-open to "replace" all lines as unrea

# reference for capstone: http://rstudio-pubs-static.s3.amazonaws.com/41915_025a9bc2f28949c8a2118256cd2638ff.html

setwd("C:/Users/Stephen/Desktop/R/coursera_code/capstone")

# read in twitter data
con <- file("en_US/en_US.twitter.txt", "r")
twitter <- readLines(con)
close(con)
twitter2 <- data.frame(twitter)


# read in blogs data
con <- file("en_US/en_US.blogs.txt", "r")
blogs <- readLines(con)
close(con)
blogs2 <- data.frame(blogs)

# read in news data
con <- file("en_US/en_US.news.txt", "r")
news <- readLines(con)
close(con)
news2 <- data.frame(news)

# exploratory analysis
file.info("en_US/en_US.twitter.txt")
str(twitter)
length(twitter)
nchar(twitter[1])

file.info("en_US/en_US.news.txt")
str(news)
length(news)

file.info("en_US/en_US.blogs.txt")
file.info(bl)
str(blogs)
length(blogs)

# longest line in any of three files
twitter_max <- 0
twitter_longest <- for(i in 1:length(twitter)){
        nchar <- nchar(twitter[i])
        if(nchar > twitter_max){
                twitter_max <- nchar
                print(twitter_max)
        }
}

blogs_max <- 0
blogs_longest <- for(i in 1:length(blogs)){
        nchar <- nchar(blogs[i])
        if(nchar > blogs_max){
                blogs_max <- nchar
                print(blogs_max)
        }
}

news_max <- 0
news_longest <- for(i in 1:length(news)){
        nchar <- nchar(news[i])
        if(nchar > news_max){
                news_max <- nchar
                print(news_max)
        }
}

max(twitter_max, blogs_max, news_max)


# for twitter set, divide number of lines with word love by number with word hate
twitter <- data.frame(twitter, stringsAsFactors = FALSE)
names(twitter)[1] <- "text"
love_logical <- sapply(1:length(twitter), function(x) grepl("love", twitter$text[x]))
length(which(love_logical == TRUE))

hate_logical <- sapply(1:length(twitter), function(x) grepl("hate", twitter$text[x]))
length(which(hate_logical == TRUE))

90956 / 22138

# find one tweet referencing "biostats"
biostats <- grep("biostats", twitter$text, value = TRUE)

# find tweets with "A computer once beat me at chess, but it was no match for me at kickboxing"
chess <- length(grep("A computer once beat me at chess, but it was no match for me at kickboxing", twitter$text))


dim(twitter)

