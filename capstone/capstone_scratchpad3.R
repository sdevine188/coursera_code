library(tm)
library(SnowballC)
library(stringr)
library(dplyr)
library(ggplot2)
library(textir)

# personal computer
setwd("C:/Users/Stephen/Desktop/R/coursera_code/capstone")

# work computer
setwd("H:/R/coursera_code/capstone")

# read in twitter data
con <- file("en_US/en_US.twitter.txt", "r")
twitter <- readLines(con, encoding = "UTF-8")
close(con)

length(twitter)
# twitter_sample <- sample(seq(1:length(twitter)), size = length(twitter) * .2)
twitter_sample <- sample(seq(1:length(twitter)), size = 100000)
twitter2 <- twitter[twitter_sample]
write(twitter2, "twitter2.txt")

con <- file("twitter2.txt", "r")
x <- readLines(con, encoding = "UTF-8")
close(con)

# read in blogs data
con <- file("en_US/en_US.blogs.txt", "r")
blogs <- readLines(con, encoding = "UTF-8")
close(con)

length(blogs)
# blogs_sample <- sample(seq(1:length(blogs)), size = length(blogs) * .2)
blogs_sample <- sample(seq(1:length(blogs)), size = 100000)
blogs2 <- blogs[blogs_sample]
write(blogs2, "blogs2.txt")

# read in news data
con <- file("en_US/en_US.news.txt", "r")
news <- readLines(con, encoding = "UTF-8")
close(con)

length(news)
news_sample <- sample(seq(1:length(news)), size = length(news) * .2)
news2 <- news[news_sample]
write(news2, "news2.txt")

# combine twitter, news, and blogs
corpus <- c(twitter2, news2, blogs2)
corpus2 <- Corpus(VectorSource(corpus), readerControl = list(reader = readPlain, language = 'en'))

# clean corpus
corpus2 <- tm_map(corpus2, removeNumbers)
corpus2 <- tm_map(corpus2, removePunctuation)
# corpus2 <- tm_map(corpus2, removeWords, stopwords("english")) # may not want to remove stopwords for predictive model
# corpus2 <- tm_map(corpus2, stemDocument, language = "english") also might not want to stem document for predictive model
# remove any custom words
# corpus2 <- tm_map(corpus2, removeWords, c("baseball", "football")) 
corpus2 <- tm_map(corpus2, stripWhitespace)
corpus2 <- tm_map(corpus2, tolower)
corpus3 <- str_replace_all(corpus2,"[^[:graph:]]", " ") 
corpus2 <- tm_map(corpus2, PlainTextDocument)

# create a DocumentTermMatrix corpus_dtm
corpus_dtm <- DocumentTermMatrix(corpus2)


