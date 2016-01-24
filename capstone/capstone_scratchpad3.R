library(tm)
library(SnowballC)
library(stringr)
library(dplyr)
library(ggplot2)
library(textir)
library(readr)

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

# read in sample twitter2
con <- file("twitter2.txt", "r")
twitter2 <- readLines(con, encoding = "UTF-8")
close(con)

twitter_df <- data.frame(twitter2)

# read in blogs data
con <- file("en_US/en_US.blogs.txt", "r")
blogs <- readLines(con, encoding = "UTF-8")
close(con)

length(blogs)
# blogs_sample <- sample(seq(1:length(blogs)), size = length(blogs) * .2)
blogs_sample <- sample(seq(1:length(blogs)), size = 100000)
blogs2 <- blogs[blogs_sample]
write(blogs2, "blogs2.txt")

# read in sample blogs2
con <- file("blogs2.txt", "r")
blogs2 <- readLines(con, encoding = "UTF-8")
close(con)

# read in news data
con <- file("en_US/en_US.news.txt", "r")
news <- readLines(con, encoding = "UTF-8")
close(con)

length(news)
news_sample <- sample(seq(1:length(news)), size = 77259)
news2 <- news[news_sample]
write(news2, "news2.txt")

# read in sample news2
con <- file("news2.txt", "r")
news2 <- readLines(con, encoding = "UTF-8")
close(con)

# create twitter corpus
twitter_corpus <- Corpus(VectorSource(twitter2), readerControl = list(reader = readPlain, language = 'en'))
as.character(twitter_corpus[[1]])

# create blogs corpus
blogs_corpus <- Corpus(VectorSource(blogs2), readerControl = list(reader = readPlain, language = 'en'))
as.character(blogs_corpus[[1]])

# create news corpus
news_corpus <- Corpus(VectorSource(news2), readerControl = list(reader = readPlain, language = 'en'))
as.character(news_corpus[[1]])

# clean twitter corpus
twitter_corpus <- tm_map(twitter_corpus, removeNumbers)
twitter_corpus <- tm_map(twitter_corpus, removePunctuation)
twitter_corpus <- tm_map(twitter_corpus, stripWhitespace)
twitter_corpus <- tm_map(twitter_corpus, tolower)
twitter_corpus <- tm_map(twitter_corpus, PlainTextDocument)

# clean blogs corpus
blogs_corpus <- tm_map(blogs_corpus, removeNumbers)
blogs_corpus <- tm_map(blogs_corpus, removePunctuation)
blogs_corpus <- tm_map(blogs_corpus, stripWhitespace)
blogs_corpus <- tm_map(blogs_corpus, tolower)
blogs_corpus <- tm_map(blogs_corpus, PlainTextDocument)

# clean news corpus
news_corpus <- tm_map(news_corpus, removeNumbers)
news_corpus <- tm_map(news_corpus, removePunctuation)
news_corpus <- tm_map(news_corpus, stripWhitespace)
news_corpus <- tm_map(news_corpus, tolower)
news_corpus <- tm_map(news_corpus, PlainTextDocument)

# create a bigram tdm for twitter_corpus
BigramTokenizer <- function(x) {
        unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}
twitter_bigram_tdm <- TermDocumentMatrix(twitter_corpus, control = list(tokenize = BigramTokenizer))
# inspect(twitter_bigram_tdm)
# inspect(twitter_bigram_tdm[1:10, 1:10])
twitter_bigram_tdm_row_sums <- as.matrix(slam::row_sums(twitter_bigram_tdm, na.rm=TRUE))
# convert to a dataframe
twitter_bigram  <- data.frame(term = rownames(twitter_bigram_tdm_row_sums), freq = twitter_bigram_tdm_row_sums[, 1])
rownames(twitter_bigram) <- NULL
twitter_bigram  <- arrange(twitter_bigram, desc(freq))
head(twitter_bigram)
write_csv(twitter_bigram, "twitter_bigram.csv")

# create trigram tdm for twitter_corpus
trigramTokenizer <- function(x) {
        unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
}
twitter_trigram_tdm <- TermDocumentMatrix(twitter_corpus, control = list(tokenize = trigramTokenizer))
twitter_trigram_tdm_row_sums <- as.matrix(slam::row_sums(twitter_trigram_tdm, na.rm=TRUE))
twitter_trigram  <- data.frame(term = rownames(twitter_trigram_tdm_row_sums), freq = twitter_trigram_tdm_row_sums[, 1])
rownames(twitter_trigram) <- NULL
twitter_trigram  <- arrange(twitter_trigram, desc(freq))
head(twitter_trigram)
write_csv(twitter_trigram, "twitter_trigram.csv")

# create a bigram tdm for blogs_corpus
BigramTokenizer <- function(x) {
        unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}
blogs_bigram_tdm <- TermDocumentMatrix(blogs_corpus, control = list(tokenize = BigramTokenizer))
# inspect(blogs_bigram_tdm)
# inspect(blogs_bigram_tdm[1:10, 1:10])
blogs_bigram_tdm_row_sums <- as.matrix(slam::row_sums(blogs_bigram_tdm, na.rm=TRUE))
# convert to a dataframe
blogs_bigram  <- data.frame(term = rownames(blogs_bigram_tdm_row_sums), freq = blogs_bigram_tdm_row_sums[, 1])
rownames(blogs_bigram) <- NULL
blogs_bigram  <- arrange(blogs_bigram, desc(freq))
head(blogs_bigram)
write_csv(blogs_bigram, "blogs_bigram.csv")

# create trigram tdm for blogs_corpus
trigramTokenizer <- function(x) {
        unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
}
blogs_trigram_tdm <- TermDocumentMatrix(blogs_corpus, control = list(tokenize = trigramTokenizer))
blogs_trigram_tdm_row_sums <- as.matrix(slam::row_sums(blogs_trigram_tdm, na.rm=TRUE))
blogs_trigram  <- data.frame(term = rownames(blogs_trigram_tdm_row_sums), freq = blogs_trigram_tdm_row_sums[, 1])
rownames(blogs_trigram) <- NULL
blogs_trigram  <- arrange(blogs_trigram, desc(freq))
head(blogs_trigram)
write_csv(blogs_trigram, "blogs_trigram.csv")

# create a bigram tdm for news_corpus
BigramTokenizer <- function(x) {
        unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}
news_bigram_tdm <- TermDocumentMatrix(news_corpus, control = list(tokenize = BigramTokenizer))
# inspect(news_bigram_tdm)
# inspect(news_bigram_tdm[1:10, 1:10])
news_bigram_tdm_row_sums <- as.matrix(slam::row_sums(news_bigram_tdm, na.rm=TRUE))
# convert to a dataframe
news_bigram  <- data.frame(term = rownames(news_bigram_tdm_row_sums), freq = news_bigram_tdm_row_sums[, 1])
rownames(news_bigram) <- NULL
news_bigram  <- arrange(news_bigram, desc(freq))
head(news_bigram)
write_csv(news_bigram, "news_bigram.csv")

# create trigram tdm for news_corpus
trigramTokenizer <- function(x) {
        unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
}
news_trigram_tdm <- TermDocumentMatrix(news_corpus, control = list(tokenize = trigramTokenizer))
news_trigram_tdm_row_sums <- as.matrix(slam::row_sums(news_trigram_tdm, na.rm=TRUE))
news_trigram  <- data.frame(term = rownames(news_trigram_tdm_row_sums), freq = news_trigram_tdm_row_sums[, 1])
rownames(news_trigram) <- NULL
news_trigram  <- arrange(news_trigram, desc(freq))
head(news_trigram)
write_csv(news_trigram, "news_trigram.csv")
