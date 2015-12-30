library(tm)
library(SnowballC)
library(stringr)
library(dplyr)
library(ggplot2)

setwd("C:/Users/Stephen/Desktop/R/coursera_code/capstone")

# read in twitter data
con <- file("en_US/en_US.twitter.txt", "r")
twitter <- readLines(con, encoding = "UTF-8")
close(con)

# read in blogs data
con <- file("en_US/en_US.blogs.txt", "r")
blogs <- readLines(con, encoding = "UTF-8")
close(con)

# read in news data
con <- file("en_US/en_US.news.txt", "r")
news <- readLines(con, encoding = "UTF-8")
close(con)

# convert character strings to corpus
twitter_corpus <- Corpus(VectorSource(twitter), readerControl = list(reader = readPlain, language = 'en'))
blogs_corpus <- Corpus(VectorSource(blogs), readerControl = list(reader = readPlain, language = 'en'))
news_corpus <- Corpus(VectorSource(news), readerControl = list(reader = readPlain, language = 'en'))

# exploratory analysis of corpus
inspect(news_corpus[1]) # view meta data on first document in corpus
as.character(news_corpus[[1]]) # view text of first document in corpus
writeLines(as.character(news_corpus[[1]])) # also views text, but without the quotes (not a big improvement)

# clean corpus
news_corpus2 <- tm_map(news_corpus, removeNumbers)
news_corpus2 <- tm_map(news_corpus2, removePunctuation)
# news_corpus2 <- tm_map(news_corpus2, removeWords, stopwords("english")) # may not want to remove stopwords for predictive model
# news_corpus2 <- tm_map(news_corpus2, stemDocument, language = "english") also might not want to stem document for predictive model

# remove any custom words
# news_corpus2 <- tm_map(news_corpus2, removeWords, c("baseball", "football")) 

news_corpus2 <- tm_map(news_corpus2, stripWhitespace)
news_corpus2 <- tm_map(news_corpus2, tolower)
news_corpus2 <- tm_map(news_corpus2, PlainTextDocument)

# create a DocumentTermMatrix
news_dtm <- DocumentTermMatrix(news_corpus2)
inspect(news_dtm)
inspect(news_dtm[1:50, 1:10])

# inspect dtm
# get term frequency by col sums
news_dtm_col_sums <- as.matrix(slam::col_sums(news_dtm, na.rm=TRUE))
# convert to a dataframe
news_freq <- data.frame(term = rownames(news_dtm_col_sums), freq = news_dtm_col_sums[, 1])
rownames(news_freq) <- NULL
news_freq <- arrange(news_freq, desc(freq))
head(news_freq)

# remove sparse terms from dtm
news_dtms <- removeSparseTerms(news_dtm, 0.9)   
inspect(news_dtms)
inspect(news_dtms[1:10, 1:10])

# find frequent terms
findFreqTerms(news_dtm, lowfreq=10000)
findFreqTerms(news_dtm, lowfreq=1000)

# create a TermDocumentMatrix
news_tdm <- TermDocumentMatrix(news_corpus2)
inspect(news_tdm)
inspect(news_tdm[1:50, 1:10])
inspect(news_tdm[11350:11360, 1:10])

# inspect tdm
# get term frequency by col sums
news_tdm_row_sums <- as.matrix(slam::row_sums(news_tdm, na.rm=TRUE))
# convert to a dataframe
news_freq2 <- data.frame(term = rownames(news_tdm_row_sums), freq = news_tdm_row_sums[, 1])
rownames(news_freq2) <- NULL
news_freq2 <- arrange(news_freq2, desc(freq))
head(news_freq2)

# create bigram tdm
BigramTokenizer <- function(x) {
                unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
        }
news_bigram_tdm <- TermDocumentMatrix(news_corpus2, control = list(tokenize = BigramTokenizer))

# inspect bigram tdm
inspect(news_bigram_tdm)
inspect(news_bigram_tdm[1000:1050, 1:10])
news_bigram_tdm_row_sums <- as.matrix(slam::row_sums(news_bigram_tdm, na.rm=TRUE))
# convert to a dataframe
news_bigram_freq <- data.frame(term = rownames(news_bigram_tdm_row_sums), freq = news_bigram_tdm_row_sums[, 1])
rownames(news_bigram_freq) <- NULL
news_bigram_freq <- arrange(news_bigram_freq, desc(freq))
head(news_bigram_freq)

# create trigram tdm
trigramTokenizer <- function(x) {
        unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
}
news_trigram_tdm <- TermDocumentMatrix(news_corpus2, control = list(tokenize = trigramTokenizer))

# inspect trigram tdm
inspect(news_trigram_tdm)
inspect(news_trigram_tdm[1000:1050, 1:10])
news_trigram_tdm_row_sums <- as.matrix(slam::row_sums(news_trigram_tdm, na.rm=TRUE))
# convert to a dataframe
news_trigram_freq <- data.frame(term = rownames(news_trigram_tdm_row_sums), freq = news_trigram_tdm_row_sums[, 1])
rownames(news_trigram_freq) <- NULL
news_trigram_freq <- arrange(news_trigram_freq, desc(freq))
head(news_trigram_freq)

# graph frequency of ngrams
# unigram bar chart
news_unigram_bar <- ggplot(data = news_freq[1:50, ], aes(x = reorder(term, freq), y = freq, fill = freq)) + 
        geom_bar(color = "black", stat = "identity") +
        coord_flip() + scale_fill_gradient(low = "lightskyblue", high = "darkblue")

# bigram bar chart
news_bigram_bar <- ggplot(data = news_bigram_freq[1:50, ], aes(x = reorder(term, freq), y = freq, fill = freq)) + 
        geom_bar(color = "black", stat = "identity") +
        coord_flip() + scale_fill_gradient(low = "lightskyblue", high = "darkblue")

# trigram bar chart
news_trigram_bar <- ggplot(data = news_trigram_freq[1:50, ], aes(x = reorder(term, freq), y = freq, fill = freq)) + 
        geom_bar(color = "black", stat = "identity") +
        coord_flip() + scale_fill_gradient(low = "lightskyblue", high = "darkblue")

# find most frequent ngrams for test phrases
test1 <- news_trigram_freq[grep("^the best ", news_trigram_freq$term), ]



