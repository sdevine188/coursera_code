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

# create tdm for unigrams


