library(shiny)
library(stringr)
library(readr)
library(dplyr)

# load bigram frequency data frame
bigram <- read_csv("data/bigram.csv")

# load trigram frequency data frame
trigram <- read_csv("data/trigram.csv")

shinyServer(
        function(input, output){
                output$prediction_output <- eventReactive(input$predict_button, {
                        text_input <- input$text_input
                        text_split <- str_split(text_input, " ")
                        text_split <- unlist(text_split)
                        text_length <- length(text_split)
                        if(text_length > 1){
                                text_last_two_index <- seq(text_length - 1, text_length)
                                text_last_two_words <- str_c(text_split[text_last_two_index], collapse = " ")
                                trigram_match_index <- grep(str_c("^", text_last_two_words, " ", sep = ""), trigram$term)
                                trigram_match <- trigram[trigram_match_index, ]
                                trigram_match <- arrange(trigram_match, desc(freq))
                                output_split <- str_split(trigram_match$term[1], " ")
                                return(unlist(output_split)[3])                
                        }
                        if(text_length == 1){
                                bigram_match_index <- grep(str_c("^", text_input, sep = ""), bigram$term)
                                bigram_match <- bigram[bigram_match_index, ]
                                bigram_match <- arrange(bigram_match, desc(freq))
                                output_split <- str_split(bigram_match$term[1], " ")
                                return(unlist(output_split)[2])   
                        }
                })
                
        }
)