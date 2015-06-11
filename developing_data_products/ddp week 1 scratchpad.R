install.packages("shiny")
install.packages('Rcpp')
library(shiny)
runExample("01_hello")

runApp("App-1")
getwd()

## to run app in showcase mode showing ui and server scripts
runApp("App-1", display.mode = "showcase")

## Quiz 1

## q1
library(manipulate)
myPlot <- function(s) {
        plot(cars$dist - mean(cars$dist), cars$speed - mean(cars$speed))
        abline(0, s)
}


manipulate(myPlot(s), s = slider(0, 2, step = 0.1))
