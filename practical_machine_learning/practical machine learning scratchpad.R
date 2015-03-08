install.packages("kernlab")
library(kernlab)

data(spam)
head(spam)
tail(spam)
names(spam)
head(spam$your)
which(names(spam) == "your")

library(ggplot2)

plot1 <- ggplot(spam, aes(x = your, color = type)) + geom_density() + 
        geom_vline(xintercept = .5, linetype = "dotted")
plot1


prediction <- ifelse(spam$your > .5, "p spam", "p non-spam")
table(prediction, spam$type)/length(spam$type)



