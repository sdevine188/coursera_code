library(UsingR)
data(father.son)
install.packages("UsingR")
head(father.son)

x <- father.son$sheight
n <- length(x)
B <- 10000
resamples <- matrix(sample(x, n * B, replace = TRUE), B, n)
resampledMedians <- apply(resamples, 1, median)

?sample
?matrix
?apply
length(resampledMedians)

sd(resampledMedians)
quantile(resampledMedians, c(.025, .975))

medians <- data.frame(resampledMedians)
head(medians)
names(medians) <- "median"
ggplot(medians, aes(x = median)) + geom_histogram(binwidth = .01, color = "black", fill = "white")
ggplot(medians, aes(x = median)) + geom_histogram(aes(y = ..density..),      
               binwidth=.5,
               colour="black", fill="white") +
        geom_density(alpha=.2, fill="#FF6666")


baseline <- c(140, 138, 150, 148, 135)
week2 <- c(132, 135, 151, 146, 130)
df <- data.frame(baseline, week2)
head(df)
t.test(baseline, week2, paired = TRUE)

n <- 9
s <- 30
mean <- 1100

error <- qt(0.975,df=n-1) * s/sqrt(n)
mean + error
mean - error

?qt

binom.test(3, 4, .5, alternative = "greater")
ppois(.0056 , lamda = .01)

sqrt((1.5^2)/9 + (1.8^2)/9)
(1.5^2)/9 + (1.8^2)/9
sqrt(.61)
-3/.781025
