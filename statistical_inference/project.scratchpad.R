ppois(10, lambda = 5 * 3)
rexp(n, lambda)
## The mean of exponential distribution is 1/lambda and the standard deviation is also also 1/lambda. 
## need t
?rexp
all.sample <- list()

for(i in 1:1000){
        sample <- rexp(40, .2)
        all.sample <- append(all.sample, sample)
}

head(all.sample)
all.sample
length(all.sample)

df.sample <- data.frame(all.sample)
head(df.sample)
str(all.sample)
?data.frame
x <- c(1, 2, 3)
str(x)
all.sample
z <- c(all.sample)
str(z)
z <- as.data.frame(all.sample)
nrow(z)
ncol(z)
df.samples <- data.frame()
names(df.samples) <- c("obs")
str(df.samples)
?t
df.samples <- t(z)
nrow(df.samples)
ncol(df.samples)

head(df.samples)
str(df.samples)



head(ToothGrowth)
str(ToothGrowth)
ToothGrowth
head(tooth)
tooth
tooth <- ToothGrowth
tooth.agg <- aggregate(tooth$len, list(supp = tooth$supp), mean)
tooth.agg

tooth.agg2 <- aggregate(tooth$len, list(supp = tooth$supp, dose = tooth$dose), mean)
tooth.agg2

summary(tooth)

vc.0.5 <- nrow(subset(tooth, tooth$supp))

sum.tooth <- ddply(tooth, c(""))
tooth.freq <- table(tooth$supp, tooth$dose)

x <- as.data.frame(tooth.freq)
names(x) <- c("supp", "dose", "count")
x

tooth.vc <- subset(tooth, tooth$supp == "VC")
library(plyr)
ddply(tooth, c("supp", "dose"), summarise, n = length(len), mean = mean(len), sd = sd(len), se = sd/sqrt(n))

cdata <- ddply(data, c("sex", "condition"), summarise,
               N    = length(change),
               mean = mean(change),
               sd   = sd(change),
               se   = sd / sqrt(N) )
summary(tooth)

t.test(len ~ supp, data = tooth)


qt(c(.025, .975), df=99)
