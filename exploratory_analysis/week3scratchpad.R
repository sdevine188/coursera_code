set.seed(1234)
par(mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
summary(x)
text(x + .1, y + .1, labels = as.character(1:12))
dataFrame <- data.frame(x = x, y = y)
names(dataFrame)
head(dataFrame)
nrow(dataFrame)
dist(dataFrame)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
str(hClustering)
hClustering
plot(hClustering)


myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){
        ## modifiction of plclust for plotting hclust objects *in colour*!
        ## Copyright Eva KF Chan 2009
        ## Arguments:
        ##    hclust:    hclust object
        ##    lab:        a character vector of labels of the leaves of the tree
        ##    lab.col:    colour for the labels; NA=default device foreground colour
        ##    hang:     as in hclust & plclust
        ## Side effect:
        ##    A display of hierarchical cluster with coloured leaf labels.
        y <- rep(hclust$height,2)
        x <- as.numeric(hclust$merge)
        y <- y[which(x<0)]
        x <- x[which(x<0)]
        x <- abs(x)
        y <- y[order(x)]
        x <- x[order(x)]
        plot( hclust, labels=FALSE, hang=hang, ... )
        text( x=x, y=y[hclust$order]-(max(hclust$height)*hang), labels=lab[hclust$order], col=lab.col[hclust$order], srt=90, adj=c(1,0.5), xpd=NA, ... )
}

myplclust(hClustering, lab = rep(1:3, each = 4), lab.col = rep(1:3, each = 4))

dataFrame <- data.frame(x = x, y = y)
set.seed(143)
dataMatrix <- as.matrix(dataFrame) [sample(1:12), ]
heatmap(dataMatrix)
dataMatrix


kmeansObj <- kmeans(dataFrame, centers = 4) 
names(kmeansObj)
kmeansObj$cluster
kmeansObj$centers
par(mar = rep(0.2, 4))
plot(x, y, col = kmeansObj$cluster, pch = 19, cex = 2)
points(kmeansObj$centers, col = 1:4, pch = 3, cex = 3, lwd = 3)


set.seed(12345)
par(mar = rep(0.2, 4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[ , nrow(dataMatrix):1])
names(dataMatrix)
nrow(dataMatrix)
head(dataMatrix)
dataMatrix
heatmap(dataMatrix)

set.seed(678910)
for (i in 1:40) {
        # flip a coin
        coinFlip <- rbinom(1, size = 1, prob = 0.5)
        # if coin is heads add a common pattern to that row
        if (coinFlip) {
                dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 3), each = 5)
        }
}

image(1:10, 1:40, t(dataMatrix)[ , nrow(dataMatrix):1])
heatmap(dataMatrix)




