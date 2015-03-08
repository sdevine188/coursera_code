library(lattice)
library(datasets)
xyplot(Ozone ~ Wind, data = airquality)

airquality <- transform(airquality, Month = factor(Month))
xyplot(Ozone ~ Wind | Month * Temp, data = airquality, layout = c(5, 1))
windows()
names(airquality)

library(ggplot2)
str(mpg)
head(mpg)
windows()
qplot(displ, hwy, data = mpg, geom = c("point", "smooth"))
qplot(hwy, data = mpg, fill = drv)
qplot(hwy, data = mpg, geom = "density"))
qplot(displ, hwy, data = mpg, facets = . ~ drv)
qplot(hwy, data = mpg, facets = drv ~ ., binwidth = 2)


library(nlme)
library(lattice)
xyplot(weight ~ Time | Diet, BodyWeight)
str(Bodyweight)


library(datasets)
data(airquality)
names(airquality)
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5, 1))

qplot(Wind, Ozone, data = airquality, facets = . ~ Month)
