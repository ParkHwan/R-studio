rm(list = ls())
#문제1
library(showtext)
library(plotrix)
product_click <- read.table("data/product_click.log", sep=" ", col.names=c("time", "product"))
product_click$day <- as.Date(paste(substr(product_click$time, 1,4),"-",substr(product_click$time, 5,6),"-",substr(product_click$time, 7,8), sep = ""))
product_click$date <- weekdays(product_click$day)
product_click

date_click <- table(product_click$date)
date_click <- date_click[c(4,6,3,2,1,5)]
date_click <- data.frame(date_click)
names(date_click) <- c("date","click")

date_click

png(filename="output/clicklog4.png", height = 500, width = 700, bg = "white")
par(mfrow=c(1,2))
plot(date_click$click,
     type = "o",
     main="요일별 클릭수",
     col="orange",
     xlab="",
     ylab="",
     ylim=c(0, 300),
     family="cat",
     cex.main = 2,
     axes=F
     )
axis(1,at=1:6,lab=date_click$date,family="cat")
axis(2,ylim=c(0,300),family="cat")
barplot(date_click$click~date_click$date,
        col=topo.colors(6),
        family="cat",
        ylim=c(0, 300),
        xlab="",
        ylab="",
        main="요일별 클릭수",
        cex.main = 2
        )
dev.off()

#문제2
movie_review <- read.csv("output/movie_reviews3.csv")
movie_vpoint <- movie_review$vpoint[movie_review$vtitle =="모가디슈"]

png(filename="output/clicklog5.png", height = 500, width = 900, bg = "white")
par(mfrow=c(1,3))
hist(movie_vpoint,
     main = "모가디슈 영화 평점 히스토그램(auto)",
     family= 'maple',
     xlab = "평점",
     ylab = "평가자수",
     col = topo.colors(10),
     )
hist(movie_vpoint,
     main = "모가디슈 영화 평점 히스토그램(1~5,6~10)",
     family= 'maple',
     xlab = "평점",
     ylab = "평가자수",
     col = rainbow(10),
     breaks = 2
     )
hist(movie_vpoint,
     main = "모가디슈 영화 평점 히스토그램(1~5,6,7,8,9,10)",
     family= 'maple',
     xlab = "평점",
     ylab = "평가자수",
     col = rainbow(10),
     breaks = c(0,5,6,7,8,9,10),
     freq=T
     )
dev.off()

rm(list=ls())
#문제3
library(plyr)
one <- read.csv("data/one.csv")
one1 <- data.frame(one$구별, one$X1인가구)
names(one1) <- c("loc1","num1")

png(filename="output/clicklog6.png", height = 600, width = 700, bg = "white")
boxplot(one1$num1~one1$loc1,
        col = topo.colors(25),
        las = 2,
        xlab = "",
        ylab = "",
        family= 'maple',
        col.main = "orange",
        cex.main = 2,
        main = "구별 1인 가구")
dev.off()
