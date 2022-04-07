#문제1
rm(list = ls())
library(dplyr)
library(fmsb) 
picherStats <- read.csv("data/picher_stats_2017.csv", encoding = "UTF-8")
yangstats <- picherStats %>%
                filter(선수명 == "양현종") %>%
                select(선수명, 삼진.9, 볼넷.9, 홈런.9)
colnames(yangstats) <- c('선수명','삼진', '볼넷', '홈런')
rownames(yangstats) <- yangstats$선수명
yangstats <- yangstats[,-1]
max.score <- rep(10,3)          
min.score <- rep(0,3)            
yangstats <- rbind(max.score,min.score, yangstats)
yangstats

png("output/lab20.png", width = 800, height = 500)
radarchart(yangstats,
           pfcol=rgb(0.2,0.5,0.5,0.5),
           plwd=3,
           cglty=1,
           cglwd=0.8,
           axistype=1,
           seg=4,
           axislabcol='grey',
           caxislabels=seq(0,10,2.5),
           title = "양현종 선수의 성적"
           )
dev.off()

#문제2
iris <- iris

#(1)
mean(iris$Sepal.Width)
sd(iris$Sepal.Width)
quantile(iris$Sepal.Width, 0.75)

#(2)
hist(iris$Sepal.Width,
     main = "꽃받침 너비",
     xlab = "Sepal.Width")

#(3)
irismin <- aggregate(iris$Sepal.Width,
                     by=list("품종"=iris$Species),min)
irismin

#(4)
irissd <- aggregate(iris$Sepal.Width,
                       list(iris$Species),sd)
irissd
irismax$x - irismin$x
# 가장 범위가 큰 품종은 setosa이다

#(5)
test <- iris %>%
  filter(Species == "virginica")
test <- test[,1:4]

#(6)
cor(test)
# Sepal.Length와 나머지 변수는 양의 상관관계를 가지며
# (단,Petal.Width는 아주 약한 양의 상관관계를 가진다)
# Sepal.Length와 가장 상관관계가 높은 변수는 Petal.Length이다.