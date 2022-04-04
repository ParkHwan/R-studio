#문제1
library(ggplot2)
library(dplyr)
data1 <- read.csv("data/성적2.csv", encoding="UTF-8")
View(data1)
boxplot(data1$국어,
        data1$수학,
        names= c("국어", "수학"),
        range= 1)
data1$수학 <- ifelse(data1$수학 > 10, round(mean(data1$수학[!is.na(data1$수학)])), data1$수학)
View(data1)
data1$수학 <- ifelse(is.na(data1$수학), 7 , data1$수학)
data1$국어 <- ifelse(is.na(data1$국어), 7 , data1$국어)
View(data1)

par(mfrow=c(1,2))
ggplot(data = data1, aes(x = 국어, y = 수학)) + 
  geom_point(aes(color=성명),
             size = 3)
ggsave("output/result5.png")

#문제2
library(tidyr)
library(dplyr)

reshapedata <- read.csv("data/reshapedata.csv")
View(reshapedata)

longdata <- reshapedata %>%
  gather(exam, jumsu, starts_with('math'),
         starts_with('eng'))
View(longdata)

widedata <- longdata %>%
  spread(exam, jumsu)
View(widedata)

result <- separate(longdata, exam,
           into = c('subname', 'subnum'))
View(result)

#문제3
library(tm)
library(proxy)

fruit <- NULL
듀크 <- c("사과 포도 망고")
둘리 <- c("포도 자몽 자두")
또치 <- c("복숭아 사과 포도")
도우너 <- c("오렌지 바나나 복숭아")
길동 <- c("포도 바나나 망고")
희동 <- c("포도 귤 오렌지")
fruit <- c(듀크, 둘리, 또치, 도우너, 길동, 희동)
fruit
cps <- VCorpus(VectorSource(fruit))
dtm <- DocumentTermMatrix(cps)
m <- as.matrix(dtm)
doccom <- m %*% t(m)
dist(doccom, method = "cosine")
dist(doccom, method = "Euclidean")
simil(doccom, method = "cosine")
simil(doccom, method = "Euclidean")
