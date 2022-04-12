library(dplyr)
library(plyr)
library(ggplot2)

#jobData1 <- read.csv("project/output/jobdata1.csv")
#jobData2 <- read.csv("project/output/jobdata2.csv")
jobData3 <- read.csv("project/output/jobdata3.csv")

#View(jobData1)
#View(jobData2)
View(jobData3)


filter <- jobData3 %>% 
  filter(as.numeric(jobData3$평점) >= 3.0)

hist(jobData3$평점)
hist(filter$평점)

hire <- filter[,2:4]
hire <- unique(hire)
View(hire)
industry1 <- hire %>% 
  group_by(hire$산업군) %>%
  tally()
industry1
names(industry1) <- c('산업군','기업수')

hire2 <- unique(jobData3[,2:4])
View(hire2)
industry2 <- hire2 %>% 
  group_by(hire2$산업군) %>%
  tally()
industry2
names(industry2) <- c('산업군','기업수')

ggplot(data=industry,
       aes(x=산업군, y=기업수)) + 
  geom_point() +
  labs(title = "산업군별 평점 3.0이상 기업수")

boxplot(industry$기업수~industry$산업군,
        main = "산업군별 평점 3.0이상 기업수")

ggplot(data=totalIndustry,
       aes(x=산업군, y=비율)) + 
  geom_point() +
  labs(title = "산업군별 평점 3.0이상 기업수")

industry3 <- hire2 %>% 
  group_by(산업군) %>%
  dplyr::summarise(avg=round(mean(평점),1))

#비율로 바꾸기
totalIndustry <- cbind.data.frame(industry1,industry2,industry3)
totalIndustry <- totalIndustry[,-3]
totalIndustry <- totalIndustry[,-4]
names(totalIndustry) <- c('산업군','기업수','전체기업수','평균')
totalIndustry[,"비율"] <- round(as.numeric(totalIndustry$기업수)/as.numeric(totalIndustry$전체기업수)*100,1)
totalIndustry

g1 <- ggplot(data=totalIndustry, aes(x=산업군)) + labs(title = "기업 분포")
g1 <- g1  + scale_y_continuous(sec.axis = sec_axis(~./10,name="평점 평균"))
g1 <- g1 + geom_point(aes(y=비율), colour = "Red", show.legend = TRUE)
g1 <- g1 + geom_point(aes(y=평균*10),colour = "Blue", show.legend = TRUE)
g1 <- g1 + geom_text(aes(x=산업군, y=round(비율),colour = "비율", label=비율, hjust = 1.5))
g1 <- g1 + geom_text(aes(x=산업군, y=평균*10, colour = "평균", label=평균))
g1

gIT <- jobData3 %>% 
  filter(jobData3$산업군 == "IT/웹/통신")
hist(gIT$평점, breaks = seq(1,4.5,0.1))

gCsrt <- jobData3 %>% 
  filter(jobData3$산업군 == "건설업")
hist(gCsrt$평점, breaks = seq(1,4.5,0.1))

gEdu <- jobData3 %>% 
  filter(jobData3$산업군 == "교육업")
hist(gEdu$평점, breaks = seq(1,4.5,0.1))

gInA <- jobData3 %>% 
  filter(jobData3$산업군 == "기관/협회")
hist(gInA$평점, breaks = seq(1,4.5,0.1))

gMnD <- jobData3 %>% 
  filter(jobData3$산업군 == "미디어/디자인")
hist(gMnD$평점, breaks = seq(1,4.5,0.1))

gServ <- jobData3 %>% 
  filter(jobData3$산업군 == "서비스업")
hist(gServ$평점, breaks = seq(1,4.5,0.1))

gTnT<- jobData3 %>% 
  filter(jobData3$산업군 == "유통/무역/운송")
hist(gTnT$평점, breaks = seq(1,4.5,0.1))

gBank <- jobData3 %>% 
  filter(jobData3$산업군 == "은행/금융업")
hist(gBank$평점, breaks = seq(1,4.5,0.1))

gMnP <- jobData3 %>% 
  filter(jobData3$산업군 == "의료/제약/복지")
hist(gMnP$평점, breaks = seq(1,4.5,0.1))

gMnC <- jobData3 %>% 
  filter(jobData3$산업군 == "제조/화학")
hist(gMnC$평점, breaks = seq(1,4.5,0.1))

hist(jobData3$평점, breaks = seq(1,4.5,0.1))
