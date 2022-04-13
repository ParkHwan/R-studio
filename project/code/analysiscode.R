rm(list=ls())

library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)

jobData <- read.csv("project/output/jobdata3.csv")

View(jobData)

df <- jobData %>%
  group_by(jobData$평점) %>%
  dplyr::summarise(기업수 = n())
names(df) <- c("평점","기업수")
df$평점 <- as.character(df$평점)
H1 <- hist(jobData$평점,
     main = "평점별 기업 분포",
     xlab = "평점",
     ylab = "기업 수",
     ylim = c(0,700),
     breaks = seq(1,4.5,0.1),
     )
text(H1$mids,H1$counts,H1$counts,adj=c(0.5,-0.5),cex=0.8) 
df
gp <- ggplot(df, aes(x=평점,y=기업수))
gp <- gp + geom_bar(stat = "identity",)
gp <- gp + coord_flip()
gp1 <- gp + geom_text(aes(label=기업수),hjust= -0.1)
gp1 <- gp1 + ggtitle("평점별 분포도")
gp1

filter <- jobData %>% 
  filter(as.numeric(jobData$평점) >= 3.0)
hist(filter$평점)

hire <- filter[,2:4]
hire <- unique(hire)
View(hire)
industry1 <- hire %>% 
  group_by(hire$산업군) %>%
  tally()
industry1
names(industry1) <- c('산업군','기업수')

hire2 <- unique(jobData[,2:4])
View(hire2)
industry2 <- hire2 %>% 
  group_by(hire2$산업군) %>%
  tally()
industry2
names(industry2) <- c('산업군','기업수')

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

ggplot(data=totalIndustry,
       aes(x=산업군, y=비율)) + 
  geom_bar(stat = "identity") +
  labs(title = "산업군별 평점 3.0이상 기업수")

?ggplot
g1 <- ggplot(data=totalIndustry, aes(x=산업군),y) + labs(title = "산업별 기업 분포")
g1 <- g1  + scale_y_continuous(sec.axis = sec_axis(~./10,name="평점 평균"))
g1 <- g1 + geom_point(aes(y=비율), colour = "Red", show.legend = TRUE)
g1 <- g1 + geom_point(aes(y=평균*10),colour = "Blue", show.legend = TRUE)
g1 <- g1 + geom_text(aes(x=산업군, y=round(비율),colour = "3.0 기업 비율", label=비율, hjust = -0.5))
g1 <- g1 + geom_text(aes(x=산업군, y=평균*10, colour = "평균", label=평균, hjust = 1.5))
g1 <- g1 + ylab("3.0 기업 비율")
g1 <- g1 + theme(axis.text.x = element_text(size=7))
g1
?element_text

jobDataA <- jobData[,-1]
jobDataA <- jobDataA[,-4:-8]

emp <- jobDataA %>%
  group_by(직원수) %>%
  dplyr::summarise(n = n())

emp <- na.omit(emp)
emp <- emp[-1,]
emp <- emp[c(3,5,7,9,2,4,6,8,1),]
emp
View(emp)
ggplot(data = emp, aes(x=직원수, y=n)) + 
  geom_bar(stat="identity") +
  scale_x_discrete(limits=c("1명 이상 ~ 20명 미만",
                            "20명 이상 ~ 40명 미만",
                            "40명 이상 ~ 70명 미만",
                            "70명 이상 ~ 100명 미만",
                            "100명 이상 ~ 200명 미만",
                            "200명 이상 ~ 400명 미만",
                            "400명 이상 ~ 700명 미만",
                            "700명 이상 ~ 1000명 미만",
                            "1,000명 이상")) +
  ggtitle("직원수별 분포도")+
  coord_flip() +
  geom_text(aes(label=n),hjust= -0.1) +
  ylab("기업수")

NewjobData <- na.omit(unique(jobDataA))
View(NewjobData)
NewjobData <- NewjobData %>%
  filter(직원수 != "-")
View(NewjobData)



NewjobData <- NewjobData %>%
mutate(직원지수 = ifelse(직원수 == "1명 이상 ~ 20명 미만" , 1,
                          ifelse(직원수 == "20명 이상 ~ 40명 미만",2,
                                    ifelse(직원수 == "40명 이상 ~ 70명 미만",3,
                                              ifelse(직원수 == "70명 이상 ~ 100명 미만",4,
                                                        ifelse(직원수 == "100명 이상 ~ 200명 미만",5,
                                                                  ifelse(직원수 == "200명 이상 ~ 400명 미만",6,
                                                                            ifelse(직원수 == "400명 이상 ~ 700명 미만",7,
                                                                                      ifelse(직원수 == "700명 이상 ~ 1000명 미만" ,8,9
                                                                                      )))))))))
str(NewjobData)

NewjobData$New매출액 <- ifelse(NewjobData$X2020년매출액.천원. == "~ 5억원 이하",250000,
                                      ifelse(NewjobData$X2020년매출액.천원. == "5억원 ~ 10억원",750000,
                                             ifelse(NewjobData$X2020년매출액.천원. == "10억원 ~ 50억원",3000000,
                                                    ifelse(NewjobData$X2020년매출액.천원. == "50억원 ~ 100억원",7500000,
                                                           ifelse(NewjobData$X2020년매출액.천원. == "100억원 ~ 500억원",30000000,
                                                                  ifelse(NewjobData$X2020년매출액.천원. == "500억원 ~ 1000억원",75000000,
                                                                         ifelse(NewjobData$X2020년매출액.천원. == "1000억원 ~ 5000억원",300000000,
                                                                                ifelse(NewjobData$X2020년매출액.천원. == "5000억 이상 ~",500000000,NewjobData$X2020년매출액.천원.)
                                                    )))))))
NewjobData$New매출액 <- as.numeric(gsub(",","",NewjobData$New매출액))
View(NewjobData)
NewjobData <- NewjobData %>%
  mutate(매출액범위 = ifelse(New매출액< 500000, "5억원 미만",
                         ifelse(New매출액< 1000000,"5억원 ~ 10억원",
                              ifelse(New매출액< 5000000,"10억원 ~ 50억원",
                                    ifelse(New매출액< 10000000,"50억원 ~ 100억원",
                                           ifelse(New매출액< 50000000,"100억원 ~ 500억원",
                                                  ifelse(New매출액< 100000000,"500억원 ~ 1000억원",
                                                        ifelse(New매출액< 500000000,"1000억원 ~ 5000억원",
                                                              ifelse(New매출액< 1000000000,"5000억원~1조", "1조 이상"
                                                              )))))))))

# NewjobData <- NewjobData %>%
#   mutate(매출액지수 = ifelse(New매출액== "5억원 미만",1,
#                          ifelse(New매출액 == "5억원 ~ 10억원",2,
#                                 ifelse(New매출액 == "10억원 ~ 50억원",3,
#                                        ifelse(New매출액 == "50억원 ~ 100억원",4,
#                                               ifelse(New매출액 == "100억원 ~ 500억원",5,
#                                                      ifelse(New매출액 == "500억원 ~ 1000억원",6,
#                                                             ifelse(New매출액 == "1000억원 ~ 5000억원",7,
#                                                                    ifelse(New매출액 == "5000억원~1조",8,9
#                                                                    )))))))))

str(NewjobData)
NewjobData <- NewjobData %>%
  separate(입사율.연간.입사자., c("입사율","입사자수"),"%")
NewjobData <- NewjobData %>%
  separate(퇴사율.연간.퇴사자, c("퇴사율","퇴사자수"),"%")
NewjobData$입사자수 <- gsub("[[:punct:]]","",NewjobData$입사자수)
NewjobData$입사자수 <- gsub(" 명","",NewjobData$입사자수)
NewjobData$퇴사자수 <- gsub("[[:punct:]]","",NewjobData$퇴사자수)
NewjobData$퇴사자수 <- gsub(" 명","",NewjobData$퇴사자수)
NewjobData$입사자수 <- as.numeric(NewjobData$입사자수)
NewjobData$퇴사자수 <- as.numeric(NewjobData$퇴사자수)

NewjobData <- NewjobData %>%
  mutate(퇴사지수 = (입사자수-퇴사자수)/(입사자수+퇴사자수))
NewjobData <- NewjobData %>%
  mutate(퇴사비율 = 입사자수/퇴사자수)
View(NewjobData)

profit <- NewjobData %>%
  group_by(매출액범위) %>%
  dplyr::summarise(n = n())
profit

hist


#전체그래프
ggplot(data = profit, aes(x=매출액범위, y=n)) + 
  geom_bar(stat="identity") +
  scale_x_discrete(limits=c("5억원 미만",
                            "5억원 ~ 10억원",
                            "10억원 ~ 50억원",
                            "50억원 ~ 100억원",
                            "100억원 ~ 500억원",
                            "500억원 ~ 1000억원",
                            "1000억원 ~ 5000억원",
                            "5000억원~1조",
                            "1조 이상")) +
  ggtitle("매출액별 분포도")+
  coord_flip() +
  geom_text(aes(label=n),hjust= -0.1) +
  ylab("기업수")

ggplot(data=NewjobData, aes(x=평점, y=퇴사지수)) + 
  geom_point()
ggplot(data=NewjobData, aes(x=평점, y=매출액범위)) + 
  geom_point()+
  scale_y_discrete(limits=c("5억원 미만",
                            "5억원 ~ 10억원",
                            "10억원 ~ 50억원",
                            "50억원 ~ 100억원",
                            "100억원 ~ 500억원",
                            "500억원 ~ 1000억원",
                            "1000억원 ~ 5000억원",
                            "5000억원~1조",
                            "1조 이상"))

  

NewjobDataA <- NewjobData[-2524,]
NewjobDataA <- NewjobDataA[-126,]

ggplot(data=NewjobDataA, aes(x=평점, y=log(New매출액))) + 
  geom_point()+
  ylab("매출액")+
  ggtitle("평점 대비 매출액")
fitprof <- lm(log(New매출액) ~ 평점, data=NewjobDataA)

summary(fitprof)
ggplot(data=NewjobData, aes(x=평점, y=직원수)) + 
  geom_point() + 
  scale_y_discrete(limits=c("1명 이상 ~ 20명 미만",
                            "20명 이상 ~ 40명 미만",
                            "40명 이상 ~ 70명 미만",
                            "70명 이상 ~ 100명 미만",
                            "100명 이상 ~ 200명 미만",
                            "200명 이상 ~ 400명 미만",
                            "400명 이상 ~ 700명 미만",
                            "700명 이상 ~ 1000명 미만",
                            "1,000명 이상"))+
  ggtitle("평점 대비 직원수")

ggplot(data=NewjobData, aes(x=New매출액, y=직원수)) + 
  geom_point() + 
  scale_x_discrete(limits=c("5억원 미만",
                            "5억원 ~ 10억원",
                            "10억원 ~ 50억원",
                            "50억원 ~ 100억원",
                            "100억원 ~ 500억원",
                            "500억원 ~ 1000억원",
                            "1000억원 ~ 5000억원",
                            "5000억원~1조",
                            "1조 이상"))+
  scale_y_discrete(limits=c("1명 이상 ~ 20명 미만",
                            "20명 이상 ~ 40명 미만",
                            "40명 이상 ~ 70명 미만",
                            "70명 이상 ~ 100명 미만",
                            "100명 이상 ~ 200명 미만",
                            "200명 이상 ~ 400명 미만",
                            "400명 이상 ~ 700명 미만",
                            "700명 이상 ~ 1000명 미만",
                            "1,000명 이상"))
ggplot(data=NewjobDataA, aes(x=평점, y=퇴사지수)) + 
  geom_point()+
  ggtitle("평점 대비 퇴사지수")



fit <- lm(New매출액 ~ 평점, data=NewjobData[-126,])
summary(fit)

# 산업군별
#IT
gIT <- NewjobData %>% 
  filter(NewjobData$산업군 == "IT/웹/통신" & NewjobData$평점 >= 3 )
gIT
ggplot(data=gIT, aes(x=평점, y=log(New매출액))) + 
  geom_point()
ggplot(data=gIT, aes(x=직원지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gIT, aes(x=퇴사지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gIT, aes(x=평점, y=퇴사지수)) + 
  geom_point()
fitIT <- lm(평점 ~퇴사지수, data=gIT)
summary(fitIT)

hist(gIT$평점, breaks = seq(1,4.5,0.1))

#건설업
gCsrt <- NewjobData %>% 
  filter(NewjobData$산업군 == "건설업")
ggplot(data=gCsrt, aes(x=평점, y=log(New매출액))) + 
  geom_point()
ggplot(data=gCsrt, aes(x=직원지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gCsrt, aes(x=평점, y=퇴사지수)) + 
  geom_point()
ggplot(data=gCsrt, aes(x=평점, y=퇴사비율)) + 
  geom_point()


hist(gCsrt$평점, breaks = seq(1,4.5,0.1))

#교육업
gEdu <- NewjobData %>% 
  filter(NewjobData$산업군 == "교육업")
ggplot(data=gEdu, aes(x=평점, y=log(New매출액))) + 
  geom_point()
ggplot(data=gEdu, aes(x=직원지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gEdu, aes(x=평점, y=퇴사지수)) + 
  geom_point()
ggplot(data=gEdu, aes(x=평점, y=퇴사비율)) + 
  geom_point()
hist(gEdu$평점, breaks = seq(1,4.5,0.1))

#기관/협회
gInA <- NewjobData %>% 
  filter(NewjobData$산업군 == "기관/협회")
ggplot(data=gInA, aes(x=평점, y=log(New매출액))) + 
  geom_point()
ggplot(data=gInA, aes(x=직원지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gInA, aes(x=평점, y=퇴사지수)) + 
  geom_point()
ggplot(data=gInA, aes(x=평점, y=퇴사비율)) + 
  geom_point()
hist(gInA$평점, breaks = seq(1,4.5,0.1))

#미디어/디자인
gMnD <- NewjobData %>% 
  filter(NewjobData$산업군 == "미디어/디자인")
ggplot(data=gMnD, aes(x=평점, y=log(New매출액))) + 
  geom_point()
ggplot(data=gMnD, aes(x=직원지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gMnD, aes(x=평점, y=퇴사지수)) + 
  geom_point()
ggplot(data=gMnD, aes(x=평점, y=퇴사비율)) + 
  geom_point()
hist(gMnD$평점, breaks = seq(1,4.5,0.1))

#서비스업
gServ <- NewjobData %>% 
  filter(NewjobData$산업군 == "서비스업")
ggplot(data=gServ, aes(x=평점, y=log(New매출액))) + 
  geom_point()
ggplot(data=gServ, aes(x=직원지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gServ, aes(x=평점, y=퇴사지수)) + 
  geom_point()
ggplot(data=gServ, aes(x=평점, y=퇴사비율)) + 
  geom_point()
hist(gServ$평점, breaks = seq(1,4.5,0.1))

#유통/무역/운송
gTnT<- NewjobData %>% 
  filter(NewjobData$산업군 == "유통/무역/운송")
ggplot(data=gTnT, aes(x=평점, y=log(New매출액))) + 
  geom_point()
ggplot(data=gTnT, aes(x=직원지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gTnT, aes(x=평점, y=퇴사지수)) + 
  geom_point()
ggplot(data=gTnT, aes(x=평점, y=퇴사비율)) + 
  geom_point()
hist(gTnT$평점, breaks = seq(1,4.5,0.1))

#은행/금융업
gBank <- NewjobData %>% 
  filter(NewjobData$산업군 == "은행/금융업")
ggplot(data=gBank, aes(x=평점, y=log(New매출액))) + 
  geom_point()
ggplot(data=gBank, aes(x=직원지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gBank, aes(x=평점, y=퇴사지수)) + 
  geom_point()
ggplot(data=gBank, aes(x=평점, y=퇴사비율)) + 
  geom_point()
hist(gBank$평점, breaks = seq(1,4.5,0.1))

#의료/제약/복지
gMnP <- NewjobData %>% 
  filter(NewjobData$산업군 == "의료/제약/복지")
ggplot(data=gMnP, aes(x=평점, y=log(New매출액))) + 
  geom_point()
ggplot(data=gMnP, aes(x=직원지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gMnP, aes(x=평점, y=퇴사지수)) + 
  geom_point()
ggplot(data=gMnP, aes(x=평점, y=퇴사비율)) + 
  geom_point()
hist(gMnP$평점, breaks = seq(1,4.5,0.1))

#제조/화학
gMnC <- NewjobData %>% 
  filter(NewjobData$산업군 == "제조/화학")
ggplot(data=gMnC, aes(x=평점, y=log(New매출액))) + 
  geom_point()
ggplot(data=gMnC, aes(x=직원지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gMnC, aes(x=평점, y=퇴사지수)) + 
  geom_point()
ggplot(data=gMnC, aes(x=평점, y=퇴사비율)) + 
  geom_point()
hist(gMnC$평점, breaks = seq(1,4.5,0.1))

hist(NewjobData$평점, breaks = seq(1,4.5,0.1))

#구인중인 기업 대상

jobData2 <- read.csv("project/output/jobdata3.csv")
jobData2 <- na.omit(jobData2)
jobData2 <- jobData2[,-1]
jobData2 <- jobData2[,-4:-8]
jobData2 <- unique(jobData2)
View(jobData2)

jobData2 <- jobData2 %>%
  filter(직원수 != "-")
View(jobData2)



jobData2 <- jobData2 %>%
  mutate(직원지수 = ifelse(직원수 == "1명 이상 ~ 20명 미만" , 1,
                          ifelse(직원수 == "20명 이상 ~ 40명 미만",2,
                                    ifelse(직원수 == "40명 이상 ~ 70명 미만",3,
                                              ifelse(직원수 == "70명 이상 ~ 100명 미만",4,
                                                        ifelse(직원수 == "100명 이상 ~ 200명 미만",5,
                                                                  ifelse(직원수 == "200명 이상 ~ 400명 미만",6,
                                                                            ifelse(직원수 == "400명 이상 ~ 700명 미만",7,
                                                                                      ifelse(직원수 == "700명 이상 ~ 1000명 미만" ,8,9
                                                                                      )))))))))


jobData2$New매출액 <- ifelse(jobData2$X2020년매출액.천원. == "~ 5억원 이하",250000,
                            ifelse(jobData2$X2020년매출액.천원. == "5억원 ~ 10억원",750000,
                                   ifelse(jobData2$X2020년매출액.천원. == "10억원 ~ 50억원",3000000,
                                          ifelse(jobData2$X2020년매출액.천원. == "50억원 ~ 100억원",7500000,
                                                 ifelse(jobData2$X2020년매출액.천원. == "100억원 ~ 500억원",30000000,
                                                        ifelse(jobData2$X2020년매출액.천원. == "500억원 ~ 1000억원",75000000,
                                                               ifelse(jobData2$X2020년매출액.천원. == "1000억원 ~ 5000억원",300000000,
                                                                      ifelse(jobData2$X2020년매출액.천원. == "5000억 이상 ~",500000000,jobData2$X2020년매출액.천원.)
                                                               )))))))
jobData2$New매출액 <- as.numeric(gsub(",","",jobData2$New매출액))
jobData2 <- unique(jobData2[,-4])

jobData2 <- jobData2 %>%
  separate(입사율.연간.입사자., c("입사율","입사자수"),"%")
jobData2 <- jobData2 %>%
  separate(퇴사율.연간.퇴사자, c("퇴사율","퇴사자수"),"%")
jobData2$입사자수 <- gsub("[[:punct:]]","",jobData2$입사자수)
jobData2$입사자수 <- gsub(" 명","",jobData2$입사자수)
jobData2$퇴사자수 <- gsub("[[:punct:]]","",jobData2$퇴사자수)
jobData2$퇴사자수 <- gsub(" 명","",jobData2$퇴사자수)
jobData2$입사자수 <- as.numeric(jobData2$입사자수)
jobData2$퇴사자수 <- as.numeric(jobData2$퇴사자수)
jobData2 <- jobData2 %>%
  mutate(퇴사지수 = (입사자수-퇴사자수)/(입사자수+퇴사자수))
jobData2 <- jobData2 %>%
  mutate(퇴사비율 = 입사자수/퇴사자수)
View(jobData2)

# 산업군별
#IT
gIT2 <- jobData2 %>% 
  filter(jobData2$산업군 == "IT/웹/통신")
ggplot(data=gIT2, aes(x=평점, y=log(New매출액))) + 
  geom_point()
ggplot(data=gIT2, aes(x=직원지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gIT2, aes(x=퇴사지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gIT2, aes(x=평점, y=퇴사지수)) + 
  geom_point()
View(gIT2)
fitIT <- lm(평점 ~퇴사지수, data=gIT2)
summary(fitIT)

hist(gIT2$평점, breaks = seq(1,4.5,0.1))

#건설업
gCsrt2 <- jobData2 %>% 
  filter(jobData2$산업군 == "건설업")
ggplot(data=gCsrt2, aes(x=평점, y=log(New매출액))) + 
  geom_point()
ggplot(data=gCsrt2, aes(x=직원지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gCsrt2, aes(x=평점, y=퇴사지수)) + 
  geom_point()
ggplot(data=gCsrt2, aes(x=평점, y=퇴사비율)) + 
  geom_point()


hist(gCsrt2$평점, breaks = seq(1,4.5,0.1))

#교육업
gEdu2 <- jobData2 %>% 
  filter(jobData2$산업군 == "교육업")
ggplot(data=gEdu2, aes(x=평점, y=log(New매출액))) + 
  geom_point()
ggplot(data=gEdu2, aes(x=직원지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gEdu2, aes(x=평점, y=퇴사지수)) + 
  geom_point()
ggplot(data=gEdu2, aes(x=평점, y=퇴사비율)) + 
  geom_point()
hist(gEdu2$평점, breaks = seq(1,4.5,0.1))

#기관/협회
gInA2 <- jobData2 %>% 
  filter(jobData2$산업군 == "기관/협회")
ggplot(data=gInA2, aes(x=평점, y=log(New매출액))) + 
  geom_point()
ggplot(data=gInA2, aes(x=직원지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gInA2, aes(x=평점, y=퇴사지수)) + 
  geom_point()
ggplot(data=gInA2, aes(x=평점, y=퇴사비율)) + 
  geom_point()
hist(gInA2$평점, breaks = seq(1,4.5,0.1))

#미디어/디자인
gMnD2 <- jobData2 %>% 
  filter(jobData2$산업군 == "미디어/디자인")
ggplot(data=gMnD2, aes(x=평점, y=log(New매출액))) + 
  geom_point()
ggplot(data=gMnD2, aes(x=직원지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gMnD2, aes(x=평점, y=퇴사지수)) + 
  geom_point()
ggplot(data=gMnD2, aes(x=평점, y=퇴사비율)) + 
  geom_point()
hist(gMnD2$평점, breaks = seq(1,4.5,0.1))

#서비스업
gServ2 <- jobData2 %>% 
  filter(jobData2$산업군 == "서비스업")
ggplot(data=gServ2, aes(x=평점, y=log(New매출액))) + 
  geom_point()
ggplot(data=gServ2, aes(x=직원지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gServ2, aes(x=평점, y=퇴사지수)) + 
  geom_point()
ggplot(data=gServ2, aes(x=평점, y=퇴사비율)) + 
  geom_point()
hist(gServ2$평점, breaks = seq(1,4.5,0.1))

#유통/무역/운송
gTnT2<- jobData2 %>% 
  filter(jobData2$산업군 == "유통/무역/운송")
ggplot(data=gTnT2, aes(x=평점, y=log(New매출액))) + 
  geom_point()
ggplot(data=gTnT2, aes(x=직원지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gTnT2, aes(x=평점, y=퇴사지수)) + 
  geom_point()
ggplot(data=gTnT2, aes(x=평점, y=퇴사비율)) + 
  geom_point()
hist(gTnT2$평점, breaks = seq(1,4.5,0.1))

#은행/금융업
gBank2 <- jobData2 %>% 
  filter(jobData2$산업군 == "은행/금융업")
ggplot(data=gBank2, aes(x=평점, y=log(New매출액))) + 
  geom_point()
ggplot(data=gBank2, aes(x=직원지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gBank2, aes(x=평점, y=퇴사지수)) + 
  geom_point()
ggplot(data=gBank2, aes(x=평점, y=퇴사비율)) + 
  geom_point()
hist(gBank2$평점, breaks = seq(1,4.5,0.1))

#의료/제약/복지
gMnP2 <- jobData2 %>% 
  filter(jobData2$산업군 == "의료/제약/복지")
ggplot(data=gMnP2, aes(x=평점, y=log(New매출액))) + 
  geom_point()
ggplot(data=gMnP2, aes(x=직원지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gMnP2, aes(x=평점, y=퇴사지수)) + 
  geom_point()
ggplot(data=gMnP2, aes(x=평점, y=퇴사비율)) + 
  geom_point()
hist(gMnP2$평점, breaks = seq(1,4.5,0.1))

#제조/화학
gMnC2 <- jobData2 %>% 
  filter(jobData2$산업군 == "제조/화학")
ggplot(data=gMnC2, aes(x=평점, y=log(New매출액))) + 
  geom_point()
ggplot(data=gMnC2, aes(x=직원지수, y=log(New매출액))) + 
  geom_point()
ggplot(data=gMnC2, aes(x=평점, y=퇴사지수)) + 
  geom_point()
ggplot(data=gMnC2, aes(x=평점, y=퇴사비율)) + 
  geom_point()
hist(gMnC2$평점, breaks = seq(1,4.5,0.1))

hist(jobData2$평점, breaks = seq(1,4.5,0.1))

