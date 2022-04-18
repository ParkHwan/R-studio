rm(list=ls())

library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)

jobData <- read.csv("project/output/jobdata3.csv")

View(jobData)

jobDataSmry <- jobData[,-1]
summary(jobDataSmry)

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
gp1 <- gp1 + scale_x_discrete(limits=seq(1,4.5,0.1))
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


jobDataA <- jobData[,-1]
jobDataA <- jobDataA[,-4:-8]
jobDataA <- unique(jobDataA)
jobDataA <- na.omit(jobDataA)
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
  ylab("기업수") + 
  ylim(c(0,1250))

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

NewjobData <- NewjobData %>%
  mutate(직원대표값 = ifelse(직원수 == "1명 이상 ~ 20명 미만" , 10,
                          ifelse(직원수 == "20명 이상 ~ 40명 미만",30,
                                    ifelse(직원수 == "40명 이상 ~ 70명 미만",55,
                                              ifelse(직원수 == "70명 이상 ~ 100명 미만",85,
                                                        ifelse(직원수 == "100명 이상 ~ 200명 미만",150,
                                                                  ifelse(직원수 == "200명 이상 ~ 400명 미만",300,
                                                                            ifelse(직원수 == "400명 이상 ~ 700명 미만",550,
                                                                                      ifelse(직원수 == "700명 이상 ~ 1000명 미만" ,850,1500
                                                                                      )))))))))


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

ctgry <- NewjobData %>%
  group_by(산업군) %>%
  dplyr::summarise(n = n())
ctgry
names(ctgry) <- c("산업군","기업수")
ggplot(ctgry, aes(x=산업군,y=기업수)) +
  geom_bar(stat = "identity",) +
  geom_text(aes(label=기업수),hjust= -0.5) +
  ggtitle("산업군별 분포도") +
  ylim(c(0,2100)) +
  coord_flip() +
  scale_x_discrete(limits=c("제조/화학",
                            "의료/제약/복지",
                            "은행/금융업",
                            "유통/무역/운송",
                            "서비스업",
                            "미디어/디자인",
                            "기관/협회",
                            "교육업",
                            "건설업",
                            "IT/웹/통신"))
  
  

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
fitout <- lm(퇴사지수 ~ 평점, data=NewjobDataA)
summary(fitout)

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
fitemp <- lm(직원대표값 ~ 평점, data=NewjobDataA)
summary(fitemp)

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
fitout <- lm(퇴사지수 ~ 평점, data=NewjobDataA)
summary(fitout)


fit <- lm(New매출액 ~ 평점, data=NewjobData[-126,])
summary(fit)

# 산업군별
g1 <- ggplot(data=totalIndustry, aes(x=산업군),y) + labs(title = "산업별 기업 분포")
g1 <- g1  + scale_y_continuous(sec.axis = sec_axis(~./10,name="평점 평균"))
g1 <- g1 + geom_point(aes(y=비율), colour = "Red", show.legend = TRUE)
g1 <- g1 + geom_point(aes(y=평균*10),colour = "Blue", show.legend = TRUE)
g1 <- g1 + geom_text(aes(x=산업군, y=round(비율),colour = "3.0 기업 비율", label=비율, hjust = -0.5))
g1 <- g1 + geom_text(aes(x=산업군, y=평균*10, colour = "평균", label=평균, hjust = 1.5))
g1 <- g1 + ylab("3.0 기업 비율")
g1 <- g1 + theme(axis.text.x = element_text(size=7))
g1
#IT
gIT <- NewjobData %>% 
  filter(NewjobData$산업군 == "IT/웹/통신")
gIT <- gIT[-614,]
View(gIT)
dfIT <- gIT %>%
  group_by(gIT$평점) %>%
  dplyr::summarise(기업수 = n())
names(dfIT) <- c("평점","기업수")
dfIT$평점 <- as.numeric(dfIT$평점)
dfIT
ggplot(dfIT, aes(x=평점,y=기업수)) +
  geom_bar(stat = "identity",) +
  geom_text(aes(label=기업수),vjust= -0.5) +
  ggtitle("평점별 분포도") +
  ylim(0,100)

ggplot(data=gIT, aes(x=평점, y=log(New매출액))) + 
  geom_point()+
  ylab("매출액")+
  ggtitle("평점 대비 매출액")
fitITprof <- lm(log(New매출액) ~ 평점, data=gIT)
summary(fitITprof)
ggplot(data=gIT, aes(x=평점, y=퇴사지수)) + 
  geom_point()+
  ggtitle("평점 대비 퇴사지수")
fitITout <- lm(퇴사지수 ~ 평점, data=gIT)
summary(fitITout)

ggplot(data=gIT, aes(x=평점, y=직원수)) + 
  geom_point()+ 
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
fitITout <- lm(퇴사지수 ~ 평점, data=gIT)
summary(fitITout)

#건설업
gCsrt <- NewjobData %>% 
  filter(NewjobData$산업군 == "건설업")
View(gCsrt)
dfCsrt <- gCsrt %>%
  group_by(gCsrt$평점) %>%
  dplyr::summarise(기업수 = n())
names(dfCsrt) <- c("평점","기업수")
dfCsrt$평점 <- as.numeric(dfCsrt$평점)
dfIT
ggplot(dfCsrt, aes(x=평점,y=기업수)) +
  geom_bar(stat = "identity",) +
  geom_text(aes(label=기업수),vjust= -0.5) +
  ggtitle("평점별 분포도") +
  ylim(0,25)

ggplot(data=gCsrt, aes(x=평점, y=퇴사지수)) + 
  geom_point()+
  ggtitle("평점 대비 퇴사지수")
fitCsrtout <- lm(퇴사지수 ~ 평점, data=gCsrt)
summary(fitCsrtout)


#교육업
gEdu <- NewjobData %>% 
  filter(NewjobData$산업군 == "교육업")
dfEdu <- gEdu %>%
  group_by(gEdu$평점) %>%
  dplyr::summarise(기업수 = n())
names(dfEdu) <- c("평점","기업수")
dfEdu$평점 <- as.numeric(dfEdu$평점)
dfEdu
ggplot(dfEdu, aes(x=평점,y=기업수)) +
  geom_bar(stat = "identity",) +
  geom_text(aes(label=기업수),vjust= -0.5) +
  ggtitle("평점별 분포도") +
  ylim(0,20)

ggplot(data=gEdu, aes(x=평점, y=퇴사지수)) + 
  geom_point()+
  ggtitle("평점 대비 퇴사지수")
fitEduout <- lm(퇴사지수 ~ 평점, data=gEdu)
summary(fitEduout)

#기관/협회
gInA <- NewjobData %>% 
  filter(NewjobData$산업군 == "기관/협회")
dfInA <- gInA %>%
  group_by(gInA$평점) %>%
  dplyr::summarise(기업수 = n())
names(dfInA) <- c("평점","기업수")
dfInA$평점 <- as.numeric(dfInA$평점)
dfInA
ggplot(dfInA, aes(x=평점,y=기업수)) +
  geom_bar(stat = "identity",) +
  geom_text(aes(label=기업수),vjust= -0.5) +
  ggtitle("평점별 분포도") +
  ylim(0,20)

ggplot(data=gInA, aes(x=평점, y=퇴사지수)) + 
  geom_point()+
  ggtitle("평점 대비 퇴사지수")
fitInAout <- lm(퇴사지수 ~ 평점, data=gInA)
summary(fitInAout)

#미디어/디자인
gMnD <- NewjobData %>% 
  filter(NewjobData$산업군 == "미디어/디자인")
dfMnD <- gMnD %>%
  group_by(gMnD$평점) %>%
  dplyr::summarise(기업수 = n())
names(dfMnD) <- c("평점","기업수")
dfMnD$평점 <- as.numeric(dfMnD$평점)
dfMnD
ggplot(dfMnD, aes(x=평점,y=기업수)) +
  geom_bar(stat = "identity",) +
  geom_text(aes(label=기업수),vjust= -0.5) +
  ggtitle("평점별 분포도") +
  ylim(0,40)

ggplot(data=gMnD, aes(x=평점, y=퇴사지수)) + 
  geom_point()+
  ggtitle("평점 대비 퇴사지수")
fitMnDout <- lm(퇴사지수 ~ 평점, data=gMnD)
summary(fitMnDout)

#서비스업
gServ <- NewjobData %>% 
  filter(NewjobData$산업군 == "서비스업")
dfServ <- gServ %>%
  group_by(gServ$평점) %>%
  dplyr::summarise(기업수 = n())
names(dfServ) <- c("평점","기업수")
dfServ$평점 <- as.numeric(dfServ$평점)
dfServ
ggplot(dfServ, aes(x=평점,y=기업수)) +
  geom_bar(stat = "identity",) +
  geom_text(aes(label=기업수),vjust= -0.5) +
  ggtitle("평점별 분포도") +
  ylim(0,40)

ggplot(data=gServ, aes(x=평점, y=퇴사지수)) + 
  geom_point()+
  ggtitle("평점 대비 퇴사지수")
fitServout <- lm(퇴사지수 ~ 평점, data=gServ)
summary(fitServout)

#유통/무역/운송
gTnT<- NewjobData %>% 
  filter(NewjobData$산업군 == "유통/무역/운송")
dfTnT <- gTnT %>%
  group_by(gTnT$평점) %>%
  dplyr::summarise(기업수 = n())
names(dfTnT) <- c("평점","기업수")
dfTnT$평점 <- as.numeric(dfTnT$평점)
dfTnT
ggplot(dfTnT, aes(x=평점,y=기업수)) +
  geom_bar(stat = "identity",) +
  geom_text(aes(label=기업수),vjust= -0.5) +
  ggtitle("평점별 분포도") +
  ylim(0,80)

ggplot(data=gTnT, aes(x=평점, y=퇴사지수)) + 
  geom_point()+
  ggtitle("평점 대비 퇴사지수")
fitTnTout <- lm(퇴사지수 ~ 평점, data=gTnT)
summary(fitTnTout)


#은행/금융업
gBank <- NewjobData %>% 
  filter(NewjobData$산업군 == "은행/금융업")
dfBank <- gBank %>%
  group_by(gBank$평점) %>%
  dplyr::summarise(기업수 = n())
names(dfBank) <- c("평점","기업수")
dfBank$평점 <- as.numeric(dfBank$평점)
dfBank
ggplot(dfBank, aes(x=평점,y=기업수)) +
  geom_bar(stat = "identity",) +
  geom_text(aes(label=기업수),vjust= -0.5) +
  ggtitle("평점별 분포도") +
  ylim(0,10)

ggplot(data=gBank, aes(x=평점, y=퇴사지수)) + 
  geom_point()+
  ggtitle("평점 대비 퇴사지수")
fitBankout <- lm(퇴사지수 ~ 평점, data=gBank)
summary(fitBankout)

#의료/제약/복지
gMnP <- NewjobData %>% 
  filter(NewjobData$산업군 == "의료/제약/복지")
dfMnP <- gMnP %>%
  group_by(gMnP$평점) %>%
  dplyr::summarise(기업수 = n())
names(dfMnP) <- c("평점","기업수")
dfMnP$평점 <- as.numeric(dfMnP$평점)
dfMnP
ggplot(dfMnP, aes(x=평점,y=기업수)) +
  geom_bar(stat = "identity",) +
  geom_text(aes(label=기업수),vjust= -0.5) +
  ggtitle("평점별 분포도") +
  ylim(0,25)

ggplot(data=gMnP, aes(x=평점, y=퇴사지수)) + 
  geom_point()+
  ggtitle("평점 대비 퇴사지수")
fitMnPout <- lm(퇴사지수 ~ 평점, data=gMnP)
summary(fitMnPout)

#제조/화학
gMnC <- NewjobData %>% 
  filter(NewjobData$산업군 == "제조/화학")
dfMnC <- gMnC %>%
  group_by(gMnC$평점) %>%
  dplyr::summarise(기업수 = n())
names(dfMnC) <- c("평점","기업수")
dfMnC$평점 <- as.numeric(dfMnC$평점)
dfMnC
ggplot(dfMnC, aes(x=평점,y=기업수)) +
  geom_bar(stat = "identity",) +
  geom_text(aes(label=기업수),vjust= -0.5) +
  ggtitle("평점별 분포도") +
  ylim(0,200)

ggplot(data=gMnC, aes(x=평점, y=퇴사지수)) + 
  geom_point()+
  ggtitle("평점 대비 퇴사지수")
fitMnCout <- lm(퇴사지수 ~ 평점, data=gMnC)
summary(fitMnCout)

