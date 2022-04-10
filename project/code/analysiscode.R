library(dplyr)
library(plyr)
library(ggplot2)

jobData1 <- read.csv("project/output/jobdata1.csv")
jobData2 <- read.csv("project/output/jobdata2.csv")
jobData3 <- read.csv("project/output/jobdata3.csv")

jobData1
jobData2
jobData3

filter <- jobData3 %>% 
  filter(as.numeric(jobData3$평점) >= 3.0)

hist(jobData3$평점)
hist(filter$평점)

hire <- filter[,2:4]
hire <- unique(hire)
industry <- hire %>% 
  group_by(hire$산업군) %>%
  summarise(n = n())
names(industry) <- c('산업군','기업수')

?geom_boxplot
ggplot(data=industry,
       aes(x=산업군, y=기업수),
       ) + 
  geom_boxplot() +
  labs(title = "산업군별 평점 3.0이상 기업수")

boxplot(industry$기업수~industry$산업군,
        main = "산업군별 평점 3.0이상 기업수")

industry2 <- hire %>% 
  group_by(hire$산업군) %>%
  summarise(meanPoint = mean(평점))
names(industry2) <- c('산업군','평균')
#비율로 바꾸기

ggplot(data=industry2,
       aes(x=산업군, y=평균)) + 
  geom_point(color = "Blue") +
  labs(title = "산업군별 평점 평균")
