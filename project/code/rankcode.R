library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)

jobData <- read.csv("project/output/jobdata3.csv")


#기업 랭킹
##토탈 랭킹
rank1 <- jobData[,-1] 
rank1 <- rank1[,-4:-8]
rank1 <- unique(rank1)
View(rank1)
totalrank <- head(rank1 %>% select(기업명,산업군,평점),100)
View(totalrank)
write.csv(totalrank,"project/output/totalrank.csv")

##산업별 랭킹

ITpartrank <- head(rank1 %>% filter(산업군=="IT/웹/통신") %>% select(기업명,산업군,평점),10)
Cntpartrank <- head(rank1 %>% filter(산업군=="건설업") %>% select(기업명,산업군,평점),10)
Edupartrank <- head(rank1 %>% filter(산업군=="교육업") %>% select(기업명,산업군,평점),10)
InApartrank <- head(rank1 %>% filter(산업군=="기관/협회") %>% select(기업명,산업군,평점),10)
MnDpartrank <- head(rank1 %>% filter(산업군=="미디어/디자인") %>% select(기업명,산업군,평점),10)
Servpartrank <- head(rank1 %>% filter(산업군=="서비스업") %>% select(기업명,산업군,평점),10)
TnTpartrank <- head(rank1 %>% filter(산업군=="유통/무역/운송") %>% select(기업명,산업군,평점),10)
Bankpartrank <- head(rank1 %>% filter(산업군=="은행/금융업") %>% select(기업명,산업군,평점),10)
MnPpartrank <- head(rank1 %>% filter(산업군=="의료/제약/복지") %>% select(기업명,산업군,평점),10)
MnCpartrank <- head(rank1 %>% filter(산업군=="제조/화학") %>% select(기업명,산업군,평점),10)

partrank <- cbind.data.frame(ITpartrank,Cntpartrank,Edupartrank,InApartrank,MnDpartrank,Servpartrank,TnTpartrank, Bankpartrank,MnPpartrank, MnCpartrank)

write.csv(partrank,"project/output/partrank.csv")

##구인중인 기업 랭킹

rank2 <- jobData[,-1] 
rank2 <- rank2 %>%
  filter(모집내용 != is.na(모집내용)) %>% select(기업명,산업군,평점)
rank2 <- unique(rank2)
emprank <- head(rank2,60)

write.csv(emprank,"project/output/emprank.csv")
