library(dplyr)

jobPlanetData <- read.csv("project/output/jobPlanet.csv")
jobPlanetData$unlist.company. <- gsub("\\(.*?\\)","",jobPlanetData$unlist.company.)
jobPlanetData <- jobPlanetData[,-1]
names(jobPlanetData) <- c("기업명","산업군","평점")
View(jobPlanetData)

saraminData <- read.csv("project/output/saramin_job_posting.csv", fileEncoding = "UTF-8")
saraminData <- saraminData[,-1]
saraminData$마감일.등록일 <- gsub("\\(.*?\\)","",saraminData$마감일.등록일)
View(saraminData)

bizData <- read.csv("project/output/nicebizinfo_cm_Info.csv", fileEncoding = "UTF-8")
bizData <- bizData[,-1]
bizData$기업명 <- gsub("\\(.*?\\)","",bizData$기업명)
View(bizData)

joindata1 <- inner_join(jobPlanetData,saraminData,by='기업명')
joindata2 <- full_join(jobPlanetData,saraminData,by='기업명')
joindata3 <- left_join(jobPlanetData,saraminData,by='기업명')
joindata3 <- left_join(joindata3,bizData,by='기업명')

View(joindata1)
View(joindata3)

write.csv(joindata1,"project/output/jobdata1.csv")
write.csv(joindata2,"project/output/jobdata2.csv")
write.csv(joindata3,"project/output/jobdata3.csv")
