library(dplyr)

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
