install.packages("RSelenium")
library(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "localhost" , 
                      port = 4445, browserName = "chrome")

url <- "https://www.megabox.co.kr/movie-detail/comment?rpstMovieNo=21049700"
remDr$open()
remDr$navigate(url)

storyPoint1 <- NULL
storyRcmd1 <- NULL
storyTxt1 <- NULL
for (i in 1:10) {
  nextCss <- paste0("div.movie-idv-story > nav > a:nth-child(",i,")", sep="")
  nextPage<-remDr$findElement(using='css selector', nextCss)
  nextPage$clickElement()
  Sys.sleep(2)
  storyPointNodes<-remDr$findElements(using ="css selector","div.story-area > div.story-box > div > div.story-cont > div.story-point > span")
  storyPoint <- sapply(storyPointNodes,function(x){x$getElementText()})
  storyPoint1 <- c(storyPoint1, unlist(storyPoint))
  storyRcmdNodes <- remDr$findElements(using ="css selector","div.story-area > div.story-box > div > div.story-cont > div.story-recommend")
  storyRcmd <- sapply(storyRcmdNodes,function(x){x$getElementText()})
  storyRcmd1 <- c(storyRcmd1, unlist(storyRcmd))
  storyTxtNodes <- remDr$findElements(using ="css selector","div.story-area > div.story-box > div > div.story-cont > div.story-txt")
  storyTxt <- sapply(storyTxtNodes,function(x){x$getElementText()})
  storyTxt1 <- c(storyTxt1, unlist(storyTxt))
}
movie <- data.frame(storyPoint1,storyRcmd1,storyTxt1)
write.csv(movie, "./output/movie.csv")
