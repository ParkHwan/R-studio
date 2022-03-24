library(rvest)
library(XML)
library(httr)

site <- "https://comic.naver.com/genre/bestChallenge?&page="
navercomic2 <- NULL

for (i in 1:50){
  url <- paste(site, i, sep="")
  text <- read_html(url)
  nodes1 <- html_nodes(text, ".challengeTitle > a")
  comic.name <- html_text(nodes1, trim = TRUE)
  nodes2 <- html_nodes(text, ".challengeInfo > .summary")
  comic.summary <- html_text(nodes2, trim = TRUE)
  nodes3 <- html_nodes(text, ".rating_type > strong")
  comic.grade <- html_text(nodes3, trim = TRUE)
  page <- data.frame(comic.name, comic.summary, comic.grade)
  navercomic2 <- rbind(navercomic2, page)
}
navercomic2
write.csv(navercomic2, "output/navercomic.csv")
