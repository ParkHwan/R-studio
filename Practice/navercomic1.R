library(rvest)
library(XML)
library(httr)

url <- "https://comic.naver.com/genre/bestChallenge.nhn"
text <- read_html(url)

nodes1 <- html_nodes(text, ".challengeTitle > a")
comic.name <- html_text(nodes1, trim = TRUE)

nodes2 <- html_nodes(text, ".challengeInfo > .summary")
comic.summary <- html_text(nodes2, trim = TRUE)

nodes3 <- html_nodes(text, ".rating_type > strong")
comic.grade <- html_text(nodes3, trim = TRUE)

navercomic1 <- data.frame(comic.name, comic.summary, comic.grade)

View(navercomic1)

