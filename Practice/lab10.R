library(rvest)
library(XML)
library(httr)

url <- "http://media.daum.net/ranking/popular/"
text <- read_html(url)
text
nodes1 <- html_nodes(text, ".tit_g > .link_txt")
nodes1
newstitle <- gsub('"','',html_text(nodes1,trim =TRUE))[1:20]
newstitle
nodes2 <- html_nodes(text, ".info_thumb > .txt_category")
nodes2
newscategory <- html_text(nodes2,trim =TRUE)
newscategory

daumnews <- data.frame(newstitle,newscategory)
daumnews

write.csv(daumnews, "output/daumnews.csv")
