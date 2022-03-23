library(rvest)
library(XML)
library(httr)

rm(list = ls())

url <- "http://unico2013.dothome.co.kr/crawling/exercise_bs.html"
text <- read_html(url)
text

# <h1> 태그의 컨텐츠 
h1 <- html_nodes(text, "h1")
h1 <- html_text(h1)
h1

# <a> 태그의 컨텐츠와 href 속성값
a <- html_nodes(text, "a")
a <- html_text(a)
a
aHref <- html_nodes(text, "a")
aHref <- html_attr(aHref, "href")
aHref

# <img> 태그의 src 속성값
img <- html_nodes(text, "img")
img <- html_attr(img, "src")
img

# 첫 번째 <h2> 태그의 컨텐츠
h2_1 <- html_nodes(text, "h2:nth-child(9)")
h2_1 <- html_text(h2_1)
h2_1

# <ul> 태그의 자식 태그들 중 style 속성의 값이 green으로 끝나는 태그의 컨텐츠
ul <- html_nodes(text, 'ul > [style$=green]')
ul <- html_text(ul) 
ul

# 두 번째 <h2> 태그의 컨텐츠
h2_2 <- html_nodes(text, "h2:nth-child(11)")
h2_2 <- html_text(h2_2)
h2_2

# <ol> 태그의 모든 자식 태그들의 컨텐츠 
ol <- html_nodes(text, 'ol > *')
ol <- html_text(ol)
ol

# <table> 태그의 모든 자손 태그들의 컨텐츠 
table <- html_nodes(text, "table *")
table <- html_text(table)
table

# name이라는 클래스 속성을 갖는 <tr> 태그의 컨텐츠
tableName <- html_nodes(text, '.name')
tableName <- html_text(tableName)
tableName

# target이라는 아이디 속성을 갖는 <td> 태그의 컨텐츠
target <- html_nodes(text, '#target')
target <- html_text(target)
target
