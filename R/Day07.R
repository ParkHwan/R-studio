# 데이터 전처리(1) - apply 계열의 함수를 알아보자
weight <- c(65.4, 55, 380, 72.2, 51, NA)
height <- c(170, 155, NA, 173, 161, 166)
gender <- c("M", "F","M","M","F","F")

df <- data.frame(w=weight, h=height)
df
?apply
?lapply
apply(df, 1, sum, na.rm=TRUE)
apply(df, 2, sum, na.rm=TRUE)
lapply(df, sum, na.rm=TRUE)
sapply(df, sum, na.rm=TRUE)
tapply(1:6, gender, sum) # rm(sum)
tapply(df$w, gender, mean, na.rm=TRUE)
mapply(paste, 1:5, LETTERS[1:5], month.abb[1:5])

v<-c("abc", "DEF", "TwT")
sapply(v, function(d) paste("-",d,"-", sep=""))

l<-list("abc", "DEF", "TwT")
sapply(l, function(d) paste("-",d,"-", sep=""))
lapply(l, function(d) paste("-",d,"-", sep=""))

flower <- c("rose", "iris", "sunflower", "anemone", "tulip")
length(flower)
nchar(flower)
sapply(flower, function(d) if(nchar(d) > 5) return(d)) 
sapply(flower, function(d) if(nchar(d) > 5) d)
sapply(flower, function(d) if(nchar(d) > 5) return(d) else return(NA))
sapply(flower, function(d) paste("-",d,"-", sep=""))

sapply(flower, function(d, n=5) if(nchar(d) > n) return(d))
sapply(flower, function(d, n=5) if(nchar(d) > n) return(d), 3)
sapply(flower, function(d, n=5) if(nchar(d) > n) return(d), n=4)

count <- 1
myf <- function(x, wt=T){
  cat(x,"(",count,")", "\n")
  Sys.sleep(1)
  if(wt) 
    r <- paste("*", x, "*")
  else
    r <- paste("#", x, "#")
  count <<- count + 1;
  return(r)
}
result <- sapply(df$w, myf) #c(65.4, 55, 380, 72.2, 51, NA)
length(result)
result
sapply(df$w, myf, F)
sapply(df$w, myf, wt=F)
rr1 <- sapply(df$w, myf, wt=F)
str(rr1)

count <- 1
sapply(df, myf)
rr2 <- sapply(df, myf)
str(rr2)
rr2[1,1]
rr2[1,"w"]

# 설치된 패키지 리스트
installed.packages()
# 현재 세션에 로드된 패키지 리스트
search()

if(!file.exists("output")) {
  cat("워킹디렉토리에 output 폴더 생성함\n")
  dir.create("output")
}

# 확장(추가) 패키지의 설치작업은 하는 일이 많고 좀... 민감함...(^^)
# 가급적 하나하나 실행시키숑~~
install.packages("rvest") 
install.packages("XML")
install.packages("httr")
install.packages("jsonlite")
install.packages("rtweet") 

library(rvest)
library(XML)
library(httr)

# [ 예제1 ]
library(rvest)

url <- "http://unico2013.dothome.co.kr/crawling/tagstyle.html"
text <- read_html(url)
text

nodes <- html_nodes(text, "div") # 태그 선택자
nodes
title <- html_text(nodes)
title

node1 <- html_nodes(text, "div:nth-of-type(1)")
node1
html_text(node1)
html_attr(node1, "style")

node2 <- html_nodes(text, "div:nth-of-type(2)")
node2
html_text(node2)
html_attr(node2, "style")

node3 <- html_node(text, "div:nth-of-type(3)")
node3
html_text(node3)
html_attr(node3, "style")

# [ 예제2 ]
# 웹문서 읽기
url <- "https://www.data.go.kr/tcs/dss/selectDataSetList.do"
html <- read_html(url)
html

# 목록 아이템 추출
title <- html_text(html_nodes(html, "#apiDataList span.title"), trim=T)
title

# 목록 아이템 설명 추출
desc <- html_text(html_nodes(html, "#apiDataList .ellipsis"))
desc

# 데이터 정제: 제어문자를 공백으로 대체
title <- gsub("[[:cntrl:]]", "", title)
title

desc <- gsub("[[:cntrl:]]", "", desc)
desc

# 데이터 출력
api <- data.frame(title, desc)
api
View(api)

# [ 예제3 ]
# 단일 페이지(rvest 패키지 사용)
library(rvest)
# 영화 제목과 평점 읽기
url<- "http://movie.naver.com/movie/point/af/list.nhn"
text <- read_html(url)
text
# 영화제목
nodes <- html_nodes(text, ".movie")
title <- html_text(nodes)
title
# 영화평점
nodes <- html_nodes(text, ".list_netizen_score > em")
point <- html_text(nodes)
point
page <- data.frame(title, point)
print(page)


# [ 예제4 ]
# 단일 페이지(rvest 패키지 사용)

# 영화 제목, 평점, 리뷰글 읽기
text<- NULL 
title<-NULL 
point<-NULL 
review<-NULL 
page<-NULL
url<- "http://movie.naver.com/movie/point/af/list.nhn"
text <- read_html(url)
text
#save(text, file="imsi.rda")
# 영화제목
nodes <- html_nodes(text, ".movie")
title <- html_text(nodes)
title
# 영화평점
nodes <- html_nodes(text, ".title em")
point <- html_text(nodes)
point
# 영화리뷰

review <- html_nodes(text, xpath="//*[@id='old_content']/table/tbody/tr/td[2]/text()")
review <- html_text(review, trim=TRUE)
review <- review[nchar(review) > 0]
review
page <- data.frame(title, point, review) # 리뷰글이 생략된 경우 오류 발생!!!
print(page)


# [ 예제5 ]
# 여러 페이지1
site<- "http://movie.naver.com/movie/point/af/list.nhn?page="
text <- NULL
movie.review <- NULL
for(i in 1: 100) {
  url <- paste(site, i, sep="")
  text <- read_html(url)
  nodes <- html_nodes(text, ".movie")
  title <- html_text(nodes)
  nodes <- html_nodes(text, ".title em")
  point <- html_text(nodes)
  nodes <- html_nodes(text, xpath="//*[@id='old_content']/table/tbody/tr/td[2]/text()")
  imsi <- html_text(nodes, trim=TRUE)
  review <- imsi[nchar(imsi) > 0] 
  if(length(review) == 10) {
    page <- data.frame(title, point, review)
    movie.review <- rbind(movie.review, page)
  } else {
    cat(paste(i," 페이지에는 리뷰글이 생략된 데이터가 있어서 수집하지 않습니다.ㅜㅜ\n"))
  }
}
write.csv(movie.review, "output/movie_reviews1.csv")


# [ 예제6 ]
# 영화 제목, 평점, 리뷰글 읽기
text<- NULL; vtitle<-NULL; vpoint<-NULL; vreview<-NULL; page=NULL
url<- "http://movie.naver.com/movie/point/af/list.nhn"
text <- read_html(url)
text

for (index in 1:10) {
  # 영화제목
  node <- html_node(text, paste0("#old_content > table > tbody > tr:nth-child(", index, ") > td.title > a.movie.color_b"))
  title <- html_text(node)
  vtitle[index] <- title
  # 영화평점
  node <- html_node(text, paste0("#old_content > table > tbody > tr:nth-child(", index,") > td.title > div > em"))
  point <- html_text(node)
  vpoint <- c(vpoint, point)
  # 영화리뷰 
  node <- html_nodes(text, xpath=paste0('//*[@id="old_content"]/table/tbody/tr[', index,"]/td[2]/text()"))
  node <- html_text(node, trim=TRUE)
  print(node)
  review = node[4] # 리뷰글이 생략된 경우에 대한 처리를 위해...
  vreview <- append(vreview, review)
}
page <- data.frame(vtitle, vpoint, vreview)
View(page)
write.csv(page, "output/movie_reviews2.csv")


# [ 예제7 ]
# 여러 페이지2
site<- "http://movie.naver.com/movie/point/af/list.nhn?page="
text <- NULL
movie.review <- NULL
for(i in 1: 100) {
  url <- paste(site, i, sep="")
  print(url)
  text <- read_html(url)
  vtitle <- NULL
  vpoint <- NULL 
  vreview <- NULL
  for (index in 1:10) {
    # 영화제목
    node <- html_node(text, paste0("#old_content > table > tbody > tr:nth-child(", index, ") > td.title > a.movie.color_b"))
    title <- html_text(node)
    vtitle[index] <- title
    # 영화평점
    node <- html_node(text, paste0("#old_content > table > tbody > tr:nth-child(", index,") > td.title > div > em"))
    point <- html_text(node)
    vpoint <- c(vpoint, point)
    # 영화리뷰 
    node <- html_nodes(text, xpath=paste0('//*[@id="old_content"]/table/tbody/tr[', index,"]/td[2]/text()"))
    node <- html_text(node, trim=TRUE)
    #print(node)
    review = node[4] 
    vreview <- append(vreview, review)
  }
  page <- data.frame(vtitle, vpoint, vreview)
  movie.review <- rbind(movie.review, page)
}
View(movie.review)
str(movie.review)
write.csv(movie.review, "output/movie_reviews3.csv")


# [ 예제8 ]
# W3C의 HTTP 프로토콜 스팩에서 Table of Contents 읽기
title2 = html_nodes(read_html('http://www.w3.org/Protocols/rfc2616/rfc2616.html'), 'div.toc > h2')
title2 = html_text(title2)
title2

title2 = html_nodes(read_html('http://www.w3.org/Protocols/rfc2616/rfc2616.html'), 'body > div > h2')
title2 = html_text(title2)
title2

title2 = html_nodes(read_html('http://www.w3.org/Protocols/rfc2616/rfc2616.html'), xpath='/html/body/div/h2')
title2 = html_text(title2)
title2

title2 = html_nodes(read_html('http://www.w3.org/Protocols/rfc2616/rfc2616.html'), xpath='//div/h2')
title2 = html_text(title2)
title2


# [ 예제9 ]
# YES24에서 IT신간 정보 추출(도서명, 출판사명)
site<- "http://www.yes24.com/24/Category/NewProductList/001001003?sumGb=04&PageNumber="
itbooks <- NULL
i <- 1
while(TRUE) {
  nodes <- NULL
  url <- paste0(site, i)
  doc <- read_html(url)
  # 도서명
  nodes <- html_nodes(doc, 'td.goodsTxtInfo > p:nth-child(1) > a:nth-child(1)')
  print(length(nodes))
  if (length(nodes) == 0){
    break
  }
  title.books <- html_text(nodes)
  # 출판사명
  nodes <- html_nodes(doc, 'td.goodsTxtInfo > div > a:last-child')
  publisher.books <- html_text(nodes)
  
  page <- data.frame(title.books, publisher.books)
  itbooks <- rbind(itbooks, page)
  i <- i+1
}
View(itbooks)
write.csv(itbooks, "output/yes24_new_itbooks.csv")


# [ 예제10 ]
# 한겨레 페이지(XML 패키지 사용)
library(XML)
library(rvest)
imsi <- read_html("http://www.hani.co.kr/")
t <- htmlParse(imsi)
content<- xpathSApply(t,'//*[@id="main-top01-scroll-in"]/div/div/h4/a', xmlValue); 
content



# [ 예제11 ]
# 뉴스, 게시판 등 글 목록에서 글의 URL만 뽑아내기 
htxt = read_html("https://news.naver.com/main/list.nhn?mode=LSD&mid=sec&sid1=001")
link = html_nodes(htxt, 'div.list_body a')
length(link)
link
article.href = unique(html_attr(link, 'href'))
article.href

# [ 예제12 ]
# 이미지, 첨부파일 다운 받기 
# pdf
library(httr)
res = GET('http://cran.r-project.org/web/packages/httr/httr.pdf')
writeBin(content(res, 'raw'), 'c:/Temp/httr.pdf')

# [ 예제13 ]
# jpg
h = read_html('http://unico2013.dothome.co.kr/productlog.html')
imgs = html_nodes(h, 'img')
img.src = html_attr(imgs, 'src')
for(i in 1:length(img.src)){
  res = GET(paste('http://unico2013.dothome.co.kr/',img.src[i], sep=""))
  writeBin(content(res, 'raw'), paste('c:/Temp/', img.src[i], sep=""))
} 

