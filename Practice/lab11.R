library(rvest)
library(XML)
library(httr)
library(jsonlite)

#실습1
searchUrl1<- "https://openapi.naver.com/v1/search/blog.xml"
Client_ID <- "izGsqP2exeThwwEUVU3x"
Client_Secret <- "WrwbQ1l6ZI"
query1 <- URLencode("맛집")
url1 <- paste0(searchUrl1, "?query=", query1, "&display=100")
doc <- GET(url1, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))
doc2 <- xmlParse(doc, encoding="UTF-8")
text<- xpathSApply(doc2, "//item/description", xmlValue); 
text <- gsub("</?b>", "", text)
text <- gsub("&.+t;", "", text)
write(text, "./output/naverblog.txt")

#실습2
API_key  <- "%2BjzsSyNtwmcqxUsGnflvs3rW2oceFvhHR8AFkM3ao%2Fw50hwHXgGyPVutXw04uAXvrkoWgkoScvvhlH7jgD4%2FRQ%3D%3D"
bus_No <- "360"
url2 <- paste("http://ws.bus.go.kr/api/rest/busRouteInfo/getBusRouteList?ServiceKey=", API_key, "&strSrch=", bus_No, sep="")
url2
doc <- xmlParse(url2, encoding="UTF-8")
dfBus360 <- xmlToDataFrame(getNodeSet(doc, "//itemList"))
bus360 <- dfBus360[dfBus360$busRouteNm == 360,]
bus360
cat("[360번 버스정보]","\n",
    "노선ID :",bus360$busRouteId,"\n",
    "노선길이 :",bus360$length,"\n",
    "기점 :",bus360$stStationNm,"\n",
    "종점 :",bus360$edStationNm,"\n",
    "배차간격 :",bus360$term)

#실습3
searchUrl2<- "https://openapi.naver.com/v1/search/news.json"
Client_ID <- "izGsqP2exeThwwEUVU3x"
Client_Secret <- "WrwbQ1l6ZI"
query2 <- URLencode("빅데이터")
url3 <- paste0(searchUrl2, "?query=", query2, "&display=100")
doc3 <- GET(url3, add_headers("Content_Type" = "application/xml",
                              "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))
doc3
json_data <- content(doc3, type = 'text', encoding = "UTF-8")
json_obj <- fromJSON(json_data)
df <- data.frame(json_obj)
df$items.description <- gsub("</?b>", "", df$items.description)
df$items.description <- gsub("&quot;;", "", df$items.description)
df$items.title <- gsub("</?b>", "", df$items.title)
df$items.title <- gsub("&quot;", "", df$items.title)
write(df$items.title, "./output/navernews.txt")
