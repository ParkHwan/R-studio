rm(list = ls())

library(RSelenium)

url <- "http://gs25.gsretail.com/gscvs/ko/products/event-goods"
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445, browserName = "chrome")
remDr$open()
remDr$navigate(url)

goodsname <- NULL
goodsprice <- NULL

twoToOne <- ("#TWO_TO_ONE")
twoToOnePage <- remDr$findElement(using='css selector', twoToOne)
twoToOnePage$clickElement()

repeat {
  nameNodes <- remDr$findElements(using='css selector', '#contents > div.cnt > div.cnt_section.mt50 > div > div > div:nth-child(5) > ul > li > div > p.tit')
  goodsnameData <- sapply(nameNodes, function(x) {x$getElementText()})
  goodsname <- c(goodsname, unlist(goodsnameData))
  priceNodes <- remDr$findElements(using='css selector', '#contents > div.cnt > div.cnt_section.mt50 > div > div > div:nth-child(5) > ul > li > div > p.price > span')
  goodspriceData <- sapply(priceNodes, function(x) {x$getElementText()})
  goodsprice <- c(goodsprice, unlist(goodspriceData))
  if (length(unlist(goodsnameData)) < 8) {
    break
  }
  nextCss <- paste0("#contents > div.cnt > div.cnt_section.mt50 > div > div > div:nth-child(5) > div > a.next")
  nextPage<-remDr$findElement(using='css selector', nextCss)
  nextPage$clickElement()
  Sys.sleep(3)
}
goodsprice <- gsub("[,|원]","",goodsprice)
gs25_twotoone <- data.frame(goodsname = unlist(goodsname), goodsprice = unlist(goodsprice))
write.csv(gs25_twotoone, 'output/gs25_twotoone.csv')
