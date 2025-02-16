library(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "localhost" , 
                      port = 4445,
                      browserName = "chrome")
url <- "https://www.jobplanet.co.kr/companies?&sort_by=review_avg_cache"
remDr$open()
remDr$navigate(url)

company <- NULL
industry <- NULL
TPoint <- NULL
#기업명 : div > div > dl.content_col2_3.cominfo > dt > a
#직군 : div > div > dl.content_col2_3.cominfo > dd:nth-child(2) > span:nth-child(1)
#평점 : div > div > dl.content_col2_4 > dd.gf_row > span
#다음 페이지 : div.pg_bottom.um_paginnation > article > a.btn_pgnext

repeat {
  companyNode <- remDr$findElements(using='css selector', 'div > div > dl.content_col2_3.cominfo > dt > a')
  companyData <- sapply(companyNode, function(x) {x$getElementText()})
  company <- c(company, companyData)
  industryNode <- remDr$findElements(using='css selector', 'div > div > dl.content_col2_3.cominfo > dd:nth-child(2) > span:nth-child(1)')
  industryData <- sapply(industryNode, function(x) {x$getElementText()})
  industry <- c(industry, industryData)
  TPointNode <- remDr$findElements(using='css selector', 'div > div > dl.content_col2_4 > dd.gf_row > span')
  TPointData <- sapply(TPointNode, function(x) {x$getElementText()})
  TPoint <- c(TPoint, TPointData)
  if (length(unlist(companyData)) < 10) {
    break
  }
  nextCss <- "div.pg_bottom.um_paginnation > article > a.btn_pgnext"
  nextPage<-remDr$findElement(using='css selector', nextCss)
  nextPage$clickElement()
  Sys.sleep(5)
}
jobPlanet <- data.frame(unlist(company), unlist(industry), unlist(TPoint))
write.csv(jobPlanet,"project/output/jobPlanet.csv")



