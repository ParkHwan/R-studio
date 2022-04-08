rm(list = ls())

library(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "localhost" , 
                      port = 4445,
                      browserName = "chrome")
loginPage <- "https://www.jobplanet.co.kr/users/sign_in?_nav=gb"
url <- "https://www.jobplanet.co.kr/companies?sort_by=review_avg_cache&city_id=1"
remDr$open()
remDr$navigate(loginPage)

ID <-remDr$findElement(using="css selector", "#user_email")
ID$clickElement()

PW <- remDr$findElement(using="css selector", "#user_password")


remDr$navigate(url)

# 기업 태그 : section:nth-child(i) > div > div > dl.content_col2_3.cominfo > dt > a
# 장점 : div > div.ctbody_col2 > div > dl > dd:nth-child(2) > span
# 단점 : div > div.ctbody_col2 > div > dl > dd:nth-child(4) > span
# 바라는 점 : div > div.ctbody_col2 > div > dl > dd:nth-child(6) > span

j <- 1
review1 <- NULL
review2 <- NULL
pageNode <- remDr$findElements(using='css selector', '#listCompaniesTitle > span')
pageNum <- as.numeric(unlist(sapply(pageNode, function(x) {x$getElementText()})))

repeat {
  for (i in 1:10) {
    reviewCompany <- NULL
    reviewStrong <- NULL
    reviewWeak <- NULL
    reviewWish <- NULL
    reviewStrongNode <- NULL
    reviewStrongData <- NULL
    reviewWeakNode <- NULL
    reviewWeakData <- NULL
    reviewWishNode <- NULL
    reviewWishData <- NULL
    reviewCompanyNode <- NULL
    reviewCompanyData <- NULL
    url1 <- paste("https://www.jobplanet.co.kr/companies?sort_by=review_avg_cache&city_id=1&page=",j, sep = "")
    Tab <- paste("section:nth-child(",i,") > div > div > dl.content_col2_3.cominfo > dt > a",sep = "")
    reviewcompanyTab <-remDr$findElement(using="css selector", Tab)
    reviewcompanyTab$clickElement()
    Sys.sleep(5)
    reviewTab <- remDr$findElement(using="css selector", "#viewCompaniesMenu > ul > li.viewReviews > a")
    reviewTab$clickElement()
    Sys.sleep(5)
    repeat {
      numNode <- remDr$findElements(using='css selector', '#viewCompaniesMenu > ul > li.viewReviews > a > span')
      num <- as.numeric(unlist(sapply(numNode, function(x) {x$getElementText()})))
      reviewStrongNode <- remDr$findElements(using='css selector', 'div > div.ctbody_col2 > div > dl > dd:nth-child(2) > span')
      reviewStrongData <- sapply(reviewStrongNode, function(x) {x$getElementText()})
      reviewStrong <- c(reviewStrong, reviewStrongData)
      reviewWeakNode <- remDr$findElements(using='css selector', 'div > div.ctbody_col2 > div > dl > dd:nth-child(4) > span')
      reviewWeakData <- sapply(reviewWeakNode, function(x) {x$getElementText()})
      reviewWeak <- c(reviewWeak, reviewWeakData)
      reviewWishNode <- remDr$findElements(using='css selector', 'div > div.ctbody_col2 > div > dl > dd:nth-child(6) > span')
      reviewWishData <- sapply(reviewWishNode, function(x) {x$getElementText()})
      reviewWish <- c(reviewWish, reviewWishData)
      if (as.numeric(length(unlist(reviewStrong))) >= num ) {
        reviewCompanyNode <- remDr$findElements(using='css selector', 'div.company_name > h1 > a')
        reviewCompanyData <- sapply(reviewCompanyNode, function(x) {x$getElementText()})
        reviewCompany <- rep(reviewCompanyData, num)
        review1 <- data.frame(unlist(reviewCompany),unlist(reviewStrong),unlist(reviewWeak),unlist(reviewWish))
        review2 <- rbind(review2, review1)
        remDr$navigate(url1)
        Sys.sleep(5)
        break
      }
      nextReview <- "div.pg_bottom.um_paginnation > article > a.btn_pgnext"
      nextReviewPage <-remDr$findElement(using='css selector', nextReview)
      nextReviewPage$clickElement()
      Sys.sleep(5)
    }
    if (i == 10) {
      nextCss <- "div.pg_bottom.um_paginnation > article > a.btn_pgnext"
      nextPage<-remDr$findElement(using='css selector', nextCss)
      nextPage$clickElement()
      Sys.sleep(5)
      i <- 1
      j <- j + 1
      if (j == ceiling(pageNum / 10)){
        break
      }
    }
  }
}

View(review)
