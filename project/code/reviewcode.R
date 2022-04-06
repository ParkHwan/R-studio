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
PW$clickElement()

loginbtn <- remDr$findElement(using='css selector', "section.section_email.er_r > fieldset > button")
loginbtn$clickElement()

remDr$navigate(url)


reviewCompany <- NULL
reviewStrong <- NULL
reviewWeak <- NULL
reviewWish <- NULL


remDr$goBack()
