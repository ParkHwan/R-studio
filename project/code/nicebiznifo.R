library(RSelenium)
library(XML)
library(rvest)
library(httr)
library(dplyr)

# data_na <- function() { # 반복되는 부분들 함수로 만들어 보자!!!
#   cm_addr <- NA; cm_addr_total <- c(cm_addr_total, cm_addr)
#   cm_emp <- NA; cm_emp_total <- c(cm_emp_total, cm_emp)
#   cm_sales <- NA; cm_sales_total <- c(cm_sales_total, cm_sales)
#   cm_EntryRate <- NA; cm_EntryRate_total <- c(cm_EntryRate_total, cm_EntryRate)
#   cm_RetirementRate <- NA; cm_RetirementRate_total <- c(cm_RetirementRate_total, cm_RetirementRate)
# }

remDr <- remoteDriver(remoteServerAddr = "localhost" , 
                      port = 4445, browserName = "chrome")
remDr$open()
nicebizinfo_site <- "https://www.nicebizinfo.com/cm/CM0100M001GE.nice"
remDr$navigate(nicebizinfo_site)

i <- 1; num <- 7; n <- 1; pageNo <- 1
cm_addr_total <- NULL; cm_emp_total <- NULL; cm_sales_total <- NULL
cm_nm_total <- NULL; cm_EntryRate_total <- NULL; cm_RetirementRate_total <- NULL
company_list_v <- NULL

company_list <- read.table("project/output/jobPlanet.csv", sep = ",")[2]
company_list_v <- company_list[-1,]

company_list_v <- gsub("\\(주\\)|\\(유\\)|㈜|주식회사|유한책임회사|(재)", "", company_list_v)
(company_list_v <- gsub("에스케이", "SK", company_list_v))
# nrow(company_list)
# window.prompt 창 띄우기 코드 remDr$executeScript("alert('alert창에 메세지 입력할 수 있다.');")
i <- 1449 # 시작 위치 설정
while (i < nrow(company_list)) {
  cat(i, "번째 기업 검색\n")
  cm_search <- remDr$findElement(using = "css selector", "#CM0100M001GE_itgSrch")
  cm_search$sendKeysToElement(list(company_list_v[i], key = "enter"))
  Sys.sleep(1)
  repeat {
    if (num == 187) {
      next_page <- remDr$findElement(using = "css selector", "div.pagingList > ul > li > a.next")
      next_page$clickElement()
      num <- 7; n <- 1
      pageNo <- pageNo + 1
      Sys.sleep(1)
      next
    }
    cat("tr:nth-child(", num, "):", n,"번째 기업 매칭 중", "<", pageNo, "페이지>\n",  sep = "")
    cm_node <- NULL
    try(cm_node <- remDr$findElement(using = "css selector", paste0("div.mb60 > table > tbody > tr:nth-child(", num, ") > th > span> a")))
    try(cm_nm <- gsub("\\(주\\)|\\(유\\)|㈜|주식회사|유한책임회사|(재)", "", cm_node$getElementText()))
    if (company_list_v[i] == cm_nm) {
      cm_nm_total <- c(cm_nm_total, cm_nm)
      cm_node$clickElement()
      num <- 7; n <- 1; pageNo <- 1
      break
    } else if (length(cm_node) == 0) {
      cm_nm_total <- c(cm_nm_total, company_list_v[i])
      cm_addr <- NA; cm_addr_total <- c(cm_addr_total, cm_addr)
      cm_emp <- NA; cm_emp_total <- c(cm_emp_total, cm_emp)
      cm_sales <- NA; cm_sales_total <- c(cm_sales_total, cm_sales)
      cm_EntryRate <- NA; cm_EntryRate_total <- c(cm_EntryRate_total, cm_EntryRate)
      cm_RetirementRate <- NA; cm_RetirementRate_total <- c(cm_RetirementRate_total, cm_RetirementRate)
      break
    }
    num <- num + 9; n <- n + 1
  }
  Sys.sleep(2)

  alertmessage <- NULL
  try(alertmessage <- remDr$getAlertText())
  if (length(alertmessage) != 0) {
    cm_addr <- NA; cm_addr_total <- c(cm_addr_total, cm_addr)
    cm_emp <- NA; cm_emp_total <- c(cm_emp_total, cm_emp)
    cm_sales <- NA; cm_sales_total <- c(cm_sales_total, cm_sales)
    cm_EntryRate <- NA; cm_EntryRate_total <- c(cm_EntryRate_total, cm_EntryRate)
    cm_RetirementRate <- NA; cm_RetirementRate_total <- c(cm_RetirementRate_total, cm_RetirementRate)
    remDr$acceptAlert()
    i <- i + 1
    next
  }
  if (company_list_v[i] != cm_nm) {
    i <- i + 1; num <- 7
    next
  }
  page_error_node <- NULL
  try(page_error_node <- remDr$findElement(using = "css selector", "body > div.tac > a"), silent = F)
  if (length(page_error_node) != 0) {
    cm_addr <- NA; cm_addr_total <- c(cm_addr_total, cm_addr)
    cm_emp <- NA; cm_emp_total <- c(cm_emp_total, cm_emp)
    cm_sales <- NA; cm_sales_total <- c(cm_sales_total, cm_sales)
    cm_EntryRate <- NA; cm_EntryRate_total <- c(cm_EntryRate_total, cm_EntryRate)
    cm_RetirementRate <- NA; cm_RetirementRate_total <- c(cm_RetirementRate_total, cm_RetirementRate)
    page_error_node$clickElement()
    i <- i + 1
    next
  }

  cm_addr_node <- remDr$findElement(using = "css selector", "div.mb10 > table > tbody > tr:nth-child(1) > td:nth-child(2) > div.bg2 > strong")
  cm_addr <- cm_addr_node$getElementText()
  cm_addr_total <- c(cm_addr_total, unlist(cm_addr))
  
  cm_emp_node <- NULL
  try(cm_emp_node <- remDr$findElement(using = "css selector", "div.sp2 > table > tbody > tr > td:nth-child(3) > div.bg6 > strong"))
  if (length(cm_emp_node) == 0) {
    cm_emp <- NA
  } else {
    cm_emp <- cm_emp_node$getElementText()
  }
  cm_emp_total <- c(cm_emp_total, unlist(cm_emp))
  
  cm_sales_node <- NULL
  try(cm_sales_node <- remDr$findElement(using = "css selector", "div.mb10 > table > tbody > tr.fwb > td:nth-child(3)"))
  if (length(cm_sales_node) == 0) {
    cm_sales <- NA
  } else {
    cm_sales <- cm_sales_node$getElementText()
    if (cm_sales == "-") {
      cm_sales <- NA
    }
  }
  cm_sales_total <- c(cm_sales_total, unlist(cm_sales))
  
  cm_EntryRate_node <- NULL
  try(cm_EntryRate_node <- remDr$findElement(using = "css selector", "div.mt10 > table > tbody > tr > td:nth-child(1) > div.bg10 > strong"))
  if (length(cm_EntryRate_node) == 0) {
    cm_EntryRate <- NA
  } else {
    cm_EntryRate <- cm_EntryRate_node$getElementText()
  }
  cm_EntryRate_total <- c(cm_EntryRate_total, unlist(cm_EntryRate))
  
  cm_RetirementRate_node <- NULL
  try(cm_RetirementRate_node <- remDr$findElement(using = "css selector", "div.mt10 > table > tbody > tr > td:nth-child(2) > div.bg11 > strong"))
  if (length(cm_RetirementRate_node) == 0) {
    cm_RetirementRate <- NA
  } else {
    cm_RetirementRate <- cm_RetirementRate_node$getElementText()
  }
  cm_RetirementRate_total <- c(cm_RetirementRate_total, unlist(cm_RetirementRate))
  
  i <- i + 1
  cat("기업명:",length(cm_nm_total), "본사주소:", length(cm_addr_total), "직원수:", length(cm_emp_total),
      "매출액(2020년):", length(cm_sales_total), "입사율(연간 입사자):",length(cm_EntryRate_total), 
      "퇴사율(연간 퇴사자):", length(cm_RetirementRate_total), "\n")
}

cm_EntryRate_total[6826]
cm_Info_total <- NULL
cm_Info_total <- data.frame(cm_nm_total, cm_addr_total, cm_emp_total,
                            cm_sales_total, cm_EntryRate_total, cm_RetirementRate_total); str(cm_addr_total)
names(cm_Info_total) <- c("기업명", "본사주소", "직원수", "2020년매출액(천원)", "입사율(연간 입사자)", "퇴사율(연간 퇴사자")
View(cm_Info_total)
cm_Info_total_r1 <- cm_Info_total
cm_Info_total_r1[2] <- gsub("\\, [[:alnum:]]+", "", cm_Info_total[2])
View(cm_Info_total_r1)
write.csv(cm_Info_total, "project/output/nicebizinfo_cm_Info.csv", fileEncoding = "UTF-8")
