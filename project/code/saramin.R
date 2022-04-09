library(RSelenium)
library(XML)
library(rvest)
library(httr)
library(dplyr)
remDr <- remoteDriver(remoteServerAddr = "localhost" , 
                      port = 4445, browserName = "chrome")
remDr$open()

saramin_site <- "https://www.saramin.co.kr/zf_user/jobs/list/domestic"
# 사람인 서울 전체로 채용 공고 설정 url
url <- "https://www.saramin.co.kr/zf_user/jobs/list/domestic?loc_mcd=101000&panel_type=&search_optional_item=n&search_done=y&panel_count=y"
remDr$navigate(url)
Sys.sleep(1)

i <- 2
prev_Page_no <- 1
company_nm_total <- NULL; notification_info_total <- NULL; recruit_condition_total <- NULL
company_info_total <- NULL; support_info_total <- NULL; job_group_total <- NULL
search_click <- remDr$findElement(using = "css selector", "li.keyword_section > button") # 검색어 입력 하기전 클릭 태그
search_click$clickElement()                                                              # 검색어 입력 위한 클릭 이벤트 발생
word_input <- remDr$findElement(using = "css selector", "#total_ipt_keyword")            # 검색어 입력 위한 태그
word_input$sendKeysToElement(list("데이터 엔지니어", key = "enter"))                     # 검색어 입력
inputwoard_search <- remDr$findElement(using = "css selector", "#search_btn")            # 입력된 검색어와 매칭되는 검색 태그
inputwoard_search$clickElement()
Sys.sleep(1) 
# 검색 클릭 이벤트 발생
# remDr$mouseMoveToLocation(webElement = word_search1)
# remDr$click()
# remDr$executeScript("arguments[0].click()",list(word_search1))

# job_Posting_node <- remDr$findElements(using = "css selector", "#default_list_wrap div.list_item")              # 전체 채용 공고에 대한 태그
# job_Posting <- sapply(job_Posting_node, function(x) {x$getElementText()})                                       # 채용 공고 관련 컨텐츠 전부 크롤링
# job_Posting_total <- c(job_Posting_total, unlist(job_Posting))                                                  # 결과값 누적하여 벡터로 출력
# remDr$goBack() # 페이지 뒤로 가기
# remDr$goForward() # 페이지 앞으로 가기
# 사람인 채용 공고: 검색어에 따른 채용공고 크롤링

while (TRUE) {
  company_nm_node <- remDr$findElements(using = "css selector", "#default_list_wrap div.company_nm > a> span")
  company_nm <- sapply(company_nm_node, function(x) {x$getElementText()})
  company_nm_total <- c(company_nm_total, unlist(company_nm))
  
  notification_info_node <- remDr$findElements(using = "css selector", "#default_list_wrap div.job_tit > a > span")
  notification_info <- sapply(notification_info_node, function(x) {x$getElementText()})
  notification_info_total <- c(notification_info_total, unlist(notification_info))
  
  job_group_node <- remDr$findElements(using = "css selector", "#default_list_wrap div.job_meta > span")
  job_group <- sapply(job_group_node, function(x) {x$getElementText()})
  job_group_total <- c(job_group_total, unlist(job_group))
  
  recruit_condition_node <- remDr$findElements(using = "css selector", "#default_list_wrap div.list_item > div.recruit_condition")
  recruit_condition <- sapply(recruit_condition_node, function(x) {x$getElementText()})
  recruit_condition_total <- c(recruit_condition_total, unlist(recruit_condition))
  
  company_info_node <- remDr$findElements(using = "css selector", "#default_list_wrap div.list_item > div.company_info")
  company_info <- sapply(company_info_node, function(x) {x$getElementText()})
  company_info_total <- c(company_info_total, unlist(company_info))
  
  support_info_node <- remDr$findElements(using = "css selector", "#default_list_wrap div.list_item > div.support_info")
  support_info <- sapply(support_info_node, function(x) {x$getElementText()})
  support_info_total <- c(support_info_total, unlist(support_info))

  for (i in i:(i + 8)) {
    next_to_page <- NULL
    # next_to_page <- remDr$findElement(using = "css selector", paste0("#default_list_wrap > div.pagination > a:nth-child(", i, ")"))
    try(next_to_page <- remDr$findElement(using = "css", paste0("[page=", "'", i, "']")))
    if (length(next_to_page) == 0) {
      break
    }
    curr_Page_no <- as.numeric(next_to_page$getElementText())
    next_to_page$clickElement()
    cat(paste("이전:",prev_Page_no, "page 현재:", curr_Page_no, "page\n"))
    prev_Page_no <- curr_Page_no
    Sys.sleep(1)

    company_nm_node <- remDr$findElements(using = "css selector", "#default_list_wrap div.company_nm > a> span")
    company_nm <- sapply(company_nm_node, function(x) {x$getElementText()})
    company_nm_total <- c(company_nm_total, unlist(company_nm))
    
    notification_info_node <- remDr$findElements(using = "css selector", "#default_list_wrap div.job_tit > a > span")
    notification_info <- sapply(notification_info_node, function(x) {x$getElementText()})
    notification_info_total <- c(notification_info_total, unlist(notification_info))
    
    job_group_node <- remDr$findElements(using = "css selector", "#default_list_wrap div.job_meta > span")
    job_group <- sapply(job_group_node, function(x) {x$getElementText()})
    job_group_total <- c(job_group_total, unlist(job_group))
    
    recruit_condition_node <- remDr$findElements(using = "css selector", "#default_list_wrap div.list_item > div.recruit_condition")
    recruit_condition <- sapply(recruit_condition_node, function(x) {x$getElementText()})
    recruit_condition_total <- c(recruit_condition_total, unlist(recruit_condition))
    
    company_info_node <- remDr$findElements(using = "css selector", "#default_list_wrap div.list_item > div.company_info")
    company_info <- sapply(company_info_node, function(x) {x$getElementText()})
    company_info_total <- c(company_info_total, unlist(company_info))
    
    support_info_node <- remDr$findElements(using = "css selector", "#default_list_wrap div.list_item > div.support_info")
    support_info <- sapply(support_info_node, function(x) {x$getElementText()})
    support_info_total <- c(support_info_total, unlist(support_info))
  }
  next_Page <- NULL
  try(next_Page <- remDr$findElement(using = "css selector", "#default_list_wrap > div.pagination > a.btnNext"))
  if (length(next_Page) == 0) {
    break
  }
  next_Page$clickElement()
  Sys.sleep(1)
  i <- i + 2; prev_Page_no <- prev_Page_no + 1
}

company_nm_total_r1 <- gsub("\\(주\\)|\\(유\\)|㈜|주식회사|유한책임회사|(재)", "", company_nm_total)
# notification_info_total_r1 <- gsub("인사통", "", notification_info_total)
job_Posting_total <- data.frame(company_nm_total_r1, notification_info_total, job_group_total, recruit_condition_total, company_info_total, support_info_total)
names(job_Posting_total) <- c("기업명", "모집내용", "직군", "지원자격", "근무조건/근무지역", "마감일/등록일")
job_Posting_total$기업명 <- trimws(job_Posting_total$기업명)
View(job_Posting_total)

write.csv(job_Posting_total, "project/output/saramin_job_posting.csv", fileEncoding = "UTF-8")

# 채용공고 기업 주소 크롤링
notification_info_node <- remDr$findElement(using = "css selector", "#default_list_wrap div.list_body > div:nth-of-type(2) div.job_tit > a")
notification_info_node$clickElement()
remDr$getCurrentWindowHandle()
windowId <- remDr$getWindowHandles()
remDr$switchToWindow("CDwindow-E821C0459A11CE6ACA84A6328A2E032D")
remDr$switchToWindow(windowId[[2]]); remDr$goBack()
company_addr_node <- remDr$findElement(using = "css selector", "#content > div.wrap_jview > div.jview.jview-0-42702901 > div.wrap_jv_cont > div.jv_cont.jv_company > div:nth-child(2) > div.wrap_info > div.info > dl.wide > dd")
company_addr_node$getElementText(); remDr$closeWindow()
?remoteDriver
