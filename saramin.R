library(RSelenium)
library(XML)
library(rvest)
library(httr)
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
company_info_total <- NULL; support_info_total <- NULL
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

while (TRUE) {
  company_nm_node <- remDr$findElements(using = "css selector", "#default_list_wrap div.list_item > div.company_nm")
  company_nm <- sapply(company_nm_node, function(x) {x$getElementText()})
  company_nm_total <- c(company_nm_total, unlist(company_nm))
  
  notification_info_node <- remDr$findElements(using = "css selector", "#default_list_wrap div.list_item > div.notification_info")
  notification_info <- sapply(notification_info_node, function(x) {x$getElementText()})
  notification_info_total <- c(notification_info_total, unlist(notification_info))
  
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

    company_nm_node <- remDr$findElements(using = "css selector", "#default_list_wrap div.list_item > div.company_nm")
    company_nm <- sapply(company_nm_node, function(x) {x$getElementText()})
    company_nm_total <- c(company_nm_total, unlist(company_nm))
    
    notification_info_node <- remDr$findElements(using = "css selector", "#default_list_wrap div.list_item > div.notification_info")
    notification_info <- sapply(notification_info_node, function(x) {x$getElementText()})
    notification_info_total <- c(notification_info_total, unlist(notification_info))
    
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
  try(next_Page <- remDr$findElement(using = "css selector", "#default_list_wrap > div.pagination > a.btnNext"))
  if (length(next_Page) == 0) {
    break
  }
  next_Page$clickElement()
  Sys.sleep(1)
  i <- i + 2; prev_Page_no <- prev_Page_no + 1
}

company_nm_total_r1 <- gsub("\\(주\\)|\\(유\\)|㈜", "", company_nm_total)
notification_info_total_r1 <- gsub("인사통", "", notification_info_total)
job_Posting_total <- data.frame(company_nm_total_r1, notification_info_total_r1, recruit_condition_total, company_info_total, support_info_total)
names(job_Posting_total) <- c("기업명", "모집내용", "지원자격", "근무조건/근무지역", "마감일/등록일")
View(job_Posting_total)
write.csv(job_Posting_total, "output/saramin_job_posting.csv", fileEncoding = "UTF-8")
