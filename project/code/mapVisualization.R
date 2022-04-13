library(showtext)
showtext_auto() 
font_add(family = "cat", regular = "fonts/HoonWhitecatR.ttf")
font_add(family = "dog", regular = "fonts/THEdog.ttf")
font_add(family = "maple", regular = "fonts/MaplestoryBold.ttf")

library(ggplot2)
library(ggiraphExtra)
library(tibble)
library(kormaps2014)
library(dplyr) 
library(stringr)
library(plyr)

seoul_code <- kormap2 %>% 
  filter(as.numeric(as.character(kormap2$code)) <= 11250) %>% 
  select(name1, code) %>% 
  unique
names(seoul_code) <- c("행정구명", "code")
View(kormap2); View(seoul_code)

join_data <- read.csv("project/output/jobdata3.csv")
View(join_data)

cm_addr <- join_data %>% 
            filter(!is.na(본사주소)) %>% 
            select(기업명, 산업군, 평점, 본사주소)
View(cm_addr)

cm_addr$본사주소 <- gsub("(\\,[[:space:][:alnum:]]+)|(\\.[[:space:][:alnum:]]+)", "", cm_addr$본사주소)

cm_addr_seoul <- cm_addr %>%
  filter(str_detect(cm_addr$본사주소, "서울")) %>% 
  unique
View(cm_addr_seoul)
tally(cm_addr_seoul)
i <- 1
seoulCMaddr <- NULL
while (i <= length(seoul_code$행정구명)) {
  cm_addr_seoul1 <- cm_addr_seoul %>% 
                    filter(str_detect(cm_addr_seoul$본사주소, as.character(seoul_code$행정구명[i]))) %>% 
                    bind_cols(seoul_code[i,])
  seoulCMaddr  <- rbind(seoulCMaddr, cm_addr_seoul1)
  i <- i + 1
}
View(seoulCMaddr)
seoulCMaddr_count <- seoulCMaddr %>%
                      group_by(code, 행정구명, 산업군) %>%
                      tally

# IT/웹/통신 별
seoulCMaddr_IT <- seoulCMaddr %>%
                      filter(산업군 == "IT/웹/통신") %>% 
                      group_by(code, 행정구명, 산업군) %>%
                      tally
# 건설업 별
seoulCMaddr_Con <- seoulCMaddr %>%
                      filter(산업군 == "건설업") %>% 
                      group_by(code, 행정구명, 산업군) %>%
                      tally
# 교육업 별
seoulCMaddr_Edu <- seoulCMaddr %>%
                      filter(산업군 == "교육업") %>% 
                      group_by(code, 행정구명, 산업군) %>%
                      tally
# 기관/협회 별
seoulCMaddr_Ins <- seoulCMaddr %>%
                      filter(산업군 == "기관/협회") %>% 
                      group_by(code, 행정구명, 산업군) %>%
                      tally
# 미디어/디자인 별
seoulCMaddr_Media <- seoulCMaddr %>%
                      filter(산업군 == "미디어/디자인") %>% 
                      group_by(code, 행정구명, 산업군) %>%
                      tally
# 서비스업 별
seoulCMaddr_Ser <- seoulCMaddr %>%
                      filter(산업군 == "서비스업") %>% 
                      group_by(code, 행정구명, 산업군) %>%
                      tally
# 유통/무역/운송 별 
seoulCMaddr_Trade <- seoulCMaddr %>%
                      filter(산업군 == "유통/무역/운송") %>% 
                      group_by(code, 행정구명, 산업군) %>%
                      tally
# 은행/금융업 별 
seoulCMaddr_Fin <- seoulCMaddr %>%
                      filter(산업군 == "은행/금융업") %>% 
                      group_by(code, 행정구명, 산업군) %>%
                      tally
# 의료/제약/복지 별 
seoulCMaddr_Medical <- seoulCMaddr %>%
                      filter(산업군 == "의료/제약/복지") %>% 
                      group_by(code, 행정구명, 산업군) %>%
                      tally
# 제조/화학 별 
seoulCMaddr_Manuf <- seoulCMaddr %>%
                      filter(산업군 == "제조/화학") %>% 
                      group_by(code, 행정구명, 산업군) %>%
                      tally
seoulCMaddr_3orMore <- seoulCMaddr %>%
                      filter(평점 >= 3) %>% 
                      group_by(code, 행정구명, 산업군) %>%
                      tally
View(seoulCMaddr_count)
cm_seoul_max <- seoulCMaddr_count %>%
                  filter(n == max(n))

cm_seoul_max_3orMore <- seoulCMaddr_3orMore %>%
                          filter(n == max(n))

View(cm_seoul_max)  
seoulmap <- kormap2 %>% filter(startsWith(as.character(code), '11'))

i <- 1
seoul_centerMap <- NULL
while (i <= length(seoul_code$행정구명)) {
  seoul_map <- seoulmap %>% 
    filter(name1 == seoul_code$행정구명[i]) %>% 
    dplyr::select (name1, code, long, lat) %>% 
    dplyr::summarise(행정구명 = unique(name1), code = unique(code), mean_long = mean(long), mean_lat = mean(lat))
  seoul_centerMap  <- rbind(seoul_centerMap, seoul_map)
  i <- i + 1
}

seoul_CMmap <- join_all(list(cm_seoul_max, seoul_centerMap1[-2]), by = "행정구명", type = "left")
seoul_CMmap_IT <- join_all(list(seoul_centerMap1, seoulCMaddr_IT[-1]), by = "행정구명", type = "left")
seoul_CMmap_Con <- join_all(list(seoul_centerMap1, seoulCMaddr_Con[-1]), by = "행정구명", type = "left")
seoul_CMmap_Edu <- join_all(list(seoul_centerMap1, seoulCMaddr_Edu[-1]), by = "행정구명", type = "left")
seoul_CMmap_Ins <- join_all(list(seoul_centerMap1, seoulCMaddr_Ins[-1]), by = "행정구명", type = "left")
seoul_CMmap_Media <- join_all(list(seoul_centerMap1, seoulCMaddr_Media[-1]), by = "행정구명", type = "left")
seoul_CMmap_Ser <- join_all(list(seoul_centerMap1, seoulCMaddr_Ser[-1]), by = "행정구명", type = "left")
seoul_CMmap_Trade <- join_all(list(seoul_centerMap1, seoulCMaddr_Trade[-1]), by = "행정구명", type = "left")
seoul_CMmap_Fin <- join_all(list(seoul_centerMap1, seoulCMaddr_Fin[-1]), by = "행정구명", type = "left")
seoul_CMmap_Medical <- join_all(list(seoul_centerMap1, seoulCMaddr_Medical[-1]), by = "행정구명", type = "left")
seoul_CMmap_Manuf <- join_all(list(seoul_centerMap1, seoulCMaddr_Manuf[-1]), by = "행정구명", type = "left")
seoul_CMmap_3orMore <- join_all(list(seoul_centerMap1, cm_seoul_max_3orMore[-1]), by = "행정구명", type = "left")

View(seoul_centerMap1)
seoul_CMmap_r1 <- seoul_CMmap
seoul_centerMap1 <- seoul_CMmap_r1[c(1,2,5:6)]
seoul_centerMap1 <- seoul_centerMap1[-c(8, 12:16, 18),]

ggplot(seoul_CMmap_Medical, aes(map_id = code, fill = n))+
  geom_map(map = seoulmap, colour = "black", size = 0.1)+
  expand_limits(x = seoulmap$long, y = seoulmap$lat)+
  scale_fill_gradient(low = "#FFDFD3", 
                      high = "#D291BC", 
                      space = "Lab", 
                      guide = "colourbar")+
  ggtitle('서울 행정구별 분포되어 있는 의료/제약/복지 업계')+
  labs(fill = "의료/제약/복지") + 
  theme_void() + 
  theme(title = element_text(family = "maple", size = 20),
        legend.position = c(.15, .85),
        legend.title = element_text(family = "maple", face = "bold", size = 15),
        legend.text = element_text(family = "maple", face = "bold", size = 10),
        legend.box.background = element_rect(fill = "#e6f2ff"), 
        legend.box.margin = margin(6, 6, 6, 6)) +
  geom_text(data = seoul_CMmap_Medical, 
            aes(x = mean_long, 
                y = mean_lat,
                label = paste(행정구명, ifelse(is.na(n), 0, n), sep = "\n")),
            size = 5,
            family = "dog",
            fontface = "bold") -> seoul_CM_map_visual_Medical
seoul_CM_map_visual
seoul_CM_map_visual_IT
seoul_CM_map_visual_Con
seoul_CM_map_visual_Edu
seoul_CM_map_visual_Ins
seoul_CM_map_visual_Media
seoul_CM_map_visual_Ser
seoul_CM_map_visual_Trade
seoul_CM_map_visual_Fin
seoul_CM_map_visual_Medical
seoul_CM_map_visual_Manuf
seoul_CM_map_visual_3orMore

ggsave("project/output/seoul_CM_map_visual_Medical.png", seoul_CM_map_visual_Medical, dpi = 120)
# cm_addr_lonlat <- geocode(enc2utf8(cm_addr_seoul), source = "google")
# save(cm_addr_lonlat, file = "project/output/cm_addr_lonlat.RData")
# load("project/output/cm_addr_lonlat.RData")

gyeonggidomap <- kormap2 %>% filter(startsWith(as.character(code), '31'))

gyeonggi_code <- kormap2 %>% 
  filter(as.numeric(as.character(kormap2$code)) <= 31380 & as.numeric(as.character(kormap2$code)) > 29010) %>% 
  select(name1, code) %>% 
  unique
names(gyeonggi_code) <- c("행정구명", "code"); View(gyeonggi_code)
gyeonggi_code$행정구명 <- gsub("시", "시 ", gyeonggi_code$행정구명)
gyeonggi_code$행정구명 <- gsub("시 흥", "시흥", gyeonggi_code$행정구명)

cm_addr_gyeonggi <- cm_addr %>%
  filter(str_detect(cm_addr$본사주소, "경기")) %>% 
  unique

i <- 1
gyeonggiCMaddr <- NULL
while (i <= length(gyeonggi_code$행정구명)) {
  cm_addr_gyeonggi1 <- cm_addr_gyeonggi %>% 
    filter(str_detect(cm_addr_gyeonggi$본사주소, as.character(gyeonggi_code$행정구명[i]))) %>% 
    bind_cols(gyeonggi_code[i,])
  gyeonggiCMaddr  <- rbind(gyeonggiCMaddr, cm_addr_gyeonggi1)
  i <- i + 1
}
View(gyeonggiCMaddr_count)

gyeonggiCMaddr_count <- gyeonggiCMaddr %>%
                          group_by(code, 행정구명, 산업군) %>%
                          tally

# IT/웹/통신 별
gyeonggiCMaddr_IT <- gyeonggiCMaddr %>%
                    filter(산업군 == "IT/웹/통신") %>% 
                    group_by(code, 행정구명, 산업군) %>%
                    tally
# 건설업 별
gyeonggiCMaddr_Con <- gyeonggiCMaddr %>%
                    filter(산업군 == "건설업") %>% 
                    group_by(code, 행정구명, 산업군) %>%
                    tally
# 교육업 별
gyeonggiCMaddr_Edu <- gyeonggiCMaddr %>%
                    filter(산업군 == "교육업") %>% 
                    group_by(code, 행정구명, 산업군) %>%
                    tally
# 기관/협회 별
gyeonggiCMaddr_Ins <- gyeonggiCMaddr %>%
                    filter(산업군 == "기관/협회") %>% 
                    group_by(code, 행정구명, 산업군) %>%
                    tally
# 미디어/디자인 별
gyeonggiCMaddr_Media <- gyeonggiCMaddr %>%
                    filter(산업군 == "미디어/디자인") %>% 
                    group_by(code, 행정구명, 산업군) %>%
                    tally
# 서비스업 별
gyeonggiCMaddr_Ser <- gyeonggiCMaddr %>%
                    filter(산업군 == "서비스업") %>% 
                    group_by(code, 행정구명, 산업군) %>%
                    tally
# 유통/무역/운송 별 
gyeonggiCMaddr_Trade <- gyeonggiCMaddr %>%
                    filter(산업군 == "유통/무역/운송") %>% 
                    group_by(code, 행정구명, 산업군) %>%
                    tally
# 은행/금융업 별 
gyeonggiCMaddr_Fin <- gyeonggiCMaddr %>%
                    filter(산업군 == "은행/금융업") %>% 
                    group_by(code, 행정구명, 산업군) %>%
                    tally
# 의료/제약/복지 별 
gyeonggiCMaddr_Medical <- gyeonggiCMaddr %>%
                    filter(산업군 == "의료/제약/복지") %>% 
                    group_by(code, 행정구명, 산업군) %>%
                    tally
# 제조/화학 별 
gyeonggiCMaddr_Manuf <- gyeonggiCMaddr %>%
                    filter(산업군 == "제조/화학") %>% 
                    group_by(code, 행정구명, 산업군) %>%
                    tally
gyeonggiCMaddr_3orMore <- gyeonggiCMaddr %>%
                    filter(평점 >= 3) %>% 
                    group_by(code, 행정구명, 산업군) %>%
                    tally

cm_gyeonggi_max <- gyeonggiCMaddr_count %>%
                    filter(n == max(n))

i <- 1
gyeonggi_centerMap <- NULL
while (i <= length(gyeonggi_code$행정구명)) {
  gyeonggi_map <- gyeonggidomap %>% 
    filter(name1 == gsub("시 ", "시", gyeonggi_code$행정구명[i])) %>% 
    dplyr::select (name1, code, long, lat) %>% 
    dplyr::summarise(행정구명 = unique(name1), code = unique(code), mean_long = mean(long), mean_lat = mean(lat))
  gyeonggi_centerMap  <- rbind(gyeonggi_centerMap, gyeonggi_map)
  i <- i + 1
}
View(gyeonggi_CMmap_r1)
gyeonggi_centerMap <- gyeonggi_centerMap[-1]

gyeonggi_centerMap_r1 <- join_all(list(gyeonggi_CMmap_r1[-(3:4)], gyeonggi_centerMap[-(2:3)]), by = "code", type = "left")
gyeonggi_CMmap <- join_all(list(cm_gyeonggi_max, gyeonggi_centerMap_r1[-1]), by = "code", type = "left")
gyeonggi_CMmap_IT <- join_all(list(gyeonggi_centerMap_r1, gyeonggiCMaddr_IT[-2]), by = "code", type = "left")
gyeonggi_CMmap_Con <- join_all(list(gyeonggi_centerMap_r1, gyeonggiCMaddr_Con[-2]), by = "code", type = "left")
gyeonggi_CMmap_Edu <- join_all(list(gyeonggi_centerMap_r1, gyeonggiCMaddr_Edu[-2]), by = "code", type = "left")
gyeonggi_CMmap_Ins <- join_all(list(gyeonggi_centerMap_r1, gyeonggiCMaddr_Ins[-2]), by = "code", type = "left")
gyeonggi_CMmap_Media <- join_all(list(gyeonggi_centerMap_r1, gyeonggiCMaddr_Media[-2]), by = "code", type = "left")
gyeonggi_CMmap_Ser <- join_all(list(gyeonggi_centerMap_r1, gyeonggiCMaddr_Ser[-2]), by = "code", type = "left")
gyeonggi_CMmap_Trade <- join_all(list(gyeonggi_centerMap_r1, gyeonggiCMaddr_Trade[-2]), by = "code", type = "left")
gyeonggi_CMmap_Fin <- join_all(list(gyeonggi_centerMap_r1, gyeonggiCMaddr_Fin[-2]), by = "code", type = "left")
gyeonggi_CMmap_Medical <- join_all(list(gyeonggi_centerMap_r1, gyeonggiCMaddr_Medical[-2]), by = "code", type = "left")
gyeonggi_CMmap_Manuf <- join_all(list(gyeonggi_centerMap_r1, gyeonggiCMaddr_Manuf[-2]), by = "code", type = "left")
gyeonggi_CMmap_3orMore <- join_all(list(gyeonggi_centerMap_r1, gyeonggiCMaddr_3orMore[-2]), by = "code", type = "left")
View(gyeonggi_CMmap_IT)

gyeonggi_CMmap_r1 <- gyeonggi_CMmap


ggplot(gyeonggi_CMmap, aes(map_id = code, fill = n))+
  geom_map(map = gyeonggidomap, colour = "black", size = 0.1)+
  expand_limits(x = gyeonggidomap$long, y = gyeonggidomap$lat)+
  scale_fill_gradient(low = "#FFDFD3", 
                      high = "#D291BC", 
                      space = "Lab", 
                      guide = "colourbar")+
  ggtitle("경기도 행정구별 가장 많이 분포되어 있는 산업군")+
  labs(fill = "산업군") + 
  theme_void() + 
  theme(title = element_text(family = "maple", size = 20),
        legend.position = c(.15, .85),
        legend.title = element_text(family = "maple", face = "bold", size = 15),
        legend.text = element_text(family = "maple", face = "bold", size = 10),
        legend.box.background = element_rect(fill = "#e6f2ff"), 
        legend.box.margin = margin(6, 6, 6, 6)) +
  geom_text(data = gyeonggi_CMmap, 
            aes(x = mean_long, 
                y = mean_lat,
                label = paste(행정구명, 산업군, ifelse(is.na(n), 0, n), sep = "\n")),
            size = 4,
            family = "dog",
            fontface = "bold",
            check_overlap = T) -> gyeonggi_CM_map_visual

gyeonggi_CM_map_visual
gyeonggi_CM_map_visual_IT
gyeonggi_CM_map_visual_Con
gyeonggi_CM_map_visual_Edu
gyeonggi_CM_map_visual_Ins
gyeonggi_CM_map_visual_Media
gyeonggi_CM_map_visual_Ser
gyeonggi_CM_map_visual_Trade
gyeonggi_CM_map_visual_Fin
gyeonggi_CM_map_visual_Medical
gyeonggi_CM_map_visual_Manuf

ggsave("project/output/gyeonggi_CM_map_visual.png", gyeonggi_CM_map_visual, dpi = 120)
gyeonggi_CMmap_Medical$mean_long[5] <- gyeonggi_CMmap_Medical$mean_long[5] - 0.1    # 좌우 이동
gyeonggi_CMmap_Medical$mean_lat[5] <- gyeonggi_CMmap_Medical$mean_lat[5] + 0.01      # 상하 이동

save(list=ls(),file="all.RData") # "all.rda" 
# load("all.RData")