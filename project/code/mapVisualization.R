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
str(seoul_code); View(seoul_code)

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
View(seoulCMaddr_count)
cm_seoul_max <- seoulCMaddr_count %>%
                  filter(n == max(n))

View(cm_seoul_max)  
seoulmap <- kormap2 %>% filter(startsWith(as.character(code), '11'))

cm_addr_gyeonggi <- cm_addr %>%
                      filter(str_detect(cm_addr$본사주소, "경기")) %>% 
                      select(기업명, 산업군, 본사주소)

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

seoul_CMmap <- join_all(list(cm_seoul_max, seoul_centerMap[-2]), by = "행정구명", type = "left")
View(seoul_CMmap)
seoul_CMmap_r1 <- seoul_CMmap
ggplot(seoulCMaddr_count, aes(map_id=code, fill=n))+
  geom_map(map = seoulmap, colour="black",size=0.1)+
  expand_limits(x= seoulmap$long,y = seoulmap$lat)+
  scale_fill_gradient(low = "#FFDFD3", 
                      high = "#D291BC", 
                      space = "Lab", 
                      guide = "colourbar")+
  ggtitle('서울내 기업 위치')+
  labs(fill = "산업군") + 
  theme_void() + 
  theme(legend.position = c(.15, .85)) +
  geom_text(data = seoul_CMmap, 
            aes(x = mean_long, 
                y = mean_lat,
                label = paste(행정구명, 산업군, n, sep = "\n")),
            size = 4,
            family = "dog",
            fontface = "bold")

# cm_addr_lonlat <- geocode(enc2utf8(cm_addr_seoul), source = "google")
# save(cm_addr_lonlat, file = "project/output/cm_addr_lonlat.RData")
# load("project/output/cm_addr_lonlat.RData")

gyeonggidomap <- kormap2 %>% filter(startsWith(as.character(code), '31'))




seoul_CMmap$mean_long[29] <- seoul_CMmap$mean_long[29] -0.01      # 좌우 이동
seoul_CMmap$mean_lat[30] <- seoul_CMmap$mean_lat[30] - 0.01 # 상하 이동
