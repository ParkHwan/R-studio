#문제1
library(showtext)
showtext_auto() 
font_add(family = "cat", regular = "fonts/HoonWhitecatR.ttf")
font_add(family = "dog", regular = "fonts/THEdog.ttf")
font_add(family = "maple", regular = "fonts/MaplestoryBold.ttf")
library(leaflet)
library(dplyr)
library(ggmap)

register_google(key='AIzaSyDy81EbO46BRSnX1DOgg_F84bhsdbku2z4')
addr <- "서울특별시 서초구 주흥11길 35-4"
msg <- '<strong>나의 집</a></strong><hr>home sweat home'
MyHome <- geocode(enc2utf8(addr))
View(MyHome)
save(MyHome, file="output/myhome.RData")
leaflet(MyHome) %>% 
  setView(lng = MyHome[1], 
          lat = MyHome[2], 
          zoom = 16) %>% 
  addTiles() %>% 
  addCircles(lng = ~lon, lat = ~lat, popup = msg)


#문제2

library(kormaps2014)
library(dplyr)

korpop2 <- rename(korpop2,
                  pop = 총인구_명,
                  name = 행정구역별_읍면동)
View(korpop2)
korpop2$name <- iconv(korpop2$name, "UTF-8", "CP949")

seoulmap <- kormap2 %>% filter(startsWith(as.character(code), '11'))
seoulpop <- korpop2 %>% filter(startsWith(as.character(code), '11'))
seoulmap$name <- iconv(seoulmap$name, "UTF-8", "CP949")

myseoulpop <- seoulpop
myseoulpop <- rename(myseoulpop,
                     외국인수 = 외국인_계_명)
myseoulpop$외국인수 <- as.numeric(myseoulpop$외국인수)
myseoulpop

ggplot(myseoulpop,aes(map_id=code, fill=외국인수))+
  geom_map(map = seoulmap, colour="black",size=0.1)+
  expand_limits(x= seoulmap$long,
                y = seoulmap$lat)+
  scale_fill_gradientn(colours = c('green', 'darkgreen'))+
  ggtitle('각 구별 외국인 수')+coord_map()

ggChoropleth(data = myseoulpop,
             aes(fill = 외국인수,
                 map_id = code,
                 tooltip = name),
             map = seoulmap,
             palette="Blues",
             title = "각 구별 외국인 수",
             interactive = T)
