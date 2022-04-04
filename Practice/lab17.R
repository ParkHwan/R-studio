library(ggplot2)
library(dplyr)
library(showtext)
showtext_auto() 
font_add(family = "cat", regular = "fonts/HoonWhitecatR.ttf")
font_add(family = "dog", regular = "fonts/THEdog.ttf")
font_add(family = "maple", regular = "fonts/MaplestoryBold.ttf")

mpg <- as.data.frame(ggplot2::mpg)
mpg

#문제1
ggplot(mpg,aes(cty,hwy)) + geom_point(col = "blue")
ggsave("output/result1.png", dpi=100)

#문제2
ggplot(mpg,aes(class)) + geom_bar(aes(fill=drv))
ggsave("output/result2.png", dpi=100)

#문제3
product_click <- read.table("data/product_click.log",
                            sep=" ",
                            col.names=c("time", "product"))
product_click
ggplot(product_click,aes(product)) + 
  geom_bar(fill=rainbow(10)) + 
  theme_light() +
  labs(x="상품ID", y="클릭수", title="제품당 클릭수",
  subtitle="제품당 클릭수를 바 그래프로 표현")
ggsave("output/result3.png", dpi=100)  

#문제4
library(treemap)
data("GNI2014")
?treemap
png("output/result4.png", width = 800, height = 500)
treemap(GNI2014, vSize="population", index=c("continent", "iso3"),
        title="전세계 인구 정보", fontsize.title = 20,
        fontfamily.title="maple",
        border.col = "green",
        width = 800, height = 500)
dev.off()
