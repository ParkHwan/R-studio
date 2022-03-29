#문제1

product_click <- read.table("data/product_click.log", sep=" ", col.names=c("time", "product"))

head(product_click)
click_table <- table(product_click$product)

png(filename="output/clicklog1.png", height = 500, width = 700, bg = "white")
barplot(click_table,
        main="세로바 그래프 실습",
        col.main="red",
        xlab="상품ID",
        ylab="클릭수")
dev.off()

#문제2
product_click <- read.table("data/product_click.log", sep=" ", col.names=c("time", "product"))
hour_click <- as.numeric(substr(product_click$time, 9,10))
hour_table <- table(hour_click)
names(hour_table) <- paste(names(hour_table),"~",(as.numeric(names(hour_table))+1))
png(filename="output/clicklog2.png",width=500,height=500,bg="white")
pie(hour_table,
    col=rainbow(17) ,
    main="파이그래프 실습")
dev.off()