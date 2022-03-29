#문제1
productClick1 <- readLines("./data/product_click.log")
productClick1 <- strsplit(productClick1, split = " ")
productClick1 <- unlist(productClick1)
productClick1 <- productClick1[seq(2,length(productClick1),2)]

productClickTable1 <- table(productClick1)

png(filename="clicklog1.png",width=600,height=800,bg="white")
barplot(productClickTable1, main="세로바 그래프 실습",
     col=terrain.colors(10),
     xlab="상품10",
     ylab="클릭수")
dev.off()

#문제2
productClick2 <- readLines("./data/product_click.log")
productClick2 <- strsplit(productClick2, split = " ")
productClick2 <- unlist(productClick2)
productClick2 <- productClick2[seq(1,length(productClick2),2)]
productClick2 <- substr(productClick2,9,10)

productClickTable2 <- table(as.character(productClick2))
productClickTable2 <- data.frame(productClickTable2)
names(productClickTable2) <- c("time","click")
productClickTable2

png(filename="clicklog2.png",width=500,height=500,bg="white")
pie(productClickTable2$click, 
    labels = productClickTable2$time ,
    col=rainbow(17) ,
    main="파이그래프 실습")
dev.off()



#문제3
score <- read.table("data/성적.txt", header=TRUE)
score1 <- score[3:5]
xname <- score$성명
png(filename="clicklog3.png",width=500,height=500,bg="white")
barplot(t(score1), main="학생별 점수", 
       col=rainbow(3),
       names.arg=xname)
legend(names(score1), cex=0.8, fill=rainbow(3), x="topright")
dev.off()
