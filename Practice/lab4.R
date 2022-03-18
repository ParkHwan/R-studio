rm(list=ls())

#문제1
l1 <- list(
  name = 'scott',
  sal = 3000
)
l1
result <- l1[['sal']]*2
result

#문제2
l2 <-list(
  c("scott"),c(100,200,300)
)
l2

#문제3
l3 <- list(c(3,5,7), c("A", "B", "C"))
l3[[2]][1] <- "Alpha"
l3

#문제4
l4 <- list(alpha=0:4, beta=sqrt(1:5), gamma=log(1:5))
l4[[1]]+10

#문제5
emp <- read.csv("data/emp.csv")
l5 = list(
  data1 = LETTERS[],
  data2 = emp[1:3,],
  data3 = l4
)

l5$data1[1]
l5$data2$ename
l5$data3$gamma

#문제6
l6 <- list(
  math=list(95, 90),
  writing=list(90, 85),
  reading=list(85, 80)
)
mean1 <- mean(unlist(l6))
mean1
