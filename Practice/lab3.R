#문제1
summary(iris)
str(iris)

#문제2
x <- seq(1,5,1)
y <- seq(2,10,2)
df1 <- data.frame(x, y)
df1

#문제3
col1 <- seq(1,5,1)
col2 <- letters[1:5]
col3 <- seq(6,10,1)
df2 <- data.frame(col1, col2, col3)
df2

#문제4
제품명 <- c('사과','딸기','수박')
가격 <- c(1800,1500,3000)
판매량 <- c(24,38,13)
df3 <- data.frame(제품명, 가격, 판매량)

#문제5
mean(df3$가격)
mean(df3$판매량)

#문제6
name <- c("Potter", "Elsa", "Gates", "Wendy", "Ben")
gender <- factor(c("M", "F", "M", "F", "M"))
math <- c(85, 76, 99, 88, 40)
df4 <- data.frame(name,gender,math)
df4$stat <- c(76, 73, 95, 82, 35)
df4$score <- df4$math+df4$stat
str(df4)
df4$grade <- ifelse(df4$score >= 150,"A", ifelse(df4$score >= 100,"B", ifelse(df4$score >= 70,"C","D")))
df4

myemp <- read.csv("data/emp.csv")
#문제7
class(myemp)
str(myemp)

#문제8
myemp[3:5,]

#문제9
myemp[,-4]

#문제10
myemp['ename']

#문제11
myemp[c('ename','sal')]

#문제12
myemp
subset(myemp, myemp$job == 'SALESMAN', c('ename', 'sal', 'job'))

#문제13
subset(myemp, myemp$sal >= 1000 & myemp$sal <= 3000 , c('ename', 'sal', 'deptno'))

#문제14
subset(myemp, myemp$job != 'ANALYST' , c('ename', 'job', 'sal'))

#문제15
subset(myemp, myemp$job == 'ANALYST' | myemp$job == 'SALESMAN' , c('ename', 'job', 'sal'))

#문제16
subset(myemp, is.na(myemp$comm), c('ename','sal'))

#문제17
myemp[c(order(myemp$sal)),]

#문제18
str(myemp)
summary(myemp)
dim(myemp)

#문제19
table(myemp$deptno)

#문제20
table(myemp$job)

