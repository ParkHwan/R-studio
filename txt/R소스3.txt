# print() 함수와 cat() 함수

print(100)
print(pi)
data <- "가나다"
print(data)
print(data, quote=FALSE)
v1 <- c("사과", "바나나", "포도")
print(v1)
print(v1, print.gap=10)
cat(100)
cat(100,200)
cat(100,200,"\n")
cat("aaa", "bbb", "ccc", "ddd", "\n")
cat(v1, "\n")
cat(v1, sep="-", "\n")

print(paste("R", "은 통계분석", "전용 언어입니다."))
cat("R", "은 통계분석", "전용 언어입니다.", "\n")



#제어문
#if else
(randomNum <-sample(1:10,1))
if(randomNum>5){
  cat(randomNum,": 5보다 크군요","\n")
}else{
  cat(randomNum,": 5보다 작거나 같군요","\n")
}

if(randomNum%%2 == 1) {
  cat(randomNum,": 홀수\n")
} else {
  cat(randomNum,": 짝수","\n")
}


if(randomNum%%2 == 1){
  cat(randomNum,";홀수","\n")
  cat("종료")
}else{
  cat(randomNum,";짝수","\n")
  cat("종료")
}

if(randomNum%%2 == 1){
  cat(randomNum,": 홀수")
  cat("종료")
}else{
  cat(randomNum,": 짝수")
  cat("종료")
}

score <- sample(0:100, 1)  # 0~100 숫자 한 개를 무작위로 뽑아서
if (score >=90){
  cat(score,"는 A등급입니다","\n")
}else if (score >=80){
  cat(score,"는 B등급입니다","\n")
}else if (score >=70){
  cat(score,"는 C등급입니다","\n")
}else if (score >=60){
  cat(score,"는 D등급입니다","\n")
}else {
  cat(score,"는 F등급입니다","\n")
}

#for문
#for 실습
for(data in month.name) 
  print(data)
for(data in month.name)print(data);print("가나다")
for(data in month.name){print(data);print("가나다")}

for(n in 1:5)
  cat("hello?","\n")

for(i in 1:5){
  for(j in 1:5){
    cat("i=",i,"j=",j,"\n")
  }
}
# 구구단
for(dan in 1:9){
  for(num in 1:9){
    cat(dan,"x",num,"=",dan*num,"\t") # \n : 개행문자, \t : 탭문자
  }
  cat("\n")
}



for(i in 1:9){
  for(j in 1:9){
    if(i*j>30){
      break 
    } 
    cat(i,"*",j,"=",i*j,"\t")
  }
  cat("\n")
}

bb <- F
for(i in 1:9){
  for(j in 1:9){
    if(i*j>30){
      bb<-T
      break
    } 
    cat(i,"*",j,"=",i*j,"\t")
  }
  cat("\n")
  if(bb) #bb가 TRUE이면
    break
}



#while문
i<-1
while(i <= 10){
  cat(i,"\n")
  i <- i+1
}
cat("종료 후 :",i,"\n")

i<-1
while (i<=10) {
  cat(i,"\n")
}

i<-1
while (i<=10) {
  cat(i,"\n")
  i<-i+2
}

i<-1
while (i<=10) {
  cat(i,"\n")
  i<-i+1
}

#switch 문을 대신하는 함수
(month <- sample(1:12,1))
(month <- paste(month,"월",sep="")) # "3월"  "3 월"
result <- switch(EXPR=month,
                 "12월"=,"1월"=,"2월"="겨울",
                 "3월"=,"4월"=,"5월"="봄",
                 "6월"=,"7월"=,"8월"="여름",
                 "가을")
cat(month,"은 ",result,"입니다\n",sep="")

num <- sample(1:10,1)
num
switch(EXPR = num,"A","B","C","D")

for(num in 1:10){
  cat(num,":",switch(EXPR = num,"A","B","C","D"),"\n")
}

for(num in 1:10){
  num <- as.character(num) 
  cat(num,":",switch(EXPR = num,
                     "7"="A","8"="B","9"="C","10"="D","ㅋ"),"\n")
}


for(data in month.name) 
  print(data)

for(data in month.name) 
  cat(data)

sum <- 0
for(i in 5:15){
  if(i%%10==0){
    break
  }
  sum <- sum + i
  print(paste(i,":",sum))
}

sum <- 0
for(i in 5:15){
  if(i%%10==0){
    break
  }
  sum <- sum + i
  cat(i,":",sum,"\n")
}

sum <-0
for(i in 5:15){
  if(i%%10==0){
    next;  #continue와 동일
  }
  sum <- sum + i
  print(paste(i,":",sum))
}

sumNumber <- 0
while(sumNumber <= 20) { 
  i <- sample(1:5, 1) 
  sumNumber <-sumNumber+i; 
  cat(sumNumber,"\n")
} 

repeat {
  cat("ㅋㅋㅋ\n")
}

sumNumber <- 0
repeat { 
  i <- sample(1:5, 1) 
  sumNumber <-sumNumber+i; 
  cat(sumNumber,"\n")
  if(sumNumber > 20)
    break;
}

# 객체를 파일에 저장하고 읽기

ls()
length(ls())
save(list=ls(),file="all.RData") # "all.rda" 
rm(list=ls())
ls()
load("all.RData")
ls()

# 파일에서 데이터 읽기
nums <- scan("data/sample_num.txt")
nums
word_ansi <- scan("data/sample_ansi.txt",what="")
word_ansi
words_utf8 <- scan("data/sample_utf8.txt", what="",encoding="UTF-8")
words_utf8
words_err <- scan("data/sample_utf8.txt", what="")
words_err

lines_ansi <- readLines("data/sample_ansi.txt")
lines_ansi 
lines_utf8 <- readLines("data/sample_utf8.txt",encoding="UTF-8")
lines_utf8

df2 <- read.table("data/product_click.log", stringsAsFactors = T)
str(df2)
head(df2)
summary(df2$V2)