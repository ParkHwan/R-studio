#문제1
grade <- sample(1:6,1)
if (grade <= 3){
  cat(grade,'학년은 저학년 입니다.')
}else{
  cat(grade,'학년은 고학년 입니다.')
}
#문제2
choice <- sample(1:5,1)
if (choice == 1){
  a <- 300+50
} else if(choice == 2) {
  a <- 300-50
} else if(choice == 3) {
  a <- 300*50
} else if(choice == 4) {
  a <-300/50
} else if(choice == 5) {
  a <- 300%%50
}
cat('결과값 :',a)

#문제3
count <- sample(3:10,1)
deco <- sample(1:3,1)
if (deco == 1){
  for(i in 1:count){
    cat('"*"')
  }
} else if (deco == 2){
  for(i in 1:count){
    cat('"$"')
  }
} else if (deco == 3){
  for(i in 1:count){
    cat('"#"')
  }
}

#문제4
score <- sample(0:100,1)
score_ten <- paste(score %/% 10); score_ten
score <- paste(score,"점",sep="")
level <- switch(EXPR = score_ten, 
                "10"=,"9"="A등급",
                "8"= "B등급",
                "7"= "C등급",
                "6"= "B등급",
                "F등급")
cat(score,'은 ',level,'입니다\n',sep="")

#문제5
char1 <- letters[]
char2 <- LETTERS[]
alpha <- paste(char2,char1, sep = "")
alpha

#문제6
read1 <- scan("data/iotest1.txt")
cat(" 오름차순 :", sort(read1),"\n",
    "내림차순: ", sort(read1, decreasing = T),"\n",
    "합 :", sum(read1),"\n",
    "평균 :", mean(read1))

#문제7
read2 <- scan("data/iotest2.txt", what =" ")
read2 <- factor(read2)
most <- names(which.max(table(read2)))
cat('가장 많이 등장한 단어는 "',most,'" 입니다.')
