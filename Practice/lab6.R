#문제1
exam1 <- function(){
  x<-LETTERS[]
  y<-letters[]
  return(paste(x,y,sep=""))
}
exam1()

#문제2
exam2 <- function(x){
  y <- 0
  for(i in 1:x){
    y <- y+i
  }
  return(y)
}
cat("함수호출 결과 :",exam2(100))

#문제3
exam3 <- function(x,y){
  if (x > y){
    a <- x-y
  } else if (x < y) {
    a <- y-x
  } else {
    a <- 0
  }
  return(a)
}
print(paste("함수 호출 결과 :",exam3(30,10)))

#문제4
exam4 <- function(num1,oper,num2) {
  if (oper == "+"){
    result <- num1+num2
  } else if (oper == "-"){
    result <- num1-num2
  } else if (oper == "*"){
    result <- num1*num2
  } else if (oper == "%/%") {
    if (num1 == 0) {
      result <- "오류1"
    } else if (num2==0) {
      result <- "오류2"
    } else {
      result <- num1 %/% num2
    }
  } else if (oper == "%%") {
    if (num1 == 0) {
      result <- "오류1"
    } else if (num2==0) {
      result <- "오류2"
    } else {
      result <- num1 %% num2
    }
  } else {
    result <- "규격의 연산자만 전달하세요"
  }
  return(result)
}
print(exam4(3,"+",1))
print(exam4(3,"-",1))
print(exam4(3,"*",1))
print(exam4(3,"%/%",1))
print(exam4(3,"%%",1))
print(exam4(3,"_",1))
print(exam4(0,"%%",1))
print(exam4(1,"%%",0))

#문제5
exam5 <- function(renum,basic="#") {
  if (renum > 0 ){
    for (i in 1:renum) {
      cat(basic)
    }
    cat("\n")
  }
  return(NULL)
}
exam5(10)

#문제6
exam6 <- function(...) {
  data <- c(...)
  for (i in data){
    if (is.na(i)){
      cat("NA는 처리불가","\n")
    } else if (i >= 85){
      cat(i,"점은 상등급 입니다","\n")
    } else if (i >= 70) {
      cat(i,"점은 중등급 입니다","\n")
    } else if (i < 70) {
      cat(i,"점은 하등급 입니다","\n")
    }
  }
  return(NULL)
}
exam6(c(80, 50, 70, 66, NA, 35))
