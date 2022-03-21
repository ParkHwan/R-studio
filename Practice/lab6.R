rm(list = ls())

#문제1
exam1 <- function(){
  x<-LETTERS[]
  y<-letters[]
  paste(x,y,sep="")
}
print(exam1())

#문제2
exam2 <- function(x){
  y <- 0
  for(i in 1:x){
    y <- y+i
  }
  cat("함수 호출 결과 :",y)
}
exam2(100)

#문제3
exam3 <- function(x,y){
  x <- x
  y <- y
  if (x > y){
    a <- x-y
    return(a)
  } else if (x < y) {
    a <- y-x
    return(a)
  } else {
    a <- 0
    return(a)
  }
  print("함수호출 결과 :",a)
}
exam3(30,10)

#문제4
exam4 <- function(x,y,z) {
  x <- x
  y <- y
  z <- z
  if (y == "+"){
    result <- x+z
  } else if (y == "-"){
    result <- x-z
  } else if (y == "*"){
    result <- x*z
  } else if (y == "%/%") {
    if (x == 0) {
      result <- "오류1"
    } else if (z==0) {
      result <- "오류2"
    } else {
      result <- x %/% z
    }
  } else if (y == "%%") {
    if (x == 0) {
      result <- "오류1"
    } else if (z==0) {
      result <- "오류2"
    } else {
      result <- x %% z
    }
  }
}
print(exam4(3,"+",1))
print(exam4(3,"-",1))
print(exam4(3,"*",1))
print(exam4(3,"%/%",1))
print(exam4(3,"%%",1))

#문제5
exam5 <- function(renum,basic="#") {
  renum <- renum
  basic <- basic
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
