test1 <- matrix(1:12, nrow = 3)
test2 <- c(100,11,12,102,81)
test3 <- c("사과","바나나", "포도")
test4 <- c(100,11,12,102,81,NA)
test5 <- data.frame(letters[1:9], 1:9)


#문제1
countEvenOdd <- function (num1) {
  odd <- 0
  even <- 0
  if (is.vector(num1) && is.numeric(num1)){
    for (i in num1) {
      if (i %% 2 == 0){
        even <- even + 1
      } else if (i %% 2 ==1) {
        odd <- odd + 1
      }
    }
    return(list(even=even, odd=odd))
  } else {
    return(NULL)
  }
}
countEvenOdd(test1)
countEvenOdd(test2)

#문제2
vmSum <- function(vec){
  result <- 0
  if (is.vector(vec)){
    if (is.numeric(vec)){
      for (i in vec){
        result <- result + i
      }
    }else{
      result <- 0
      print("숫자 벡터만 전달하숑!")  
    }
  }else{
    result <- "벡터만 전달하숑!"
  } 
  return(result)
}
vmSum(test1)
vmSum(test2)
vmSum(test3)

#문제3
vmSum2 <- function(vec){
  result <- 0
  if (is.vector(vec)){
    if (is.numeric(vec)){
      for (i in vec){
        result <- result + i
      }
    }else{
      result <- 0
      warning("숫자 벡터만 전달하숑!")
    }
  }else{
    stop( "벡터만 전달하숑!")
  } 
  return(result)
}
vmSum2(test1)
vmSum2(test2)
vmSum2(test3)

#문제4
mySum <- function(vec){
  oddSum <- 0
  evenSum <- 0
  if (is.vector(vec)){
    if (any(is.na(vec))){
      vec[is.na(vec)] <- min(vec,na.rm = TRUE)
      for (i in vec){
        if (i %% 2 == 0){
          evenSum <- evenSum + i
        } else if (i %% 2 == 1){
          oddSum <- oddSum + i
        }
      }
      warning("NA를 최저값으로 변경하여 처리함!!")
      
    } else {
      for (i in vec){
        if (i %% 2 == 0){
          evenSum <- evenSum + i
        } else if (i %% 2 == 1){
          oddSum <- oddSum + i
        }
      }
    }
  } else{
    stop("벡터만 처리 가능!!")
  }
  if (is.null(vec)){
    return(NULL)
  }
  return(list(evenSum, oddSum))
}
mySum(test1)
mySum(test2)
mySum(test4)

#문제5
myExpr <- function(func){
  if (is.function(func)){
    num1 <- sample(1:45,6)
    func1 <- func(num1)
  } else {
    stop("수행 안할꺼임!!")
  }
  return(func1)
}
myExpr(max)

#문제6
creatVector1 <- function(...){
  x <- c(...)
  y <- c()
 if (any(is.na(x))){
   return(NA)
 } else if (is.null(x)){
   return(NULL)
 } else {
   for (i in x){
     y <- c(y, i)  
   }
 }
  return(y)
}
creatVector1(test1)
creatVector1(test2)
creatVector1(test3)
creatVector1(test4)
creatVector1(test5)

#문제7
creatVector2 <- function(...){
  x <- c(...)
  num <- c()
  char <- c()
  logi <- c()
  etc <- c()
  if (is.null(x)){
    return(NULL)
  } else{
    for (i in x){
      if (is.numeric(i)){
        num <- c(num, i)
      } else if (is.character(i)){
        char <- c(char, i)
      } else if (is.logical(i)){
        logi <- c(logi, i)
      } else {
        etc <- c(etc, i)
      }  
    }
  }
  return(list(num, char, logi, etc))
}
creatVector2(test1)
creatVector2(test2)
creatVector2(test3)
creatVector2(test4)
creatVector2(test5)
