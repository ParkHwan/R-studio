read2 <- scan("data/iotest2.txt", what =" ")
read2 <- factor(read2)
most <- names(which.max(table(read2)))
cat('가장 많이 등장한 단어는 "',most,'" 입니다.')