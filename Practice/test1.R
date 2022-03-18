read1 <- scan("data/iotest1.txt")
cat(" 오름차순 :", sort(read1),"\n",
    "내림차순: ", sort(read1, decreasing = T),"\n",
    "합 :", sum(read1),"\n",
    "평균 :", mean(read1))