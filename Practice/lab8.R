#문제1
v <- sample(1:26,10)
sapply(v, function(v) return(LETTERS[v]))
sort(sapply(v, function(v) LETTERS[v]))

#문제2-1
memo <- readLines("./data/memo.txt",encoding="UTF-8")
memo[1] <- gsub('[&$!#@%]','',memo[1])
memo[2] <- gsub('e','E',memo[2])
memo[3] <- gsub('[0-9]','',memo[3])
memo[4] <- gsub("[A-Z|a-z]", "", memo[4])
memo[4] <- gsub("  ", "", memo[4])
memo[5] <- gsub("[!123456789<>]", "", memo[5])
memo[6] <- gsub(" ", "", memo[6])
memo[7] <- gsub("YOU", "you", memo[7])
memo[7] <- gsub("OK", "ok", memo[7])
write(memo, "./data/memo_new.txt")

#문제2-2
memo <- readLines("./data/memo.txt",encoding="UTF-8")
memo[1] <- gsub('[&$!#@%]','',memo[1])
memo[2] <- gsub('e','E',memo[2])
memo[3] <- gsub('[0-9]','',memo[3])
memo[4] <- gsub("[A-z]", "", memo[4])
memo[4] <- gsub("  ", "", memo[4])
memo[5] <- gsub("[!123456789<>]", "", memo[5])
memo[6] <- gsub(" ", "", memo[6])
memo[7] <- tolower(memo[7]) # gsub("[[:upper:]]", "\\L\\1",memo[7], perl=T)
write(memo, "./data/memo_new.txt")


#문제3
myBirthday <- as.POSIXlt("1990-10-24")
weekdays(myBirthday)
