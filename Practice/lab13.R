# 문제1

library(KoNLP)

movieWords <- read.csv("output/movie_reviews3.csv")
movieWords <- sapply(movieWords$vreview, extractNoun, USE.NAMES = F)
movieWords1 <- unlist(movieWords)
movieWords2 <- gsub("[^가-힣]"," ",movieWords1)
movieWords2 <- gsub("  "," ",movieWords2)
movieWords2 <- gsub("들이"," ",movieWords2)
movieWords3 <- Filter(function(x) {nchar(x) >= 2}, movieWords2)

word_table <- table(movieWords3)
word_table1 <- sort(word_table, decreasing = T)
word_table1 <- head(word_table1, 10)
word_table1
word_data <- data.frame(word_table1)
names(word_data) <- c("wname", "wcount")
word_data
write.csv (word_data, "./output/movie_top_word.csv")

# 문제2

#(1)
txtWord <- readLines("output/yes24.txt")
txtWord <- sapply(txtWord, extractNoun, USE.NAMES = F)
txtWord1 <- unlist(txtWord)

#(2)
txtWord2 <- gsub("[^가-힣]"," ",txtWord1)
txtWord2 <- gsub("  "," ",txtWord2)

#(3)
txtWord3 <- Filter(function(x) {nchar(x) >= 2 && nchar(x) <= 4}, txtWord2)

#(4)
txt_table <- table(txtWord3)

#(5)
txt_table1 <- sort(txt_table, decreasing = T)

#(6)
txt_data <- data.frame(txt_table1)

#(7)
result <- wordcloud2(txt_data, fontFamily = "휴먼옛체")

#(8)
htmltools::save_html(result,"output/yes24.html")
