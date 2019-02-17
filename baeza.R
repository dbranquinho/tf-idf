
df <- read.csv2("baeza.txt",header = T,sep = ",",stringsAsFactors = F)

book_words1 <- cbind(file = rep("d1",13),word = df$term,tf = df$tf1,n = df$n)
book_words2 <- cbind(file = rep("d2",13),word = df$term,tf = df$tf2,n = df$n)
book_words3 <- cbind(file = rep("d3",13),word = df$term,tf = df$tf3,n = df$n)
book_words4 <- cbind(file = rep("d4",13),word = df$term,tf = df$tf4,n = df$n)

book_words <- as.data.frame(rbind(book_words1,book_words2,book_words3,book_words4),stringsAsFactors = F)
book_words$tf <- as.integer(book_words$tf)
book_words$n <- as.integer(book_words$n)

book_words$tfi <- 1+log2(book_words$tf)
book_words$idf <- log2(4/book_words$n)

book_words$tf_idf <- book_words$tfi * book_words$idf
