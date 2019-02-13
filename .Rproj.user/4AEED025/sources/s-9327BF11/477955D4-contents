library(dplyr)
library(tidytext)
library(ggplot2)
library(tm)
library(dtplyr)
library("data.table")

df1 <- readLines(con = "doc1.txt")
df2 <- readLines(con = "doc2.txt")
df3 <- readLines(con = "doc3.txt")
df4 <- readLines(con = "doc4.txt")

df1 <- cbind("doc1.txt",df1,"h")
df2 <- cbind("doc2.txt",df2,"m")
df3 <- cbind("doc3.txt",df3,"m")
df4 <- cbind("doc4.txt",df4,"h")

myDataset <- data.frame(stringsAsFactors = FALSE)

datasetTemp <- as.data.frame(rbind(df1,df2,df3,df4),stringsAsFactors = F)
names(datasetTemp) <- c("id","text","class")
ct <- 0         # counter to read files
for(ind in 1:length(datasetTemp$id)) {
        texto <- datasetTemp$text[ind]
        texto <- paste(texto,collapse = " ")
        texto <- gsub("<.*?>", "", texto)
        documents <- Corpus(VectorSource(texto))
        documents = tm_map(documents, tolower)
        documents = tm_map(documents, removePunctuation)
        texto  = tm_map(documents, removeNumbers)$content
        #texto <- tm_map(documents, removeWords,stopwords("en"))$content
        myDataset <- rbind(myDataset,c(datasetTemp$id[ind],datasetTemp$class[ind], 
                                       t(texto)),deparse.level = 0, stringsAsFactors =  FALSE)
        
}

myDataset <- data.frame(lapply(myDataset,as.character), stringsAsFactors = FALSE)
colnames(myDataset) <- c("file","class","text")
book_words <- myDataset %>%
        unnest_tokens(word, text,to_lower = TRUE) %>%
        count(file,word, sort = TRUE) %>%
        ungroup()

total_words <- book_words %>% group_by(file) %>% summarize(total = sum(n))
book_words <- left_join(book_words, total_words)
head(book_words)

# Create matrix with TF-IDF
book_words <- book_words %>% bind_tf_idf(word, file, n)
book_words <- as.data.table(book_words)

book_words$class <- myDataset[match(book_words$file,myDataset$file),"class"]

setkey(book_words,file,word,class)

write.csv(book_words,file = "tf-idf.csv")
book_words
