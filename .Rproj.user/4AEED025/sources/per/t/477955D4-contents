library(dplyr)
library(tidytext)
library(ggplot2)
library(tm)
library(dtplyr)
library("data.table")

df1 <- readChar(con = "doc1.txt",nchars = 100)
df2 <- readChar(con = "doc2.txt",nchars = 100)
df3 <- readChar(con = "doc3.txt",nchars = 100)
df4 <- readChar(con = "doc4.txt",nchars = 100)

df1 <- cbind("doc1.txt",df1,"1")
df2 <- cbind("doc2.txt",df2,"2")
df3 <- cbind("doc3.txt",df3,"3")
df4 <- cbind("doc4.txt",df4,"4")

myDataset <- data.frame(stringsAsFactors = FALSE)

datasetTemp <- as.data.frame(rbind(df1,df2,df3,df4),stringsAsFactors = F)
names(datasetTemp) <- c("id","text","class")
datasetTemp
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
        count(file,word, sort = TRUE) %>% ungroup()

total_words <- book_words %>% group_by(word) %>% summarize(total = sum(n))
book_words <- left_join(book_words, total_words, by = "word")
head(book_words)

# Create matrix with TF-IDF
book_words <- book_words %>% bind_tf_idf(term = word, document = file, n = n) 
book_words <- as.data.table(book_words)

book_words$class <- myDataset[match(book_words$file,myDataset$file),"class"]

setkey(book_words,file,word,class)

write.csv(book_words,file = "tf-idf.csv")
book_words
