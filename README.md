### The Dataset

Reading file and create dataset.

``` r
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
```

**Original files**

``` r
print(readLines("doc1.txt"))
```

    ## [1] "To do is to be." "To be is to do."

``` r
print(readLines("doc2.txt"))
```

    ## [1] "To be or not to be." "I am what I am."

``` r
print(readLines("doc3.txt"))
```

    ## [1] "I think therefore I am." "Do be do be do."

``` r
print(readLines("doc4.txt"))
```

    ## [1] "Do do do, da da da."  "Let it be, le it be."

**The dataset**

``` r
print(datasetTemp)
```

    ##         id                                            text class
    ## 1 doc1.txt          To do is to be.\r\nTo be is to do.\r\n     1
    ## 2 doc2.txt      To be or not to be.\r\nI am what I am.\r\n     2
    ## 3 doc3.txt  I think therefore I am.\r\nDo be do be do.\r\n     3
    ## 4 doc4.txt Do do do, da da da.\r\nLet it be, le it be.\r\n     4

**Cleaning and preparing datase**

``` r
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
```

**Creating Corpus**

``` r
myDataset <- data.frame(lapply(myDataset,as.character), stringsAsFactors = FALSE)
colnames(myDataset) <- c("file","class","text")
book_words <- myDataset %>%
        unnest_tokens(word, text,to_lower = TRUE) %>%
        count(file,word, sort = TRUE) %>% ungroup() %>% as.data.frame()
book_words <- book_words[order(book_words$file),]
names(book_words) <- c("file","word","f")
print(book_words)
```

    ##        file      word f
    ## 1  doc1.txt        to 4
    ## 5  doc1.txt        be 2
    ## 6  doc1.txt        do 2
    ## 7  doc1.txt        is 2
    ## 8  doc2.txt        am 2
    ## 9  doc2.txt        be 2
    ## 10 doc2.txt         i 2
    ## 11 doc2.txt        to 2
    ## 16 doc2.txt       not 1
    ## 17 doc2.txt        or 1
    ## 18 doc2.txt      what 1
    ## 2  doc3.txt        do 3
    ## 12 doc3.txt        be 2
    ## 13 doc3.txt         i 2
    ## 19 doc3.txt        am 1
    ## 20 doc3.txt therefore 1
    ## 21 doc3.txt     think 1
    ## 3  doc4.txt        da 3
    ## 4  doc4.txt        do 3
    ## 14 doc4.txt        be 2
    ## 15 doc4.txt        it 2
    ## 22 doc4.txt        le 1
    ## 23 doc4.txt       let 1

**Count** - This function make a count the number of files was found for each term in collection.

``` r
conta <- function(x) {
        y <- 0
        for(i in 1:length(x)) {
              y <- y+1  
        }
        return(y)
}
```

**bind\_tfidf** function - Bind tf-idf values from dataset

``` r
bind_tfidf <- function(files,f,n) {
        nfiles <- length(unique(book_words$file))
        nfiles <- rep(nfiles,length(book_words$file))
        tf <- 1+log2(f)
        idf <- log2(nfiles/n)
        tf_idf <- tf * idf
        book_words$df <- tf
        book_words$idf <- idf
        book_words$tf_idf <- tf_idf
}
```

**Calculating values of tf-idf and printing**

``` r
total_file <- book_words
total_file <- aggregate(total_file,by = list(total_file$word),conta)

total_file <- total_file[,-2:-3]
names(total_file) <- c("word","f")
book_words <- left_join(book_words, total_file, by = "word")
names(book_words) <- c("file","word","n","f")
head(book_words)
```

    ##       file word n f
    ## 1 doc1.txt   to 4 2
    ## 2 doc1.txt   be 2 4
    ## 3 doc1.txt   do 2 3
    ## 4 doc1.txt   is 2 1
    ## 5 doc2.txt   am 2 2
    ## 6 doc2.txt   be 2 4

``` r
# Create matrix with TF-IDF
book_words$tf_idf <- bind_tfidf(book_words$file,book_words$f,book_words$n)
print(book_words)
```

    ##        file      word n f    tf_idf
    ## 1  doc1.txt        to 4 2 0.0000000
    ## 2  doc1.txt        be 2 4 3.0000000
    ## 3  doc1.txt        do 2 3 2.5849625
    ## 4  doc1.txt        is 2 1 1.0000000
    ## 5  doc2.txt        am 2 2 2.0000000
    ## 6  doc2.txt        be 2 4 3.0000000
    ## 7  doc2.txt         i 2 2 2.0000000
    ## 8  doc2.txt        to 2 2 2.0000000
    ## 9  doc2.txt       not 1 1 2.0000000
    ## 10 doc2.txt        or 1 1 2.0000000
    ## 11 doc2.txt      what 1 1 2.0000000
    ## 12 doc3.txt        do 3 3 1.0728564
    ## 13 doc3.txt        be 2 4 3.0000000
    ## 14 doc3.txt         i 2 2 2.0000000
    ## 15 doc3.txt        am 1 2 4.0000000
    ## 16 doc3.txt therefore 1 1 2.0000000
    ## 17 doc3.txt     think 1 1 2.0000000
    ## 18 doc4.txt        da 3 1 0.4150375
    ## 19 doc4.txt        do 3 3 1.0728564
    ## 20 doc4.txt        be 2 4 3.0000000
    ## 21 doc4.txt        it 2 1 1.0000000
    ## 22 doc4.txt        le 1 1 2.0000000
    ## 23 doc4.txt       let 1 1 2.0000000
