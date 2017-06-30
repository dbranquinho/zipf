plotWords <- function(lfile, maxWords = 100, cut = 0) {
        loadConfig()
        loadPackage("data.table")
        loadPackage("ggplot2")
        loadPackage("wordcloud")
        dirData <- config.get("dirData")
        book_words <- read.table(file = paste0(dirData,"/Book_Words.csv"),
                                 stringsAsFactors = FALSE)
        corM <- 0
        doc  <- subset(book_words,file == lfile)
        doc <- subset(doc, tf_idf > cut)
        doc$i <- 1:length(doc$word)
        doc <- data.table(doc)
        setkey(doc,word)
        doc <- doc[order(doc$tf_idf, decreasing = TRUE),]
        if(dim(doc)[1] > 100)
                doc <- doc[1:100,]
        wordcloud(doc$word,doc$tf_idf, scale=c(max(doc$tf_idf)+3,min(doc$tf_idf)),min.freq=0.01,
                  max.words=100, random.order=FALSE, rot.per=.35,
                  colors=brewer.pal(8,"Dark2"))
}

plotFile <- function(file1 = NULL, file2 = NULL, wplot = TRUE, typePlot = "p") {
        loadConfig()
        dirData <- config.get("dirData")
        book_words <- read.table(file = paste0(dirData,"/Book_Words.csv"),
                                 stringsAsFactors = FALSE)
        corM <- 0
        doc1  <- subset(book_words,file == file1)
        centroid <- doc1$word
        if(!is.null(file2)) {
                doc2  <- subset(book_words,file == file2)
                centroid <- c(doc1$word,doc2$word)
        }
        centroid <- unique(sort(centroid))
        ni <- data.frame(word = centroid, stringsAsFactors = FALSE)
        remove(centroid)
        ni$tfidf <- 0
        ni$mean <- 0
        ni$i <- 0
        if(wplot) {
                ni$tfidf <- doc1[match(ni$word,doc1$word),"tf_idf"]
                if(!is.null(file2)) {
                        ni$mean <- doc2[match(ni$word,doc2$word),"tf_idf"]
                }
                ni <- subset(ni, tfidf > 0)
                if(length(ni$word) < 5) {
                        return(paste0("File ",file1," has less than 5 words to show",length(ni$word)))
                }
                ni[is.na(ni)] <- 0
                ni <- ni[order(ni$tfidf,decreasing = FALSE),]
                ni$i <- 1:length(ni$word)
                model1 <- lm(ni$tfidf ~ ni$i + I(ni$i^2))
                corM <- 1
                if(!is.null(file2)) {
                        ni <- subset(ni, mean > 0)
                        if(length(ni$word) < 5) {
                                return(paste0("File ",file2," has less than 5 words to show ",length(ni$word)))
                        }
                        model1 <- lm(ni$tfidf ~ ni$i + I(ni$i^2))
                        model2 <- lm(ni$mean ~ ni$i + I(ni$i^2))
                        corM <- cor(predict(model1),predict(model2))
                }
                plot(ni$i, ni$tfidf, col = "blue",
                     type = "p", main = paste(file1,file2),
                     xlim = c(0,max(ni$i)), ylim = c(0,max(ni$tfidf,ni$mean)),
                     xlab = paste("correlation: ",corM), ylab = "TF-IDF")
                lines(ni$i, predict(model1), col = c("black"))
                if(!is.null(file2)) {
                        par(new = "T")
                        plot(ni$i, ni$mean, col = "red",
                             pch = 16,
                             xlim = c(0,max(ni$i)), ylim = c(0,max(ni$tfidf,ni$mean)),
                             xlab = paste("correlation: ",corM), ylab = "TF-IDF")
                        lines(ni$i, predict(model2), col = c("green"))
                }
                return(corM)
        }
        return(c("ERRO",length(compare)[1]))
}

