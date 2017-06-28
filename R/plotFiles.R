plotWords <- function(lfile, corte = 0) {
        source("loadConfig.R")
        library("ggplot2")
        library("data.table")
        if(!file.exists(paste0(collection,"/",lfile))) {
                return(sprintf("ERRO - %s not found",paste0(collection,"/",lfile)))
        }
        if(!file.exists(paste0(index,"/",lfile,".idx"))) {
                return(sprintf("ERRO - %s not found",paste0(index,"/",lfile,".idx")))
        }
        ni <- scan(paste0(collection,"/",lfile),what = "character")
        temp <- tolower(ni)
        temp <- gsub("[[:digit:]]", "", temp)
        temp <- gsub("[[:punct:]]", "", temp)
        temp = gsub("[ \t]{2,}", " ", temp)
        temp = gsub("^\\s+|\\s+$", "", temp)
        ni <- data.frame(i = 1:length(temp), term = temp,stringsAsFactors = FALSE)
        ni$mean <- 0
        doc <- read.csv(paste0(index,"/",lfile,".idx"),
                        stringsAsFactors = FALSE,encoding = "UTF-8",sep = ";",
                        header = FALSE,col.names = c("term","tfidf"))
        doc <- subset(doc, tfidf > corte)
        doc$i <- 1:length(doc$term)
        ni <- data.table(ni)
        doc <- data.table(doc)
        setkey(doc,term)
        setkey(ni,term)
        #ni <- ni[order(ni$mean,decreasing = FALSE),]
        sapply(doc$term, function(i) {
                ni[i]$mean <- doc[i]$tfidf
        })
        if(dim(doc)[1] > 50)
                doc <- doc[1:50,]
        p <- ggplot(ni, aes(i, mean, label = ni$term)) +
                geom_text(check_overlap = TRUE,size = ni$mean, aes(colour = ni$mean)) +
                theme(legend.position="none")
        print(p)
        corM <- lm(ni$mean ~ ni$i + I(ni$i^2))
        return(corM)
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

plotFileAtractors <- function(lfile = NULL, class2plot = NULL) {
        source("loadConfig.R")
        fclassAtractorsPlot <- list.files(paste0(classAtractors,"/",class2plot))
        #par(mfrow = c(length(fclassAtractorsPlot)/5, 4),
        #    mar = c(3, 3, 1, 1), oma = c(0, 0, 2, 0))
        for(classAtractorsPlot in fclassAtractorsPlot) {
                classAtractorsPlot <- substr(classAtractorsPlot,1,nchar(classAtractorsPlot)-4)
                print(plotFile(lfile,classAtractorsPlot))
        }
}
