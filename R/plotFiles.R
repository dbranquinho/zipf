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

plotFile <- function(file1 = file1, file2 = file2, wplot = TRUE, typePlot = "p") {
        source("loadConfig.R")
        corM <- 0
        if(!file.exists(paste0(index,"/",file1,".idx"))) {
                return(sprintf("ERRO - %s not found",paste0(index,"/",file1,".idx")))
        }
        if(!file.exists(paste0(index,"/",file2,".idx"))) {
                return(sprintf("ERRO - %s not found",paste0(index,"/",file2,".idx")))
        }
        doc1  <- read.csv(paste0(index,"/",file1,".idx"),stringsAsFactors = FALSE,
                          header = FALSE, col.names = c("term","tfidf"),
                          sep = ";", encoding = "UTF-8")
        doc2  <- read.csv(paste0(index,"/",file2,".idx"),stringsAsFactors = FALSE,
                          header = FALSE, col.names = c("term","tfidf"),
                          sep = ";", encoding = "UTF-8")
        centroid <- c(doc1$term,doc2$term)
        centroid <- unique(sort(centroid))
        ni <- data.frame(term = centroid, stringsAsFactors = FALSE)
        ni$tfidf <- 0
        ni$mean <- 0
        ni$i <- 0
        if(wplot) {
                soma <- 0
                for(i  in 1:length(doc1$term)[1]) {
                        ind <- which(ni$term == doc1$term[i])
                        ni$tfidf[ind] <- doc1$tfidf[i]
                }
                for(i  in 1:length(doc2$term)[1]) {
                        ind <- which(ni$term == doc2$term[i])
                        ni$mean[ind] <- doc2$tfidf[i]
                }
                ni <- subset(ni, tfidf > 0)
                ni <- subset(ni, mean > 0)
                if(length(ni$term) < 10) {
                        return(paste0("File ",index,"/",file2,".idx has less than 10 characters"))
                }
                ni <- ni[order(ni$mean,decreasing = FALSE),]
                ni$i <- 1:length(ni$term)
                model1 <- lm(ni$mean ~ ni$i + I(ni$i^2))
                model2 <- lm(ni$tfidf ~ ni$i + I(ni$i^2))
                corM <- abs(cor(predict(model1),predict(model2)))
                plot(ni$i, ni$mean, col = "blue",
                     type = "p", main = paste(file1,file2),
                     xlim = c(0,max(ni$i)), ylim = c(0,max(ni$tfidf)),
                     xlab = paste("correlation: ",corM), ylab = "TF-IDF")
                lines(ni$i, predict(lm(ni$mean ~ ni$i + I(ni$i^2))), col = c("blue"))
                #abline(lm(ni$mean ~ ni$i + I(ni$i^2)), col = "blue")
                par(new = "T")
                plot(ni$i, ni$tfidf, col = "red",
                     pch = 16,
                     xlim = c(0,max(ni$i)), ylim = c(0,max(ni$tfidf)),
                     xlab = paste("correlation: ",corM), ylab = "TF-IDF")
                lines(ni$i, predict(lm(ni$tfidf ~ ni$i + I(ni$i^2))), col = c("red"))
                #abline(lm(ni$tfidf ~ ni$i + I(ni$i^2)), col = "red")
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
