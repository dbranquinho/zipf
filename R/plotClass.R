# Plot Class from Centroids in files2test
#

plotClassAll <- function(compare = NULL, corteG = 0) {
        loadConfig()
        myClass <- config.get("myClass")
        par(mfrow=c(5,5))
        par(mar = c(4.5,0.2,0.2,0.2))
        classes <- list.files((myClass))
        myClasses <- as.character(lapply(paste0(classes),FUN = paste0))
        return(lapply(myClasses,wplot = TRUE,compare = compare, multiPlot = TRUE,
                      cut = corteG, interact = FALSE, FUN = plotClass))
}

plotClass <- function(classe = NULL, compare = NULL, all = FALSE,
                      wplot = TRUE, cut = 0, typePlot = "p",
                      multiPlot = FALSE, interact = FALSE) {
        if(is.null(classe))
                return("Class can't be NULL")
        if(cut < 0)
                return("ERROR - cut must be greather than zero")
        loadConfig()
        myClass <- config.get("myClass")
        dirData <- config.get("dirData")
        if(!multiPlot)
                par(mfrow=c(1,1))
        nrFiles <- 1
        colorPlot <- 1
        corM <- 0
        if(!file.exists(paste0(dirData,"/centroid.",classe))) {
                return(sprintf("ERRO - %s not found",paste0(dirData,"/centroid/",classe)))
        }
        ni <- read.csv(paste0(dirData,"/centroid.",classe),header = FALSE,sep = " ",
                       col.names = c("word","mean"))
        ni[is.na(ni)] <- 0
        if(wplot) {
                ni$tfidf <- 0
                if(length(compare)[1] == 0) {
                        ni <- subset(ni, mean > cut)
                        ni <- ni[order(ni$mean,decreasing = FALSE),]
                        ni$i <- 1:length(ni$word)
                        par(new = "F")
                        plot(ni$i, ni$mean, col = "blue", xlim = c(0,max(ni$i)),
                             ylim = c(0,max(ni$mean)), type = typePlot, main = classe,
                             xlab = "Terms", ylab = "TF-IDF")
                        lines(ni$i, predict(lm(ni$mean ~ ni$i + I(ni$i^2))),
                              col = c("red"),lwd = 2)
                        corM <- lm(ni$mean ~ ni$i + I(ni$i^2))
                        return(corM)
                }
                if(length(compare)[1] > 0) {
                        soma <- 0
                        book_words <- read.table(file = paste0(dirData,"/Book_Words.csv"),
                                                 stringsAsFactors = FALSE)
                        doc <- subset(book_words,file == compare)
                        ni$tfidf <- doc[match(ni$word,doc$word),"tf_idf"]
                        ni[is.na(ni)] <- 0
                        ni <- subset(ni, tfidf > cut)
                        ni <- subset(ni, mean > cut)
                        ni <- ni[order(ni$mean,decreasing = FALSE),]
                        ni$i <- 1:length(ni$word)


                        model1 <- lm(ni$mean ~ ni$i + I(ni$i^2))
                        model2 <- lm(ni$tfidf ~ ni$i + I(ni$i^2))
                        corM <- cor(predict(model1),predict(model2))

                        plot(ni$i, ni$mean, col = "blue",
                             type = typePlot, main = paste("Class",classe,"compared to file",compare),
                             xlim = c(0,max(ni$i)), ylim = c(0,max(ni$tfidf)),
                             xlab = paste("correlation: ",corM), ylab = "TF-IDF/Mean")
                        lines(ni$i, model1$fitted.values, col = c("blue"))
                        #abline(lm(ni$mean ~ ni$i + I(ni$i^2)), col = "blue")
                        par(new = "T")
                        plot(ni$i, ni$tfidf, col = "red",
                             pch = 16,
                             xlim = c(0,max(ni$i)), ylim = c(0,max(ni$tfidf)),
                             xlab = paste("correlation: ",corM), ylab = "TF-IDF/Mean")
                        lines(ni$i, model2$fitted.values, col = c("red"))
                        #abline(lm(ni$tfidf ~ ni$i + I(ni$i^2)), col = "red")
                        return(corM)
                }
        }
        return(c("ERRO",length(compare)[1]))
}
