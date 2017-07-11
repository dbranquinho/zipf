createFiles2Test <- function(kfold = .7) {
        myClass <- config.get("myClass")
        dirData <- config.get("dirData")
        classes <- read.csv(paste0(dirData,"/class.txt"),
                            stringsAsFactors = FALSE, header = TRUE)
        book_words <- read.table(file = paste0(dirData,"/Book_Words.csv"),
                                 stringsAsFactors = FALSE)
        for(classe in classes$class) {
                print(paste("processing class test ", classe))
                subClass <- subset(book_words, class == classe)
                files <- as.character(unique(sort(subClass$file)))
                nfiles <- as.integer(length(files)*kfold)
                files <- files[nfiles:length(files)]
                write(x = files,file = paste0(dirData,"/files2test.",classe))
        }
}

createClassCentroid <- function(kfold = .7) {
        loadConfig()
        myClass <- config.get("myClass")
        dirData <- config.get("dirData")
        loadPackage("data.table")
        classes <- read.csv(paste0(dirData,"/class.txt"),
                            stringsAsFactors = FALSE, header = TRUE)
        book_words <- read.table(file = paste0(dirData,"/Book_Words.csv"),
                                 stringsAsFactors = FALSE)
        for(classe in classes$class) {
                print(paste("processing Class ", classe))
                subClass <- subset(book_words, class == classe)
                files <- as.character(unique(sort(subClass$file)))
                nfiles <- as.integer(length(files)*kfold)
                files <- files[1:nfiles]
                subClass <- subset(book_words, file %in% files)
                centroid <- as.character(unique(sort(subClass$word)))
                centroid <- as.data.table(centroid)
                colnames(centroid) <- c("word")
                setkey(centroid,word)
                centroid$mean <- 0
                lixo <- sapply(centroid$word, function(myword) {
                        centroid[myword,"mean"] <-
                                mean(subClass[which(subClass$word == myword),]$tf_idf)
                })
                centroid <- as.data.table(lixo,keep.rownames = TRUE)
                colnames(centroid) <- c("word","mean")
                write.csv(centroid,paste0(dirData,"/centroid.",classe),
                          fileEncoding = "UTF-8")
                remove(centroid,subClass,nfiles,files,lixo)
        }
        createFiles2Test(kfold)
}
