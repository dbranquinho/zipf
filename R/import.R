dataset2Dataset <- function(lfile = NULL, colClass = NULL, colFiles = NULL, colText = NULL) {
        loadPackage("dplyr")
        loadPackage("tidytext")
        loadPackage("tm")
        loadPackage("dtplyr")
        loadPackage("data.table")
        if(is.null(lfile)) {
                return("lfile can not be NULL")
        }
        if(is.null(colClass)) {
                return("colClass can not be NULL")
        }
        if(is.null(colFiles)) {
                return("colFiles can not be NULL")
        }
        if(is.null(colText)) {
                return("colText can not be NULL")
        }
        loadConfig()
        dirData <- config.get("dirData")
        if(!dir.exists(dirData)) {
                dir.create(dirData)
        }
        if(!file.exists(lfile)) {
                return(paste(lfile, " not found"))
        }
        dataset <- read.csv(file = lfile, stringsAsFactors = FALSE)
        datasetTemp <- dataset[,c(colClass,colFiles,colText)]
        ct <- 0         # counter to read files
        myDataset <- data.frame()
        for(ind in 1:length(datasetTemp$response_id)) {
                texto <- datasetTemp$response_text[ind]
                texto <- paste(texto,collapse = " ")
                texto <- gsub("<.*?>", "", texto)
                documents <- Corpus(VectorSource(texto))
                documents = tm_map(documents, tolower)
                documents = tm_map(documents, removePunctuation)
                texto  = tm_map(documents, removeNumbers)$content
                #texto <- tm_map(documents, removeWords,stopwords("en"))$content
                myDataset <- rbind(myDataset,c(datasetTemp$response_id[ind],
                                               datasetTemp$class[ind],
                                               t(texto)),deparse.level = 0,
                                   stringsAsFactors =  FALSE)

        }

        myDataset <- data.frame(lapply(myDataset,as.character), stringsAsFactors = FALSE)
        colnames(myDataset) <- c("file","class","text")
        book_words <- myDataset %>%
                unnest_tokens(word, text,to_lower = TRUE) %>%
                count(file,word, sort = TRUE) %>%
                ungroup()

        total_words <- book_words %>% group_by(file) %>% summarize(total = sum(n))
        book_words <- left_join(book_words, total_words)

        # Create matrix with TF-IDF
        book_words <- book_words %>% bind_tf_idf(word, file, n)
        book_words <- as.data.table(book_words)

        book_words$class <- myDataset[match(book_words$file,myDataset$file),"class"]

        setkey(book_words,file,word,class)

        write.table(book_words,file = paste0(dirData,"/Book_Words.csv"))
        class <- unique(sort(book_words$class))
        class <- data.frame(class = class)
        colnames(class) <- "class"
        write.csv(class,paste0(dirData,"/class.txt"))
}

files2Dataset <- function(dirClass = NULL, nRead = Inf) {
        loadPackage("dplyr")
        loadPackage("tidytext")
        loadPackage("tm")
        loadPackage("dtplyr")
        loadPackage("data.table")
        loadConfig()
        dirData <- config.get("dirData")
        if(!dir.exists(dirData)) {
                dir.create(dirData)
        }
        print("Processing ...")
        ## Get new words  to lemmatize words into text see more ??getLemma
        getLemma <- function(text = text, lang = "br") {
                ntermo <- character()
                nr     <- grep(text,lemmaWords)[1]
                ntermo <- lemmaWords[nr,1]
                ifelse(is.na(ntermo),return(text),return(ntermo[[1]]))
        }

        if(is.null(dirClass)) {
                dirClass <- config.get("myClass")
        }
        if(!dir.exists(dirClass))
                return(paste("Directory ",dirClass," not found"))
        myClass <- list.files(dirClass)
        myClass <- data.frame(class = myClass,stringsAsFactors = FALSE)
        colnames <- "class"
        write.csv(x = myClass,file = paste0(dirData,"/class.txt"))
        myDataset <- data.frame(stringsAsFactors = FALSE)
        id <- 0
        for(classe in list.dirs(dirClass, full.names = FALSE, recursive = FALSE)) {
                ct <- 0         # counter to read files
                for(lfile in list.files(paste0(dirClass,"/",classe))) {
                        texto <- scan(paste0(dirClass,"/",classe,"/",lfile),
                                      what = "character",quiet = TRUE)
                        #texto <- sapply(texto,getLemma)
                        texto <- paste(texto,collapse = " ")
                        texto <- gsub("<.*?>", "", texto)
                        documents <- Corpus(VectorSource(texto))
                        documents = tm_map(documents, tolower)
                        documents = tm_map(documents, removePunctuation)
                        documents = tm_map(documents, removeNumbers)
                        texto <- tm_map(documents, removeWords,stopwords("portuguese"))$content
                        id <- id +1
                        myDataset <- rbind(myDataset,c(id,classe, lfile, t(texto)),
                                           deparse.level = 0,
                                           stringsAsFactors =  FALSE)
                        ct <- ct+1
                        if(ct>=nRead)
                                break
                }
        }

        myDataset <- data.frame(lapply(myDataset,as.character), stringsAsFactors = FALSE)
        colnames(myDataset) <- c("ID","class","file","text")
        book_words <- myDataset %>%
                unnest_tokens(word, text,to_lower = TRUE) %>%
                count(file,word, sort = TRUE) %>%
                ungroup()

        total_words <- book_words %>% group_by(file) %>% summarize(total = sum(n))
        book_words <- left_join(book_words, total_words)

        # Create matrix with TF-IDF
        book_words <- book_words %>% bind_tf_idf(word, file, n)
        book_words <- as.data.table(book_words)
        book_words$class <- myDataset[match(book_words$file,myDataset$file),"class"]
        setkey(book_words,file,word,class)
        write.table(book_words,file = paste0(dirData,"/Book_Words.csv"))
}
