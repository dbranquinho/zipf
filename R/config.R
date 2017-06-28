# Load configuration file and create all variables into de system
#
loadConfig <- function() {
        if(!dir.exists("config")) {
                dir.create("config")
        }


        if(!file.exists("config/config.dat")) {
                var <- c("collection","myClass","index")
                value <- c("collection","Class","index")
                config <- data.frame(var = var, value = value, stringsAsFactors = FALSE)
                write.csv(config,"config/config.dat")
        }

        config<<-read.csv("config/config.dat",stringsAsFactors = FALSE)
        }


config.get <-function(var = NULL) {
        return(config$value[[which(config$var==var)]])
}

loadPackage <- function(mypkg = NULL) {
        if(is.null(mypkg))
                return("mypkg can't be NULL")
        installed <- is.element(mypkg, installed.packages()[,1])
        if (!installed){
                install.packages(mypkg)
        }
        require(package = mypkg,quietly = TRUE,warn.conflicts = FALSE,
                character.only = TRUE)
}

