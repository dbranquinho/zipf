\name{createCorrelation}
\alias{createCorrelation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
createCorrelation(iclass = FALSE, clean = FALSE, max = Inf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{iclass}{
%%     ~~Describe \code{iclass} here~~
}
  \item{clean}{
%%     ~~Describe \code{clean} here~~
}
  \item{max}{
%%     ~~Describe \code{max} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (iclass = FALSE, clean = FALSE, max = Inf)
{
    loadConfig()
    myClass <- config.get("myClass")
    dirData <- config.get("dirData")
    classe <- read.csv(paste0(dirData, "/class.txt"), stringsAsFactors = FALSE,
        header = TRUE)
    if (iclass == FALSE) {
        names(classe) <- "Choose one Class"
        return(classe)
    }
    if (is.numeric(iclass))
        classe <- as.character(classe[[2]])[iclass]
    else classe <- iclass
    if (clean == TRUE) {
        if (!dir.exists("./correlation"))
            dir.create("./correlation")
        unlink("./correlation/*")
    }
    print(as.data.frame(classe))
    ni <- read.csv(paste0(dirData, "/centroid.", classe), stringsAsFactors = FALSE)
    ni$tfidf <- 0
    ni <- ni[order(ni$mean, decreasing = FALSE), ]
    ni$i <- 1:length(ni$word)
    result <- data.frame(stringsAsFactors = FALSE, Filename = NULL,
        Correlation = NULL)
    nibkp <- ni
    soma <- 0
    cor7 <- 0
    classes <- read.csv(paste0(dirData, "/class.txt"), stringsAsFactors = FALSE,
        header = TRUE)
    book_words <- read.table(file = paste0(dirData, "/Book_Words.csv"),
        stringsAsFactors = FALSE)
    for (lfile in classes$class) {
        soma = soma + 1
        if (soma > max)
            break
        doc <- subset(book_words, file == lfile)
        if (length(doc$word)[1] < 10) {
            printf("\%5s - \%15s length < 10", soma, lfile)
            next
        }
        ni <- nibkp
        for (i in 1:length(doc$term)[1]) {
            ind <- which(ni$term == doc$term[i])
            if (length(ni$term[ind])[1] > 0) {
                ni$tfidf[ind] <- doc$tfidf[i]
            }
        }
        ni <- subset(ni, tfidf > 0)
        ni <- ni[order(ni$mean, decreasing = FALSE), ]
        ni$i <- 1:length(ni$term)
        model1 <- lm(ni$mean ~ ni$i + I(ni$i^2))
        model2 <- lm(ni$tfidf ~ ni$i + I(ni$i^2))
        corr <- abs(cor(predict(model1), predict(model2)))
        if (corr < 0.7) {
            cor7 = cor7 + 1
        }
        printf("\%5s - \%15s \%2.5f \%2.5f", soma, lfile, corr, (cor7 *
            100)/soma)
        result <- rbind(result, data.frame(Filename = lfile,
            Correlation = corr))
    }
    write.csv(result, paste0("./correlation/", classe, ".cor"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
