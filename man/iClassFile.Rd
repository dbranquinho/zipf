\name{iClassFile}
\alias{iClassFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
iClassFile(lfile = lfile, wplot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfile}{
%%     ~~Describe \code{lfile} here~~
}
  \item{wplot}{
%%     ~~Describe \code{wplot} here~~
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
function (lfile = lfile, wplot = FALSE)
{
    source("loadConfig.R")
    if (!file.exists(lfile)) {
        return(sprintf("ERROR - File \%s not found", lfile))
    }
    doc3 <- "none"
    rho <- -999999
    rhoClass <- "none"
    lClasses <- list.files(statistic)
    response <- data.frame(lClasses)
    response$cor <- 0
    doc <- read.csv(lfile, stringsAsFactors = FALSE, header = FALSE,
        col.names = c("term", "tfidf"), sep = ";", encoding = "UTF-8")
    if (length(doc$term)[1] < 10)
        return(list(response, c(rhoClass, rho)))
    for (niFiles in list.files(statistic)) {
        ni <- read.csv(paste0(statistic, "/", niFiles), stringsAsFactors = FALSE)
        ni$tfidf <- 0
        soma <- 0
        total <- length(ni$term)[1]
        ni$tfidf <- doc[match(ni$term, doc$term), "tfidf"]
        ni <- subset(ni, tfidf > 0)
        ni <- ni[order(ni$mean, decreasing = FALSE), ]
        ni$i <- 1:length(ni$term)
        ni <- subset(ni, mean > 0)
        model1 <- lm(ni$mean ~ ni$i + I(ni$i^2))
        model2 <- lm(ni$tfidf ~ ni$i + I(ni$i^2))
        corr <- abs(cor(predict(model1), predict(model2)))
        response$cor[which(response$lClasses == niFiles)] <- corr
        if (corr > rho) {
            rho <- corr
            rhoClass <- niFiles
            doc3 <- ni
        }
    }
    if (wplot) {
        par(new = F)
        ifelse(max(doc3$tfidf) > max(doc3$mean), maxylim <- max(doc3$tfidf),
            maxylim <- max(doc3$mean))
        maxylim <- as.numeric(maxylim)
        plot(doc3$i, doc3$mean, col = "blue", type = "p", main = rhoClass,
            xlim = c(0, max(doc3$i)), ylim = c(0, maxylim + 5),
            xlab = "Terms", ylab = "TF-IDF/Mean")
        lines(doc3$i, predict(lm(doc3$mean ~ doc3$i + I(doc3$i^2))),
            col = c("blue"))
        par(new = T)
        plot(doc3$i, doc3$tfidf, col = "red", pch = 16, xlim = c(0,
            max(doc3$i)), ylim = c(0, maxylim + 5), xlab = "Terms",
            ylab = "TF-IDF/Mean")
        lines(doc3$i, predict(lm(doc3$tfidf ~ doc3$i + I(doc3$i^2))),
            col = c("red"))
    }
    response <- response[order(response$cor, decreasing = TRUE),
        ]
    return(list(response, c(rhoClass, rho)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
