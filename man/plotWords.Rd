\name{plotWords}
\alias{plotWords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Plot Words in cloud from file
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function plot most valorable words by TF_IDF values from file into book_words dataset format.
}
\usage{
plotWords(lfile, maxWords = 100, cut = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfile}{
%%     ~~Describe \code{lfile} here~~
is file name into book_words format
}
  \item{maxWords}{
%%     ~~Describe \code{maxWords} here~~
Maximum words to plot, default 100
}
  \item{cut}{
%%     ~~Describe \code{cut} here~~
Minimum value of TF IDF that words will be plot.
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
Delermando Branquinho Filho
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
plotWords(lfile = "MyFile", maxWords = 150, cut = 0.043)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
