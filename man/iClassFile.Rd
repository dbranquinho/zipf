\name{iClassFile}
\alias{iClassFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Classifying files through the classes
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
File name that you want to classify. This file must be into the book words.
}
  \item{wplot}{
%%     ~~Describe \code{wplot} here~~
If you want to plot change this flag to TRUE
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
\item{Correlation }{ Returned a correlation value of the file to all classes tried to classify}
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
iClassFile(lfile = "004062006at2.txt", wplot = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
