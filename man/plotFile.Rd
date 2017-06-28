\name{plotFile}
\alias{plotFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Plot files according to Zipf's Law.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function plot files about terms from TF-idf values. Visually creating one file's chromosome to compare each other.
}
\usage{
plotFile(file1 = file1, file2 = file2, wplot = TRUE, typePlot = "p")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file1}{
%%     ~~Describe \code{file1} here~~
The first file to compare or/and plot
}
  \item{file2}{
%%     ~~Describe \code{file2} here~~
The second file to compare or/and plot
}
  \item{wplot}{
%%     ~~Describe \code{wplot} here~~
A chart will be displayed by default containing the comparison between the files.
}
  \item{typePlot}{
%%     ~~Describe \code{typePlot} here~~
If you want to change the chart of plot, change to "l", "p" or "o".
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
\item{cor}{This function will return a correlation between both files}
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
xxxxxxxxxxxxxxxxxx
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
