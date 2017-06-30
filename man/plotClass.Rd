\name{plotClass}
\alias{plotClass}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Plot a Class compared with one file
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function plot a Class in two dimensions. At axis X is the words from class, at axis Y the mean from TF-IDF for all file into the class. With you compare this centroid with a file, this function will show a graphics with points and multiplay regression at the same plot.
}
\usage{
plotClass(classe = NULL, compare = NULL, all = FALSE, wplot = TRUE, cut = 0, typePlot = "p", multiPlot = FALSE, interact = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{classe}{
%%     ~~Describe \code{classe} here~~
What class you want to plot
}
  \item{compare}{
%%     ~~Describe \code{compare} here~~
If you want to compare one file with that centroid, or the other centroid class, you must write the file name, if NULL nothing will be compare, than the function will show you just centroid plot.
}
  \item{all}{
%%     ~~Describe \code{all} here~~
If TRUE the function will show you all classes plot. If compare was defined, this file will be compared with all classes.
}
  \item{wplot}{
%%     ~~Describe \code{wplot} here~~
Default is TRUE, FALSE if you want to see only correlation between then.
}
  \item{cut}{
%%     ~~Describe \code{cut} here~~
Cut minimum value of mean and TF-IDF
}
  \item{typePlot}{
%%     ~~Describe \code{typePlot} here~~
This option is to change type of plot, with choose "p, "o" or "l"
}
  \item{multiPlot}{
%%     ~~Describe \code{multiPlot} here~~
The default plot function is all graphics in the same plot. If you want one graphics to plot, change to TRUE.
}
  \item{interact}{
%%     ~~Describe \code{interact} here~~
To interact with plot, this option must be used with R Studio
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
\item{Correlation }{Correlation between mean from centroid compared with TF-IDF from file}
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
