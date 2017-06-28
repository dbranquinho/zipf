\name{dataset2Dataset}
\alias{dataset2Dataset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert a dataset into Zipf dataset
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use this function to load a dataset into Zipf system.
}
\usage{
dataset2Dataset(lfile = NULL, colClass = NULL, colFiles = NULL, colText = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfile}{
%%     ~~Describe \code{lfile} here~~
Where your source dataset file is stored.
}
  \item{colClass}{
%%     ~~Describe \code{colClass} here~~
What column from your dataset describe previus label of your classes
}
  \item{colFiles}{
%%     ~~Describe \code{colFiles} here~~
What column from your dataset describe the name of file text
}
  \item{colText}{
%%     ~~Describe \code{colText} here~~
What column from your dataset have a text from file
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
Delermando Branquinho Filho
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

dataset2Dataset(lfile = "../myDir/dataset.csv", colClass = 3, colFiles = 2, colText = 4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ load }% use one of  RShowDoc("KEYWORDS")
\keyword{ dataset }% __ONLY ONE__ keyword per line
