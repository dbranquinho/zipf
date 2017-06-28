\name{createClassCentroid}
\alias{createClassCentroid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Creating Class Centroid
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
createClassCentroid(kfold = 0.7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{kfold}{
%%     ~~Describe \code{kfold} here~~
This number represents the size of your train test set. Normaly 70\% is great, but you casn change this number any time.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
After processing, this function will call another function to complete this achivement named as createFiles2Test().
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
\item{nothing}{This function will not return a object, but files will be written into your configuration data folder.}
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
createFiles2Test()
}
\examples{
createClassCentroid(kfold = 0.7)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ createFiles2Test }% use one of  RShowDoc("KEYWORDS")
\keyword{ config }% __ONLY ONE__ keyword per line
