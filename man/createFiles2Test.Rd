\name{createFiles2Test}
\alias{createFiles2Test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
createFiles2Test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function called by createClassCentroid() create files into data folder to test the algorithm performance.
}
\usage{
createFiles2Test(kfold = 0.7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{kfold}{
%%     ~~Describe \code{kfold} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The value of .7 passed by argument function is the same value of the createClassCentroid() function becouse this value actualy is .3. If you change default value to .5 in this previus funcion, tho value passed of to this function is the same.
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
createClassCentroid()
}
\examples{
createFiles2Test(kfold = 0.7)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ test }% use one of  RShowDoc("KEYWORDS")
\keyword{ centroid }% __ONLY ONE__ keyword per line
