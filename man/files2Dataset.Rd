\name{files2Dataset}
\alias{files2Dataset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Convert files into a classified directories into Zipf dataset
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use this function to load directories with classes and theier files into Zipf system.
}
\usage{
files2Dataset(dirClass = NULL, nRead = Inf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dirClass}{
Directory of you classes with theier files. The files must be into a classes directories.
}
  \item{nRead}{
Number of files to read eatch class directory.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The new dataset into a Zipf dataset model can be find into a data directory.
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

files2Dataset(dirClass = "dirMyClass")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ load }% use one of  RShowDoc("KEYWORDS")
\keyword{ dataset }% __ONLY ONE__ keyword per line
