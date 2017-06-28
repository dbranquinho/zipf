\name{config.get}
\alias{config.get}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Get a especific Configuration variable value.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function get a value of especific variable name storaged into config file.
You can edit config file directaly.
}
\usage{
config.get(var = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{
%%     ~~Describe \code{var} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~

}
\value{
%%  ~Describe the value returned
The function will return value of variable funcion, otherwise NA.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
\item{value  }{From variable name}
\item{NA  }{If not found}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Delermando Braquinho Filho
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~

\code{\link{loadConfig}}
}

\examples{
config.get("myClass")
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}

\keyword{ get }% use one of  RShowDoc("KEYWORDS")
\keyword{ configuration }% __ONLY ONE__ keyword per line
