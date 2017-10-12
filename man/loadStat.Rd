\name{loadStat}
\alias{loadStat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
loadStat(object, file.name, file.content)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{file.name}{
%%     ~~Describe \code{file.name} here~~
}
  \item{file.content}{
%%     ~~Describe \code{file.content} here~~
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
structure(function (object, file.name, file.content) 
{
    standardGeneric("loadStat")
  }, generic = structure("loadStat", package = "RQuantiNemo"), package = "RQuantiNemo", group = list(), valueClass = character(0), signature = c("object", 
"file.name", "file.content"), default = `\001NULL\001`, skeleton = (function (object, 
    file.name, file.content) 
stop("invalid call in method dispatch to 'loadStat' (no default method)", 
    domain = NA))(object, file.name, file.content), class = structure("nonstandardGenericFunction", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
