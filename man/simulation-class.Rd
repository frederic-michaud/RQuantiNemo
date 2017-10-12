\name{simulation-class}
\Rdversion{1.1}
\docType{class}
\alias{simulation-class}
\alias{addFile,simulation-method}
\alias{getCommand,simulation-method}
\alias{initialize,simulation-method}
\alias{loadStat,simulation-method}
\alias{run,simulation-method}
\alias{setParameter,simulation-method}
\alias{writeInput,simulation-method}

\title{Class \code{"simulation"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("simulation", sim.name, parameters, sim.dir, exe.dir, exe.name)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{parameters}:}{Object of class \code{"list"} ~~ }
    \item{\code{sim.name}:}{Object of class \code{"character"} ~~ }
    \item{\code{sim.directory}:}{Object of class \code{"character"} ~~ }
    \item{\code{exe.directory}:}{Object of class \code{"character"} ~~ }
    \item{\code{exe.name}:}{Object of class \code{"character"} ~~ }
    \item{\code{stat}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{params.file}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{addFile}{\code{signature(object = "simulation")}: ... }
    \item{getCommand}{\code{signature(object = "simulation")}: ... }
    \item{initialize}{\code{signature(.Object = "simulation")}: ... }
    \item{loadStat}{\code{signature(object = "simulation")}: ... }
    \item{run}{\code{signature(object = "simulation")}: ... }
    \item{setParameter}{\code{signature(object = "simulation")}: ... }
    \item{writeInput}{\code{signature(object = "simulation")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("simulation")
}
\keyword{classes}
