% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base.R
\docType{methods}
\name{initialize,simulation-method}
\alias{initialize,simulation-method}
\title{Build a minimal object for a QuantiNemo simulation}
\usage{
\S4method{initialize}{simulation}(.Object, sim.name = "my_simulation",
  parameters = list(generations = 1000, patch_capacity = 100),
  sim.dir = "./", exe.dir = "", exe.name = NULL)
}
\arguments{
\item{sim.name}{Name of the simulation (input file and output folder)}

\item{parameters}{The list of parameters that you want to pass to quantiNemo .}

\item{exe.dir}{Where is the executable}

\item{exe.name}{Name of the executable}

\item{sim.directory}{The directory where you want to execute the simulation}
}
\value{
A simulation object ready to be run.
}
\description{
Build a minimal object for a QuantiNemo simulation
}
\examples{
my_simulation <- new("simulation", parameters = (list(generations = 1000, patch_capacity =100)), sim.name = "my_new_sim")
}
