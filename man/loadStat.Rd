% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{loadStat}
\alias{loadStat}
\title{Load the statistic of a QuantiNemo simulation}
\usage{
loadStat(object)
}
\description{
Load the statistic of a QuantiNemo simulation
}
\examples{
my_sim <- new("simulation")
my_sim <- setParameter(my_sim,"stat","adlt.ind")
run(my_sim)
stat <- loadStat(my_sim)
plot(stat$adlt.ind)
}
