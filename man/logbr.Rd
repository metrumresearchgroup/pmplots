% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{logbr}
\alias{logbr}
\alias{logbr3}
\title{Create breaks on log scale}
\usage{
logbr(from = -10, to = 10)

logbr3()
}
\arguments{
\item{from}{smallest break on log10 scale (see default value)}

\item{to}{largest break on log10 scale (see default value)}
}
\description{
Create breaks on log scale
}
\examples{
logbr()
logbr(-5,8)
logbr3()

}
