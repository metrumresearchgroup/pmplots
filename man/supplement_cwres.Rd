% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{supplement_cwres}
\alias{supplement_cwres}
\title{Add \code{CWRES} column from \code{CWRESI} if needed}
\usage{
supplement_cwres(x)
}
\arguments{
\item{x}{a data frame}
}
\description{
The intention here is to make sure there is a \code{CWRES} column if applicable
so that the \code{cwres} variant functions can be used rather than the
\code{cwresi} variants.
}
\details{
If \code{CWRESI} was tabled out, then a \code{CWRES} column is added.  No change
is made in case there is already a \code{CWRES} column.  This function is
called with every call to a  \code{cwresi} variant, so, ideally, the user
can call this function once upon loading the data.
}
