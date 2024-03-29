% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{pm_log}
\alias{pm_log}
\alias{pm_ident}
\title{Identity and log scale helpers}
\usage{
pm_log(
  breaks = NULL,
  limits = NULL,
  transform = "log10",
  ...,
  trans = deprecated()
)

pm_ident(breaks, limits = range(breaks), ...)
}
\arguments{
\item{breaks}{passed to scale function}

\item{limits}{passed to scale function}

\item{transform}{passed to scale function}

\item{...}{passed to scale function}

\item{trans}{deprecated; use \code{transform} argument instead.}
}
\description{
Identity and log scale helpers
}
