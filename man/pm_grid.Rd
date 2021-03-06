% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{pm_grid}
\alias{pm_grid}
\title{Arrange a list of plots in a grid}
\usage{
pm_grid(x, ..., ncol = 2)
}
\arguments{
\item{x}{a list of plots}

\item{...}{passed to \code{\link[cowplot]{plot_grid}}}

\item{ncol}{passed to \code{\link[cowplot]{plot_grid}}}
}
\description{
Arrange a list of plots in a grid
}
\details{
The cowplot package must be installed to use this function.
}
\examples{

data <- pmplots_data_obs()

plot <- wres_cont(data, x = c("WT", "ALB"))

pm_grid(plot)

}
