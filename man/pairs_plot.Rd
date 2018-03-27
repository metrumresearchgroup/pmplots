% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eta_pairs.R
\name{pairs_plot}
\alias{pairs_plot}
\alias{eta_pairs}
\title{Pairs plots using ggpairs}
\usage{
pairs_plot(x, etas, bins = 15, alpha = 0.6, fill = "black",
  col = "grey", upper_fun = pairs_upper, lower_fun = pairs_lower, ...)

eta_pairs(...)
}
\arguments{
\item{x}{plotting data.frame}

\item{etas}{character col//label for pairs data; see \code{\link{col_label}}}

\item{bins}{passed to \code{geom_histogram}}

\item{alpha}{passed to \code{geom_histogram}}

\item{fill}{passed to \code{geom_histogram}}

\item{col}{passed to \code{geom_histogram}}

\item{upper_fun}{function to use for \code{upper} argument}

\item{lower_fun}{function to use for \code{lower} argument}

\item{...}{passed to \code{GGally::ggpairs}}
}
\value{
The result from a ggpairs call.
}
\description{
Pairs plots using ggpairs
}
\details{
This function requires the \code{GGally} package to be installed.
}
\examples{

df <- dplyr::filter(pmplots_data(), EVID==0)
id <- dplyr::distinct(df, ID, .keep_all = TRUE)


eta_pairs(id, c("ETA1//ETA-CL", "ETA2//ETA-VC", "ETA3//ETA-KA"))

}