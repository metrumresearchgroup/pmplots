% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eta_cat.R
\name{eta_cat}
\alias{eta_cat}
\title{Plot ETAs versus categorical variables}
\usage{
eta_cat(df, x, y, hline = 0, ...)
}
\arguments{
\item{df}{data frame to plot}

\item{x}{character col//title for x-axis data; see \code{\link{col_label}}}

\item{y}{character col//title for y-axis data; see \code{\link{col_label}}}

\item{hline}{passed to \code{\link{boxwork}}}

\item{...}{other arguments passed to \code{\link{cont_cat}}}
}
\value{
A single plot when a single value for \code{x}
and \code{y} are supplied; a list of plots of either \code{x}
or \code{y} have length greater than 1.
}
\description{
This is a vectorized function; see details.
}
\details{
This is a vectorized functions; if either x or y
are a character vector (in col//title format), a list
of plots is returned.  If both x and y are length 1,
then a single plot object (not a list) is returned.

Summary numbers located below each box are described in
\code{\link{boxwork}}.  The summaries will not be correct if the plot
is eventually faceted by another variable in the data set.  In this case,
either use \code{shown=FALSE} or create the plot with
\code{\link{split_plot}}.
}
\examples{
df <- pmplots_data_id()

eta_cat(df, x = "STUDYc//Study", y = "ETA1//ETA-CL")

}
\seealso{
\code{\link{eta_cont}}
}
