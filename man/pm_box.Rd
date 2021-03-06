% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/box.R
\name{pm_box}
\alias{pm_box}
\alias{pm_box_list}
\alias{cont_cat}
\title{Plot continuous variable against a categorical variable}
\usage{
pm_box(df, x, y, xs = defcx(), ys = defy(), ...)

pm_box_list(df, x, y, ...)

cont_cat(df, x, y, xs = defcx(), ys = defy(), ...)
}
\arguments{
\item{df}{data frame to plot}

\item{x}{character col//title for x-axis data; see \code{\link{col_label}}}

\item{y}{character col//title for y-axis data; see \code{\link{col_label}}}

\item{xs}{see \code{\link{defcx}}}

\item{ys}{see \code{\link{defy}}}

\item{...}{other arguments passed to \code{\link{boxwork}}}
}
\description{
This function is primarily called by other functions.
\code{pm_box_list} is a vectorized version of \code{cont_cat}.
}
\details{
Since this function creates a boxplot,
the \code{x} column must be character, factor
or logical and \code{y} column must
be numeric.

Summary numbers located below each box are described in
\code{\link{boxwork}}.  The summaries will not be correct if the plot
is eventually faceted by another variable in the data set.  In this case,
either use \code{shown=FALSE} or create the plot with
\code{\link{split_plot}}.
}
\examples{
df <- pmplots_data_id()


cont_cat(df, x="STUDYc//Study name", y="WT//Weight (kg)")

set.seed(12345)
cont_cat(df, x="STUDYc", y="WT", points = list(width  = 0.1))

}
\seealso{
\code{\link{boxwork}}
}
