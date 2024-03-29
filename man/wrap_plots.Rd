% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrap_plots.R
\name{wrap_plots}
\alias{wrap_plots}
\alias{wrap_cont_cont}
\alias{wrap_cont_time}
\alias{wrap_res_time}
\alias{wrap_eta_cont}
\alias{wrap_hist}
\alias{wrap_dv_preds}
\alias{wrap_cont_cat}
\title{Faceted plots}
\usage{
wrap_cont_cont(
  df,
  x,
  y,
  ...,
  fun = pm_scatter,
  title = NULL,
  scales = "free_y",
  ncol = NULL,
  use_labels = FALSE,
  label_fun = deprecated(),
  labeller = label_tex
)

wrap_cont_time(df, ..., x = pm_axis_time())

wrap_res_time(df, ..., x = pm_axis_time())

wrap_eta_cont(df, x, y, scales = "fixed", ...)

wrap_hist(
  df,
  x,
  title = NULL,
  scales = "free_x",
  ncol = NULL,
  use_labels = FALSE,
  labeller = label_tex,
  label_fun = deprecated(),
  ...
)

wrap_dv_preds(
  df,
  ...,
  title = "Predicted {yname}",
  xname = "",
  scales = "fixed"
)

wrap_cont_cat(
  df,
  x,
  y,
  ...,
  title = NULL,
  scales = "free_y",
  ncol = NULL,
  use_labels = FALSE,
  labeller = label_tex,
  label_fun = deprecated()
)
}
\arguments{
\item{df}{Data frame to plot.}

\item{x}{x-axis data in \code{\link[=col_label]{col_label()}} format; if \code{y} has length greater than 1,
then \code{x} must be length equal to 1.}

\item{y}{y-axis data in \code{\link[=col_label]{col_label()}} format; if \code{x} has length greater than 1,
then \code{y} must be length equal to 1.}

\item{...}{Passed to \code{fun}.}

\item{fun}{The plotting function.}

\item{title}{A title to use for the axis with faceting groups.}

\item{scales}{Passed to \code{facet_wrap}.}

\item{ncol}{Passed to \code{facet_wrap}.}

\item{use_labels}{If \code{TRUE}, the label part of \code{col_label} will
be used in the strip; the column name is used otherwise.}

\item{label_fun}{Deprecated; use \code{labeller} instead.}

\item{labeller}{A labeller function; passed to \code{\link[ggplot2:facet_wrap]{ggplot2::facet_wrap()}}; the
default is based on \code{\link[=parse_label]{parse_label()}} and allows latex markup in the label.}

\item{xname}{Placeholder.}
}
\description{
For these plots, data sets made long with respect to several
y-axis variables and then plotted and faceted with
\code{\link[ggplot2:facet_wrap]{ggplot2::facet_wrap()}}.
}
\details{
\code{wrap_cont_cont} is a general function used by
the others to create faceted plots of two continuous variables.
\code{wrap_cont_time} plots several continuous variables versus time.
\code{wrap_res_time} plots several different residuals (or NPDE) versus time.
\code{wrap_eta_cont} plots etas versus a continuous covariate. \code{wrap_hist}
creates a faceted histogram plot. \code{wrap_cont_cat} plots continuous versus
categorical data as a boxplot.

The following functions are called (as \code{fun}) to make each wrapped plot
\itemize{
\item \code{wrap_cont_cont} calls \code{\link[=pm_scatter]{pm_scatter()}}
\item \code{wrap_cont_time} calls \code{\link[=y_time]{y_time()}}
\item \code{wrap_res_time} calls \code{\link[=res_time]{res_time()}}
\item \code{wrap_eta_cont} calls \code{\link[=eta_cont]{eta_cont()}}
\item \code{wrap_hist} calls \code{\link[=cont_hist]{cont_hist()}}
\item \code{wrap_cont_cat} calls \code{\link[=pm_box]{pm_box()}};
}

For all plots, both \code{x} and \code{y} should name numeric data columns with the
exception of \code{wrap_cont_cat} which expects \code{x} to name a categorical
data column (the data are sent to \code{\link[=pm_box]{pm_box()}}).

When \code{\link[=pm_box]{pm_box()}} is called by \code{wrap_cont_cat}, the \code{shown} argument will be
forced to the value \code{FALSE}.

For all plots, either \code{x} or \code{y} may contain multiple columns, but an error
will be generated if both \code{x} and \code{y} list multiple columns.
}
