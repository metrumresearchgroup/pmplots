##' Generate a histogram plot
##'
##' \code{cont_hist_list} is a vectorized version
##' of \code{cont_hist}.  \code{pm_histogram} is a generic histogram
##' function that is called by other functions in pmplots.
##'
##' @param df the data frame containing plotting data
##' @param x the x column for \code{geom_histogram}
##' @param y what to use for the y-axis on the histogram; can be
##' \code{"..count.."} or \code{"..density.."}
##' @param add_density if \code{TRUE}, a normal density line will
##' be plotted over the histogram using \code{\link{add_density}}
##' @param xs a list of information for the x axis
##' @param fill a character value passed to \code{geom_histogram}
##' @param col a character value passed to \code{geom_histogram}
##' @param alpha a numeric value passed to \code{geom_histogram}
##' @param ... passed to \code{geom_histogram} and \code{add_density}
##'
##'
##' @examples
##'
##' data <- data.frame(WT = rnorm(1000,80,20))
##'
##' cont_hist(data, x = "WT//Weight (kg)")
##'
##'
##' @export
cont_hist <- function(df, x, xs = defx(), fill = "black",
                      col = "white", alpha = 0.6, y = "..count..",
                      add_density = y=="..density..", ...) {
  xscale <- do.call("scale_x_continuous", xs)
  xx <- col_label(x)
  require_numeric(df,xx[1])
  out <-
    ggplot(data=df, aes_string(x = xx[1])) +
    pm_histogram(mapping = aes_string(y = y), ...,
                 col = col, fill = fill, alpha = alpha) +
    xscale + pm_theme() + pm_labs(x = xx[2])
  if(add_density) {
    out <- out + add_density(...)
  }
  out
}

##' @rdname cont_hist
##' @export
cont_hist_list <- function(df, x, ...) {
  list_plot_x(df, x, ...)
}

##' @rdname cont_hist
##' @export
pm_histogram <- function(... , col = "white", fill = "black",
                         alpha = 0.6) {
  geom_histogram(..., col = col, fill = fill, alpha = alpha)
}
