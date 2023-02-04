##' Generate a histogram plot
##'
##' `cont_hist_list` is a vectorized version
##' of `cont_hist`.  `pm_histogram` is a generic histogram
##' function that is called by other functions in pmplots.
##'
##' @param df the data frame containing plotting data.
##' @param x the x column for [ggplot2::geom_histogram()].
##' @param y what to use for the y-axis on the histogram; can be
##' `"count"` or `"density"`.
##' @param add_density if `TRUE, a normal density line will
##' be plotted over the histogram using [add_density()].
##' @param xs a list of information for the x axis.
##' @param fill a character value passed to [ggplot2::geom_histogram()].
##' @param col a character value passed to [ggplot2::geom_histogram()].
##' @param alpha a numeric value passed to [ggplot2::geom_histogram()].
##' @param add_layers extra layers will be added only if `TRUE`.
##' @param ... passed to [ggplot2::geom_histogram()] and [add_density()].
##'
##'
##' @examples
##'
##' data <- data.frame(WT = rnorm(1000,80,20))
##'
##' cont_hist(data, x = "WT//Weight (kg)")
##'
##' @return A single plot.
##' @md
##' @export
cont_hist <- function(df, x, xs = defx(),
                      y = "count",
                      add_density = y=="density", add_layers=TRUE, ...) {
  xscale <- do.call("scale_x_continuous", xs)
  xx <- col_label(x)
  require_numeric(df,xx[1])
  out <-
    ggplot(data=df, aes(x = .data[[xx[1]]])) +
    pm_histogram(mapping = aes(y = after_stat(!!sym(y))), ...) +
    xscale + pm_theme() + pm_labs(x = xx[2])
  if(add_density & add_layers) {
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
pm_histogram <- function(... ,
                         col = opts$histogram.col,
                         fill = opts$histogram.fill,
                         alpha = opts$histogram.alpha) {
  geom_histogram(..., col = col, fill = fill, alpha = alpha)
}
