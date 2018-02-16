


##' Generate a histogram plot
##'
##' @param df the data frame containing plotting data
##' @param x the x column
##' @param xs a list of information for the x axis
##' @param fill a character value passed to \code{geom_histogram}
##' @param col a character value passed to \code{geom_histogram}
##' @param alpha a numeric value passed to \code{geom_histogram}
##' @param ... passed to \code{geom_histogram}
##'
##' @export
cont_hist <- function(df, x, xs = defx(), fill = "black",
                      col = "white", alpha = 0.6, ...) {
  xscale <- do.call("scale_x_continuous", xs)
  xx <- col_label(x)
  xscale$name <- xx[2]
  require_numeric(df,xx[1])
  ggplot(data=df, aes_string(x = xx[1])) +
    geom_histogram(..., col = col, fill = fill, alpha = alpha) +
    xscale + pm_theme()
}


