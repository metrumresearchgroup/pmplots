##' Plot ETAs versus categorical variables
##'
##' This is a vectorized function; see details.
##'
##' @param df data frame to plot
##' @param x character col//title for x-axis data; see \code{\link{col_label}}
##' @param y character col//title for y-axis data; see \code{\link{col_label}}
##' @param hline passed to \code{\link{boxwork}}
##' @param ... other arguments passed to \code{\link{cont_cat}}
##'
##' @details
##' This is a vectorized functions; if either x or y
##' are a character vector (in col//title format), a list
##' of plots is returned.  If both x and y are length 1,
##' then a single plot object (not a list) is returned.
##'
##' @seealso \code{\link{eta_cont}}
##'
##' @export
eta_cat <- function(df, x, y, hline=0, ...) {
  out <- list_plot_xy(df,x,y,.fun = cont_cat, hline = hline, ...)
  if(length(out)==1) return(out[[1]])
  return(out)
}

