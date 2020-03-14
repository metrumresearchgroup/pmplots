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
##' Summary numbers located below each box are described in
##' \code{\link{boxwork}}.  The summaries will not be correct if the plot
##' is eventually faceted by another variable in the data set.  In this case,
##' either use \code{shown=FALSE} or create the plot with
##' \code{\link{split_plot}}.
##'
##' @seealso \code{\link{eta_cont}}
##'
##' @examples
##' df <- pmplots_data_id()
##'
##' eta_cat(df, x = "STUDYc//Study", y = "ETA1//ETA-CL")
##'
##' @return A single plot when a single value for \code{x}
##' and \code{y} are supplied; a list of plots of either \code{x}
##' or \code{y} have length greater than 1.
##'
##' @export
eta_cat <- function(df, x, y, hline=0, ...) {
  out <- list_plot_xy(df,x,y,.fun = pm_box, hline = hline, ...)
  if(length(out)==1) return(out[[1]])
  return(out)
}
