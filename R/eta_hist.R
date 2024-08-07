
##' ETA histograms
##'
##' @param df data frame to plot
##' @param x character col//label for x-axis data; see \code{\link{col_label}}
##' @param xs see \code{\link{defx}}
##' @param add_density if \code{TRUE}, a normal density line will
##' be plotted on top of the histogram via \code{\link{add_density}}
##' @param ... other arguments for \code{geom_histogram}
##'
##' @details
##' The \code{x} column must be numeric.  This function
##' calls \code{\link{cont_hist}}.
##'
##' @examples
##'
##' df <- pmplots_data_id()
##'
##' etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")
##'
##' eta_hist(df, etas)
##'
##' @return A single plot when a single value for \code{x}
##' and \code{y} are supplied; a list of plots of either \code{x}
##' or \code{y} have length greater than 1.
##'
##' @export
eta_hist <- function(df, x, xs=defx(), add_density = FALSE, ...) {
  out <- vector(mode="list", length=length(x))
  for(i in seq_along(x)) {
    out[[i]] <- cont_hist(df, x = x[[i]], xs, add_density = add_density, ...)
  }
  if(length(out)==1) return(out[[1]])
  return(out)
}
