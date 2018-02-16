##' Plot continuous variable versus continuous variable
##'
##' This function is primarily called by other functions.
##'
##' @param df data frame to plot
##' @param x character col//title for x-axis data; see \code{\link{col_label}}
##' @param y character col//title for y-axis data; see \code{\link{col_label}}
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param ... passed to \code{\link{scatt}}  and \code{\link{layer_hs}}
##'
##' @details
##' Since this function creates a scatter plot,
##' both the \code{x} and \code{y} columns must
##' be numeric.
##'
##' The \code{eta_cont} function can make a plot of ETA versus
##' continuous variable for multiple ETAs when a vector
##' of values for \code{y} are passed in.  Accordingly,
##' \code{eta_cont} always returns a list of plots of the same
##' length as \code{y}.
##'
##' @seealso \code{\link{scatt}}
##'
##' @examples
##' df <- dplyr::filter(pmplots_data(), EVID==0)
##' df <- dplyr::distinct(df, ID, .keep_all = TRUE)
##'
##' cont_cont(df, x="WT//Weight (kg)", y="HT//Height (cm)")
##'
##' @export
cont_cont <- function(df, x, y, xs = defx(), ys=defy(),...) {
  y <- col_label(y)
  x <- col_label(x)
  ys$name <- y[2]
  xs$name <- x[2]
  require_numeric(df, x[1])
  require_numeric(df, y[1])
  scatt(df,x[1],y[1],xs,ys,...)
}


