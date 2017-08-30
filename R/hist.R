
##' ETA histograms
##'
##' @param df data frame to plot
##' @param x character col//label for x-axis data; see \code{\link{col_label}}
##' @param xs see \code{\link{defx}}
##' @param fill passed to \code{geom_histogram}
##' @param col passed to \code{geom_histogram}
##' @param alpha passed to \code{geom_histogram}
##' @param ... other arguments for \code{geom_histogram}
##'
##' @details
##' The \code{x} column must be numeric.
##'
##' @examples
##' df <- dplyr::filter(superset2(), EVID==0)
##' etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")
##' eta_hist(df, etas)
##'
##' @export
eta_hist <- function(df, x, xs=defx(), fill="black", col="white",
                     alpha=0.6, ...) {
  out <- vector(mode="list", length=length(x))
  for(i in seq_along(x)) {
    xx <- col_label(x[i])
    require_numeric(df,xx[1])
    xs$name <- xx[2]
    xscale <- do.call("scale_x_continuous", xs)
    out[[i]] <- ggplot(data=df, aes_string(x=xx[1])) +
      geom_histogram(...,col=col,fill=fill,alpha=alpha) +
      xscale
  }
  return(out)
}
