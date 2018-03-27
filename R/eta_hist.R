
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
##' The \code{x} column must be numeric.  This function
##' calls \code{\link{cont_hist}}.
##'
##' @examples
##'
##' df <- dplyr::filter(pmplots_data(), EVID==0)
##' id <- dplyr::distinct(df, ID, .keep_all = TRUE)
##'
##' etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")
##'
##' eta_hist(id, etas)
##'
##' @export
eta_hist <- function(df, x, xs=defx(), fill="black", col="white",
                     alpha=0.6, ...) {
  out <- vector(mode="list", length=length(x))
  for(i in seq_along(x)) {
    out[[i]] <- cont_hist(df, x = x[i], xs, ...)
  }
  if(length(out)==1) return(out[[1]])
  return(out)
}