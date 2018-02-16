
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


