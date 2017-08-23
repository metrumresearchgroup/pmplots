


##' QQ plot for conditional residuals
##'
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param abline numeric vector with two elements to be passed to \code{geom_abline}
##' @param ... not used
##'
##' @examples
##'
##' df <- dplyr::filter(superset2(), EVID==0)
##'
##' cwres_q(df)
##'
##' @export
wres_q <- function(df, x="WRES", xs = defx(), ys=defy(), abline=c(0,1), ...) {
  require_numeric(df,x)
  xs$name <- "Standard normal quantile"
  ys$name <- paste0(x, " distribution quantile")
  xscale <- do.call("scale_x_continuous", xs)
  yscale <- do.call("scale_y_continuous", ys)
  p <- ggplot(data=df, aes_string(sample=x))
  p <- p + stat_qq(color=.ggblue, alpha=1, distribution=qnorm)
  p <- p + xscale + yscale
  if(!is.null(abline)) {
    p <- p + geom_abline(intercept=abline[1], slope=abline[2])
  }
  p
}

##' @export
##' @rdname wres_q
cwres_q <- function(df, x="CWRES", ...) {
  wres_q(df, x, ...)
}



