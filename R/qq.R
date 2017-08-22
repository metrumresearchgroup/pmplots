# ggplot(dat2, aes(sample = dat2$CWRESI)) + stat_qq(color="blue", alpha=1, distirbution=qnorm) +
#   labs(x='Standard Normal Quantiles', y='CWRES Distribution Quantiles', title='') +
#   geom_abline(intercept = 0, slope = 1) +
#   theme(axis.title=element_text(size=20)) +
#   blck
#

##' QQ plot for conditional weighted residuals
##'
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param abline numeric vector with two elements to be passed to \code{geom_abline}
##' @param ... not used
##'
##' @examples
##' df <- dplyr::filter(superset2(), EVID==0)
##' cwres_q(df)
##'
##' @export
cwres_q<- function(df, x="CWRES", xs = defx(), ys=defy(), abline=c(0,1), ...) {
  require_column(df,x)
  xs$name <- "Standard normal quantiles"
  ys$name <- paste0(x, " distribution quantiles")
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
##' @rdname cwres_q
wres_q<- function(df, x="WRES", xs = defx(), ys=defy(), abline=c(0,1), ...) {
  require_column(df,x)
  xs$name <- "Standard normal quantiles"
  ys$name <- paste0(x, " distribution quantiles")
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
##' @rdname cwres_q
res_q<- function(df, x="RES", xs = defx(), ys=defy(), abline=c(0,1), ...) {
  require_column(df,x)
  xs$name <- "Standard normal quantiles"
  ys$name <- paste0(x, " distribution quantiles")
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
