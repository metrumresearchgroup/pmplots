
qq_reg_data <- function(y) {
  probs <- c(0.25, 0.75)
  y <- quantile(y,  probs, names=FALSE, type=7, na.rm=TRUE)
  x <- qnorm(probs)
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  c(int = int, slope = slope)
}

##' QQ plot for conditional residuals or NPDE
##'
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param abline numeric vector with two elements to be passed to
##' \code{geom_abline}
##' @param ... arguments passed back to \code{wres_q}
##'
##' @details
##' The \code{x} column must be numeric.
##'
##' @name res_q
##' @rdname res_q
##'
##' @examples
##'
##' df <- pmplots_data_obs()
##'
##' cwresi_q(df)
##'
##' @export
wres_q <- function(df, x="WRES", xs = defx(), ys=defy(), abline=NULL, ...) {

  require_numeric(df,x)
  if(is.null(abline)) abline <- qq_reg_data(df[[x]])
  xscale <- do.call("scale_x_continuous", xs)
  yscale <- do.call("scale_y_continuous", ys)
  p <- ggplot(data=df, aes_string(sample=x))
  p <- p + stat_qq(color=.ggblue, alpha=1, distribution=qnorm)
  p <- p + xscale + yscale
  p <- p + pm_labs(x = "Standard normal quantile", y = paste0(x, " distribution quantile"))
  if(!is.null(abline)) {
    p <- p + geom_abline(intercept=abline[1], slope=abline[2])
  }
  p + pm_theme()
}

##' @export
##' @rdname res_q
cwres_q <- function(df, x="CWRES", ...) {
  wres_q(df, x, ...)
}

##' @export
##' @rdname res_q
cwresi_q <- function(df, x="CWRESI", ...) {
  cwres_q(df, x, ...)
}

##' @export
##' @rdname res_q
npde_q <- function(df, x="NPDE", ...) {
  wres_q(df, x, ...)
}



