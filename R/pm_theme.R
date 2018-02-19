##' A plain ggplot2 theme
##'
##' @param ... passed to \code{ggplot2::theme}
##' @export
theme_plain <- function(...) {
  ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major=noline,panel.grid.minor=noline,
                   plot.margin=margin(0.5,0.5,1,0.5,unit="cm"),...)
}



##' Add loess line
##' @param method passed to \code{geom_smooth}
##' @param se passed to \code{geom_smooth}
##' @param lty passed to \code{geom_smooth}
##' @param lwd passed to \code{geom_smooth}
##' @param col passed to \code{geom_smooth}
##' @param ... passed to \code{geom_smooth}
##' @export
pm_smooths <- function(method="loess", se=FALSE, lty=2, lwd=1.3, col=.ggblue,...) {
  geom_smooth(method=method,se=se,lty=lty,lwd=lwd,col=col)
}

##' The standard pmplots theme
##'
##' @param ... passed to \code{\link{gs}} (smooth), \code{\link{gh}} (hline),
##' or \code{\link{ga}} (abline).
##'
##' @details
##' \code{pm_theme} is an alias to \code{theme_bw}.
##'
##' @examples
##'
##' data <- pmplots_data_obs()
##'
##' ggplot(data,aes(PRED,DV)) + geom_point() +
##'   pm_theme() + pm_smooth() + pm_abline()
##'
##' @export
pm_theme <- function() {
  theme_bw()
}

##' @rdname pm_theme
##' @export
pm_abline <- function(...) {
  do.call(geom_abline, ga(...))
}

##' @rdname pm_theme
##' @export
pm_smooth <- function(...) {
  do.call(geom_smooth, gs(...))
}

##' @rdname pm_theme
##' @export
pm_hline <- function(...) {
  do.call(geom_hline, gh(...))
}

