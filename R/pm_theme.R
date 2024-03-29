##' A plain ggplot2 theme
##'
##' @param ... passed to \code{ggplot2::theme}
##' @export
theme_plain <- function(...) {
  ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major=noline,panel.grid.minor=noline,...)
}

##' Add loess line
##' @param method passed to \code{geom_smooth}
##' @param se passed to \code{geom_smooth}
##' @param lty passed to \code{geom_smooth}
##' @param lwd passed to \code{geom_smooth}
##' @param col passed to \code{geom_smooth}
##' @param ... passed to \code{geom_smooth}
##' @export
pm_smooths <- function(method=opts$smooth.method, se=FALSE,
                       lty=opts$smooth.lty, lwd=opts$smooth.lwd,
                       col=opts$smooth.col,...) {
  geom_smooth(method=method,se=se,lty=lty,linewidth=lwd,col=col)
}

##' The standard pmplots theme
##'
##' @param ... arguments passed to \code{\link{gs}} (smooth),
##' \code{\link{gh}} (hline), or \code{\link{ga}} (abline) and
##' then to the appropriate \code{geom}.
##'
##' @details
##' \code{pm_theme} is an alias to \code{theme_bw}.
##'
##' @examples
##'
##' data <- pmplots_data_obs()
##'
##' ggplot(data,aes(PRED,DV)) +
##'   geom_point() +
##'   pm_theme() +
##'   pm_smooth(col = "firebrick") +
##'   pm_abline()
##'
##' @seealso \code{\link{pm_histogram}},
##' \code{\link{theme_plain}}.
##'
##' @export
pm_theme <- function(...) {
  theme_bw(...)
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

