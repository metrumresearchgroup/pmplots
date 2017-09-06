
gs <- function(method="loess", se=FALSE, lty=2, lwd=1.35, col = .ggblue,...) {
  args <- list(...)
  def <- list(method=method,se=se,lty=lty,lwd=lwd,col=col)
  merge.list(def,args)
}
ga <- function(intercept=0, slope=1, lwd=1.35, col="darkgrey",...) {
  args <- list(...)
  def <- list(intercept=intercept, slope=slope,col=col,lwd=lwd)
  merge.list(def,args)
}
gh <- function(yintercept=0, lwd=1.35, col="darkgrey",...) {
  args <- list(...)
  def <- list(yintercept=yintercept,lwd=lwd,col=col)
  merge.list(def,args)
}

##' Layer functions
##'
##' @param x a \code{ggplot}
##' @param smooth list of arguments for \code{geom_smooth}
##' @param hline list of arguments for \code{geom_hline}
##' @param abline list of arguments for \code{geom_abline}
##' @param ... not used
##'
##' @examples
##' df <- dplyr::filter(superset2(), EVID==0 & BLQ==0)
##'
##' cwres_time(df) + geom_3s()
##'
##' @name layer
##' @rdname layer
##' @export
layer_hs <- function(x, smooth = gs(), hline = gh(), ...) {
  if(!is.null(hline)) {
    if(!missing(hline)) hline <- merge.list(gh(),hline,open=TRUE)
    x <- x + do.call(geom_hline,hline)
  }
  if(!is.null(smooth)) {
    if(!missing(smooth)) smooth <- merge.list(gs(),smooth,open=TRUE)
    x <- x + do.call(geom_smooth,smooth)
  }

  x
}

##' @export
##' @rdname layer
layer_sh <- function(x, smooth = gs(), hline = gh(), ...) {
  if(!is.null(smooth)) {
    if(!missing(smooth)) smooth <- merge.list(gs(),smooth,open=TRUE)
    x <- x + do.call(geom_smooth,smooth)
  }
  if(!is.null(hline)) {
    if(!missing(hline)) hline <- merge.list(gh(),hline,open=TRUE)
    x <- x + do.call(geom_hline,hline)
  }
  x
}

##' @export
##' @rdname layer
layer_as <- function(x, smooth = gs(), abline = ga(), ...) {
  if(!is.null(abline)) {
    if(!missing(abline)) abline <- merge.list(ga(),abline,open=TRUE)
    x <- x + do.call(geom_abline,abline)
  }
  if(!is.null(smooth)) {
    if(!missing(smooth)) smooth <- merge.list(gs(),smooth,open=TRUE)
    x <- x + do.call(geom_smooth,smooth)
  }
  x
}

##' @export
##' @rdname layer
layer_sa <- function(x, smooth = gs(), abline = ga(), ...) {
  if(!is.null(smooth)) {
    if(!missing(smooth)) smooth <- merge.list(gs(),smooth,open=TRUE)
    x <- x + do.call(geom_smooth,smooth)
  }
  if(!is.null(abline)) {
    if(!missing(abline)) abline <- merge.list(ga(),abline,open=TRUE)
    x <- x + do.call(geom_abline,abline)
  }
  x
}

##' @export
##' @rdname layer
layer_dots <- function(x,...) {
  args <- list(...)
  for(i in seq_along(args)) {
    x <- x + args[[i]]
  }
  x
}


##' Additional reference lines
##'
##' @param x ggplot object
##' @param lwd passed to \code{geom_hline}
##' @param lty passed to \code{geom_hline}
##' @param col passed to \code{geom_hline}
##' @param yintercept passed to \code{geom_hline}
##' @param ... passed to \code{geom_hline}
##'
##' @export
geom_3s <- function(lwd=1.35, lty=1, col = "darkgrey", yintercept = c(-3,3), ...) {
  geom_hline(yintercept = yintercept, col = col, lwd = lwd, lty = lty, ...)
}

##' @export
##' @rdname geom_3s
layer_3s <- function(x, lwd=1.35, lty=1, col = "darkgrey", yintercept = c(-3,3), ...) {
  x + geom_hline(yintercept = yintercept, col = col, lwd = lwd, lty = lty,...)
}
