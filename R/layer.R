
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
##' @param smooth list of arguments for \code{geom_smooth}
##' @param hline list of arguments for \code{geom_hline}
##' @param abline list of arguments for \code{geom_abline}
##'
##' @name layer
##' @rdname layer
##' @export
layer_hs <- function(x, smooth = gs(), hline = gh(), ...) {
  if(!missing(smooth)) smooth <- merge.list(gs(),smooth,open=TRUE)
  if(!missing(hline)) hline <- merge.list(gh(),hline,open=TRUE)
  if(!is.null(hline)) x <- x + do.call(geom_hline,hline)
  if(!is.null(smooth)) x <- x + do.call(geom_smooth,smooth)
  x
}
##' @export
##' @rdname layer
layer_sh <- function(x, smooth = gs(), hline = gh(), ...) {
  if(!missing(smooth)) smooth <- merge.list(gs(),smooth,open=TRUE)
  if(!missing(hline)) hline <- merge.list(gh(),hline,open=TRUE)
  if(!is.null(smooth)) x <- x + do.call(geom_smooth,smooth)
  if(!is.null(hline)) x <- x + do.call(geom_hline,hline)
  x
}
##' @export
##' @rdname layer
layer_as <- function(x, smooth = gs(), abline = ga(), ...) {
  if(!missing(smooth)) smooth <- merge.list(gs(),smooth,open=TRUE)
  if(!missing(abline)) abline <- merge.list(ga(),abline,open=TRUE)
  if(!is.null(abline)) x <- x + do.call(geom_abline,abline)
  if(!is.null(smooth)) x <- x + do.call(geom_smooth,smooth)
  x
}
##' @export
##' @rdname layer
layer_sa <- function(x, smooth = gs(), abline = ga(), ...) {
  if(!missing(smooth)) smooth <- merge.list(gs(),smooth,open=TRUE)
  if(!missing(abline)) abline <- merge.list(ga(),abline,open=TRUE)
  if(!is.null(smooth)) x <- x + do.call(geom_smooth,smooth)
  if(!is.null(abline)) x <- x + do.call(geom_abline,abline)
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

##' @export
##' @rdname layer
geom_3s <- function(x,lwd=1.35, lty=3, ...) {
  geom_hline(yintercept=c(-3,3), lwd = lwd, lty=lty,...)
}
##' @export
##' @rdname layer
layer_3s <- function(x,lwd=1.35, lty=3, ...) {
  x + geom_hline(yintercept=c(-3,3), lwd = lwd, lty=lty,...)
}
