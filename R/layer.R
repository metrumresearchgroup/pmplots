
gs <- function(method="loess", se=FALSE, lty=2, lwd=1.35, col = .ggblue,...) {
  args <- list(...)
  c(args,list(method=method,se=se,lty=lty,lwd=lwd,col=col))
}
ga <- function(intercept=0, slope=1, lwd=1.35, col="darkgrey",...) {
  args <- list(...)
  c(args,list(intercept=intercept, slope=slope,col=col,lwd=lwd))
}
gh <- function(yintercept=0, lwd=1.35, col="darkgrey",...) {
  args <- list(...)
  c(args,list(yintercept=yintercept,lwd=lwd,col=col))
}

##' Layer functions
##'
##' @name layer
##' @rdname layer
##' @export
layer_hs <- function(x, smooth = gs(), hline = gh(), ...) {
  if(!is.null(hline)) x <- x + do.call(geom_hline,hline)
  if(!is.null(smooth)) x <- x + do.call(geom_smooth,smooth)
  x
}
##' @export
##' @rdname layer
layer_sh <- function(x, smooth = gs(), hline = gh(), ...) {
  if(!is.null(smooth)) x <- x + do.call(geom_smooth,smooth)
  if(!is.null(hline)) x <- x + do.call(geom_hline,hline)
  x
}
##' @export
##' @rdname layer
layer_as <- function(x, smooth = gs(), abline = ga(), ...) {
  if(!is.null(abline)) x <- x + do.call(geom_abline,abline)
  if(!is.null(smooth)) x <- x + do.call(geom_smooth,smooth)
  x
}
##' @export
##' @rdname layer
layer_sa <- function(x, smooth = gs(), abline = ga(), ...) {
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


