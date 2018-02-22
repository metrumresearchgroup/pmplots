

##' Layer functions
##'
##' @param x a \code{ggplot} object
##' @param smooth list of arguments for \code{geom_smooth}
##' @param hline list of arguments for \code{geom_hline}
##' @param abline list of arguments for \code{geom_abline}
##' @param method passed to the appropriate \code{geom_}
##' @param se passed to the appropriate \code{geom_}
##' @param lty passed to the appropriate \code{geom_}
##' @param lwd passed to the appropriate \code{geom_}
##' @param col passed to the appropriate \code{geom_}
##' @param slope passed to \code{geom_abline}
##' @param intercept passed to \code{geom_abline}
##' @param yintercept passed to \code{geom_hline}
##' @param ... passed to layering functions and geoms
##'
##' @examples
##' df <- dplyr::filter(pmplots_data(), EVID==0 & BLQ==0)
##'
##' cwres_time(df) + geom_3s()
##'
##'
##' @details
##' Function names can be decoded as: \code{h} indicates horizontal
##' reference line, \code{s} indicates smoothing line,
##' \code{a} indicates \code{abline} (typically a line of
##' identity). The order of the the codes indicates the
##' order in which the layers are applied. For example,
##' \code{layer_hs} means to first add a horizontal reference
##' line and then add a smoothing line.  Likewise, \code{layer_s}
##' adds a smoother, \code{layer_a} adds identity line, and
##' \code{layer_y} adds a horizontal reference line.
##'
##' \code{gs}, \code{ga}, and \code{gh} are helper functions to create
##' default arguments to \code{geom_smooth}, \code{geom_abline}, and
##' \code{geom_hline}, respectively.  These functions are not exported.
##'
##' @name layer
##' @rdname layer
##'
##' @export
layer_hs <- function(x, smooth = gs(), hline = gh(), ...) {
  if(!is.null(hline)) {
    if(!missing(hline)) hline <- combine_list(gh(),hline)
    x <- x + do.call(geom_hline,hline)
  }
  if(!is.null(smooth)) {
    if(!missing(smooth)) smooth <- combine_list(gs(),smooth)
    x <- x + do.call(geom_smooth,smooth)
  }

  x
}

##' @export
##' @rdname layer
layer_sh <- function(x, smooth = gs(), hline = gh(), ...) {
  if(!is.null(smooth)) {
    if(!missing(smooth)) smooth <- combine_list(gs(),smooth)
    x <- x + do.call(geom_smooth,smooth)
  }
  if(!is.null(hline)) {
    if(!missing(hline)) hline <- combine_list(gh(),hline)
    x <- x + do.call(geom_hline,hline)
  }
  x
}

##' @export
##' @rdname layer
layer_as <- function(x, smooth = gs(), abline = ga(), ...) {
  if(!is.null(abline)) {
    if(!missing(abline)) abline <- combine_list(ga(),abline)
    x <- x + do.call(geom_abline,abline)
  }
  if(!is.null(smooth)) {
    if(!missing(smooth)) smooth <- combine_list(gs(),smooth)
    x <- x + do.call(geom_smooth,smooth)
  }
  x
}

##' @export
##' @rdname layer
layer_sa <- function(x, smooth = gs(), abline = ga(), ...) {
  if(!is.null(smooth)) {
    if(!missing(smooth)) smooth <- combine_list(gs(),smooth)
    x <- x + do.call(geom_smooth,smooth)
  }
  if(!is.null(abline)) {
    if(!missing(abline)) abline <- combine_list(ga(),abline)
    x <- x + do.call(geom_abline,abline)
  }
  x
}

##' @export
##' @rdname layer
layer_s <- function(...) {
  layer_sa(..., abline = NULL)
}

##' @export
##' @rdname layer
layer_a <- function(...) {
  layer_sa(..., smooth = NULL)
}

##' @export
##' @rdname layer
layer_h <- function(...) {
  layer_hs(..., smooth = FALSE)
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


##' @rdname layer
gs <- function(method="loess", se=FALSE, lty=2, lwd=1.35, col = .ggblue,...) {
  args <- list(...)
  def <- list(method=method,se=se,lty=lty,lwd=lwd,col=col)
  update_list(def,args)
}

##' @rdname layer
ga <- function(intercept=0, slope=1, lwd=1.35, col="darkgrey",...) {
  args <- list(...)
  def <- list(intercept=intercept, slope=slope,col=col,lwd=lwd)
  update_list(def,args)
}

##' @rdname layer
gh <- function(yintercept=0, lwd=1.35, col="darkgrey",...) {
  args <- list(...)
  def <- list(yintercept=yintercept,lwd=lwd,col=col)
  update_list(def,args)
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
geom_3s <- function(lwd = 1.35, lty=1, col = "darkgrey", yintercept = c(-3,3), ...) {
  geom_hline(yintercept = yintercept, col = col, lwd = lwd, lty = lty, ...)
}

##' @export
##' @rdname geom_3s
layer_3s <- function(x, lwd = 1.35, lty = 1, col = "darkgrey", yintercept = c(-3,3), ...) {
  x + geom_hline(yintercept = yintercept, col = col, lwd = lwd, lty = lty,...)
}
