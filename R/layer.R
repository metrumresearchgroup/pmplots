

##' Layer functions
##'
##' @param x a \code{ggplot} object
##' @param smooth list of arguments for \code{geom_smooth}
##' @param hline list of arguments for \code{geom_hline}
##' @param abline list of arguments for \code{geom_abline}
##' @param add_layers if \code{FALSE} no layers are added from
##' \code{layer_s}, \code{layer_a}, \code{layer_h}, or
##' combinations
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
##' cwresi_time(df) + geom_3s()
##'
##' pmplots:::gs()
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
##' \code{geom_hline}, respectively.  The \code{gx} series
##' of functions are not exported.
##'
##' @seealso \code{\link{pm_smooth}}, \code{\link{pm_hline}},
##' \code{\link{pm_abline}}
##'
##' @name layer
##' @rdname layer
##'
##' @export
layer_h <- function(x, hline = gh(), add_layers = TRUE, ...) {
  if(!add_layers) return(x)
  if(!is.null(hline)) {
    if(!missing(hline)) hline <- combine_list(gh(),hline)
    x <- x + do.call(geom_hline,hline)
  }
  x
}

##' @export
##' @rdname layer
layer_s <- function(x, smooth = gs(), add_layers = TRUE,  ...) {
  if(!add_layers) return(x)
  if(!is.null(smooth)) {
    if(!missing(smooth)) smooth <- combine_list(gs(),smooth)
    x <- x + do.call(geom_smooth,smooth)
  }
  x
}

##' @export
##' @rdname layer
layer_a <- function(x, abline = ga(), add_layers = TRUE, ...) {
  if(!add_layers) return(x)
  if(!is.null(abline)) {
    if(!missing(abline)) abline <- combine_list(ga(),abline)
    x <- x + do.call(geom_abline,abline)
  }
  x
}

##' @export
##' @rdname layer
layer_hs <- function(x, ...) {
  layer_s(layer_h(x,...),...)
}

##' @export
##' @rdname layer
layer_sh <- function(x, ...) {
  layer_h(layer_s(x,...), ...)
}

##' @export
##' @rdname layer
layer_as <- function(x, ...) {
  layer_s(layer_a(x,...), ...)
}

##' @export
##' @rdname layer
layer_sa <- function(x, ...) {
  layer_a(layer_s(x,...), ...)
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
gs <- function(method=pm_opts$smooth.method, se=FALSE, lty=pm_opts$smooth.lty,
               lwd=pm_opts$smooth.lwd, col = pm_opts$smooth.col,...) {
  args <- list(...)
  def <- list(method=method,se=se,lty=lty,lwd=lwd,col=col)
  update_list(def,args)
}

##' @rdname layer
ga <- function(intercept=0, slope=1,
               lwd = pm_opts$abline.lwd,
               lty = pm_opts$abline.lty,
               col=pm_opts$abline.col,...) {
  args <- list(...)
  def <- list(intercept=intercept, slope=slope,col=col,lwd=lwd,lty=lty)
  update_list(def,args)
}

##' @rdname layer
gh <- function(yintercept=0,
               lwd = pm_opts$hline.lwd,
               lty = pm_opts$hline.lty,
               col = pm_opts$hline.col,...) {
  args <- list(...)
  def <- list(yintercept=yintercept,lwd=lwd,col=col,lty=lty)
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


##' Add a density curve to a plot
##'
##' For example, a plot a normal density over a histogram
##' of conditional weighted residuals.
##'
##' @param fun passed to \code{ggplot2::stat_function}
##' @param col passed to \code{ggplot2::stat_function}
##' @param lwd passed to \code{ggplot2::stat_function}
##' @param lty passed to \code{ggplot2::stat_function}
##' @param x a \code{ggplot} object to which the density line will be added
##' @param sd passed to \code{stats::dnorm}
##' @param mean passed to \code{stats::dnorm}
##' @param ... passed to \code{ggplot2::stat_function}
##'
##'
add_density <- function(fun = dnorm,
                        col = pm_opts$density.col,
                        lwd = pm_opts$density.lwd,
                        lty = pm_opts$density.lty, ...) {
  ggplot2::stat_function(fun = fun, col = col, lwd = lwd, lty = lty, ...)
}

##' @rdname add_density
##' @export
layer_dnorm <- function(x, sd = 1, mean = 0, ...) {
  args <- list(mean = mean, sd = sd)
  x + add_density(fun = dnorm, args = args, ...)
}

##' Input parameters for NPDE reference lines
##'
##' @param y used to set yintercept
##' @param lwd width of reference lines
##' @param ... other argument to set for \code{geom_hline}
##'
##' @export
npde_ref <- function(y = 0, lwd = 1,...) {
  c(list(yintercept = y, lwd = lwd),list(...))
}
