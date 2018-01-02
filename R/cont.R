##' Plot continuous variable versus continuous variable
##'
##' This function is primarily called by other functions.
##'
##' @param df data frame to plot
##' @param x character col//title for x-axis data; see \code{\link{col_label}}
##' @param y character col//title for y-axis data; see \code{\link{col_label}}
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param ... passed to \code{\link{scatt}}  and \code{\link{layer_hs}}
##'
##' @details
##' Since this function creates a scatter plot,
##' both the \code{x} and \code{y} columns must
##' be numeric.
##'
##' The \code{eta_cont} function can make a plot of ETA versus
##' continuous variable for multiple ETAs when a vector
##' of values for \code{y} are passed in.  Accordingly,
##' \code{eta_cont} always returns a list of plots of the same
##' length as \code{y}.
##'
##' @seealso \code{\link{scatt}}
##'
##' @examples
##' df <- dplyr::filter(superset2(), EVID==0)
##'
##' cont_cont(df, x="WT//Weight (kg)", y="HT//Height (cm)")
##'
##' @export
cont_cont <- function(df, x, y, xs = defx(), ys=defy(),...) {
  y <- col_label(y)
  x <- col_label(x)
  ys$name <- y[2]
  xs$name <- x[2]
  require_numeric(df, x[1])
  require_numeric(df, y[1])
  scatt(df,x[1],y[1],xs,ys,...)
}

##' @export
##' @rdname cont_cont
eta_cont <- function(df,x,y,...) {
  out <- vector(mode="list", length=length(y))
  for(i in seq_along(y)) {
    out[[i]] <- cont_cont(df,x,y[i],...)
  }
  out <- lapply(out, layer_hs, ...)
  return(out)
}

##' @export
##' @rdname cont_cont
res_cont <- function(df, x, y="RES//Residual",
                     xs=defx(), ys=defy(), ...) {
  x <- col_label(x)
  y <- col_label(y)
  if(length(x)!=2) .stop("invalid x specification")
  if(length(y)!=2) .stop("invalid y specification")
  ys$name <- y[2]
  xs$name <- x[2]
  require_numeric(df, x[1])
  require_numeric(df, y[1])
  out <- scatt(df,x[1],y[1],xs=xs,ys=ys,horiz = 0)
  layer_hs(out,...)
}

##' @export
##' @rdname cont_cont
wres_cont <- function(df, x, y="WRES//Weighted residual",
                      xs=defx(), ys=defy(),...) {
  x <- col_label(x)
  y <- col_label(y)
  if(length(x)!=2) .stop("invalid x specification")
  if(length(y)!=2) .stop("invalid y specification")
  ys$name <- y[2]
  xs$name <- x[2]
  require_numeric(df, x[1])
  require_numeric(df, y[1])
  out <- scatt(df,x[1],y[1],xs=xs,ys=ys,horiz = 0)
  layer_hs(out,...)
}

##' @export
##' @rdname cont_cont
cwres_cont <- function(df, x, y="CWRES//Conditional weighted residual",
                       xs=defx(), ys=defy(),...) {
  x <- col_label(x)
  y <- col_label(y)
  if(length(x)!=2) .stop("invalid x specification")
  if(length(y)!=2) .stop("invalid y specification")
  ys$name <- y[2]
  xs$name <- x[2]
  require_numeric(df, x[1])
  require_numeric(df, y[1])
  out <- scatt(df,x[1],y[1],xs=xs,ys=ys,horiz = 0)
  layer_hs(out,...)
}

