
##' Scatter plot function
##'
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param y character name for y-axis data
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param title character, passed to \code{ggtitle}
##' @param group character name of grouping variable
##' @param col not used
##' @param scale_col not used
##' @param ... not used
##'
##' @details
##' Since this function creates a scatter plot,
##' both the \code{x} and \code{y} columns must
##' be numeric.
##'
##' @export
scatt <- function(df, x, y, xs = defx(), ys = defy(),
                  title = NULL, group=NULL, col=NULL,
                  scale_col = scale_color_brewer(palette="Set2", name=""),
                  ... ) {

  xscale <- do.call("scale_x_continuous", xs)
  yscale <- do.call("scale_y_continuous", ys)

  locol <- .ggblue

  p <- ggplot(data=df,aes_string(x,y,col=col)) + geom_point() + xscale + yscale

  if(!is.null(group)) p <- p + geom_line(aes_string(group=group))
  if(is.character(title)) p <- p + ggtitle(title)
  p
}



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



##' Plot continuous data versus time
##'
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param y character name for y-axis data
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param yname used to form y-axis title
##' @param xname used to form x-axis title
##' @param xunit used to form x-axis title
##' @param log if \code{TRUE}, y-axis will be log-transformed
##' @param xby interval for breaks on x-axis (time)
##' @param ... passed to \code{\link{scatt}}
##'
##' @details
##' When the y-axis label needs a unit, include it in
##' the \code{yname} specification.
##'
##' @details
##' Since this function creates a scatter plot,
##' both the \code{x} and \code{y} columns must
##' be numeric.
##'
y_time <- function(df, x="TIME", y,
                   xname="Time", xunit="hr",
                   yname = NULL,
                   xs = defx(), ys = defy(),
                   log=FALSE, xby = NULL, ...) {

  require_numeric(df,x)
  require_numeric(df,y)

  xs$name <- paste0(xname, " (",xunit,")")
  ys$name <- yname

  if(is.numeric(xby)) {
    xs$breaks <- seq(0,max(df[,x]),xby)
  }

  if(log) {
    ys$trans <- "log"
    ys$breaks <- logbr3()
  }

  scatt(df, x, y, xs=xs, ys=ys, ...)

}


