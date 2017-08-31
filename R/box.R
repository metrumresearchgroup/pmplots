
##' Make boxplots
##'
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param y character name for y-axis data
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param fill passed to \code{geom_boxplot}
##' @param alpha passed to \code{geom_boxplot}
##' @param hline used to draw horizontal reference line
##' @param title passed to \code{ggtitle}
##' @param shown if \code{TRUE} numbers in each box are shown below tick labels
##' @param ... not used
##'
##' @details
##' Since this function creates a boxplot,
##' the \code{x} column must be character, factor
##' or logical and \code{y} column must
##' be numeric.
##'
##' @export
boxwork <- function(df, x, y, xs=defcx(), ys=defy(), fill="white",
                    alpha=1, hline = NULL, title=NULL, shown = TRUE, ...) {


  if(shown) {
    .xcol <- rlang::sym(x)
    .ycol  <- rlang::sym(y)
    .sum <- dplyr::filter(df, !is.na(!!.ycol))
    .sum <- as.data.frame(dplyr::count(.sum, !! .xcol))
    xs$labels <- paste0(.sum[,x], "\nn=", .sum[,"n"])
  }

  yscale <- do.call("scale_y_continuous", ys)
  xscale <- do.call("scale_x_discrete", xs)


  p <- ggplot(data=df, aes_string(x=x,y=y))
  p <- p + geom_boxplot(fill=fill, alpha=alpha) + yscale + xscale
  if(is.numeric(hline)) {
    p <- p + geom_hline(yintercept=hline,lwd=1, lty=2)
  }
  if(is.character(title)) p <- p + ggtitle(title)
  p
}

##' Plot continuous varible against categorical variable
##'
##' @param df data frame to plot
##' @param x character col//title for x-axis data; see \code{\link{col_label}}
##' @param y character col//title for y-axis data; see \code{\link{col_label}}
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param hline passed to \code{\link{boxwork}}
##' @param ... other arguments passed to \code{\link{boxwork}}
##'
##' @details
##' Since this function creates a boxplot,
##' the \code{x} column must be character, factor
##' or logical and \code{y} column must
##' be numeric.
##'
##' @examples
##' df <- dplyr::filter(superset2(), EVID==0)
##'
##' cont_cat(df, x="STUDYc//Study name", y="WT//Weight (kg)")
##'
##' @export
cont_cat <- function(df, x, y, xs=defcx(), ys = defy(),...) {
  x <- col_label(x)
  if(length(x)!=2) stop("invalid x value", call.=FALSE)
  y <- col_label(y)
  if(length(x)!=2) stop("invalid y value", call.=FALSE)
  require_numeric(df,y[1])
  require_discrete(df,x[1])
  xs$name <- x[2]
  ys$name <- y[2]
  boxwork(df,x[1],y[1],xs,ys,...)
}

##' @export
##' @rdname cont_cat
eta_cat <- function(df, x, y, hline=0, ...) {
  out <- vector(mode="list", length=length(y))
  xx <- col_label(x)
  require_discrete(df,xx[1])
  for(i in seq_along(y)) {
    yy <- col_label(y[i])
    require_numeric(df, yy[1])
    out[[i]] <- cont_cat(df,x,y[i],hline=hline,...)
  }
  return(out)
}

##' Plot residuals versus categorical variable
##'
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param y character name for y-axis data
##' @param hline where to draw horizontal refrence line
##' @param ... passed to \code{\link{cont_cat}}
##'
##' @details
##' Since this function creates a boxplot,
##' the \code{x} column must be character, factor
##' or logical and \code{y} column must
##' be numeric.
##'
##' @examples
##' df <- dplyr::filter(superset2(), EVID==0)
##' cwres_cat(df, x="STUDYc//Study name")
##'
##' @export
res_cat <- function(df, x, y="RES//Residual",
                    hline=0, ...) {
  cont_cat(df,x,y,hline=hline,...)
}

##' @export
##' @rdname res_cat
wres_cat <- function(df, x, y="WRES//Weighted residual",
                     hline=0, ...) {
  cont_cat(df,x,y,hline=hline,...)
}

##' @export
##' @rdname res_cat
cwres_cat <- function(df, x, y="CWRES//Conditional weighted residual",
                      hline=0, ...) {
  cont_cat(df,x,y,hline=hline,...)
}

