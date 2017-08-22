
##' Scatter plot function.
##'
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param y character name for y-axis data
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param smooth if \code{TRUE} a loess smooth will be drawn
##' @param identity if \code{TRUE} a line of identity will be drawn
##' @param hline if numeric, a horizontal reference line will be drawn
##' @param title character, passed to \code{ggtitle}
##' @param group character name of grouping variable
##' @param col character name of coloring variable
##' @param scale_col color scale
##' @param ... not used
##' @export
scatt <- function(df, x, y, xs = defx(), ys = defy(), smooth=TRUE,
                  identity=FALSE,hline = NULL,title = NULL,
                  group=NULL, col=NULL,
                  scale_col = scale_color_brewer(palette="Set2", name=""),
                  ... ) {

  xscale <- do.call("scale_x_continuous", xs)
  yscale <- do.call("scale_y_continuous", ys)

  locol <- .ggblue
  p <- ggplot(data=df,aes_string(x,y,col=col)) + geom_point() + xscale + yscale
  if(!is.null(group)) p <- p + geom_line(aes_string(group=group))

  if(identity) p <- p + geom_abline(intercept=0,slope=1)
  if(!is.null(hline)) p <- p + geom_hline(yintercept=hline, lwd=1, lty=2)
  if(is.character(title)) p <- p + ggtitle(title)
  if(is.character(col)) {
    if(missing(scale_col)) require_discrete(df, col)
    locol <- "black"
    p <- p + theme(legend.position="top") + scale_col
  }
  if(smooth) p <- p + geom_smooth(method="loess",se=FALSE,lty=2,lwd=1,col=locol)
  p
}

##' Plot DV versus predicted values
##'
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param y character name for y-axis data
##' @param what used to form y-axis label
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param loglog if \code{TRUE}, x- and y-axes will be log-transformed
##' @param prefix used internally
##' @param ... passed to \code{\link{scatt}}
##'
##' @examples
##' df <- dplyr::filter(superset2(), EVID==0)
##' dv_pred(df)
##'
##' @export
dv_pred <- function(df, x="DV", y="PRED", what="value",xs = defx(),
                    ys = defy(), loglog=FALSE, prefix="Population", ...) {

  require_column(df, x, y)

  ys$name <- paste0(prefix, " predicted ", what)
  xs$name <- paste0("Observed ", what)

  if(loglog) {
    xs$trans <- "log"
    ys$trans <- "log"
  }

  if(xs$trans %in% c("log", "log10")) {
    xkp <- df[,x] > 0
    df <- dplyr::filter(df,xkp)
    if(!is.numeric(xs$breaks)) {
      xs$breaks <- logbr()
    }
  }

  if(ys$trans %in% c("log", "log10")) {
    ykp <- df[,y] > 0
    df <- dplyr::filter(df,ykp)
    if(!is.numeric(ys$breaks) | is.null(ys$breaks)) {
      ys$breaks <- logbr()
    }
  }

  lim <- get_limits(df,x,y)
  xs$limits <- lim
  ys$limits <- lim

  scatt(df,x,y,identity=TRUE,xs=xs,ys=ys,...)
}

##' @export
##' @rdname dv_pred
dv_ipred <- function(df,...) {
  dv_pred(df,prefix="Individual", ...)
}

##' Plot DV versus time
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param y character name for y-axis data
##' @param xunit time units; used to form x-axis label
##' @param what used to form y-axis label
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param group passed to \code{\link{scatt}}
##' @param log if \code{TRUE}, y-axis will be log-transformed
##' @param ... passed to \code{\link{scatt}}
##'
##' @examples
##' df <- dplyr::filter(superset2(), EVID==0)
##' dv_time(df)
##' dv_time(df, log=TRUE, col="STUDYc")
##'
##' @export
dv_time <- function(df, x="TIME", y="DV", xunit="hr",
                    what = NULL, group="ID",
                    xs=defx(), ys=defy(), log=FALSE,...) {
  require_column(df, x, y)
  xs$name <- paste0("Time (",xunit,")")
  ys$name <- what
  if(log) {
    ys$trans <- "log"
    ys$breaks <- logbr3()
  }
  if(ys$trans %in% c("log", "log10")) {
    ykp <- df[,y] > 0
    df <- dplyr::filter(df,ykp)
    if(!is.numeric(ys$breaks) | is.null(ys$breaks)) {
      ys$breaks <- logbr()
    }
  }
  scatt(df,x,y,xs=xs,ys=ys,smooth=FALSE,group=group,...)
}

##' Plot continuous variable versus continuous variable
##'
##' @param df data frame to plot
##' @param x character col//title for x-axis data; see \code{\link{col_label}}
##' @param y character col//title for y-axis data; ; see \code{\link{col_label}}
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param ... passed to \code{\link{scatt}}
##' @seealso \code{\link{scatt}}
##' @export
cont_cont <- function(df, x, y, xs = defx(), ys=defy(),...) {
  y <- col_label(y)
  x <- col_label(x)
  ys$name <- y[2]
  xs$name <- x[2]
  require_column(df, x[1], y[1])
  scatt(df,x[1],y[1],xs,ys,...)
}

##' @export
##' @rdname cont_cont
eta_cont <- function(df,x,y,...) {
  out <- vector(mode="list", length=length(y))
  for(i in seq_along(y)) {
    out[[i]] <- cont_cont(df,x,y[i],...)
  }
  return(out)
}

##' @export
##' @rdname cont_cont
cwres_cont <- function(df,x,y="CWRES//Conditional weighted residual",
                       xs = defx(), ys = defy(),...) {
  x <- col_label(x)
  y <- col_label(y)
  if(length(x)!=2) .stop("invalid x name")
  if(length(y)!=2) .stop("invalid y name")
  ys$name <- y[2]
  xs$name <- x[2]
  require_column(df, x[1], y[1])
  scatt(df,x[1],y[1],xs=xs,ys=ys,horiz = 0)
}

##' @export
##' @rdname cont_cont
wres_cont <- function(df,x,y="WRES//Weighted residual",
                      xs = defx(), ys = defy(),...) {
  x <- col_label(x)
  y <- col_label(y)
  if(length(x)!=2) .stop("invalid x name")
  if(length(y)!=2) .stop("invalid y name")
  ys$name <- y[2]
  xs$name <- x[2]
  require_column(df, x[1], y[1])
  scatt(df,x[1],y[1],xs=xs,ys=ys,horiz = 0)
}

##' @export
##' @rdname cont_cont
res_cont <- function(df,x,y="RES//Residual",
                     xs = defx(), ys = defy(),...) {
  x <- col_label(x)
  y <- col_label(y)
  if(length(x)!=2) .stop("invalid x name")
  if(length(y)!=2) .stop("invalid y name")
  ys$name <- y[2]
  xs$name <- x[2]
  require_column(df, x[1], y[1])
  scatt(df,x[1],y[1],xs=xs,ys=ys,horiz = 0)
}

##' Plot residuals versus time
##'
##' @param df data set to plot
##' @param x character name of x-axis data
##' @param y character name of y-axis data
##' @param what used for y-axis label
##' @param xwhat used for x-axis label (not typically used)
##' @param ... passed to \code{\link{y_time}}
##' @seealso \code{\link{y_time}}
##'
##' @examples
##' df <- dplyr::filter(superset2(), EVID==0)
##' cwres_time(df)
##'
##' @export
res_time <- function(df,
                     what="Residual",
                     xwhat="Time",
                     x="TIME", y="RES", ...) {
  y_time(df, what=what, x=x, y=y, ...)
}

##' @export
##' @rdname res_time
wres_time <- function(df,
                      what="Weighted residual",
                      xwhat="Time",
                      x="TIME", y="WRES",...) {
  y_time(df,what=what,x=x,y=y,...)
}

##' @export
##' @rdname res_time
cwres_time <- function(df, what="Conditional weighted residual",
                       x="TIME", y="CWRES",...) {
  y_time(df,what=what,x=x,y=y,...)
}

##' @export
##' @rdname res_time
cwres_tad <- function(df,
                      what="Conditional weighted residual",
                      xwhat="Time after dose",
                      x="TAD", y="CWRES",...) {
  y_time(df, what=what, xwhat=xwhat, x=x, y=y, ...)
}

##' @export
##' @rdname res_time
wres_tad <- function(df,
                     what="Weighted residual",
                     xwhat="Time after dose",
                     x="TAD", y="WRES",...) {
  y_time(df, what=what, xwhat=xwhat, x=x, y=y, ...)
}

##' @export
##' @rdname res_time
res_tad <- function(df,
                    what="Residual",
                    xwhat="Time after dose",
                    x="TAD", y="RES",...) {
  y_time(df, what=what, xwhat=xwhat, x=x, y=y, ...)
}


##' Plot continuous data versus time.
##'
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param y character name for y-axis data
##' @param xunit time unit, used to for x-axis label
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param what used to form y-axis title
##' @param xwhat used to form y-axis title
##' @param log if \code{TRUE}, y-axis will be log-transformed
##' @param ... passed to \code{\link{scatt}}
y_time <- function(df, x="TIME", y,  what, xunit="hr",
                   xs = defx(), ys = defy(),
                   xwhat = "Time", log=FALSE, ...) {
  require_column(df, x, y)
  xs$name <- paste0(xwhat, " (",xunit,")")
  ys$name <- what
  if(log) {
    ys$trans <- "log"
    ys$breaks <- logbr3()
  }
  scatt(df, x, y, xs=xs, ys=ys, ...)
}


##' Residuals versus predicted values
##'
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param y character naem for y-axis data
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param what used to form y-axis label
##' @param ... passed to \code{\link{scatt}}
##'
##' @examples
##' df <- dplyr::filter(superset2(), EVID==0)
##' cwres_pred(df)
##'
##' @export
res_pred <- function(df, x="PRED", y="RES", xs=defx(), ys=defy(),
                     what="value", ...) {
  xs$name <- paste0("Population predicted ", what)
  ys$name <- "Residual"
  scatt(df,x,y,xs,ys,...)
}

##' @export
##' @rdname res_pred
cwres_pred <- function(df, x="PRED", y="CWRES", xs=defx(), ys=defy(),
                       what="value",...) {
  xs$name <- paste0("Population predicted ", what)
  ys$name <- "Conditional weighted residual"
  scatt(df,x,y,xs,ys,...)
}

##' @export
##' @rdname res_pred
wres_pred <- function(df, x="PRED", y="WRES", xs=defx(), ys=defy(),
                      what="value",...) {
  xs$name <- paste0("Population predicted ", what)
  ys$name <- "Weighted residual"
  scatt(df,x,y,xs,ys,...)
}
