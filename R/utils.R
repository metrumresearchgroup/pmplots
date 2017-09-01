
.stop <- function(...) stop(..., call.=FALSE)

require_discrete <- function(df,x) {
  require_column(df,x)
  cl <- class(unlist(df[1,x],use.names=FALSE))
  if(!is.element(cl, c("character", "factor", "logical"))) {
    .stop("column ", x, " is required to be character, factor, or logical")
  }
}

require_numeric <- function(df,x) {
  require_column(df,x)
  cl <- class(unlist(df[1,x],use.names=FALSE))
  if(!is.element(cl,"numeric")) {
     .stop("column ", x, " is required to be numeric")
  }
  return(invisible(NULL))
}

require_column <- function(df,x) {
  if(!exists(x,df)) {
    .stop("column ", x, " is required in the data set")
  }
  return(invisible(NULL))
}

require_columns <- function(df,...) {
  x <- unlist(list(...), use.names=FALSE)
  for(xx in x) {
    if(!exists(xx,df)) {
      .stop("column ", xx, " is required in the data set")
    }
  }
  return(invisible(NULL))
}


get_limits <- function(df,x,y) {
  range(c(df[,x],df[,y]),na.rm=TRUE)
}

##' Create breaks on log scale
##'
##' @examples
##' logbr()
##' logbr3()
##'
##' @export
logbr <- function() {
  10^seq(-10,10)
}
##' @export
##' @rdname logbr
logbr3 <- function() {
  x <- logbr()
  c(x,3*x)
}

##' Default setting for x-axis scale
##'
##' Basically the formal arguments for \code{scale_x_continuous}.
##' @param ... arguments for \code{scale_x_continuous}
##'
##' @examples
##' defx(trans="log")
##'
##' @export
defx <- function(...) {
  x0 <- list(...)
  x <- as.list(formals(ggplot2::scale_x_continuous))
  x <- merge.list(x,x0)
  x$oob <-  NULL
  x
}

##' Default setting for continuous y-axis scale
##'
##' Basically the formal arguments for \code{scale_y_continuous}.
##' @param ... arguments for \code{scale_y_continuous}
##'
##' @examples
##' defy(trans="log")
##'
##' @export
defy <- function(...) {
  x0 <- list(...)
  x <- as.list(formals(ggplot2::scale_y_continuous))
  x <- merge.list(x,x0)
  x$oob <-  NULL
  x
}
##' Default setting for discrete x-axis scale
##'
##' Basically the formal arguments for \code{scale_x_discrete}.
##' @param ... arguments for \code{scale_x_discrete}

##' @examples
##' defcx()
##'
##' @export
defcx <- function(...) {
  x0 <- list(...)
  x <- as.list(formals(ggplot2::scale_x_discrete))
  x <- merge.list(x,x0)
  x[[1]] <- NULL
  x
}

##' Scale information for log transformation.
##'
##' @param br breaks
##' @param ... additional parameters
##'
##' @examples
##' log_scale()
##'
##' @export
log_scale <- function(br=logbr(),...) {
  x <- list(trans="log",breaks=logbr())
  c(x,list(...))
}

split_col_label <- function(x,split="//") {
  y <- strsplit(x, split=split,fixed=TRUE)[[1]]
  sapply(y,FUN=trimws, USE.NAMES=FALSE)
}

##' Column // axis-label specification
##'
##' @param x string encoding data column and axis title
##'
##' @examples
##' col_label("CL // Clearance (L)")
##' col_label("CL//Clearance (L)")
##' col_label("CL $$ Clearance (L)")
##' col_label("CL !! Clearance (L)")
##' col_label("CL @@@@ Clearance (L)")
##'
##' try(col_label("CL / Clearance (L)"))
##'
##' @export
col_label <- function(x) {
  for(sp in c("//","$$", "@@", "!!")) {
    y <- split_col_label(x,sp)
    if(length(y)==2) return(y)
  }
  .stop("invalid 'column // label' specification:\n  ",x)
}

noline <- ggplot2::element_blank()
##' A plan ggplot2 theme.
##'
##' @param ... passed to \code{ggplot2::theme}
##' @export
theme_plain <- function(...) {
  ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major=noline,panel.grid.minor=noline,
                   plot.margin=margin(0.5,0.5,1,0.5,unit="cm"),...)
}


merge.list <- function(x,y,...,open=FALSE,
                       warn=FALSE,context="object") {
  y <- as.list(y)

  ## Merge two lists
  common <- intersect(names(x), names(y))

  x[common] <- y[common]

  if(open)  {
    nw <- !is.element(names(y),names(x))
    x <- c(x,y[nw])
  } else {
    if(length(common)==0 & warn) {
      warning(paste0("Found nothing to update: ", context), call.=FALSE)
    }
  }
  x
}


##' Add loess line
##' @param method passed to \code{geom_smooth}
##' @param se passed to \code{geom_smooth}
##' @param lty passed to \code{geom_smooth}
##' @param lwd passed to \code{geom_smooth}
##' @param col passed to \code{geom_smooth}
##' @param ... passed to \code{geom_smooth}
##' @export
pmsmooth <- function(method="loess", se=FALSE, lty=2, lwd=1.3, col=.ggblue,...) {
  geom_smooth(method=method,se=se,lty=lty,lwd=lwd,col=col)
}
