
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
  if(!is.element(cl,c("numeric","integer"))) {
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
  sort(c(x,3*x))
}

##' Default setting for x-axis scale
##'
##' A named list of the formal arguments for \code{scale_x_continuous}.  This
##' function helps you to create a fully populated list.  For any
##' function that uses this as a default, you can also
##' create your own named list for arguments you want to update, using
##' this default as a base.
##'
##' @param ... arguments for \code{scale_x_continuous}
##'
##' @details
##' In the named list, the name is the argument name and the value
##' is the argument value.
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
##' A named list of the formal arguments for \code{scale_y_continuous}.  This
##' function helps you to create a fully populated list.  For any
##' function that uses this as a default, you can also
##' create your own named list for arguments you want to update, using
##' this default as a base.
##'
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
##' A named list of the formal arguments for \code{scale_x_discrete}.  This
##' function helps you to create a fully populated list.  For any
##' function that uses this as a default, you can also
##' create your own named list for arguments you want to update using
##' this default as a base.
##'
##' @param ... arguments for \code{scale_x_discrete}
##'
##' @details
##' In the named list, the name is the argument name and the value
##' is the argument value.
##'
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

##' Scale information for log transformation
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
##' The col-label specification is a way to identify a column
##' in a data set that is to be used for plotting (the \code{col} piece) along
##' with a label to be used for the axis data (the \code{label} piece).
##'
##' @param x string encoding data column and axis title
##'
##' @details
##' Typically, the \code{col} part is separated from the \code{label}
##' part with a double front-slash.  So, to identify the column containing
##' the weight covariate we might specify \code{WT//Weight (kg)}.
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
    if(length(y)==2) return(trimws(y))
  }
  if(!grepl("[[:punct:]]",x)) {
    return(trimws(c(x,x)))
  }
  .stop("invalid 'column // label' specification:\n  ", x)
}

noline <- ggplot2::element_blank()

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

combine_list <- function(left, right) {
  if(!all(is.list(left),is.list(right))) {
    stop("input are not lists")
  }
  left[names(right)] <-  right
  left
}

update_list <- function(left, right) {
  if(!all(is.list(left),is.list(right))) {
    stop("input are not lists")
  }
  common <- intersect(names(left), names(right))
  left[common] <-  right[common]
  left
}

##' Rotate axis text
##'
##' @param angle passed to \code{ggplot::element_text}
##' @param hjust passed to \code{ggplot::element_text}
##'
##' @export
rot_x <- function(angle=30, hjust = 1) {
  theme(axis.text.x = element_text(angle = angle, hjust = hjust))
}

##' @rdname rot_x
##' @export
rot_y <- function(angle=30, hjust = 1) {
  theme(axis.text.y = element_text(angle = angle, hjust = hjust))
}

.has <- function(name,object) {
  name %in% names(object)
}

.miss <- function(name,object) {
  !(name %in% names(object))
}

glue_unit <- function(x,xunit) {
  if(is.null(xunit)) return(x)
  if(nchar(xunit) > 0) xunit <- paste0("(",xunit,")")
  glue::glue(x)
}



