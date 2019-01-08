
.stop <- function(...) stop(..., call.=FALSE)

##' Add `CWRES` column from `CWRESI` if needed
##'
##' The intention here is to make sure there is a `CWRES` column if applicable
##' so that the `cwres` variant functions can be used rather than the
##' `cwresi` variants.
##'
##' @details
##' If `CWRESI` was tabled out, then a `CWRES` column is added.  No change
##' is made in case there is already a `CWRES` column.  This function is
##' called with every call to a  `cwresi` variant, so, ideally, the user
##' can call this function once upon loading the data.
##'
##' @param x a data frame
##' @md
##' @export
supplement_cwres <- function(x) {
  if("CWRES" %in% names(x)) return(x)
  if("CWRESI" %in% names(x)) {
    message("Creating CWRES column from CWRESI")
    x[["CWRES"]] <- x[["CWRESI"]]
    return(x)
  }
  return(x)
}

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
  y <- strsplit(x,split=split,fixed=TRUE)[[1]]
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
    if(length(y)==2) {
      return(trimws(y))
    }
  }
  if(!grepl("[[:punct:]]",x)) {
    return(trimws(c(x,x)))
  }
  .stop("invalid 'column // label' specification:\n  ", x)
}

col_labels <- function(x) {
  x <- lapply(x, col_label)
  values <- sapply(x, "[",1)
  names <- sapply(x,"[",2)
  names(values) <- names
  values
}

parse_label <- function(x) {
  if(substr(x,1,2)=="!!") {
    x <- parse(text=substr(x,3,nchar(x)))
    return(x)
  }
  if(look_for_tex(x)) {
    if(requireNamespace("latex2exp")) {
      return(latex2exp::TeX(x))
    }
  }
  x
}

look_for_tex <- function(x) {
  if(getOption("pmplots_TeX_labels",FALSE)) {
    return(TRUE)
  }
  charcount(x,"$") >= 2
}

pm_labs <- function(...) {
  x <- lapply(list(...), parse_label)
  do.call(ggplot2::labs,x)
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
##' @examples
##' data <- pmplots_data_obs()
##'
##' dv_pred(data) + rot_x()
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

no_cwres <- function(object) {
  !("CWRES" %in% names(object))
}

.miss <- function(name,object) {
  !(name %in% names(object))
}

glue_unit <- function(x,xunit) {
  if(is.null(xunit)) return(x)
  if(nchar(xunit) > 0) xunit <- paste0("(",xunit,")")
  glue::glue(x)
}

charcount <- function(x,w,fx=TRUE) {
  nchar(x) - nchar(gsub(w,"",x,fixed=fx))
}

charthere <- function(x,w,fx=TRUE) {
  grepl(w,x,fixed=fx)
}

search_cwres_i <- function(col_name, data) {
  if(col_name %in% names(data)) {
    return(col_name)
  }
  if(col_name=="CWRES") {
    if("CWRESI" %in% names(data)) {
      return("CWRESI")
    }
  }
  if(col_name=="CWRESI") {
    if("CWRES" %in% names(data)) {
      return("CWRES")
    }
  }
  return(col_name)
}

parse_eval <- function(x) {
  eval(parse(text = x),envir=parent.frame(2))
}
