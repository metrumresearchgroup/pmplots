
.stop <- function(...) stop(..., call.=FALSE)

warn_cwres <- function(data) {
  warning("CWRES column does not exist in this data frame",call.=FALSE)
  return(invisible(NULL))
}

warn_cwresi <- function(data) {
  warning("CWRESI column does not exist in this data frame",call.=FALSE)
  return(invisible(NULL))
}


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
##' @param from smallest break on log10 scale (see default value)
##' @param to largest break on log10 scale (see default value)
##'
##' @examples
##' logbr()
##' logbr(-5,8)
##' logbr3()
##'
##' @export
logbr <- function(from=-10,to=10) {
  10^seq(from,to)
}
##' @export
##' @rdname logbr
logbr3 <- function() {
  x <- logbr()
  sort(c(x,3*x))
}

##' Identity and log scale helpers
##'
##' @param breaks passed to scale function
##' @param limits passed to scale function
##' @param transform passed to scale function
##' @param trans deprecated; use `transform` argument instead.
##' @param ... passed to scale function
##'
##' @export
pm_log <- function(breaks = NULL, limits=NULL, transform = "log10", ..., trans = deprecated()) {
  ans <- list(transform = transform, ...)
  if (is_present(trans)) {
    deprecate_warn("0.5.0", "pm_log(trans)", "pm_log(transform)")
    ans[["transform"]] <- trans
  }
  ans[["breaks"]] <- breaks
  ans[["limits"]] <- limits
  ans
}

##' @rdname pm_log
##' @export
pm_ident <- function(breaks, limits = range(breaks), ...) {
  ans <- list(...)
  ans[["breaks"]] <- breaks
  ans[["limits"]] <- limits
  ans
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
##' @param trans deprecated; use `transform` argument instead.
##'
##' @details
##' In the named list, the name is the argument name and the value
##' is the argument value.
##'
##' @examples
##' defx(transform="log")
##'
##' @export
defx <- function(..., trans = deprecated()) {
  x0 <- list(...)
  if (is_present(trans)) {
    deprecate_warn("0.5.0", "defx(trans)", "defx(transform)")
    x0[["transform"]] <- trans
  }
  x <- as.list(formals(ggplot2::scale_x_continuous))
  x[["trans"]] <- NULL
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
##' @param trans deprecated; use `transform` argument instead.
##'
##' @examples
##' defy(transform="log")
##'
##' @export
defy <- function(..., trans = deprecated()) {
  x0 <- list(...)
  if (is_present(trans)) {
    deprecate_warn("0.5.0", "defy(trans)", "defy(transform)")
    x0[["transform"]] <- trans
  }
  x <- as.list(formals(ggplot2::scale_y_continuous))
  x[["trans"]] <- NULL
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
  x[["..."]] <- NULL
  x
}

##' Scale information for log transformation
##'
##' @param breaks breaks
##' @param ... additional scale parameters
##'
##' @examples
##' log_scale()
##'
##' @export
log_scale <- function(breaks=NULL,...) {
  ans <- list(transform="log10",...)
  ans[["breaks"]] <- breaks
  ans
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
  if(!grepl("[ ()/$!@]",x)) {
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

col_label_col <- function(x) {
  x <- lapply(x, col_label)
  vapply(x, "[", 1, FUN.VALUE = "a")
}

#' Parse the label part of a col_label
#'
#' @details
#' The label can include TeX math expressions (more common) or plotmath.
#'
#' TeX math expressions are detected if more than two `$` are detected in the
#' string. Parsing is accomplished with the `TeX` function from the `latex2exp`
#' package; a warning if  the`latex2exp` is not available.
#'
#' Text to be parsed as plotmath is detected when the text begins with `!!`.
#'
#' @param x A character string.
#'
#' @examples
#' parse_label("foo $\\mu$")
#' parse_label("!!mu")
#'
#' @md
#' @export
parse_label <- function(x) {
  if(substr(x, 1, 2) == "!!") {
    x <- parse(text = substr(x, 3, nchar(x)))
    return(x)
  }
  if(look_for_tex(x)) {
    if(!requireNamespace("latex2exp")) {
      warning("Please install the latex2exp package to parse TeX expressions.")
    } else {
      x <- latex2exp::TeX(x)
    }
  }
  x
}

#' @rdname parse_label
#' @export
look_for_tex <- function(x) {
  if(getOption("pmplots.TeX.labels", FALSE)) {
    return(TRUE)
  }
  charcount(x,"$") >= 2
}

#' TeX labeller
#'
#' This function can be passed to the `labeller` argument of
#' [ggplot2::facet_wrap()] or [ggplot2::facet_grid()]. Labels are processed
#' using [parse_label()].
#'
#' @param x Data frame of labels.
#'
#' @rdname parse_label
#' @md
#' @export
label_tex <- function(x) {
  x <- lapply(x, as.character)
  lapply(x, function(values) {
    lapply(values, parse_label)
  })
}

#' @rdname parse_label
#' @export
label_parse_label <- label_tex

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

remap_trans_arg <- function(args, user_env = rlang::caller_env(2)) {
  # ggplot2 3.5.0 deprecated `trans` in favor of `transform`.
  if ("trans" %in% names(args)) {
    deprecate_warn(
      "0.5.0", I("`trans` argument"), I("`transform` argument"),
      user_env = user_env
    )
    args[["transform"]] <- args[["trans"]]
    args[["trans"]] <- NULL
  }
  return(args)
}

##' Rotate axis text
##'
##' @param angle passed to [ggplot2::element_text()].
##' @param hjust passed to [ggplot2::element_text()].
##' @param vjust passed to [ggplot2::element_text()].
##' @param vertical if `TRUE`, then x-axis tick labels are rotated 90 degrees
##' with `vjust` set to 0.5 and `hjust` set to 1; when using `rot_y()`,
##' y-axis tick labels are rotated 90 degrees and `hjust` is set to 0.5
##' with `vjust` set to 1; see details.
##' @param ... passed to [ggplot2::element_text()].
##'
##' @details
##'
##' If x-axis tick labels do not have enough space, consider using
##' `vertical = TRUE`.  By default, the tick labels will be justified up to the
##' x-axis line.  Use `hjust = "bottom"` (with `vertical = TRUE`)
##' to justify the axis labels toward the bottom margin of the plot. Similar
##' behavior can be made for y-axis tick labels, but use `vertical = TRUE` and
##' set `vjust` to either "left" or "right" to control proximity to the y-axis.
##'
##' @examples
##' data <- pmplots_data_obs()
##'
##' dv_pred(data) + rot_x()
##'
##' \dontrun{
##' cwres_cat(data, x = "CPc") + rot_x(vertical = TRUE)
##' cwres_cat(data, x = "CPc") + rot_x(vertical = TRUE, hjust = "b")
##' }
##'
##' @md
##' @export
rot_x <- function(angle=30, hjust = 1, vjust = NULL, vertical = FALSE, ...) {
  if(isTRUE(vertical)) {
    if(is.character(hjust)) {
      hjust <- match.arg(hjust, c("top", "bottom"))
      if(hjust=="top") hjust <- 1
      if(hjust=="bottom") hjust <- 0
    } else {
      if(missing(hjust)) hjust <- 1
    }
    angle <- 90
    vjust <- 0.5
  } else {
    if(is.character("hjust")) {
      abort("hjust must be numeric or NULL in this case.")
    }
  }
  x <- element_text(angle = angle, hjust = hjust, vjust = vjust, ...)
  theme(axis.text.x=x)
}

##' @rdname rot_x
##' @export
rot_y <- function(angle=30, hjust = 1, vjust = NULL, vertical = FALSE, ...) {
  if(isTRUE(vertical)) {
    if(is.character(vjust)) {
      vjust <- match.arg(vjust, c("top", "bottom"))
      if(vjust=="top") vjust <- 1
      if(vjust=="bottom") vjust <- 0
    } else {
      if(missing(vjust)) vjust <- 1
    }
    angle <- 90
    hjust <- 0.5
  } else {
    if(is.character(vjust)) {
      abort("`vjust` must be numeric or NULL in this case.")
    }
  }
  y <- element_text(angle = angle, hjust = hjust, vjust = vjust,, ...)
  theme(axis.text.y=y)
}


.rotxy <- function(x, ...) UseMethod(".rotxy")
#' @export
.rotxy.gg <- function(x, axis = "x", ...) {
  if(axis=="x") {
    x <- x + rot_x(...)
  } else {
    x <- x + rot_y(...)
  }
  x
}
#' @export
.rotxy.patchwork <- function(x, axis = "x", ...) {
  stopifnot(requireNamespace("patchwork", quietly = TRUE))
  if(axis=="x") {
    x <- x & rot_x(...)
  } else {
    x <- x & rot_y(...)
  }
  x
}
#' @export
.rotxy.default <- function(x, ...) {
  stop("invalid object; can only rotate gg or patchwork objects.")
}

#' Rotate axis tick marks in a list of plots
#'
#' Pass in a list of gg or patchwork objects and rotate tick marks on x or y
#' axes.
#'
#' @param x a named list of gg or patchwork objects.
#' @param at a character vector of list names to rotate.
#' @param re a regular expression for selecting names to be used as `at`.
#' @param axis which axis to rotate.
#' @param ... additional arguments passed to [rot_x()] or [rot_y()].
#'
#' @details
#' Note that all plots in the list need to be named. If
#'
#' @examples
#' data <- pmplots_data_id()
#'
#' co <- c("STUDYc", "CPc", "RF")
#' etas <- paste0("ETA", 1:3)
#'
#' x <- eta_cat(data, x = co, y = etas)
#' names(x)
#'
#' x <- rot_at(x, at = "ETA1vRF", angle = 35)
#' x$ETA1vRF
#'
#' x <- rot_at(x, re = "RF", vertical = TRUE)
#' x$ETA2vRF
#' x$ETA3vRF
#'
#' @md
#' @export
rot_at <- function(x, at = names(x), re = NULL, axis = c("x", "y"), ...) {
  if(!is.list(x) || is.ggplot(x) || inherits(x, "patchwork")) {
    abort("`x` must be a list of gg or patchwork objects.")
  }
  if(!is_named(x)) abort("`x` must be named.")
  axis <- match.arg(axis)
  if(is.character(re)) {
    where <- grep(re, names(x), perl = TRUE)
  } else {
    if(!is.character(at)) abort("`at` must be character.")
    bad <- setdiff(at, names(x))
    if(length(bad)) {
      names(bad) <- rep("x", length(bad))
      abort("requested names not found in `x`.", body = bad)
    }
    where <- which(names(x) %in% at)
  }
  if(!length(where)) {
    warn("did not find any plots for axis rotation.")
    return(x)
  }
  for(w in where) {
    x[[w]] <- .rotxy(x[[w]], axis = axis, ...)
  }
  x
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

glue_unit <- function(x, xunit) {
  if(is.null(xunit)) return(x)
  if(nchar(xunit) > 0) xunit <- paste0("(",xunit,")")
  as.character(glue(x))
}

glue_xname <- function(x, xname) {
  if(is.null(xname)) return(x)
  as.character(glue(x))
}

glue_yname <- function(x, yname) {
  if(is.null(yname)) return(x)
  as.character(glue(x))
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

#' Arrange a list of plots in a grid
#'
#' This is a light wrapper around [patchwork::wrap_plots()] to
#' arrange the plots and [patchwork::plot_annotation()] to
#' optionally tag levels in the grid of plots.
#'
#' @param x a list of plots.
#' @param ncol passed to [patchwork::wrap_plots()].
#' @param tag_levels passed to [patchwork::plot_annotation()].
#' @param ... passed to [patchwork::wrap_plots()].
#'
#' @details
#' The patchwork package must be installed to use this function.
#'
#' @examples
#' data <- pmplots_data_obs()
#'
#' plot <- wres_cont(data, x = c("WT", "ALB"))
#'
#' pm_grid(plot)
#'
#' pm_grid(plot, tag_levels = "a")
#'
#' @md
#' @export
pm_grid <- function(x, ncol = 2, tag_levels = NULL, ...) {
  require_patchwork()
  x <- patchwork::wrap_plots(x, ncol = ncol, ...)
  if(!is.null(tag_levels)) {
    x <- x + patchwork::plot_annotation(tag_levels = tag_levels)
  }
  x
}

#' Arrange a named list of plots
#'
#' @param x a named list of gg objects to arrange.
#' @param expr a `patchwork` formula for arranging plots in `x`.
#' @param tag_levels passed to [patchwork::plot_annotation()].
#'
#' @examples
#' data <- pmplots_data_id()
#' etas <- paste0("ETA", 1:3)
#' covs <- c("WT", "AGE", "ALB")
#'
#' x <- eta_covariate_list(data, x = covs, y = etas, transpose = TRUE)
#'
#' pm_with(x$WT, (ETA1 + ETA2) / ETA3)
#'
#' @export
pm_with <- function(x, expr, tag_levels = NULL) {
  # See also with method in displays.R
  if(!is_named(x)) {
    abort("`x` must be named.")
  }
  if(!is.list(x) || is.ggplot(x) || inherits(x, "patchwork")) {
    abort("`x` must be a list.")
  }
  require_patchwork()
  expr <- enexpr(expr)
  p <- eval(expr, envir = x)
  p <- p + patchwork::plot_annotation(tag_levels = tag_levels)
  p
}

#' Chunk a data frame
#'
#' @param data a data frame.
#' @param id_per_chunk number of units per chunk.
#' @param cols a character vector of one or more columns to use for deriving
#' `ID` to use for chunking.
#'
#' @return
#' A list of data frames.
#'
#' @examples
#' x <- expand.grid(ID = 1:10, B = rev(1:10))
#'
#' chunk_by_cols(x, 3, "ID")
#'
#' @keywords internal
#' @noRd
chunk_by_cols <- function(data, id_per_chunk, cols) {
  if(!is.character(cols)) {
    stop("`cols` argument must be character.")
  }
  if(!is.data.frame(data)) {
    stop("`data` argument must be a data.frame.")
  }
  for(col in cols) {
    if(!exists(col, data)) {
      stop(sprintf("chunking column %s does not exist in data.", col))
    }
  }
  if(!is.numeric(id_per_chunk)) {
    stop("`id_per_chunk` must be numeric.")
  }
  if(!(id_per_chunk > 0)) {
    stop("`id_per_chunk` must be greater than zero.")
  }
  if(length(cols) ==1) {
    id <- data[[cols]]
  } else {
    id <- do.call(paste, c(data[, cols, drop = FALSE], sep = "-@-@-"))
  }
  uids <- unique(id)
  ntot <- length(uids)
  if(!(id_per_chunk <= ntot)) {
    stop("id_per_chunk must be <= number of unique values in `cols`.")
  }
  chunkn <- seq(ceiling(ntot %/% id_per_chunk) + 1)
  a <- sort(rep(chunkn, each = id_per_chunk, length.out = ntot))
  sp <- a[match(id, uids)]
  split.data.frame(data, sp)
}

force_digits <- function(x,digits) formatC(x,digits=digits,format = 'f')

require_patchwork <- function() {
  if(!requireNamespace("patchwork", quietly = TRUE)) {
    stop('The "patchwork" package is required.')
  }
}

