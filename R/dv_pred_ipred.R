
#' Create plots showing DV, PRED, and IPRED by individual
#'
#' `DV` is plotted with a symbol while `PRED` and `IPRED` are plotted with lines.
#' The plot is faceted by unique individual identifier (like `ID` or `USUBJID`).
#'
#' @param data the data frame to plot
#' @param ... additional arguments passed to [dv_pred_ipred_impl]
#' @param id_per_plot number of subjects to include in each page
#' @param ncol number of columns in the plot grid; passed to [ggplot2::facet_wrap]
#' @param nrow number of rows in the plot grid; passed to [ggplot2::facet_wrap]
#' @param x the time-axis column, in [col_label] format; the title portion is
#' used for the x-axis title along with `xunit`; seel also the `xlab` argument
#' @param dv the `DV` column, in [col_label] format; the title portion is used
#' for the y-axis title; see also the `ylab` argument
#' @param pred the name of the `PRED` column; [col_label] format is allowed, but
#' the label portion is discarded
#' @param ipred the name of the `IPRED` column; [col_label] format is allowed,
#' but the label portion is discarded
#' @param id_col the column name for the subject identifier; the value will
#' be displayed in the plot strip
#' @param xbreaks x-axis breaks; passed to [ggplot2::scale_x_continuous]
#' @param xunit used to form x-axis title only if `xlab` is not provided
#' @param xlab x-axis title; if not `NULL`, passed to [ggplot2::xlab]
#' @param ylab y-axis title; if not `NULL`, passed to [ggplot2::ylab]
#' @param angle rotation angle for x-axis tick labels; passed to [rot_x]
#' @param font_size the font size for strip text
#' @param margin the top and bottom margin for the plot strip; passed to
#' [ggplot2::margin] as `t` and `b`
#' @param legend.position passed to [ggplot2::theme]
#' @param pred_lty `PRED` linetype; passed to [ggplot2::geom_line]
#' @param ipred_lty `IPRED` linetype; passed to [ggplot2::geom_line]
#' @param lwd line width for `PRED` and `IPRED`; passed to [ggplot2::geom_line]
#' @param size size of shape for `DV`; passed to [ggplot2::geom_point]
#' @param dv_shape shape for `DV`; passed to [ggplot2::geom_point]
#' @param dv_line logical; if `TRUE` then a line is added to the plot connecting
#' `DV` points
#' @param dv_lwd  line width for `DV`; passed to [ggplot2::geom_line] as `lwd`
#' @param scales passed to [ggplot2::facet_wrap]
#' @param log_y logical; if `TRUE` then y-axis is shown in log-scale
#' @param use_theme a theme to use for the plot
#' @param dv_color color to use for `DV` points
#' @param ipred_color color to use for `IPRED` line
#' @param pred_color color to use for `PRED` line
#' @param axis.text.rel relative text size for axis text; use this to selectively
#' decrease font size for axis tick labels
#' @param fun a function accepting a gg object as arugment and returning
#' an updated gg object; experimental
#'
#' @details
#'
#' if both `nrow` and `ncol` are supplied and numeric, `id_per_plot` will be set
#' to `nrow*ncol`.
#'
#' @examples
#'
#' data <- pmplots_data_obs()
#'
#' p <- dv_pred_ipred(data, ylab="Concentration (ng/mL)", nrow=3, ncol=3)
#'
#' @md
#' @export
dv_pred_ipred <- function(data, id_per_plot = 9,
                          id_col = "ID",nrow = NULL, ncol = NULL, fun=NULL,...) {
  if(is.numeric(nrow) && is.numeric(ncol)) {
    if(!missing(id_per_plot) & id_per_plot != nrow*ncol) {
      warning("updating id_per_plot to ", nrow*ncol,call.=FALSE)
    }
    id_per_plot <- nrow*ncol
  }
  ans <- chunk_by_id(data, nchunk=id_per_plot, id_col = id_col)
  out <- lapply(ans, dv_pred_ipred_impl, ...,  id_col = id_col,nrow=nrow,
                ncol = ncol)
  if(is.function(fun)) out <- lapply(out, fun)
  out
}

#' @rdname dv_pred_ipred
#' @export
dv_pred_ipred_impl <- function(data,
                               x = pm_axis_time(),
                               dv  = pm_axis_dv(),
                               pred = pm_axis_pred(),
                               ipred = pm_axis_ipred(),
                               id_col = "USUBJID",
                               xbreaks = waiver(),
                               xunit = "hr",
                               xlab  = NULL,
                               ylab  = NULL,
                               angle = NULL,
                               font_size = 5,
                               margin = 1,
                               legend.position = "top",
                               pred_lty = 2,
                               ipred_lty = 1,
                               lwd = 0.7,
                               size = 1.5,
                               dv_shape = 1,
                               dv_line = FALSE,
                               dv_lwd = 0.5,
                               scales = "free",
                               log_y = FALSE,
                               use_theme = theme_plain(),
                               dv_color = "black",
                               ipred_color = "firebrick",
                               pred_color = "darkslateblue",
                               ncol = NULL,
                               nrow = NULL,
                               axis.text.rel = NULL,
                               fun = NULL) {

  show_dv <- TRUE
  show_ipred <- TRUE
  show_pred <- TRUE
  ycols <- character(0)
  clrs <- character(0)
  lnes <- integer(0)
  shapes <- integer(0)
  if(!exists(id_col,data)) {
    stop("cannot find column 'id_col' (", id_col, ")",call.=FALSE)
  }
  if(is.null(dv)) {
    dv <- col_label("DV")
    yl <- dv[2]
    dv <- dv[1]
    show_dv <- FALSE
  } else {
    dv <- col_label(dv)
    yl <- dv[2]
    dv <- dv[1]
    require_numeric(data,dv)
    ycols <- c(ycols,dv)
    clrs <- c(clrs, dv_color)
    shapes <- c(shapes,dv_shape)
    lnes <- c(lnes,0)
    if(anyNA(data[[dv]])) {
      warning("[pmplots] removed missing values in dv column",call.=FALSE)
    }
  }
  if(is.null(ipred)) {
    ipred <- col_label("IPRED")[1]
    show_ipred <- FALSE
  } else {
    ipred <- col_label(ipred)[1]
    require_numeric(data,ipred)
    ycols <- c(ycols, ipred)
    clrs <- c(clrs,ipred_color)
    lnes <- c(lnes,ipred_lty)
    shapes <- c(shapes,NA)
    if(anyNA(data[[ipred]])) {
      warning("[pmplots] removed missing values in ipred column",call.=FALSE)
    }
  }
  if(is.null(pred)) {
    pred <- col_label("PRED")[1]
    show_pred <- FALSE
  } else {
    pred <- col_label(pred)[1]
    require_numeric(data,pred)
    ycols <- c(ycols,pred)
    clrs <- c(clrs,pred_color)
    lnes <- c(lnes,pred_lty)
    shapes <- c(shapes,NA)
    if(anyNA(data[[pred]])) {
      warning("[pmplots] removed missing values in pred column",call.=FALSE)
    }
  }
  if(length(ycols)==0) {
    stop("no y columns to plot",call.=FALSE)
  }
  x <- col_label(x)
  xl <- glue_unit(x[2],xunit)
  x <- x[1]
  yl <- dv[2]
  require_numeric(data,x)
  if(is.character(ylab)) yl <- ylab
  if(is.character(xlab)) xl <- xlab

  marg <- margin(margin,0,margin,0,unit = "pt")
  strip.text = element_text(size = font_size,margin=marg)
  fac <- as.formula(paste0("~",id_col))
  data <- data[,c(id_col,x,ycols),drop=FALSE]
  data <- pivot_longer(data,cols = ycols,values_to="value",names_to="name")

  p <-
    ggplot(data, aes(x=.data[[x]],y=.data[["value"]])) +
    ggplot2::scale_color_manual(name="", values = clrs) +
    ggplot2::scale_linetype_manual(name="", values=lnes) +
    ggplot2::scale_shape_manual(name="", values = shapes) +
    geom_point(aes(shape=.data[["name"]],col=.data[["name"]]),na.rm=TRUE,size=size) +
    geom_line(aes(lty=.data[["name"]],col=.data[["name"]]),lwd=lwd)

  if(dv_line) {
    dfline <- dplyr::filter(data,.data[["name"]]==dv)
    p <- p + geom_line(data = dfline,col = dv_color, lwd = dv_lwd)
  }

  p <-
    p +
    facet_wrap(fac,ncol = ncol, nrow = nrow,scales = scales) +
    scale_x_continuous(breaks = xbreaks) +
    ylab(yl) + xlab(xl)

  if(log_y) p <- p + scale_y_log10()

  if(is.numeric(angle)) p <- p + rot_x(angle)

  use_theme <-
    use_theme +
    theme(
      legend.position = legend.position,
      strip.text = strip.text
    )
  if(is.numeric(axis.text.rel)) {
    axtx <- element_text(size=ggplot2::rel(axis.text.rel))
    use_theme <- use_theme + theme(axis.text = axtx)
  }

  p + use_theme
}

#' @param options a named list of options to pass to [dv_pred_ipred]
#' @rdname dv_pred_ipred
#' @export
do_dv_pred_ipred <- function(data, options=list()) {
  options[["data"]] <- data
  do.call(dv_pred_ipred,options)
}
