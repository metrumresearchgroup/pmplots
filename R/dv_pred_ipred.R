

#' @export
dv_pred_ipred_impl <- function(df,
                               x = pm_axis_time(),
                               dv  = pm_axis_dv(),
                               pred = pm_axis_pred(),
                               ipred = pm_axis_ipred(),
                               id_col = "USUBJID",
                               xbreaks = waiver(),
                               xunit = "hr",
                               xlab  = NULL,
                               ylab  = NULL,
                               angle = 0,
                               font_size = 6,
                               margin = 2,
                               legend.position = "top",
                               pred_lty = 2,
                               ipred_lty = 1,
                               lwd = 0.7,
                               size = 2,
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
                               fun = NULL) {

  show_dv <- TRUE
  show_ipred <- TRUE
  show_pred <- TRUE
  ycols <- character(0)
  clrs <- character(0)
  lnes <- integer(0)
  shapes <- integer(0)
  if(is.null(dv)) {
    dv <- col_label("DV")
    yl <- dv[2]
    dv <- dv[1]
    show_dv <- FALSE
  } else {
    dv <- col_label(dv)
    yl <- dv[2]
    dv <- dv[1]
    require_numeric(df,dv)
    ycols <- c(ycols,dv)
    clrs <- c(clrs, dv_color)
    shapes <- c(shapes,dv_shape)
    lnes <- c(lnes,0)
    if(anyNA(df[[dv]])) {
      warning("[pmplots] removed missing values in dv column",call.=FALSE)
    }
  }
  if(is.null(ipred)) {
    ipred <- col_label("IPRED")[1]
    show_ipred <- FALSE
  } else {
    ipred <- col_label(ipred)[1]
    require_numeric(df,ipred)
    ycols <- c(ycols, ipred)
    clrs <- c(clrs,ipred_color)
    lnes <- c(lnes,ipred_lty)
    shapes <- c(shapes,NA)
    if(anyNA(df[[ipred]])) {
      warning("[pmplots] removed missing values in ipred column",call.=FALSE)
    }
  }
  if(is.null(pred)) {
    pred <- col_label("PRED")[1]
    show_pred <- FALSE
  } else {
    pred <- col_label(pred)[1]
    require_numeric(df,pred)
    ycols <- c(ycols,pred)
    clrs <- c(clrs,pred_color)
    lnes <- c(lnes,pred_lty)
    shapes <- c(shapes,NA)
    if(anyNA(df[[pred]])) {
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
  require_numeric(df,x)
  if(is.character(ylab)) yl <- ylab
  if(is.character(xlab)) xl <- xlab

  marg <- margin(margin,0,margin,0,unit = "pt")
  fac <- as.formula(paste0("~",id_col))
  df <- df[,c(id_col,x,ycols),drop=FALSE]
  df <- pivot_longer(df,cols = ycols,values_to="value",names_to="name")

  p <-
    ggplot(df, aes(x=.data[[x]],y=.data[["value"]])) +
    ggplot2::scale_color_manual(name="", values = clrs) +
    ggplot2::scale_linetype_manual(name="", values=lnes) +
    ggplot2::scale_shape_manual(name="", values = shapes) +
    geom_point(aes(shape=.data[["name"]],col=.data[["name"]]),na.rm=TRUE,size=size) +
    geom_line(aes(lty=.data[["name"]],col=.data[["name"]]),lwd=lwd)

  if(dv_line) {
    dfline <- dplyr::filter(df,.data[["name"]]==dv)
    p <- p + geom_line(data = dfline,col = dv_color, lwd = dv_lwd)
  }

  p <-
    p +
    facet_wrap(fac,ncol = ncol, nrow = nrow,scales = scales) +
    scale_x_continuous(breaks = xbreaks) +
    ylab(yl) + xlab(xl) + rot_x(angle = angle)

  if(log_y) p <- p + scale_y_log10()
  p <- p + use_theme
  if(is.function(fun)) p <- fun(p)
  p <-
    p +
    theme(
      legend.position = legend.position,
      strip.text = element_text(
        size = font_size,
        margin = marg
      )
    )
  p
}

#' @export
dv_pred_ipred <- function(data, ..., fun = NULL, id_per_plot = 9,id_col = "USUBJID") {
  ans <- chunk_by_id(data, nchunk=id_per_plot, id_col = id_col)
  out <- lapply(ans, dv_pred_ipred_impl, ...,  id_col = id_col)
  if(is.function(fun)) out <- lapply(out, fun)
  out
}

#' @export
do_dv_pred_ipred <- function(data, options) {
  do.call(dv_pred_ipred,c(list(data = data, options)))
}
