

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
                               lwd = 1,
                               size = 2,
                               dv_line = FALSE,
                               scales = "fixed",
                               log_y = TRUE,
                               ncol = NULL,
                               nrow = NULL,
                               fun = NULL) {

  df[["ONES"]] <- 1
  df[["TWOS"]] <- 2
  col <- ggthemes::ptol_pal()(3)

  show_dv <- TRUE
  show_ipred <- TRUE
  show_pred <- TRUE

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
  }
  if(is.null(ipred)) {
    ipred <- col_label("IPRED")[1]
    show_ipred <- FALSE
  } else {
    ipred <- col_label(ipred)[1]
    require_numeric(df,ipred)
  }
  if(is.null(pred)) {
    pred <- col_label("PRED")[1]
    show_pred <- FALSE
  } else {
    pred <- col_label(pred)[1]
    require_numeric(df,pred)
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

  p <- ggplot(df, aes(x = !!sym(x)))

  col_labels <- shape_labels <- character(0)

  if(show_dv) {
    if(dv_line) p <- p + geom_line(data = df,aes(y=DV), lwd = lwd, col = "darkgrey")
    p <- p + geom_point(aes(y = DV,shape=factor(ONES)),size=size)
    shape_labels <- "DV"
  }
  if(show_pred) {
    p <- p + geom_line(aes(y = PRED,col = factor(ONES)),lty=pred_lty,lwd=lwd)
    col_labels <- "PRED"
  }
  if(show_ipred) {
    p <- p + geom_line(aes(y = IPRED,col = factor(TWOS)),lwd=lwd)
    col_labels <- c(col_labels,"IPRED")
  }

  p <-
    p +
    pm_theme() +
    theme(
      legend.position = legend.position,
      strip.text = element_text(
        size = font_size,
        margin = marg
      )
    ) +
    facet_wrap(fac,ncol = ncol, nrow = nrow,scales = scales) +
    scale_x_continuous(breaks = xbreaks) +
    ylab(yl) + xlab(xl) + rot_x(angle = angle)

  if(length(col_labels) > 0) {
    p <- p + ggthemes::scale_color_ptol(name="", labels = col_labels)
  }
  if(length(shape_labels) > 0) {
    p <- p + scale_shape_discrete(name = "", solid=FALSE, labels = shape_labels)
  }
  if(log_y) p <- p + scale_y_log10()
  p

}

#' @export
dv_pred_ipred <- function(data, ..., fun = NULL, id_per_plot = 9,id_col = "USUBJID") {
  ans <- chunk_by_id(data, nchunk=id_per_plot, id_col = id_col)
  out <- lapply(ans, dv_pred_ipred_impl, ...,  id_col = id_col)
  if(is.function(fun)) out <- lapply(out, fun)
  out
}
