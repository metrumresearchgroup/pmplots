

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
                               scales = "fixed",
                               log_y = TRUE,
                               ncol = NULL,
                               nrow = NULL,
                               fun = NULL
) {

  df[["ONES"]] <- 1
  df[["TWOS"]] <- 2


  col <- ggthemes::ptol_pal()(3)
  dv <- col_label(dv)
  x <- col_label(x)
  ipred <- col_label(ipred)[1]
  pred <- col_label(pred)[1]
  xl <- glue_unit(x[2],xunit)
  x <- x[1]
  yl <- dv[2]
  if(is.character(ylab)) yl <- ylab
  if(is.character(xlab)) xl <- xlab
  dv <- dv[1]

  require_numeric(df,x)
  require_numeric(df,dv)
  require_numeric(df,pred)
  require_numeric(df,ipred)

  marg <- margin(margin,0,margin,0,unit = "pt")
  fac <- as.formula(paste0("~",id_col))

  p <- ggplot(df, aes(x = !!sym(x))) +
    geom_point(aes(y = DV,shape=factor(ONES)),size=size) +
    geom_line(aes(y = IPRED,col = factor(TWOS)),lwd=lwd) +
    geom_line(aes(y = PRED,col = factor(ONES)),lty=pred_lty,lwd=lwd) +
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
    ylab(yl) + xlab(xl) +
    ggthemes::scale_color_ptol(name="", labels = c("PRED", "IPRED")) +
    scale_shape_discrete(name = "", solid=FALSE, labels = "DV") +
    rot_x(angle = angle)
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
