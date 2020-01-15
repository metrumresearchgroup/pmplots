

#' @export
dv_pred_ipred_impl <- function(df,
                          x = "TIME",
                          ylab = "Concentration",
                          xlab = "Time after first dose (hr)",
                          dv  = "DV",
                          pred = "PRED",
                          ipred = "IPRED",
                          id_col = "USUBJID",
                          xbreaks = waiver(),
                          angle = 0,
                          font_size = 6,
                          margin = 2,
                          legend.position = "top",
                          ncol = NULL,
                          nrow = NULL,
                          fun = NULL
                          ) {

  df[["ONES"]] <- 1
  df[["TWOS"]] <- 2

  col <- ggthemes::ptol_pal()(3)

  xl <- xlab
  yl <- ylab
  marg <- margin(margin,0,margin,0,unit = "pt")
  fac <- as.formula(paste0("~",id_col))

  p <- ggplot(df, aes(x = !!sym(x))) +
    geom_point(aes(y = DV,shape=factor(ONES)),size=2) +
    geom_line(aes(y = PRED,col = factor(ONES)),lty=2,lwd=1) +
    geom_line(aes(y = IPRED,col = factor(TWOS)),lwd=1) +
    pm_theme() +
    theme(
      legend.position = legend.position,
      strip.text = element_text(
        size = font_size,
        margin = marg
      )
    ) +
    facet_wrap(fac,ncol = ncol, nrow = nrow) +
    scale_y_log10() +
    scale_x_continuous(breaks = xbreaks) +
    ylab(yl) + xlab(xl) +
    ggthemes::scale_color_ptol(name="", labels = c("PRED", "IPRED")) +
    scale_shape_discrete(name = "", solid=FALSE, labels = "DV") +
    rot_x(angle = angle)
   p

}

#' @export
dv_pred_ipred <- function(data, ..., fun = NULL, id_per_plot = 9,id_col = "USUBJID") {
  ans <- chunk_by_id(data, nchunk=id_per_plot, id_col = id_col)
  out <- lapply(ans, dv_pred_ipred_impl, ...,  id_col = id_col)
  if(is.function(fun)) out <- lapply(out, fun)
  out
}
