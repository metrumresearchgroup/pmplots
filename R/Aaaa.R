##' @importFrom dplyr filter as_data_frame arrange n_distinct group_by filter ungroup summarize
##' @importFrom grDevices pdf dev.off
##' @importFrom ggplot2 ggplot aes_string geom_boxplot geom_line
##' @importFrom ggplot2 ggtitle theme geom_point geom_smooth
##' @importFrom ggplot2 geom_abline geom_hline margin
##' @importFrom ggplot2 stat_qq facet_wrap geom_histogram
##' @importFrom ggplot2 scale_color_brewer theme_bw theme_set
##' @importFrom stats as.formula qnorm quantile cor
##' @importFrom rlang sym
##'
NULL


##' Plots for pharmacometrics.
##'
##'
##' @name pmplots
##' @rdname pmplots
NULL

.ggblue <- "#3366FF"

globalVariables(c("ID", "n"))

.onLoad <- function(libname, pkgname) {
  ggplot2::theme_set(ggplot2::theme_bw())
}

