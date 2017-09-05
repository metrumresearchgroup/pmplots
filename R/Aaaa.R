##' @importFrom dplyr filter as_data_frame arrange n_distinct group_by filter ungroup summarize
##' @importFrom grDevices pdf dev.off
##' @importFrom ggplot2 ggplot aes_string geom_boxplot geom_line
##' @importFrom ggplot2 ggtitle theme geom_point geom_smooth
##' @importFrom ggplot2 geom_abline geom_hline margin
##' @importFrom ggplot2 stat_qq facet_wrap geom_histogram scale_color_brewer
##' @importFrom stats as.formula qnorm quantile
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

