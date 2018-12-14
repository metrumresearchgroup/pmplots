##' @importFrom dplyr filter as_data_frame arrange n_distinct
##' @importFrom dplyr group_by filter ungroup summarize distinct
##' @importFrom dplyr groups is_grouped_df
##' @importFrom grDevices pdf dev.off
##' @importFrom ggplot2 ggplot aes_string geom_boxplot geom_line
##' @importFrom ggplot2 ggtitle theme geom_point geom_smooth
##' @importFrom ggplot2 geom_abline geom_hline margin
##' @importFrom ggplot2 stat_qq facet_wrap geom_histogram
##' @importFrom ggplot2 scale_color_brewer theme_bw theme_set
##' @importFrom ggplot2 element_text labs aes
##' @importFrom stats as.formula qnorm quantile cor dnorm
##' @importFrom rlang sym quo_text quos set_names quo_name as_list
##' @importFrom glue glue
##' @importFrom assertthat assert_that
##'
NULL


.ggblue <- "#3366FF"

globalVariables(c("ID", "n", "IPRED"))


##' Plots for pharmacometrics
##'
##'
##' @section Look and feel:
##'
##' See the [pmtheme] help topic for functions you
##' can use to replicate the look and feel of plots generated
##' by this package.
##'
##' @section Function listing:
##'
##' - Residuals versus time
##'   - [res_time], [res_tad], [res_tafd]
##'   - [wres_time], [wres_tad], [wres_tad]
##'   - [cwres_time], [cwres_tad], [cwres_tafd]
##' - Residuals versus PRED
##'   - [res_pred], [wres_pred], [cwres_pred]
##' - Residuals versus covariates
##'   - [res_cont], [wres_cont], [cwres_cont]
##'   - [res_cat], [wres_cat], [cwres_cat]
##' - NPDE plots
##'   - [npde_time], [npde_tad], [npde_tafd]
##'   - [npde_pred]
##'   - [npde_hist]
##'   - [npde_q]
##'   - [npde_cont]
##'   - [npde_cat]
##'
##' @rdname pmplots
##' @name pmplots
##' @md
NULL

