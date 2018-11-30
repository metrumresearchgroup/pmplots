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

# .onLoad <- function(libname, pkgname) {
#   ggplot2::theme_set(ggplot2::theme_bw())
# }


##' Plots for pharmacometrics
##'
##'
##' @section Look and feel:
##'
##' See the \code{\link{pm_theme}} help topic for functions you
##' can use to replicate the look and feel of plots generated
##' by this package.
##'
##' @details
##'
##' Help topics:
##'
##' \itemize{
##'
##' \item \code{\link{res_time}} Plots of residuals versus time
##' \item \code{\link{dv_pred}} Plots of observed data versus predictions
##' \item \code{\link{dv_time}} Plots of observed values versus time
##' \item \code{\link{res_pred}} Plots of residuals versus predictions
##' \item \code{\link{res_q}} Q-Q plots for residuals
##' \item \code{\link{eta_hist}}, \code{\link{eta_pairs}}, \code{\link{eta_cont}},
##'        \code{\link{eta_cat}} Plots for ETA
##' \item \code{\link{split_plot}} split and plot
##' \item \code{\link{cont_cont}} Plots for continuous variables
##' \item \code{\link{layer}} Functions for adding layers to plots like
##'        reference or smoothing lines
##'
##' }
##'
##'
##'
##' @rdname pmplots
##' @name pmplots
NULL

