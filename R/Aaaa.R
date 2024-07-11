#' @importFrom dplyr filter as_data_frame arrange n_distinct n bind_rows
#' @importFrom dplyr group_by filter ungroup summarize distinct pull
#' @importFrom dplyr groups is_grouped_df mutate .data rename %>%
#' @importFrom dplyr as_tibble
#' @importFrom tidyselect all_of
#' @importFrom grDevices pdf dev.off
#' @importFrom ggplot2 ggplot geom_boxplot geom_line
#' @importFrom ggplot2 ggtitle theme geom_point geom_smooth
#' @importFrom ggplot2 geom_abline geom_hline geom_text margin
#' @importFrom ggplot2 stat_qq facet_wrap geom_histogram
#' @importFrom ggplot2 scale_color_brewer theme_bw theme_set
#' @importFrom ggplot2 element_text labs aes waiver rel
#' @importFrom ggplot2 scale_y_continuous scale_x_continuous scale_y_log10
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 geom_text position_jitter label_value
#' @importFrom ggplot2 scale_color_manual scale_linetype_manual
#' @importFrom ggplot2 scale_shape_manual scale_shape_discrete
#' @importFrom ggplot2 after_stat is.ggplot
#' @importFrom stats as.formula qnorm quantile cor dnorm
#' @importFrom rlang sym quo_text quos set_names quo_name as_list is_named
#' @importFrom rlang enexpr abort warn
#' @importFrom purrr list_transpose
#' @importFrom glue glue glue_data
#' @importFrom assertthat assert_that
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_inorder
#' @importFrom utils str
#' @importFrom lifecycle deprecate_warn is_present deprecated
#' @importFrom stats setNames
#'
#' @include Aaaa.R
NULL


.ggblue <- "#3366FF"

globalVariables(c("ID", "n", "IPRED"))


#' Plots for pharmacometrics
#'
#'
#' @section Look and feel:
#'
#' See the [pm_theme()] help topic for functions you
#' can use to replicate the look and feel of plots generated
#' by this package.
#'
#' @section Function listing:
#'
#' - Basic plots
#'   - [pm_scatter()], [pm_box()], [pairs_plot()], [split_plot()]
#' - Residuals versus time
#'   - [res_time()], [res_tad()], [res_tafd()]
#'   - [wres_time()], [wres_tad()], [wres_tad()]
#'   - [cwres_time()], [cwres_tad()], [cwres_tafd()]
#'   - [cwresi_time()], [cwresi_tad()], [cwresi_tafd()]
#' - Residuals versus PRED
#'   - [res_pred()], [wres_pred()], [cwres_pred()]
#' - Residuals versus covariates
#'   - [res_cont()], [wres_cont()], [cwres_cont()]
#'   - [res_cat()], [wres_cat()], [cwres_cat()]
#' - NPDE plots
#'   - [npde_time()], [npde_tad()], [npde_tafd()]
#'   - [npde_pred()]
#'   - [npde_hist()]
#'   - [npde_q()]
#'   - [npde_cont()]
#'   - [npde_cat()]
#' - Pairs plots
#'   - [eta_pairs()], [pairs_plot()]
#' - QQ plots
#'   - [wres_q()], [cwres_q()], [cwresi_q()], [res_q()]
#' - Histograms
#'   - [cont_hist()], [eta_hist()], [cwres_hist()], [cwresi_hist()]
#' - List plots
#'   - [pm_box_list()], [pm_scatter_list()]
#'   - [list_plot_x()], [list_plot_y()], [list_plot_xy()]
#'   - [cont_hist_list()]
#' - Faceted plots
#'   - [wrap_cont_cont()], [wrap_cont_time()], [wrap_dv_preds()], [wrap_eta_cont()]
#'   - [wrap_hist()]
#'   - [wrap_cont_cat()]
#' - Standard paneled displays
#'   - [eta_covariate()], [npde_covariate()], [cwres_covariate()]
#'   - [npde_panel()], [cwres_panel()]
#'   - [npde_hist_q()], [cwres_hist_q()]
#'   - [npde_scatter()], [cwres_scatter()]
#' - Other plots
#'   - [dv_pred_ipred()]
#' - Layers
#'   - smooth: [layer_s()], hline: [layer_h()], abline: [layer_a()]
#'   - combinations: [layer_sa()], [layer_sh()]
#' - Customization
#'   - [pm_opts()]
#'   - [rot_x()], [rot_y()], [pm_theme()], [theme_plain()]
#' - Arrangement
#'   - [pm_grid]
#' - Example data sets
#'   - [pmplots_data()], [pmplots_data_obs()], [pmplots_data_id()]
#'
#'
#' @rdname pmplots
#' @name pmplots
#' @md
NULL

