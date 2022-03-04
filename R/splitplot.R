
##' Split and plot
##'
##' Split a data frame on a single factor and create a plot from each chunk.
##' Each plot is labeled with the value of `sp` using [ggplot2::facet_wrap()].
##' See examples.
##'
##' @param df Data frame to split and plot.
##' @param fun Function to use to plot.
##' @param sp Character name of column to split; if \code{df} is a
##' grouped data frame, leave this argument missing and the; regardless
##' of how this is specified, \code{sp} should name a single column
##' split column will be determined from the groups.
##' @param labeller A labeller to pass to [ggplot2::facet_wrap()]; by default,
##' the value is printed on the strip with [ggplot2::label_value]; pass
##' [label_tex] to parse TeX expressions if the latex2exp package is available.
##' @param ... passed to \code{fun}
##'
##' @examples
##'
##' df <- pmplots_data_obs()
##'
##' require(dplyr)
##'
##' df %>% group_by(STUDYc) %>% split_plot(dv_pred)
##'
##' @return
##' A list of plots, one for each grouping in `sp`.
##'
##' @export
split_plot <- function(df, fun, sp = get_split_col(df), labeller = label_value,
                       ...) {
  if(length(sp) != 1) {
    stop("Only one grouping or split variable is allowed.")
  }
  require_column(df,sp)
  l <- split(df, df[[sp]], drop = TRUE)
  form <- as.formula(paste0("~",sp))
  out <- lapply(l, function(this) {
    fun(this, ...) + facet_wrap(form, labeller = labeller)
  })
  out
}

get_split_col <- function(df) {
  if(!is_grouped_df(df)) {
    stop("either pass a grouped data frame or specify the sp argument.")
  }
  gr <- as.character(groups(df))
  return(gr)
}
