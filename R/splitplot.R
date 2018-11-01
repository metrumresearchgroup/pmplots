
##' Split and plot
##'
##' @param df data frame to split and plot
##' @param fun function to use to plot
##' @param sp character name of column to split; if \code{df} is a
##' grouped data frame, leave this argument missing and the; regardless
##' of how this is specified, \code{sp} should name a single column
##' split column will be determined from the groups
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
##' @export
split_plot <- function(df, fun, sp = get_split_col(df), ...) {
  require_column(df,sp)
  l <- split(df, df[[sp]], drop = TRUE)
  form <- as.formula(paste0("~",sp))
  out <- lapply(l, function(this) {
    fun(this, ...) + facet_wrap(form)
  })
  out
}


get_split_col <- function(df) {
  if(!is_grouped_df(df)) {
    stop("Please pass in a grouped data frame or specify the sp argument.")
  }
  gr <- as.character(groups(df))
  if(length(gr) != 1) {
    stop("Please specify exactly one grouping variable")
  }
  return(gr)
}
