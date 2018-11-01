
##' Split and plot
##'
##' @param df data frame to split and plot
##' @param sp character name of column to split
##' @param fun function to use to plot
##' @param ... passed to \code{fun}
##'
##' @examples
##'
##' df <- pmplots_data_obs()
##'
##' split_plot(df, sp="STUDYc", fun=dv_pred)
##'
##' @export
split_plot <- function(df, sp = get_split_col(df), fun,...) {
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
