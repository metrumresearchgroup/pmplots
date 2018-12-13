##' Plot residuals versus categorical variable
##'
##' These are vectorized functions.  See details.
##'
##' @param df data frame to plot
##' @param x character col//title for x-axis data; see \code{\link{col_label}}
##' @param y character col//title for y-axis data; see \code{\link{col_label}}
##' @param hline where to draw horizontal refrence line
##' @param ... passed to \code{\link{cont_cat}}
##'
##' @details
##' These are vectorized functions; if either x or y
##' are a character vector (in col//title format), a list
##' of plots is returned.  If both x and y are length 1,
##' then a single plot object (not a list) is returned.
##'
##' Since this function creates a boxplot,
##' the \code{x} column must be character, factor
##' or logical and \code{y} column must
##' be numeric.
##'
##' Summary numbers located below each box are described in
##' \code{\link{boxwork}}.
##'
##' @seealso \code{\link{boxwork}}, \code{\link{cont_cat}}
##'
##' @examples
##'
##' df <- pmplots_data_obs()
##'
##' cwresi_cat(df, x="STUDYc//Study name")
##'
##' @return A single plot when a single value for \code{x}
##' and \code{y} are supplied; a list of plots of either \code{x}
##' or \code{y} have length greater than 1.
##'
##' @export
res_cat <- function(df, x, y=pm_axis_res(), hline=0, ...) {
  out <- list_plot_xy(df, x, y, .fun = cont_cat, hline = hline, ...)
  if(length(out)==1) return(out[[1]])
  return(out)
}

##' @export
##' @rdname res_cat
wres_cat <- function(df, x, y = pm_axis_wres(), hline=0, ...) {
  res_cat(df, x, y, hline, ...)
}

##' @export
##' @rdname res_cat
cwres_cat <- function(df, x, y = pm_axis_cwres(), hline=0, ...) {
  if(no_cwres(df)) df <- supplement_cwres(df)
  res_cat(df, x, y, hline, ...)
}

##' @export
##' @rdname res_cat
cwresi_cat <- function(df, x, y = pm_axis_cwresi(), ...) {
  cwres_cat(df, x, y, ...)
}

