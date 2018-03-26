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
##' @seealso \code{\link{boxwork}}
##'
##' @examples
##'
##' df <- dplyr::filter(pmplots_data(), EVID==0)
##'
##' cwres_cat(df, x="STUDYc//Study name")
##'
##' @export
res_cat <- function(df, x, y="RES//Residual",
                    hline=0, ...) {
  out <- list_plot_xy(df, x, y, .fun = cont_cat, hline = hline, ...)
  if(length(out)==1) return(out[[1]])
  return(out)
}

##' @export
##' @rdname res_cat
wres_cat <- function(df, x, y="WRES//Weighted residual",
                     hline=0, ...) {
  res_cat(df, x, y, hline, ...)
}

##' @export
##' @rdname res_cat
cwres_cat <- function(df, x, y="CWRES//Conditional weighted residual",
                      hline=0, ...) {
  res_cat(df, x, y, hline, ...)
}

##' @export
##' @rdname res_cat
cwresi_cat <- function(df, x, y="CWRESI//Conditional weighted residual", ...) {
  cwres_cat(df, x, y, ...)
}

