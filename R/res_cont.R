
##' Plot residuals versus continuous variables
##'
##' These are vectorized functions; see details;
##'
##' @param df data frame to plot
##' @param x character col//title for x-axis data; see
##' \code{\link{col_label}}
##' @param y character col//title for y-axis data; see
##' \code{\link{col_label}}
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param ... passed to \code{\link{scatt}}  and
##' \code{\link{layer_hs}}
##'
##' @details
##' These are vectorized functions; if either x or y
##' are a character vector (in col//title format), a list
##' of plots is returned.  If both x and y are length 1,
##' then a single plot object (not a list) is returned.
##'
##' @examples
##' data <- pmplots_data_obs()
##'
##' wres_cont(data, x = "CRCL//Creatinine clearance")
##'
##' @return A single plot when a single value for \code{x}
##' and \code{y} are supplied; a list of plots of either \code{x}
##' or \code{y} have length greater than 1.
##'
##' @export
res_cont <- function(df, x, y=pm_axis_res(),
                     xs=defx(), ys=defy(), ...) {
  out <- list_plot_xy(df, x, y, xs = xs, ys = ys, ...)
  out <- lapply(out, layer_hs, ...)
  if(length(out)==1) return(out[[1]])
  return(out)
}

##' @export
##' @rdname res_cont
wres_cont <- function(df, x, y=pm_axis_wres(),
                      xs=defx(), ys=defy(),...) {
  res_cont(df, x = x, y = y, xs = xs, ys = ys, ...)
}

##' @export
##' @rdname res_cont
cwres_cont <- function(df, x, y=pm_axis_cwres(), xs=defx(), ys=defy(),...) {
  if(no_cwres(df)) df <- supplement_cwres(df)
  res_cont(df, x = x, y = y, xs = xs, ys = ys, ...)
}

##' @export
##' @rdname res_cont
cwresi_cont <- function(df, x, y=pm_axis_cwresi(), ...) {
  cwres_cont(df, x = x, y = y, ...)
}
