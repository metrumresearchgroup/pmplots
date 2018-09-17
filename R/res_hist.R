##' Histograms of residuals or NPDE
##'
##' @param df data frame to plot
##' @param ... passed to \code{\link{cont_hist}}
##' @param x character name for x-axis data
##' @param xs see \code{\link{defx}}
##' @param y what to use for the y-axis on the histogram; can be
##' \code{"..count.."} or \code{"..density.."}
##'
##' @export
res_hist <- function(df, ...,
                     x = pm_axis_res(),
                     xs = defx()) {
  cont_hist(df, x, xs, ...)
}

##' @export
##' @rdname res_hist
wres_hist <- function(df, ...,
                      x = pm_axis_wres(),
                      y = "..density..") {
  res_hist(df, x = x, y = y, ...)
}

##' @export
##' @rdname res_hist
cwres_hist <- function(df, ..., x = pm_axis_cwres()) {
  wres_hist(df, x = x, ...)
}

##' @export
##' @rdname res_hist
cwresi_hist <- function(df, x = pm_axis_cwresi(), ...) {
  cwres_hist(df, x = x, ...)
}

##' @export
##' @rdname res_hist
npde_hist <- function(df, ..., x = pm_axis("npde")) {
  wres_hist(df, x = x, ...)
}

