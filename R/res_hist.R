##' Histograms of residuals or NPDE
##'
##' @param df data frame to plot
##' @param ... passed to \code{\link{cont_hist}}
##' @param x character name for x-axis data
##' @param xs see \code{\link{defx}}
##' @param y what to use for the y-axis on the histogram; can be
##' \code{"..count.."} or \code{"..density.."}
##'
##' @examples
##' df <- pmplots_data_obs()
##'
##' cwres_hist(df)
##'
##' @return A single plot.
##'
##' @export
res_hist <- function(df, ...,
                     x = pm_axis_res(),
                     xs = defx()) {
  df <- pmplots_nlmixr2_data(df)
  cont_hist(df, x, xs, ...)
}

##' @export
##' @rdname res_hist
wres_hist <- function(df, ...,
                      x = pm_axis_wres(),
                      y = "..density..") {
  df <- pmplots_nlmixr2_data(df)
  res_hist(df, x = x, y = y, ...)
}

##' @export
##' @rdname res_hist
cwres_hist <- function(df, ..., x = pm_axis_cwres()) {
  df <- pmplots_nlmixr2_data(df)
  if(no_cwres(df)) df <- supplement_cwres(df)
  wres_hist(df, x = x, ...)
}

##' @export
##' @rdname res_hist
cwresi_hist <- function(df, x = pm_axis_cwresi(), ...) {
  df <- pmplots_nlmixr2_data(df)
  cwres_hist(df, x = x, ...)
}

##' @export
##' @rdname res_hist
npde_hist <- function(df, ..., x = pm_axis("npde")) {
  df <- pmplots_nlmixr2_data(df)
  wres_hist(df, x = x, ...)
}

