##' Histograms of residuals or NPDE
##'
##' @param df data frame to plot.
##' @param ... passed to [cont_hist()]
##' @param x character name for x-axis data.
##' @param xs see [defx()].
##' @param y what to use for the y-axis on the histogram; can be
##' "count"` or `"density"`.
##'
##' @examples
##' df <- pmplots_data_obs()
##'
##' cwres_hist(df)
##'
##' @return A single plot.
##'
##' @md
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
                      y = "density") {
  res_hist(df, x = x, y = y, ...)
}

##' @export
##' @rdname res_hist
cwres_hist <- function(df, ..., x = pm_axis_cwres()) {
  if(no_cwres(df)) df <- supplement_cwres(df)
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

