##' Histograms of residuals or NPDE
##'
##' @param df data frame to plot
##' @param ... passed to \code{\link{cont_hist}}
##' @param x character name for x-axis data
##' @param xname used to form x-axis label
##' @param xs see \code{\link{defx}}
##'
##' @export
res_hist <- function(df, ...,
                     x = "RES",
                     xname = "Residual",
                     xs = defx()) {
  x <- paste0(x, "//", xname)
  cont_hist(df, x, xs, ...)
}

##' @export
##' @rdname res_hist
wres_hist <- function(df, ...,
                      x = "WRES",
                      xname = "Weighted residual") {
  res_hist(df, x = x, xname = xname, ...)
}

##' @export
##' @rdname res_hist
cwres_hist <- function(df, ...,
                       x = "CWRES",
                       xname = "Conditional weighted residual") {
  res_hist(df, x = x, xname = xname, ...)
}

##' @export
##' @rdname res_hist
cwresi_hist <- function(df, x = "CWRESI", ...) {
  cwres_hist(df, x = x, ...)
}

##' @export
##' @rdname res_hist
npde_hist <- function(df, ...,
                      x = "NPDE",
                      xname = "Normalized prediction distribution error") {
  res_hist(df, x = x, xname = xname, ...)
}

