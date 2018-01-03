##' Residuals versus predicted values
##'
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param y character name for y-axis data
##' @param xname used to form x-axis label
##' @param yname used to form y-axis label
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##'
##' @param ... passed to \code{\link{scatt}} and \code{\link{layer_hs}}
##'
##' @details
##' Since this function creates a scatter plot,
##' both the \code{x} and \code{y} columns must
##' be numeric.
##'
##' The y axis name is always the name of the residual
##' (e.g. "Weighted residual").  Use the \code{xname} argument
##' to add specific name and or unit to the dependent variable
##' (see the example).
##'
##' @seealso \code{\link{geom_3s}}
##'
##' @examples
##'
##' df <- dplyr::filter(superset2(), EVID==0)
##'
##' cwres_pred(df, xname="MyDrug (ng/mL)")
##'
##'
##' @export
res_pred <- function(df, x="PRED", y="RES",
                     xname = "value",
                     yname = "Residual",
                     xs=defx(), ys=defy(),
                     ...) {
  require_numeric(df,x)
  require_numeric(df,y)
  xs$name <- paste0("Population predicted ", xname)
  ys$name <- yname
  out <- scatt(df, x, y, xs, ys, ...)
  layer_hs(out,...)
}

##' @export
##' @rdname res_pred
wres_pred <- function(df, ...,
                      y="WRES",
                      yname = "Weighted residual") {
  res_pred(df, y = y, yname = yname, ...)
}

##' @export
##' @rdname res_pred
cwres_pred <- function(df, ...,
                       y = "CWRES",
                       yname = "Conditional weighted residual") {
  res_pred(df, y = y, yname = yname, ...)
}


