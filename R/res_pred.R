##' Residuals or NPDE versus predicted values
##'
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param y character name for y-axis data
##' @param xname glued into x-axis title
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param hline a list of parameters to pass to \code{geom_hline} specifying
##' where to locate a horizontal reference line aesthetics to use
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
##' A loess smooth and a horizontal reference line are
##' layered on the plot.
##'
##' @seealso \code{\link{geom_3s}}
##'
##' @examples
##'
##' df <- pmplots_data_obs()
##'
##' cwresi_pred(df, xname="MyDrug (ng/mL)")
##'
##'
##' @export
res_pred <- function(df,
                     x=pm_axis_pred(),
                     y=pm_axis_res(),
                     xname = "value",
                     xs=defx(), ys=defy(),
                     ...) {

  x <- glue::glue(x)

  x <- col_label(x)
  y <- col_label(y)

  require_numeric(df,x[1])
  require_numeric(df,y[1])

  xs$name <- x[2]
  ys$name <- y[2]

  x <- x[1]
  y <- y[1]

  out <- scatt(df, x, y, xs, ys, ...)

  layer_hs(out,...)
}

##' @export
##' @rdname res_pred
wres_pred <- function(df, ..., y=pm_axis_wres()) {
  res_pred(df, y = y, ...)
}

##' @export
##' @rdname res_pred
cwres_pred <- function(df, ..., y=pm_axis_cwres()) {
  res_pred(df, y = y, ...)
}

##' @export
##' @rdname res_pred
cwresi_pred <- function(df, y=pm_axis_cwresi(), ...) {
  cwres_pred(df, y = y, ...)
}

##' @export
##' @rdname res_pred
npde_pred <- function(df, ..., y = pm_axis_npde(), hline = npde_ref()) {
  res_pred(df, y = y, hline = hline, ...)
}


