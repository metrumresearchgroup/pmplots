
##' Plot residuals versus time
##'
##' @param df data set to plot
##' @param x character name of x-axis data
##' @param y character name of y-axis data
##' @param xname used for x-axis label
##' @param yname used for y-axis label
##' @param ... passed to \code{\link{y_time}} and \code{\link{layer_hs}}
##'
##' @seealso \code{\link{y_time}}, \code{\link{geom_3s}}
##'
##' @details
##' Functions are provided for plotting \code{RES}, \code{WRES},
##' and \code{CWRES} versus \code{TIME}, \code{TAFD}, and
##' \code{TAD}.  The \code{_tafd} and \code{_tad} functions
##' call the \code{_time} function.
##'
##' By default, the time unit is assumed
##' to be hours (\code{hr}).  See the \code{xunit} argument
##' to \code{\link{y_time}} to change the time unit.
##'
##' For all of these functions, \code{yname} is the
##' full specification of the y-axis title.
##'
##' See the \code{xby} argument to \code{\link{y_time}} for a
##' convenient way to change the breaks for the x-axis (time).
##'
##' Since this function creates a scatter plot,
##' both the \code{x} and \code{y} columns must
##' be numeric.
##'
##' @examples
##' df <- dplyr::filter(superset2(), EVID==0)
##'
##' cwres_time(df)
##'
##' cwres_time(df, yname = "CWRES")
##'
##' cwres_time(df, xunit="day")
##'
##' wres_time(df, xby=48)
##'
##' wres_time(df) + geom_3s()
##'
##' @export
res_time <- function(df,
                     x = "TIME",
                     y = "RES",
                     xname = "Time",
                     yname = "Residual",
                     ...) {
  out <- y_time(df, yname=yname, xname = xname, x=x, y=y, ...)
  layer_hs(out,...)
}

##' @export
##' @rdname res_time
res_tafd <- function(df,
                     x = "TAFD", ...,
                     xname = "Time after first dose") {
  res_time(df, x=x, xname = xname, ...)
}

##' @export
##' @rdname res_time
res_tad <- function(df,
                    x = "TAD", ...,
                    xname="Time after dose") {
  res_time(df, x = x, xname = xname, ...)
}

##' @export
##' @rdname res_time
wres_time <- function(df,
                      x = "TIME",
                      y = "WRES",
                      yname="Weighted residual",
                      xname="Time",
                      ...) {
  out <- y_time(df,x = x, y = y, xname = xname, yname=yname, ...)
  layer_hs(out,...)
}

##' @export
##' @rdname res_time
wres_tafd <- function(df,
                      x = "TAFD", ...,
                      xname = "Time after first dose") {
  wres_time(df, x = x, xname = xname, ...)
}

##' @export
##' @rdname res_time
wres_tad <- function(df,
                     x = "TAD", ...,
                     xname="Time after dose") {
  wres_time(df,  x=x, xname = xname, ...)
}


##' @export
##' @rdname res_time
cwres_time <- function(df,
                       x = "TIME",
                       y = "CWRES",
                       yname = "Conditional weighted residual",
                       xname = "Time",
                       ...) {
  out <- y_time(df, x=x, y = y, xname = xname, yname = yname, ...)
  layer_hs(out,...)
}

##' @export
##' @rdname res_time
cwres_tafd <- function(df,
                       x = "TAFD", ...,
                       xname="Time after first dose") {
  cwres_time(df, x=x, xname = xname, ...)
}

##' @export
##' @rdname res_time
cwres_tad <- function(df,
                      x = "TAD", ...,
                      xname="Time after dose") {
  cwres_time(df, x=x, xname=xname, ...)
}
