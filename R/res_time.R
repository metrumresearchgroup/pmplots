
##' Plot residuals or NPDE versus time
##'
##' @param df data set to plot
##' @param x character name of x-axis data
##' @param y character name of y-axis data
##' @param ... passed to \code{\link{y_time}} and
##' \code{\link{layer_hs}}
##' @param hline a list of arguments to pass to \code{geom_hline} specifying
##' aesthetics to use
##'
##' @seealso \code{\link{y_time}}, \code{\link{geom_3s}}
##'
##' @details
##' Functions are provided for plotting \code{RES}, \code{WRES},
##' and \code{CWRES} versus \code{TIME}, \code{TAFD}, and
##' \code{TAD}.  Plots are generated using \code{\link{y_time}},
##' which then calls \code{\link{scatt}}.
##'
##' By default, the time unit is assumed
##' to be hours (\code{hr}).  See the \code{xunit} argument
##' to \code{\link{y_time}} to change the time unit.
##'
##' See the \code{xby} argument to \code{\link{y_time}} for a
##' convenient way to change the breaks for the x-axis (time).
##'
##' Since this function creates a scatter plot,
##' both the \code{x} and \code{y} columns must
##' be numeric.
##'
##' @examples
##' df <- dplyr::filter(pmplots_data(), EVID==0)
##'
##' cwresi_time(df)
##'
##' cwresi_time(df, yname = "CWRES")
##'
##' cwresi_time(df, xunit="day")
##'
##' wres_time(df, xby=48)
##'
##' wres_time(df) + geom_3s()
##'
##' @export
res_time <- function(df, x = pm_axis_time(), y = pm_axis_res(), ...) {
  out <- y_time(df, x=x, y=y, ...)
  layer_hs(out,...)
}

##' @export
##' @rdname res_time
res_tafd <- function(df, x = pm_axis_tafd(), ...) {
  res_time(df, x=x, ...)
}

##' @export
##' @rdname res_time
res_tad <- function(df, x = pm_axis_tad(), ...) {
  res_time(df, x = x, ...)
}

##' @export
##' @rdname res_time
wres_time <- function(df, x = pm_axis_time(),
                      y = pm_axis_wres(), ...) {
  res_time(df, x = x,  y = y, ...)
}

##' @export
##' @rdname res_time
wres_tafd <- function(df, x = pm_axis_tafd(), ...) {
  wres_time(df, x = x, ...)
}

##' @export
##' @rdname res_time
wres_tad <- function(df, x = pm_axis_tad(), ...) {
  wres_time(df,  x = x, ...)
}

##' @export
##' @rdname res_time
cwres_time <- function(df,
                       x = pm_axis_time(),
                       y = pm_axis_cwres(),
                       ...) {
  res_time(df, x=x, y=y, ...)
}

##' @export
##' @rdname res_time
cwresi_time <- function(df, y = pm_axis_cwresi(), ...) {
  cwres_time(df, y = y, ... )
}

##' @export
##' @rdname res_time
cwres_tafd <- function(df, x = pm_axis_tafd(), ...) {
  cwres_time(df, x=x, ...)
}

##' @export
##' @rdname res_time
cwresi_tafd <- function(df, y = pm_axis_cwresi(), ...) {
  cwres_tafd(df, y = y, ... )
}

##' @export
##' @rdname res_time
cwres_tad <- function(df, x = pm_axis_tad(), ...) {
  cwres_time(df, x=x, ...)
}

##' @export
##' @rdname res_time
cwresi_tad <- function(df, y = pm_axis_tad(), ...) {
  cwres_tad(df, y = y, ... )
}

##' @export
##' @rdname res_time
npde_time <- function(df, y  = pm_axis_npde(), ..., hline = npde_ref()) {
  res_time(df, y = y, hline = hline, ...)
}

##' @export
##' @rdname res_time
npde_tad <- function(df, x = pm_axis_tad(), ...) {
  npde_time(df, x = x,...)
}

##' @export
##' @rdname res_time
npde_tafd <- function(df, x = pm_axis_tafd(), ...) {
  npde_time(df, x = x, ...)
}
