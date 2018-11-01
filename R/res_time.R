
##' Plot residuals versus time
##'
##' These plots have \code{RES} on the y axis and
##' some time-related column on the x-axis, including \code{TIME},
##' \code{TAD}, or \code{TAFD}.
##'
##' @param df data set to plot
##' @param x x-axis data in \code{\link{col_label}} format
##' @param y y-axis data in \code{\link{col_label}} format
##' @param ... passed to \code{\link{y_time}} and
##' \code{\link{layer_hs}}
##'
##'
##' @seealso \code{\link{y_time}}, \code{\link{geom_3s}}
##'
##' @details
##' Plots are generated using \code{\link{y_time}},
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
##' df <- pmplots_data_obs()
##'
##' cwresi_time(df)
##'
##'
##' @export
res_time <- function(df, x = pm_axis_time(), y = pm_axis_res(), ...) {
  out <- y_time(df, x = x, y = y, ...)
  layer_hs(out,...)
}

##' @export
##' @rdname res_time
res_tafd <- function(df, x = pm_axis_tafd(), y = pm_axis_res(), ...) {
  res_time(df, x = x, y = y, ...)
}

##' @export
##' @rdname res_time
res_tad <- function(df, x = pm_axis_tad(), y = pm_axis_res(), ...) {
  res_time(df, x = x, y = y, ...)
}

##' Plot weighted residuals versus time
##'
##' @inheritParams res_time
##' @param ... passed to \code{\link{res_time}} and eventually to
##' \code{\link{y_time}}
##'
##' @examples
##' df <- pmplots_data_obs()
##'
##' wres_time(df)
##'
##' @details
##' See \code{\link{res_time}} for other important implementation details.
##'
##' @seealso \code{\link{res_time}}, \code{\link{cwres_time}},
##' \code{\link{npde_time}}
##'
##' @export
##' @rdname wres_time
wres_time <- function(df, x = pm_axis_time(), y = pm_axis_wres(), ...) {
  res_time(df, x = x, y = y, ...)
}

##' @export
##' @rdname wres_time
wres_tafd <- function(df, x = pm_axis_tafd(), y = pm_axis_wres(), ...) {
  wres_time(df, x = x, y = y, ...)
}

##' @export
##' @rdname wres_time
wres_tad <- function(df, x = pm_axis_tad(), y = pm_axis_wres(), ...) {
  wres_time(df,  x = x, y = y, ...)
}

##' Plot conditional weighted residuals versus time
##'
##' @inheritParams res_time
##' @param ... passed to \code{\link{res_time}} and eventually to
##' \code{\link{y_time}}
##'
##' @examples
##' df <- pmplots_data_obs()
##'
##' cwresi_time(df)
##'
##' @details
##' See \code{\link{res_time}} for other important implementation details.
##'
##' @seealso \code{\link{res_time}}, \code{\link{wres_time}},
##' \code{\link{npde_time}}
##'
##' @export
##' @rdname cwres_time
cwres_time <- function(df, x = pm_axis_time(), y = pm_axis_cwres(), ...) {
  res_time(df, x = x, y = y, ...)
}

##' @export
##' @rdname cwres_time
cwres_tafd <- function(df, x = pm_axis_tafd(), y = pm_axis_cwres(), ...) {
  cwres_time(df, x = x, y = y, ...)
}

##' @export
##' @rdname cwres_time
cwres_tad <- function(df, x = pm_axis_tad(), y = pm_axis_cwres(), ...) {
  cwres_time(df, x = x, y = y, ...)
}


##' @export
##' @rdname cwres_time
cwresi_time <- function(df, x = pm_axis_time(), y = pm_axis_cwresi(), ...) {
  cwres_time(df, x = x, y = y, ... )
}

##' @export
##' @rdname cwres_time
cwresi_tafd <- function(df, x = pm_axis_tafd(), y = pm_axis_cwresi(), ...) {
  cwres_tafd(df, x = x, y = y, ... )
}

##' @export
##' @rdname cwres_time
cwresi_tad <- function(df, x= pm_axis_tad(), y = pm_axis_cwresi(), ...) {
  cwres_tad(df, x = x, y = y, ... )
}

##' Plot NPDE versus time
##'
##' @inheritParams res_time
##' @param ... passed to \code{\link{res_time}} and eventually to
##' \code{\link{y_time}}
##' @param hline a list of arguments to pass to \code{geom_hline} specifying
##' aesthetics to use
##'
##' @examples
##' df <- pmplots_data_obs()
##'
##' npde_time(df)
##'
##' @details
##' See \code{\link{res_time}} for other important implementation details.
##'
##' @seealso \code{\link{res_time}}, \code{\link{cwres_time}},
##' \code{\link{wres_time}}
##'
##' @export
##' @rdname npde_time
##'
npde_time <- function(df, x= pm_axis_time(), y  = pm_axis_npde(), ..., hline = npde_ref()) {
  res_time(df, x = x, y = y, hline = hline, ...)
}

##' @export
##' @rdname npde_time
npde_tad <- function(df, x = pm_axis_tad(), y = pm_axis_npde(), ...) {
  npde_time(df, x = x, y = y, ...)
}

##' @export
##' @rdname npde_time
npde_tafd <- function(df, x = pm_axis_tafd(), y = pm_axis_npde(), ...) {
  npde_time(df, x = x, y = y, ...)
}
