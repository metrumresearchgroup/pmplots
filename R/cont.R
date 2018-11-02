
##' Scatter plot function
##'
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param y character name for y-axis data
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param title character, passed to \code{ggtitle}
##' @param group character name of grouping variable
##' @param col not used
##' @param scale_col not used
##' @param ... not used
##'
##' @details
##' Since this function creates a scatter plot,
##' both the \code{x} and \code{y} columns must
##' be numeric.
##'
##' @export
scatt <- function(df, x, y, xs = defx(), ys = defy(),
                  title = NULL, group=NULL, col=NULL,
                  scale_col = scale_color_brewer(palette="Set2", name=""),
                  ... ) {

  xscale <- do.call("scale_x_continuous", xs)
  yscale <- do.call("scale_y_continuous", ys)

  locol <- .ggblue

  p <- ggplot(data=df,aes_string(x,y,col=col)) + geom_point() +
    xscale + yscale

  if(!is.null(group)) p <- p + geom_line(aes_string(group=group))
  if(is.character(title)) p <- p + ggtitle(title)
  p + pm_theme()
}

##' Plot continuous data versus time
##'
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param y character name for y-axis data
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param yname used to form y-axis title
##' @param xunit used to form x-axis title
##' @param log if \code{TRUE}, y-axis will be log-transformed
##' @param xby interval for breaks on x-axis (time)
##' @param ... passed to \code{\link{scatt}}
##'
##' @details
##'
##' This function is intented for internal use.
##'
##' When the y-axis label needs a unit, include it in
##' the \code{yname} specification.
##'
##' Since this function creates a scatter plot,
##' both the \code{x} and \code{y} columns must
##' be numeric.
##'
##' The \code{xby} argument is provided for
##' convenience to set breaks for time scales.
##'
##' @seealso \code{\link{res_time}}, \code{\link{cwres_time}},
##' \code{\link{wres_time}}, \code{\link{npde_time}}
##'
y_time <- function(df,
                   x=pm_axis_time(),
                   y,
                   xunit="hr",
                   yname = NULL,
                   xs = list(), ys = list(),
                   log = FALSE, xby = NULL, ...) {

  x <- glue_unit(x,xunit)
  y <- glue::glue(y)

  x <- col_label(x)
  y <- col_label(y)

  xlab <- x[2]
  ylab <- y[2]

  require_numeric(df,x[1])
  require_numeric(df,y[1])

  inx <- xs
  iny <- ys

  xs <- update_list(defx(),xs)
  ys <- update_list(defy(), ys)

  x <- x[1]
  y <- y[1]

  if(is.numeric(xby)) {
    xs$breaks <- seq(0,max(df[,x]),xby)
  }

  if(log) {
    ys$trans <- "log"
    ys$breaks <- logbr3()
  }

  scatt(df, x, y, xs=xs, ys=ys, ...) + pm_labs(x = xlab, y = ylab)

}

##' Plot continuous variable versus continuous variable
##'
##' This function is primarily called by other functions.
##' \code{pm_scatter} is an alias to \code{cont_cont} and should be
##' used in production code. \code{pm_scatter_list} is a vectorized
##' form of \code{pm_scatter}.
##'
##'
##' @param df data frame to plot
##' @param x character col//title for x-axis data; see \code{\link{col_label}}
##' @param y character col//title for y-axis data; see \code{\link{col_label}}
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param ... passed to \code{\link{scatt}}  and \code{\link{layer_hs}}
##'
##' @details
##' Since this function creates a scatter plot,
##' both the \code{x} and \code{y} columns must
##' be numeric.
##'
##'
##'
##'
##' @seealso \code{\link{scatt}}
##'
##' @examples
##' df <- pmplots_data_obs_id()
##'
##' pm_scatter(
##'   df,
##'   x="WT//Weight (kg)",
##'   y="HT//Height (cm)"
##' )
##'
##' @export
pm_scatter <- function(df, x, y, xs = defx(), ys=defy(),...) {
  y <- col_label(y)
  x <- col_label(x)
  xlab <- x[2]
  ylab <- y[2]
  require_numeric(df, x[1])
  require_numeric(df, y[1])
  scatt(df,x[1],y[1],xs,ys,...) + pm_labs(x = xlab, y = ylab)
}

##' @rdname pm_scatter
##' @export
pm_scatter_list <- function(df, x, y, ...) {
  list_plot_xy(df, x, y, cont_cont, ...)
}



##' @rdname pm_scatter
##' @export
cont_cont <- pm_scatter


