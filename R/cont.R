
#' Scatter plot function
#'
#' @param df Data frame to plot.
#' @param x Character name for x-axis data.
#' @param y Character name for y-axis data.
#' @param xs See [defx()].
#' @param ys See [defy()].
#' @param title Character, passed to [ggplot2::ggtitle()].
#' @param group Character name of grouping variable.
#' @param col Character name of variable to color the points.
#' @param scale_col Discrete scale to use for coloring the points (see default).
#' @param plot_id If `TRUE` then subject IDs are plotted rather than points;
#' see the `size` argument - the size may need to be increased when plotting
#' IDs.
#' @param size Passed to [ggplot2::geom_point()] or [ggplot2::geom_text()].
#' @param alpha Passed to [ggplot2::geom_point()].
#' @param ... Not used.
#'
#' @details
#' Since this function creates a scatter plot, both the `x` and `y` columns must
#' be numeric.
#'
#' @return
#' A single plot.
#'
#' @md
#' @export
scatt <- function(df, x, y, xs = defx(), ys = defy(),
                  title = NULL, group=NULL, col=NULL, plot_id = FALSE,
                  size = pm_opts$scatter.size, alpha = pm_opts$scatter.alpha,
                  scale_col = scale_color_brewer(palette="Set2", name=""),
                  ... ) {

  xscale <- do.call("scale_x_continuous", xs)
  yscale <- do.call("scale_y_continuous", ys)
  locol <- .ggblue

  if(is.null(col)) {
    col <- I(glue('{scatter.col}', .envir = pm_opts))
  } else {
    if(col %in% names(df)) {
      col <- sym(col)
    } else {
      col <- I(glue('{col}'))
    }
  }

  p <- ggplot(data = df, aes(x=.data[[x]], y=.data[[y]], col={{ col }}))

  if(plot_id) {
    require_column(df,"ID")
    p <- p + geom_text(aes(label = .data$ID), alpha = alpha, size = size)
  } else {
    p <- p + geom_point(alpha = alpha, size = size)
  }
  if(!is.null(group)) {
    p <- p + geom_line(aes(group = .data[[group]]))
  }
  if(is.character(title)) p <- p + ggtitle(title)
  p + xscale + yscale + pm_theme()
}

#' Plot continuous data versus time
#'
#' @param df data frame to plot
#' @param x character name for x-axis data
#' @param y character name for y-axis data
#' @param xs see \code{\link{defx}}
#' @param ys see \code{\link{defy}}
#' @param yname used to form y-axis title
#' @param xunit used to form x-axis title
#' @param log if \code{TRUE}, y-axis will be log-transformed
#' @param xby interval for breaks on x-axis (time)
#' @param ... passed to \code{\link{scatt}}
#'
#' @details
#'
#' This function is intended for internal use.
#'
#' When the y-axis label needs a unit, include it in
#' the \code{yname} specification.
#'
#' Since this function creates a scatter plot,
#' both the \code{x} and \code{y} columns must
#' be numeric.
#'
#' The \code{xby} argument is provided for
#' convenience to set breaks for time scales.
#'
#' @return A single plot.
#'
#' @seealso \code{\link{res_time}}, \code{\link{cwres_time}},
#' \code{\link{wres_time}}, \code{\link{npde_time}}
#'
y_time <- function(df,
                   x = pm_axis_time(),
                   y,
                   xunit = opts$time.unit,
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

#' Plot continuous variable versus continuous variable
#'
#' This function is primarily called by other functions.
#' \code{pm_scatter} is an alias to \code{cont_cont} and should be
#' used in production code. \code{pm_scatter_list} is a vectorized
#' form of \code{pm_scatter}.
#'
#'
#' @param df data frame to plot
#' @param x character col//title for x-axis data; see \code{\link{col_label}}
#' @param y character col//title for y-axis data; see \code{\link{col_label}}
#' @param xs see \code{\link{defx}}
#' @param ys see \code{\link{defy}}
#' @param ... passed to \code{\link{scatt}}  and \code{\link{layer_hs}}
#'
#' @details
#' Since this function creates a scatter plot,
#' both the \code{x} and \code{y} columns must
#' be numeric.
#'
#'
#' @return \code{pm_scatter} returns a single plot;
#' \code{pm_scatter_list} returns a list of plots.
#'
#'
#' @seealso \code{\link{scatt}}
#'
#' @examples
#' df <- pmplots_data_id()
#'
#' pm_scatter(
#'   df,
#'   x="WT//Weight (kg)",
#'   y="HT//Height (cm)"
#' )
#'
#' @export
pm_scatter <- function(df, x, y, xs = defx(), ys=defy(),...) {
  if(length(x) > 1 || length(y) > 1) {
    return(pm_scatter_list(df,x,y,xs=xs,ys=ys,...))
  }
  y <- col_label(y)
  x <- col_label(x)
  xlab <- x[2]
  ylab <- y[2]
  require_numeric(df, x[1])
  require_numeric(df, y[1])
  scatt(df,x[1],y[1],xs,ys,...) + pm_labs(x = xlab, y = ylab)
}

#' @rdname pm_scatter
#' @export
pm_scatter_list <- function(df, x, y, ...) {
  list_plot_xy(df, x, y, cont_cont, ...)
}

#' @rdname pm_scatter
#' @export
cont_cont <- pm_scatter


