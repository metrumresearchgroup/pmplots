box_labels <- function(df, x, y) {
  .xcol <- rlang::sym(x)
  .ycol  <- rlang::sym(y)
  .sum <- filter(df, !is.na(!!.ycol))
  .sum <- group_by(.sum, !!.xcol)
  .sum <- summarize(.sum, n = n(), N = n_distinct(ID))
  .sum <- ungroup(.sum)
  as.data.frame(.sum)
}

#' Make boxplots
#'
#' @param df data frame to plot
#' @param x character name for x-axis data
#' @param y character name for y-axis data
#' @param xs see \code{\link{defcx}}
#' @param ys see \code{\link{defy}}
#' @param fill passed to \code{geom_boxplot}
#' @param alpha passed to \code{geom_boxplot}
#' @param hline used to draw horizontal reference line
#' @param title passed to \code{ggtitle}
#' @param shown if \code{TRUE} provide a numeric summary of each
#' box (see details)
#' @param points show points in back of transparent boxes; if \code{TRUE},
#' a default display of points is made on top of boxes; also may be passed as
#' a list of arguments to pass to \code{geom_point}; see details
#' @param outlier.shape passed to \code{geom_boxplot}
#' @param ... arguments passed to \code{geom_boxplot}
#'
#' @details
#' Since this function creates a boxplot,
#' the \code{x} column must be character, factor
#' or logical and \code{y} column must
#' be numeric.
#'
#' If \code{shown} is \code{TRUE}, a numeric summary of each
#' box is included
#' below each box.  In the summary, \code{n} is the number of
#' non-NA observations in the \code{y} column for that box and
#' \code{N} is the number of unique \code{ID} values for
#' that box.  An error will be generated if \code{ID} does
#' not exist in the plotting data frame when \code{shown} is
#' \code{TRUE}.  When \code{N} is equal to \code{n} in the
#' summary, only \code{n} is shown.
#'
#' The summaries will not be correct if the plot is eventually faceted by
#' another variable in the data set.  In this case, either use
#' \code{shown=FALSE} or create the plot with \code{\link{split_plot}}.
#'
#' When the user passes the \code{points} argument, \code{outlier.shape} is
#' automatically switched to \code{NA} so that outlier points are only plotted
#' once.  The \code{fill} argument is also set to \code{NA}, so that boxes become
#' transparent, showing the points.
#'
#' When the user sets \code{points} to \code{TRUE}, grey points are shown
#' in back of transparent boxes and the points are jittered in the x-direction.
#' The user can customize the display of the points by passing a list of
#' arguments for \code{geom_point} (for example, change the color, transparency,
#' size, jitter amount, etc).
#'
#'
#' @export
boxwork <- function(df, x, y, xs=defcx(), ys=defy(),
                    fill = opts$boxplot.fill,
                    alpha = opts$boxplot.alpha,
                    hline = NULL, title = NULL, shown = TRUE,
                    points = NULL,
                    outlier.shape = opts$boxplot.outlier.shape,
                    ...) {

  if(shown) {
    require_column(df, "ID")
    .sum <- box_labels(df, x, y)
    xs$labels <- paste0(.sum[,x], "\nn=", .sum[,"n"], "\nN=", .sum[,"N"])
    if(all(.sum$N == .sum$n)) {
      xs$labels <- paste0(.sum[,x], "\nN=", .sum[,"N"])
    }
  }

  yscale <- do.call("scale_y_continuous", ys)
  xscale <- do.call("scale_x_discrete", xs)

  p <- ggplot(data=df, aes_string(x=x,y=y))

  do_points <- !missing(points) & !is.null(points)
  if(do_points) {
    outlier.shape <- NA
    fill <- NA
    def <- list(col = "grey", position  = position_jitter(height = 0))
    if(is.list(points)) {
      points <- combine_list(def,points)
    } else {
      points <- def
    }
    p <- p + do.call(geom_point,points)
  }
  p <- p + geom_boxplot(fill=fill, alpha = alpha, outlier.shape = outlier.shape, ...)
  p <- p + yscale + xscale
  if(is.numeric(hline)) {
    p <- p + geom_hline(
      yintercept = hline,
      lwd = opts$boxplot.hline.lwd,
      lty = opts$boxplot.hline.lty,
      col = opts$boxplot.hline.col
    )
  }
  if(is.character(title)) p <- p + ggtitle(title)
  p + pm_theme()
}

#' Plot continuous variable against a categorical variable
#'
#' This function is primarily called by other functions.
#' \code{pm_box_list} is a vectorized version of \code{cont_cat}.
#'
#' @param df data frame to plot
#' @param x character col//title for x-axis data; see \code{\link{col_label}}
#' @param y character col//title for y-axis data; see \code{\link{col_label}}
#' @param xs see \code{\link{defcx}}
#' @param ys see \code{\link{defy}}
#' @param ... other arguments passed to \code{\link{boxwork}}
#'
#' @details
#' Since this function creates a boxplot,
#' the \code{x} column must be character, factor
#' or logical and \code{y} column must
#' be numeric.
#'
#' Summary numbers located below each box are described in
#' \code{\link{boxwork}}.  The summaries will not be correct if the plot
#' is eventually faceted by another variable in the data set.  In this case,
#' either use \code{shown=FALSE} or create the plot with
#' \code{\link{split_plot}}.
#'
#' @seealso \code{\link{boxwork}}
#'
#' @examples
#' df <- pmplots_data_id()
#'
#'
#' cont_cat(df, x="STUDYc//Study name", y="WT//Weight (kg)")
#'
#' set.seed(12345)
#' cont_cat(df, x="STUDYc", y="WT", points = list(width  = 0.1))
#'
#' @export
pm_box <- function(df, x, y, xs=defcx(), ys = defy(), ...) {
  if(length(x) > 1 || length(y) > 1) {
    return(pm_box_list(df,x,y,xs=xs,ys=ys,...))
  }
  x <- col_label(x)
  if(length(x)!=2) stop("invalid x value", call.=FALSE)
  y <- col_label(y)
  if(length(x)!=2) stop("invalid y value", call.=FALSE)
  require_numeric(df,y[1])
  require_discrete(df,x[1])
  boxwork(df,x[1],y[1],xs,ys,...) + pm_labs(x = x[2], y = y[2])
}

#' @rdname pm_box
#' @export
pm_box_list <- function(df, x, y, ...) {
  list_plot_xy(df, x, y, cont_cat, ...)
}

#' @rdname pm_box
#' @export
cont_cat <- pm_box

