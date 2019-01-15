box_labels <- function(df, x, y) {
  .xcol <- rlang::sym(x)
  .ycol  <- rlang::sym(y)
  .sum <- filter(df, !is.na(!!.ycol))
  .sum <- group_by(.sum, !!.xcol)
  .sum <- summarize(.sum, n = n(), N = n_distinct(ID))
  .sum <- ungroup(.sum)
  as.data.frame(.sum)
}

##' Make boxplots
##'
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param y character name for y-axis data
##' @param xs see \code{\link{defcx}}
##' @param ys see \code{\link{defy}}
##' @param fill passed to \code{geom_boxplot}
##' @param alpha passed to \code{geom_boxplot}
##' @param hline used to draw horizontal reference line
##' @param title passed to \code{ggtitle}
##' @param shown if \code{TRUE} provide a numeric summary of each
##' box (see details)
##' @param ... not used
##'
##' @details
##' Since this function creates a boxplot,
##' the \code{x} column must be character, factor
##' or logical and \code{y} column must
##' be numeric.
##'
##' If \code{shown} is \code{TRUE}, a numeric summary of each
##' box is included
##' below each box.  In the summary, \code{n} is the number of
##' non-NA observations in the \code{y} column for that box and
##' \code{N} is the number of unique \code{ID} values for
##' that box.  An error will be generated if \code{ID} does
##' not exist in the plotting data frame when \code{shown} is
##' \code{TRUE}.  When \code{N} is equal to \code{n} in the
##' summary, only \code{n} is shown.
##'
##' The summaries will not be correct if the plot is eventually faceted by
##' another variable in the data set.  In this case, either use
##' \code{shown=FALSE} or create the plot with \code{\link{split_plot}}.
##'
##' @export
boxwork <- function(df, x, y, xs=defcx(), ys=defy(), fill="white",
                    alpha=1, hline = NULL, title=NULL, shown = TRUE, ...) {

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
  # p <- p + geom_point(aes_string(x = x, y = y),
  #                     position = position_jitter(width = 0.25),
  #                     col = "black", alpha = 0.6)
  p <- p + geom_boxplot(fill=fill, alpha=alpha, ...) + yscale + xscale
  if(is.numeric(hline)) {
    p <- p + geom_hline(yintercept=hline,lwd=1, lty=2)
  }
  if(is.character(title)) p <- p + ggtitle(title)
  p + pm_theme()
}

##' Plot continuous variable against a categorical variable
##'
##' This function is primarily called by other functions.
##' \code{cont_cat_list} is a vectorized version of \code{cont_cat}.
##'
##' @param df data frame to plot
##' @param x character col//title for x-axis data; see \code{\link{col_label}}
##' @param y character col//title for y-axis data; see \code{\link{col_label}}
##' @param xs see \code{\link{defcx}}
##' @param ys see \code{\link{defy}}
##' @param ... other arguments passed to \code{\link{boxwork}}
##'
##' @details
##' Since this function creates a boxplot,
##' the \code{x} column must be character, factor
##' or logical and \code{y} column must
##' be numeric.
##'
##' Summary numbers located below each box are described in
##' \code{\link{boxwork}}.  The summaries will not be correct if the plot
##' is eventually faceted by another variable in the data set.  In this case,
##' either use \code{shown=FALSE} or create the plot with
##' \code{\link{split_plot}}.
##'
##' @seealso \code{\link{boxwork}}
##'
##' @examples
##' df <- pmplots_data_id()
##'
##'
##' cont_cat(df, x="STUDYc//Study name", y="WT//Weight (kg)")
##'
##' @export
pm_box <- function(df, x, y, xs=defcx(), ys = defy(), ...) {
  x <- col_label(x)
  if(length(x)!=2) stop("invalid x value", call.=FALSE)
  y <- col_label(y)
  if(length(x)!=2) stop("invalid y value", call.=FALSE)
  require_numeric(df,y[1])
  require_discrete(df,x[1])
  boxwork(df,x[1],y[1],xs,ys,...) + pm_labs(x = x[2], y = y[2])
}

##' @rdname pm_box
##' @export
pm_box_list <- function(df, x, y, ...) {
  list_plot_xy(df, x, y, cont_cat, ...)
}

##' @rdname pm_box
##' @export
cont_cat <- pm_box

