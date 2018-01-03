
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
  p
}

##' Plot continuous data versus time
##'
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param y character name for y-axis data
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param yname used to form y-axis title
##' @param xname used to form x-axis title
##' @param xunit used to form x-axis title
##' @param log if \code{TRUE}, y-axis will be log-transformed
##' @param xby interval for breaks on x-axis (time)
##' @param ... passed to \code{\link{scatt}}
##'
##' @details
##' When the y-axis label needs a unit, include it in
##' the \code{yname} specification.
##'
##' @details
##' Since this function creates a scatter plot,
##' both the \code{x} and \code{y} columns must
##' be numeric.
##'
##' The \code{xby} argument is provided for
##' convenience to set breaks for time scales.
##'
##'
y_time <- function(df, x="TIME", y,
                   xname="Time", xunit="hr",
                   yname = NULL,
                   xs = defx(), ys = defy(),
                   log = FALSE, xby = NULL, ...) {

  require_numeric(df,x)
  require_numeric(df,y)

  xs$name <- paste0(xname, " (",xunit,")")
  ys$name <- yname

  if(is.numeric(xby)) {
    xs$breaks <- seq(0,max(df[,x]),xby)
  }

  if(log) {
    ys$trans <- "log"
    ys$breaks <- logbr3()
  }

  scatt(df, x, y, xs=xs, ys=ys, ...)

}







