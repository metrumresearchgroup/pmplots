##' Plot DV versus time
##'
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param y character name for y-axis data
##' @param xunit time units; used to form x-axis label
##' @param yname used to form y-axis label
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param group passed to \code{\link{scatt}}
##' @param log if \code{TRUE}, y-axis will be log-transformed
##' @param xby interval for x-axis breaks
##' @param ... passed to \code{\link{scatt}}
##'
##' @details
##'
##' Plots are generated using \code{\link{y_time}},
##' which then calls \code{\link{scatt}}.
##'
##' Since this function creates a scatter plot,
##' both the \code{x} and \code{y} columns must
##' be numeric.
##'
##' If the data set includes a \code{BLQ} column,
##' the values in the \code{y} column are
##' set to \code{NA} when \code{BLQ} is
##' not equal to \code{0}.
##'
##' @examples
##' df <- pmplots_data_obs()
##'
##' dv_time(df, yname="MyDrug (ng/mL)")
##'
##' dv_time(df, yname="MyDrug (ng/mL)", xunit="day")
##'
##'
##' # Add a rug
##'
##' df <- pmplots_data()
##'
##' dv_time(df) + geom_rug(data = ~ dplyr::filter(.x,BLQ > 0))
##'
##' @return A single plot.
##'
##' @export
dv_time <- function(df, x=pm_axis_time(), y=pm_axis_dv(),
                    xunit = opts$time.unit,
                    yname = "DV", group = "ID",
                    xs=list(), ys=list(), log=FALSE, xby = NULL, ...) {

  x <- glue_unit(x,xunit)
  y <- glue::glue(y)

  x <- col_label(x)
  y <- col_label(y)

  xlab <- x[2]
  ylab <- y[2]

  df <- as.data.frame(df)

  require_numeric(df,x[1])
  require_numeric(df,y[1])

  inx <- xs
  iny <- ys

  xs <- remap_trans_arg(xs)
  ys <- remap_trans_arg(ys)

  xs <- update_list(defx(),xs)
  ys <- update_list(defy(),ys)

  x <- x[1]
  y <- y[1]

  if(exists("BLQ", df)) {
    require_numeric(df,"BLQ")
    df[df$BLQ != 0, y] <- NA
  }

  if(log) {
    ys$transform <- "log"
  }

  if(ys$transform %in% c("log", "log10")) {
    ykp <- df[,y] > 0
    df <- dplyr::filter(df,ykp)
    if(.miss("breaks", iny)) {
      ys$breaks <- logbr3()
    }
  }
  if(is.numeric(xby)) {
    xs$breaks <- seq(0,max(df[,x]),xby)
  }

  scatt(df,x,y,xs=xs,ys=ys,smooth=FALSE,group=group,...) +
    pm_labs(x = xlab, y = ylab)
}

##' @export
##' @rdname dv_time
dv_tafd <- function(df, x =  pm_axis_tafd(), ...) {
  dv_time(df, x = x, ...)
}

##' @export
##' @rdname dv_time
dv_tad <- function(df, x = pm_axis_tad(), ...) {
  dv_time(df, x = x, ...)
}
