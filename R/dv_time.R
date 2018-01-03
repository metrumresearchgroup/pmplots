##' Plot DV versus time
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param y character name for y-axis data
##' @param xname used to form x-axis label
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
##' df <- dplyr::filter(superset2(), EVID==0)
##'
##' dv_time(df, yname="MyDrug (ng/mL)")
##'
##' dv_time(df, yname="MyDrug (ng/mL)", xunit="day")
##'
##' dv_time(df, log=TRUE, col="STUDYc")
##'
##' # Add a rug
##' dv_time(df) + geom_rug(data = function(x) dplyr::filter(x, BLQ > 0))
##'
##' @export
dv_time <- function(df, x="TIME", y="DV",
                    xname = "Time", xunit = "hr",
                    yname = NULL, group = "ID",
                    xs=defx(), ys=defy(), log=FALSE, xby = NULL, ...) {

  require_numeric(df,x)
  require_numeric(df,y)

  df <- as.data.frame(df)

  if(exists("BLQ", df)) {
    require_numeric(df,"BLQ")
    df[df$BLQ != 0, y] <- NA
  }

  xs$name <- paste0(xname, " (",xunit,")")
  ys$name <- yname

  if(log) {
    ys$trans <- "log"
    ys$breaks <- logbr3()
  }
  if(ys$trans %in% c("log", "log10")) {
    ykp <- df[,y] > 0
    df <- dplyr::filter(df,ykp)
    if(!is.numeric(ys$breaks) | is.null(ys$breaks)) {
      ys$breaks <- logbr()
    }
  }
  if(is.numeric(xby)) {
    xs$breaks <- seq(0,max(df[,x]),xby)
  }

  scatt(df,x,y,xs=xs,ys=ys,smooth=FALSE,group=group,...)
}

##' @export
##' @rdname dv_time
dv_tafd <- function(df,
                    x = "TAFD", ...,
                    xname = "Time after first dose") {
  dv_time(df, x = x, xname = xname, ...)
}

##' @export
##' @rdname dv_time
dv_tad <- function(df,
                   x = "TAD", ...,
                   xname = "Time after dose") {
  dv_time(df, x = x, xname = xname, ...)
}
