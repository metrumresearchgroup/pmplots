
##' Plot DV versus predicted values
##'
##' @param df data frame to plot
##' @param x character name for x-axis data
##' @param y character name for y-axis data
##' @param xname used to form x-axis label
##' @param yname used to form y-axis label
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param loglog if \code{TRUE}, x- and y-axes will be log-transformed
##' @param prefix used internally
##' @param ... passed to \code{\link{scatt}} and \code{\link{layer_as}}
##'
##' @details
##' Since this function creates a scatter plot,
##' both the \code{x} and \code{y} columns must
##' be numeric.
##'
##' @examples
##' df <- dplyr::filter(pmplots_data(), EVID==0)
##'
##' dv_pred(df)
##'
##' dv_ipred(df, yname="MyDrug (ng/mL)")
##'
##' @export
dv_pred <- function(df, x="PRED", y="DV", xname=yname, yname="value",
                    xs = defx(), ys = defy(), loglog=FALSE,
                    prefix="Population", ...) {

  require_numeric(df,x)
  require_numeric(df,y)

  xs$name <- paste0(prefix, " predicted ", xname)
  ys$name <- paste0("Observed ", yname)

  if(loglog) {
    xs$trans <- "log"
    ys$trans <- "log"
  }

  if(xs$trans %in% c("log", "log10")) {
    xkp <- df[,x] > 0
    df <- dplyr::filter(df,xkp)
    if(!is.numeric(xs$breaks)) {
      xs$breaks <- logbr()
    }
  }

  if(ys$trans %in% c("log", "log10")) {
    ykp <- df[,y] > 0
    df <- dplyr::filter(df,ykp)
    if(!is.numeric(ys$breaks) | is.null(ys$breaks)) {
      ys$breaks <- logbr()
    }
  }

  lim <- get_limits(df,x,y)
  xs$limits <- lim
  ys$limits <- lim

  out <- scatt(df, x, y, identity = TRUE, xs = xs, ys = ys, ...)

  layer_as(out, ...)
}

##' @export
##' @rdname dv_pred
dv_ipred <- function(df, x = "IPRED", ..., prefix = "Individual") {
  out <- dv_pred(df, x = x, prefix=prefix, ...)
  layer_as(out,...)
}
