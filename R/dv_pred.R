
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
##' @param scales if \code{TRUE}, then the x- and y- axes will be forced
##' to have the same limits
##' @param ... passed to \code{\link{scatt}} and \code{\link{layer_as}}
##'
##' @details
##' Since this function creates a scatter plot,
##' both the \code{x} and \code{y} columns must
##' be numeric.
##'
##' \code{dv_preds} returns a list of two plots, with
##' the result of \code{dv_pred} in the first position
##' and the result of \code{dv_ipred} in the
##' second position.  In this case, \code{...} are
##' passed to both functions.
##'
##' @examples
##' df <- dplyr::filter(pmplots_data(), EVID==0)
##'
##' dv_pred(df)
##'
##' dv_ipred(df, yname="MyDrug (ng/mL)")
##'
##' dv_preds(df, yname = "MyDrug (ng/mL)")
##'
##' @export
dv_pred <- function(df, x=pm_axis("pred"),
                    y=pm_axis("dv"), yname="value", xname=yname,
                    xs = list(), ys = list(), loglog=FALSE,
                    scales = c("fixed", "free"), ...) {

  scales <- match.arg(scales)

  x <- glue::glue(x)
  y <- glue::glue(y)

  x <- col_label(x)
  y <- col_label(y)

  require_numeric(df,x[1])
  require_numeric(df,y[1])

  inx <- xs
  iny <- ys

  xs <- update_list(defx(),xs)
  ys <- update_list(defy(),ys)

  xs$name <- x[2]
  ys$name <- y[2]

  x <- x[1]
  y <- y[1]

  if(loglog) {
    xs$trans <- "log"
    ys$trans <- "log"
  }

  if(xs$trans %in% c("log", "log10")) {
    xkp <- df[,x] > 0
    df <- dplyr::filter(df,xkp)
    if(.miss("breaks", inx)) {
      xs$breaks <- logbr3()
    }
  }

  if(ys$trans %in% c("log", "log10")) {
    ykp <- df[,y] > 0
    df <- dplyr::filter(df,ykp)
    if(.miss("breaks", iny)) {
      ys$breaks <- logbr3()
    }
  }

  if(scales == "fixed") {
    lim <- get_limits(df,x,y)

    if(.miss("limits", inx)) {
      xs$limits <- lim
    }

    if(.miss("limits", iny)) {
      ys$limits <- lim
    }
  }

  out <- scatt(df, x, y, identity = TRUE, xs = xs, ys = ys, ...)

  layer_as(out, ...)
}

##' @export
##' @rdname dv_pred
dv_ipred <- function(df, x = pm_axis("ipred"), ...) {
  out <- dv_pred(df, x = x, ...)
  layer_as(out,...)
}

##' @export
##' @rdname dv_pred
dv_preds <- function(df, ...) {
  list(dv_pred(df, ...), dv_ipred(df, ...))
}
