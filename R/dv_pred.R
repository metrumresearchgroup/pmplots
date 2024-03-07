
#' Plot DV versus predicted values
#'
#' Plots for `DV` versus population or individual predicted values. `dv_preds`
#' makes both plots and returns them as a list (more in `details`).
#'
#' @param df data frame to plot
#' @param x character name for x-axis data
#' @param y character name for y-axis data
#' @param yname used to form y-axis label
#' @param xname used to form x-axis label
#' @param ys see [defy()]
#' @param xs see [defx()]; note that `xs` defaults to `ys` so (by
#' default) the scale configuration will be identical; pass both `xs` and `ys`
#' to have them independently configured
#' @param loglog if `TRUE`, x- and y-axes will be log-transformed
#' @param scales if `TRUE`, then the x- and y- axes will be forced
#' to have the same limits
#' @param logbr when using log scale, should the tick marks be at `full`-log
#' intervals or `half`-log intervals? If you pass `null`, the default scales
#' will be used (which might be identical to `full`). Use `xs` and `ys` to
#' pass custom scales.
#' @param ... passed to [scatt()] and [layer_as()]
#'
#' @details
#' Since this function creates a scatter plot, both the `x` and `y` columns
#' must be numeric.
#'
#' `dv_preds` returns a list of two plots, with the result of `dv_pred` in the
#' first position and the result of `dv_ipred` in the second position.  In
#' this case, `...` are passed to both functions.
#'
#' @examples
#' df <- pmplots_data_obs()
#'
#' dv_pred(df)
#'
#' dv_ipred(df, yname="MyDrug (ng/mL)")
#'
#' dv_preds(df, yname = "MyDrug (ng/mL)")
#'
#' @return
#' `dv_pred` and `dv_ipred` return a single plot; `dv_preds` returns a list
#' of plots.
#'
#' @md
#' @export
dv_pred <- function(df, x = pm_axis_pred(), y = pm_axis_dv(),
                    yname = "value", xname = "value",
                    ys = list(), xs = ys, loglog = FALSE,
                    scales = c("fixed", "free", "null"),
                    logbr = c("full", "half", "null"), ...) {

  scales <- match.arg(scales)

  if(missing(xname)) xname <- yname

  x <- glue::glue(x)
  y <- glue::glue(y)

  x <- col_label(x)
  y <- col_label(y)

  xlab <- x[2]
  ylab <- y[2]

  require_numeric(df, x[1])
  require_numeric(df, y[1])

  inx <- xs
  iny <- ys

  xs <- remap_trans_arg(xs)
  ys <- remap_trans_arg(ys)

  xs <- update_list(defx(), xs)
  ys <- update_list(defy(), ys)

  x <- x[1]
  y <- y[1]

  if(loglog) {
    xs$transform <- "log10"
    ys$transform <- "log10"
    logbr <- match.arg(logbr)
    if(logbr == "half") {
      log_breaks <- logbr3()
    }
    if(logbr == "full") {
      log_breaks <- logbr()
    }
    if(logbr == "null") {
      log_breaks <- NULL
    }
  }

  if(xs$transform %in% c("log", "log10")) {
    xkp <- df[,x] > 0
    df <- dplyr::filter(df, xkp)
    if(.miss("breaks", inx)) {
      xs$breaks <- log_breaks
    }
  }

  if(ys$transform %in% c("log", "log10")) {
    ykp <- df[,y] > 0
    df <- dplyr::filter(df, ykp)
    if(.miss("breaks", iny)) {
      ys$breaks <- log_breaks
    }
  }

  if(scales == "fixed") {
    lim <- get_limits(df, x, y)

    if(.miss("limits", inx)) {
      xs$limits <- lim
    }

    if(.miss("limits", iny)) {
      ys$limits <- lim
    }
  }

  out <- scatt(df, x, y, identity = TRUE, xs = xs, ys = ys, ...)

  layer_as(out, ...) + pm_labs(x = xlab, y = ylab)
}

#' @export
#' @rdname dv_pred
dv_ipred <- function(df, x = pm_axis_ipred(), ...) {
  out <- dv_pred(df, x = x, ...)
  layer_as(out, ...)
}

#' @export
#' @rdname dv_pred
dv_preds <- function(df, ...) {
  list(dv_pred(df, ...), dv_ipred(df, ...))
}
