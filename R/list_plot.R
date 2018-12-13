

##' Apply a plotting function to a vector of x or y values
##'
##' \code{list_plot_x} vectorizes over the \code{x} value,
##' \code{list_plot_y} vectorizes over the \code{y} value,
##' \code{list_plot_xy} vectorizes over \code{x} and
##' then \code{y}, and \code{list_plot_yx} vectorizes over
##' \code{y} before \code{x}.
##'
##' @param df the plotting data set
##' @param x the x-column, as a col_label
##' @param y the y-column, as col_label
##' @param .fun the function to call
##' @param ... arguments passed to \code{fun}
##'
##' @return a list of plots generated from \code{.fun}
##'
##' @details
##' This function was intended for use with functions
##' that ultimately call \code{\link{cont_cont}} and
##' have continuous data on the x and y axes.
##'
##'
##' @export
##' @rdname list_plot
list_plot_x <- function(df, x, y, .fun = cont_cont, ...) {
  out <- vector(mode = "list", length = length(x))
  for(i in seq_along(x)) {
    out[[i]] <- .fun(df = df, x = x[i], y = y, ...)
  }
  out
}

##' @export
##' @rdname list_plot
list_plot_y <- function(df, x, y, .fun = cont_cont, ...) {
  out <- vector(mode = "list", length = length(y))
  for(i in seq_along(y)) {
    out[[i]] <- .fun(df = df, x = x, y = y[i], ...)
  }
  out
}

##' @export
##' @rdname list_plot
list_plot_xy <- function(df, x, y, .fun = cont_cont, ...) {
  out <- vector(mode = "list", length = (length(x)*length(y)))
  k <- 1
  for(i in seq_along(x)) {
    for(j in seq_along(y)) {
      out[[k]] <- .fun(df, x = x[i], y = y[j], ...)
      k <- k + 1
    }
  }
  out
}

##' @export
##' @rdname list_plot
list_plot_yx <- function(df, x, y, .fun = cont_cont, ...) {
  out <- vector(mode = "list", length = (length(x)*length(y)))
  k <- 1
  for(j in seq_along(y)) {
    for(i in seq_along(x)) {
      out[[k]] <- .fun(df, x = x[i], y = y[j], ...)
      k <- k + 1
    }
  }
  out
}


##' Arrange a list of plots in a grid
##'
##' @param x a list of plots
##' @param ... passed to \code{\link[cowplot]{plot_grid}}
##'
##' @details
##' The cowplot package must be installed to use this function.
##'
##' @examples
##'
##' data <- pmplots_data_obs()
##'
##' plot <- wres_cont(data, x = c("WT", "ALB"))
##'
##' pm_grid(plot)
##'
##' @export
pm_grid <- function(x, ...) {
  if(!requireNamespace("cowplot")) {
    stop("Please install the cowplot package to use this function.")
  }
  cowplot::plot_grid(plotlist=x, ...)
}
