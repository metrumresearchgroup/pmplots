multi_scatter_x <- function(df, x, y,  ... ) {
  out <- vector(mode = "list", length = length(x))
  for(i in seq_along(x)) {
    out[[i]] <- cont_cont(df = df, x = x[i], y = y, ...)
  }
  out <- lapply(out, layer_hs)
  out
}

multi_scatter_y <- function(df, x,  y, ... ) {
  out <- vector(mode="list", length=length(y))
  for(i in seq_along(y)) {
    out[[i]] <- cont_cont(df,x = x, y = y[i], ...)
  }
  out <- lapply(out, layer_hs, ...)
  return(out)
}



##' Apply a plotting function to a vector of x or y values
##'
##' @param df the plotting data set
##' @param x the x-column, as a col_label
##' @param y the y-column, as col_label
##' @param .fun the function to call
##' @param ... passed to \code{fun}
##'
##' @return a list of plots generated from \code{.fun}
##'
##' @details
##' This function was intended for use with functions
##' that ultimately call \code{\link{cont_cont}} and
##' have continuous data on the x and y axes.
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