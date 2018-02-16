##' Plot continuous variable versus continuous variable
##'
##' This function is primarily called by other functions.
##'
##' @param df data frame to plot
##' @param x character col//title for x-axis data; see \code{\link{col_label}}
##' @param y character col//title for y-axis data; see \code{\link{col_label}}
##' @param xs see \code{\link{defx}}
##' @param ys see \code{\link{defy}}
##' @param ... passed to \code{\link{scatt}}  and \code{\link{layer_hs}}
##'
##' @details
##' Since this function creates a scatter plot,
##' both the \code{x} and \code{y} columns must
##' be numeric.
##'
##' The \code{eta_cont} function can make a plot of ETA versus
##' continuous variable for multiple ETAs when a vector
##' of values for \code{y} are passed in.  Accordingly,
##' \code{eta_cont} always returns a list of plots of the same
##' length as \code{y}.
##'
##' @seealso \code{\link{scatt}}
##'
##' @examples
##' df <- dplyr::filter(pmplots_data(), EVID==0)
##' df <- dplyr::distinct(df, ID, .keep_all = TRUE)
##'
##' cont_cont(df, x="WT//Weight (kg)", y="HT//Height (cm)")
##'
##' @export
cont_cont <- function(df, x, y, xs = defx(), ys=defy(),...) {
  y <- col_label(y)
  x <- col_label(x)
  ys$name <- y[2]
  xs$name <- x[2]
  require_numeric(df, x[1])
  require_numeric(df, y[1])
  scatt(df,x[1],y[1],xs,ys,...)
}


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
