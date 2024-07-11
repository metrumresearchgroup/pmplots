

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
  # Names are YvX only if there is just a single y
  clx <- col_labels(x)
  cly <- col_labels(y)
  if(length(y)==1) {
    outn <- lapply(clx, function(xi) paste0(cly, "v", xi))
  } else {
    # Otherwise, name the plots one level down in the list
    outn <- clx
    for(j in seq_along(outn)) {
      names(out[[j]]) <- cly
    }
  }
  names(out) <- unlist(outn, use.names=FALSE)
  out
}

##' @export
##' @rdname list_plot
list_plot_y <- function(df, x, y, .fun = cont_cont, ...) {
  out <- vector(mode = "list", length = length(y))
  for(i in seq_along(y)) {
    out[[i]] <- .fun(df = df, x = x, y = y[i], ...)
  }
  clx <- col_labels(x)
  cly <- col_labels(y)
  if(length(x)==1) {
    outn <- lapply(cly, function(yi) paste0(yi, "v", clx))
  } else {
    outn <- cly
    for(j in seq_along(outn)) {
      names(out[[j]]) <- clx
    }
  }
  names(out) <- unlist(outn, use.names=FALSE)
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
  clx <- col_labels(x)
  cly <- col_labels(y)
  if(length(y)==1) {
    outn <- clx
  } else {
    outn <- lapply(clx, function(xi) paste0(cly, "v", xi))
  }
  names(out) <- unlist(outn, use.names=FALSE)
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
  clx <- col_labels(x)
  cly <- col_labels(y)
  if(length(x)==1) {
    outn <- cly
  } else {
    outn <- lapply(cly, function(yi) paste0(yi, "v", clx))
  }
  names(out) <- unlist(outn, use.names=FALSE)
  out
}
