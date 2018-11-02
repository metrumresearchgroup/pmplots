##' Plot ETAs versus continuous variables
##'
##' This is a vectorized function; see details.
##'
##' @param df data frame to plot
##' @param x character col//title for x-axis data; see \code{\link{col_label}}
##' @param y character col//title for y-axis data; see \code{\link{col_label}}
##' @param ... other arguments passed to \code{\link{cont_cont}}
##'
##' @details
##' This is a vectorized functions; if either x or y
##' are a character vector (in col//title format), a list
##' of plots is returned.  If both x and y are length 1,
##' then a single plot object (not a list) is returned.
##'
##' @seealso \code{\link{eta_cat}}
##'
##' @examples
##' df <- pmplots_data_id()
##'
##' eta_cont(df, x = "WT//Weight (kg)", y = "ETA1//ETA-CL")
##'
##' @return A single plot when a single value for \code{x}
##' and \code{y} are supplied; a list of plots of either \code{x}
##' or \code{y} have length greater than 1.
##'
##' @export
eta_cont <- function(df, x, y,...) {
  out <- list_plot_xy(df, x, y, cont_cont,...)
  out <- lapply(out, layer_hs,...)
  if(length(out)==1) return(out[[1]])
  out
}


##' Make ETA labels and col-labels
##'
##' @param ... unquoted parameter names
##' @param .prefix used to generate ETA axis label
##' @param .eta_n integer ETA numbers for which to form labels
##'
##' @examples
##' eta_labs(CL, V2, KA)
##'
##' @export
##'
eta_labs <- function(..., .prefix = "ETA-") {
  labs <- sapply(quos(...), quo_text)
  labs <- paste0(.prefix, labs)
  labs
}

##' @rdname eta_labs
##' @export
eta_col_labs <- function(..., .eta_n = seq_along(labs), .prefix = "ETA-") {
  labs <- eta_labs(..., .prefix = .prefix)
  col <- paste0("ETA",.eta_n)
  set_names(paste0(col,"//",labs),labs)
}
