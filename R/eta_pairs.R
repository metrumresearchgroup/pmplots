pairs_lower <- function(data, mapping, smooth_color = .ggblue, smooth_lty = 2, ...) {

  if(is.character(mapping$smooth_color)){
    smooth_color <- mapping$smooth_color
  }
  if(is.numeric(mapping$smooth_lty)) {
    smooth_lty <- mapping$smooth_lty
  }

  ggplot(data = data, mapping = mapping) +
    geom_point() +
    geom_smooth(method = "loess", color = smooth_color, lty = smooth_lty,
                se = FALSE, lwd = 1.3)

}

pairs_upper <- function(data, mapping, ...) {
  x <- rlang::quo_name(mapping$x)[1]
  y <- rlang::quo_name(mapping$y)[1]
  label <- as.character(signif(cor(data[,x],data[,y],use = "complete.obs"), digits=3))

  label <- paste0("Corr: ", label)
  GGally::ggally_text(label = label) +
    theme(panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank())
}

##' Pairs plots using ggpairs
##'
##'
##'
##' @param x plotting data.frame
##' @param etas character col//label for pairs data; see \code{\link{col_label}}
##' @param bins passed to \code{geom_histogram}
##' @param alpha passed to \code{geom_histogram}
##' @param fill passed to \code{geom_histogram}
##' @param col passed to \code{geom_histogram}
##' @param upper_fun function to use for \code{upper} argument
##' @param lower_fun function to use for \code{lower} argument
##' @param ... passed to \code{GGally::ggpairs}
##'
##' @details
##' This function requires the \code{GGally} package to be installed.
##'
##' When the length of \code{etas} is one, arguments
##' are passed to \code{\link{eta_hist}} and that result is returned.
##'
##' @return
##' The result from a ggpairs call.
##'
##' @examples
##'
##' df <- dplyr::filter(pmplots_data(), EVID==0)
##' id <- dplyr::distinct(df, ID, .keep_all = TRUE)
##'
##'
##' eta_pairs(
##'   id,
##'   etas = c("ETA1//ETA-CL", "ETA2//ETA-VC", "ETA3//ETA-KA")
##' )
##'
##' @export
pairs_plot <- function(x, etas, bins = 15, alpha = 0.6, fill = "black",
                       col="grey",
                       upper_fun = pairs_upper, lower_fun = pairs_lower, ...) {

  if(!requireNamespace("GGally")) {
    stop("this function requires that the GGally package be installed",
         call. = FALSE)
  }

  if(length(etas)==1) {
    ans <- eta_hist(
      x, etas, bins = bins, alpha = alpha, fill = fill,
      col = col, ...
    )
    return(ans)
  }

  diag <- GGally::wrap("barDiag", bins = bins,
                       alpha = alpha, fill=fill, col=col)
  x <- as.data.frame(x)
  etal <- lapply(etas, col_label)
  cols <- sapply(etal, "[[", 1L)
  labs <- sapply(etal, "[[", 2L)
  cols <- unique(cols)
  labs <- unique(labs)
  for(col in cols) {
    require_numeric(x,col)
  }
  GGally::ggpairs(x, ...,
                  columns=cols,
                  columnLabels=labs,
                  upper = list(continuous = pairs_upper),
                  diag = list(continuous = diag),
                  lower = list(continuous = pairs_lower)) + pm_theme()
}

##' @rdname pairs_plot
##' @export
eta_pairs <- function(...) {
  pairs_plot(...)
}
