eta_pairs_fun <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point() +
    geom_smooth(method = "loess", color = .ggblue, lty=2, se = FALSE, lwd = 1.3)

}

eta_pairs_upper <- function(data, mapping, shk = list(), ...) {
  x <- deparse(mapping$x)[1]
  y <- deparse(mapping$y)[1]
  label <- as.character(signif(cor(data[,x],data[,y],use = "complete.obs"), digits=3))

  label <- paste0("Corr: ", label)
  GGally::ggally_text(label = label) +
    theme(panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank())
}



##' Pairs plot for ETAs using ggpairs
##'
##'
##'
##' @param x plotting data.frame
##' @param etas character col//label for pairs data; see \code{\link{col_label}}
##' @param bins passed to \code{geom_histogram}
##' @param alpha passed to \code{geom_histogram}
##' @param fill passed to \code{geom_histogram}
##' @param col passed to \code{geom_histogram}
##' @param shk not used
##'
##' @details
##' This funciton requires the \code{GGally} package to be installed.
##'
##' @return
##' The result from a ggpairs call.
##'
##' @examples
##'
##' df <- dplyr::filter(pmplots_data(), EVID==0)
##' id <- dplyr::distinct(df, ID, .keep_all = TRUE)
##'
##' eta_pairs(id, c("ETA1//ETA-CL", "ETA2//ETA-VC", "ETA3//ETA-KA"))
##'
##' @export
eta_pairs <- function(x, etas, bins = 15, alpha = 0.6, fill = "black",
                      col="grey", shk = list()) {

  if(!requireNamespace("GGally")) {
    stop("this function requires that the GGally package be installed",
         call. = FALSE)
  }
  diag <- GGally::wrap("barDiag", bins = bins, alpha = alpha, fill=fill, col=col)
  x <- as.data.frame(x)
  etal <- lapply(etas, col_label)
  cols <- sapply(etal, "[[", 1L)
  labs <- sapply(etal, "[[", 2L)
  cols <- unique(cols)
  labs <- unique(labs)
  for(col in cols) {
    require_numeric(x,col)
  }
  GGally::ggpairs(x[,cols],
                  columnLabels=labs,
                  upper = list(continuous = eta_pairs_upper, shk = shk),
                  diag = list(continuous = diag),
                  lower = list(continuous = eta_pairs_fun)) + pm_theme()
}
