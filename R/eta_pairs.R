##' Pairs plot for ETAs using ggpairs
##'
##'
##'
##' @param x plotting data.frame
##' @param etas character col//label for pairs data; see \code{\link{col_label}}
##'
##' @details
##' This funciton requires the \code{GGally} package to be installed.
##'
##' @return
##' The result from a ggpairs call.
##'
##' @examples
##' id <- superset2() %>% filter(EVID==0) %>% distinct(ID, .keep_all = TRUE)
##' eta_pairs(id, c("ETA1//ETA-CL", "ETA2//ETA-VC", "ETA3//ETA-KA"))
##'
##' @export
eta_pairs <- function(x, etas) {
  stopifnot(requireNamespace("GGally"))
  x <- as.data.frame(x)
  etal <- lapply(etas, col_label)
  cols <- sapply(etal, "[[", 1L)
  labs <- sapply(etal, "[[", 2L)
  cols <- unique(cols)
  labs <- unique(labs)
  for(col in cols) require_numeric(x,col)
  GGally::ggpairs(x[,cols], columnLabels=labs)
}
