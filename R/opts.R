
#' Set global plot options
#'
#' @param smooth.lwd line width for smoother
#' @param smooth.col line color for smoother
#' @param smooth.lty line type for smoother
#' @param scatter.size point size for scatter plot
#'
#' @name pm_opts
pm_options <- function(line.col = "#3366FF",
                       smooth.lwd = 1.35,
                       smooth.col = line.col,
                       smooth.lty = 2,
                       smooth.method = "loess",
                       scatter.size = 1.5,
                       density.col = line.col,
                       density.lwd = 1.35,
                       denstiy.lty = 2,
                       hline.lwd = 1.35,
                       hline.col = "darkgrey",
                       abline.lwd = 1.35,
                       abline.col = "darkgrey") {
  set <- function(...) {
    x <- list(...)
    if(length(x)==0) invisible(NULL)
    for(k in names(x)) assign(k,x[[k]],envir=self)
    return(invisible(NULL))
  }
  get <- function(x) {
    self[[x]]
  }
  reset <- function() {
    save <- c("defaults", names(defaults))
    all <- ls(self,all.names=TRUE)
    dump <- setdiff(all,save)
    if(length(dump) > 0) rm(list=dump, envir = self)
    for(k in names(defaults)) assign(k,defaults[[k]],envir=self)
    return(invisible(NULL))
  }
  mget <- function(x) base::mget(x,envir=self)
  self <- environment()
  defaults <- as.list(environment())
  self
}

#' @rdname pm_opts
#' @export
pm_opts <- pm_options()


