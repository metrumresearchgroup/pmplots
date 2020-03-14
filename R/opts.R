
#' Set global plot options
#'
#' @param smooth.lwd line width for smoother
#' @param smooth.col line color for smoother
#' @param smooth.lty line type for smoother
#' @param smooth.method method to use for creating smoother
#' @param scatter.size point size for scatter plot
#' @param scatter.col character name of color for scatter plot
#' @param density.lwd line width for density plot on histogram
#' @param density.col line color for density plot on histogram
#' @param density.lty line type for density plot on histogram
#' @param hline.lwd line width for horizontal reference line; see [layer_h]
#' and [gh]
#' @param hline.col line color for horizontal reference line; see [layer_h]
#' and [gh]
#' @param lty line type for horizontal reference line; see [layer_h] and [gh]
#' @param abline.lwd line width for diagonal reference line; see [layer_a]
#' and [ga]
#' @param abline.col line color for diagonal refrence line; see [layer_a]
#' and [ga]
#' @param abline.lty line type for diagonal reference line; see [layer_a]
#' and [ga]
#' @param time.unit default time unit
#' @md
#' @name pm_opts
pm_options <- function(smooth.lwd = 1.35,
                       smooth.col = .ggblue,
                       smooth.lty = 2,
                       smooth.method = "loess",
                       scatter.size = 1.5,
                       scatter.col = "black",
                       density.lwd = 1.35,
                       density.col = .ggblue,
                       density.lty = 2,
                       hline.lwd = 1.35,
                       hline.col = "darkgrey",
                       hline.lty = 1,
                       abline.lwd = 1.35,
                       abline.col = "darkgrey",
                       abline.lty = 1,
                       histogram.fill = "black",
                       histogram.alpha = 0.6,
                       histogram.col = "white",
                       boxplot.fill = "white",
                       boxplot.alpha = 1,
                       boxplot.outlier.shape = 19,
                       qq.color = .ggblue,
                       qq.alpha = 1,
                       pairs.col = "white",
                       pairs.fill = "grey",
                       pairs.alpha = 0.6,
                       time.unit = "hr") {
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
  as.list <- function() {
    ans <- base::as.list.environment(self)
    ans$defaults <- NULL
    ans$self <- NULL
    ans$set <- NULL
    ans$get <- NULL
    ans$reset <- NULL
    ans$mget <- NULL
    ans$as.list <- NULL
    ans
  }
  self <- environment()
  defaults <- base::as.list.environment(environment())
  self
}

#' @rdname pm_opts
#' @export
pm_opts <- pm_options()
#' @rdname pm_opts
#' @export
pm <- pm_opts
opts <- pm_opts

